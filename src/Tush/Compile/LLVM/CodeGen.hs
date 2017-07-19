{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Tush.Compile.LLVM.CodeGen where

import ClassyPrelude as CP

import Data.ByteString.Short as BSS
import qualified Data.ByteString.Char8 as BS
import Data.Map as M
import qualified Data.Vector as V
import qualified Data.Text.Encoding as TE
import Control.Monad.State
import Control.Monad.Except

import LLVM.AST as A
import LLVM.AST.Global as G
import LLVM.AST.Linkage as L
import LLVM.AST.Constant as C
import LLVM.AST.CallingConvention as CC
import LLVM.AST.ParameterAttribute as PA
import LLVM.AST.Float as F
import LLVM.Context
import LLVM.Module
import qualified LLVM.AST.FloatingPointPredicate as FPP

import qualified Tush.Parse.Syntax as S

type SymbolTable = Map Text (Vector Operand)
type Names = Map ShortByteString Int

data CodeGenState = CodeGenState {
    currentBlock :: Name
  , blocks :: Map Name BlockState
  , symtab :: SymbolTable
  , blockCount :: Int
  , count :: Word
  , names :: Names
  } deriving Show

data BlockState = BlockState {
    idx :: Int
  , stack :: [Named Instruction]
  , term :: Maybe (Named Terminator)
  } deriving Show

newtype CodeGen a = CodeGen { runCodeGen :: State CodeGenState a }
  deriving (Functor, Applicative, Monad, MonadState CodeGenState)

newtype LLVM a = LLVM (State A.Module a)
  deriving (Functor, Applicative, Monad, MonadState A.Module)

sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

createBlocks :: CodeGenState -> [BasicBlock]
createBlocks m = fmap (uncurry makeBlock) $ sortBlocks $ M.toList $ blocks m

double :: Type
double = FloatingPointType DoubleFP

runLLVM :: A.Module -> LLVM a -> A.Module
runLLVM mod (LLVM m) = execState m mod

emptyCodeGen = CodeGenState (Name entryBlockName) mempty mempty 1 0 mempty

execCodeGen :: CodeGen a -> CodeGenState
execCodeGen m = execState (runCodeGen m) emptyCodeGen

entryBlockName :: ShortByteString
entryBlockName = "entry"

emptyModule :: ShortByteString -> A.Module
emptyModule label = defaultModule { moduleName = label }

emptyBlock i = BlockState { 
    idx = i
  , stack = mempty
  , term = Nothing
  }

makeBlock :: Name -> BlockState -> BasicBlock
makeBlock l (BlockState _ s t) = BasicBlock l (reverse s) (makeTerm t)
  where
    makeTerm (Just x) = x
    makeTerm (Nothing) = error $ "Block has no terminator: " ++ show l
  
addDefn :: Definition -> LLVM ()
addDefn d = do
  defs <- gets moduleDefinitions
  modify $ \s -> s { moduleDefinitions = defs <> [d] }

define :: Type -> ShortByteString -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
define retType label argTypes body = addDefn $ 
  GlobalDefinition $ functionDefaults {
    name = Name label
  , parameters = ([Parameter ty nm [] | (ty, nm) <- argTypes], False)
  , returnType = retType
  , basicBlocks = body
  }

external :: Type -> ShortByteString -> [(Type, Name)] -> LLVM ()
external retType label argTypes = addDefn $
  GlobalDefinition $ functionDefaults {
    name = Name label
  , linkage = External
  , parameters = ([Parameter ty nm [] | (ty, nm) <- argTypes], False)
  , returnType = retType
  , basicBlocks = []
  }

entry :: CodeGen Name
entry = gets currentBlock

addBlock :: ShortByteString -> CodeGen Name
addBlock blockName = do
  bs <- gets blocks
  ix <- gets blockCount
  ns <- gets names
  let newBlock = emptyBlock ix
      (qName, supply) = uniqueName blockName ns

  modify $ \s -> s { blocks = M.insert (Name qName) newBlock bs
                   , blockCount = ix + 1
                   , names = supply
                   }
  return (Name qName)

setBlock :: Name -> CodeGen Name
setBlock blockName = do
  modify $ \s -> s { currentBlock = blockName }
  return blockName
  
getBlock :: CodeGen Name
getBlock = gets currentBlock

modifyBlock :: BlockState -> CodeGen ()
modifyBlock new = do
  active <- getBlock
  modify $ \s -> s { blocks = insert active new (blocks s) }

current :: CodeGen BlockState
current = do
  c <- getBlock
  blks <- gets blocks
  case M.lookup c blks of
    Just x -> return x
    Nothing -> error $ "No such block: " <> show c

fresh :: CodeGen Word
fresh = do
  i <- gets count
  modify $ \s -> s { count = i + 1 }
  return $ i + 1

uniqueName :: ShortByteString -> Names -> (ShortByteString, Names)
uniqueName n ns =
  case M.lookup n ns of
    Nothing -> (n, M.insert n 1 ns)
    Just i -> (n <> fromString (show i), M.insert n (i + 1) ns)

local :: Type -> Name -> Operand
local t = LocalReference t

localDouble :: Name -> Operand
localDouble = local double

constant :: C.Constant -> Operand
constant = ConstantOperand

uitofp :: Type -> Operand -> CodeGen Operand
uitofp t a = instr $ A.UIToFP a t []

externf :: Type -> Name -> Operand
externf t = ConstantOperand . GlobalReference t

assign :: Text -> Operand -> CodeGen ()
assign var x = do
  locals <- gets symtab
  modify $ \s -> s { symtab = M.insertWith (<>) var (CP.fromList [x]) locals }

getVar :: Text -> CodeGen Operand
getVar var = do
  locals <- gets symtab
  case M.lookup var locals of
    Just xs -> return (V.head xs)
    Nothing -> error $ "Local variable not in scope: " <> show var

instr :: Instruction -> CodeGen Operand
instr ins = do
  n <- fresh
  let ref = UnName n
  blk <- current
  let is = stack blk
  modifyBlock (blk { stack = (ref := ins) : is })
  return $ localDouble ref

terminator :: Named Terminator -> CodeGen (Named Terminator)
terminator trm = do
  blk <- current
  modifyBlock (blk { term = Just trm })
  return trm

fadd :: Operand -> Operand -> CodeGen Operand
fadd a b = instr $ A.FAdd NoFastMathFlags a b []

fsub :: Operand -> Operand -> CodeGen Operand
fsub a b = instr $ A.FSub NoFastMathFlags a b []

fmul :: Operand -> Operand -> CodeGen Operand
fmul a b = instr $ A.FMul NoFastMathFlags a b []

fdiv :: Operand -> Operand -> CodeGen Operand
fdiv a b = instr $ A.FDiv NoFastMathFlags a b []

fcmp :: FPP.FloatingPointPredicate -> Operand -> Operand -> CodeGen Operand
fcmp cond a b = instr $ A.FCmp cond a b [] 

lt :: Operand -> Operand -> CodeGen Operand
lt a b = do
  test <- fcmp FPP.ULT a b
  uitofp double test

br :: Name -> CodeGen (Named Terminator)
br val = terminator $ Do $ Br val []

cbr :: Operand -> Name -> Name -> CodeGen (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

ret :: Operand -> CodeGen (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []

toArg :: Operand -> (Operand, [PA.ParameterAttribute])
toArg arg = (arg, [])

call :: Operand -> [Operand] -> CodeGen Operand
call fn args = instr $ Call Nothing CC.C [] (Right fn) (toArg <$> args) [] []

alloca :: Type -> CodeGen Operand
alloca t = instr $ Alloca t Nothing 0 []

store :: Operand -> Operand -> CodeGen Operand
store ptr val = instr $ Store False ptr val Nothing 0 []

load :: Operand -> CodeGen Operand
load ptr = instr $ Load False ptr Nothing 0 []

codeGenTop :: S.Statement -> LLVM ()
codeGenTop (S.FuncS (S.FProto (S.Var name) args) body) = do
  define double (textToSBS name) (V.toList fnargs) bls
  where
    fnargs = toSig <$> args
    bls = createBlocks $ execCodeGen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      forM args $ \(S.Var a) -> do
        var <- alloca double
        store var $ localDouble $ (snd $ toSig (S.Var a))
        assign a var
      cgen body >>= ret
codeGenTop (S.ExternS (S.FProto (S.Var name) args)) = do
  external double (textToSBS name) (V.toList fnargs)
  where
    fnargs = toSig <$> args
codeGenTop (S.ExprS e) = do
  define double "main" [] blks
  where
    blks = createBlocks $ execCodeGen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      cgen e >>= ret

varToName :: S.Var -> Name
varToName (S.Var x) = Name $ textToSBS x

textToSBS :: Text -> ShortByteString
textToSBS = fromString . BS.unpack . TE.encodeUtf8

toSig :: S.Var -> (Type, Name)
toSig x = (double, varToName x)

binops :: Map S.BOp (Operand -> Operand -> CodeGen Operand)
binops = M.fromList [
    (S.Add, fadd)
  , (S.Sub, fsub)
  , (S.Mul, fmul)
  , (S.Div, fdiv)
  , (S.Lt, lt  )
  ]

cgen :: S.Expr -> CodeGen Operand
cgen (S.LitE  (S.FLit n)) = return $ constant $ C.Float (F.Double n)
cgen (S.LitE  (S.ILit n)) = return $ constant $ C.Float (F.Double (fromIntegral n))
cgen (S.VarE  (S.Var  v)) = getVar v >>= load
cgen (S.CallE fn args)    = do
  largs <- mapM cgen args
  call (externf double $ varToName fn) (V.toList largs)
cgen (S.BinOpE op a b) = do
  case M.lookup op binops of
    Just f -> do
      ca <- cgen a
      cb <- cgen b
      f ca cb
    Nothing -> error "No such operator."

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

codeGen :: A.Module -> Vector S.Statement -> IO A.Module
codeGen mod fns = withContext $ \context ->
  withModuleFromAST context newast $ \m -> do
    llstr <- moduleLLVMAssembly m
    BS.putStrLn llstr
    return newast
  where
    modn = mapM codeGenTop fns
    newast = runLLVM mod modn
