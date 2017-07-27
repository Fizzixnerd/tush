{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Tush.Compile.LLVM.CodeGen where

import ClassyPrelude as CP

import qualified Data.ByteString.Short as BSS
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
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
import qualified LLVM.AST.IntegerPredicate as IP

import qualified Tush.Parse.Syntax as S
import Tush.Compile.LLVM.Mangle

--- Type Definitions

type SymbolTable = Map Text (Vector Operand)
type Names = Map BSS.ShortByteString Int

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

data BlockHasNoTerminatorException = BlockHasNoTerminatorException Name deriving (Show, Typeable)

instance Exception BlockHasNoTerminatorException

--- Block Operations

entryBlockName :: BSS.ShortByteString
entryBlockName = "entry"

emptyBlock i = BlockState { 
    idx = i
  , stack = mempty
  , term = Nothing
  }

sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

createBlocks :: CodeGenState -> [BasicBlock]
createBlocks m = fmap (uncurry makeBlock) $ sortBlocks $ M.toList $ blocks m

makeBlock :: Name -> BlockState -> BasicBlock
makeBlock l (BlockState _ s t) = BasicBlock l (reverse s) (makeTerm t)
  where
    makeTerm (Just x) = x
    makeTerm (Nothing) = error $ "Block has no terminator: " ++ show l

--- LLVM Type Definitions

double :: Type
double = FloatingPointType DoubleFP

int :: Type
int = IntegerType 64

boolean :: Type
boolean = IntegerType 1

--- CodeGen Operations

emptyCodeGen :: CodeGenState
emptyCodeGen = CodeGenState (Name entryBlockName) mempty mempty 1 0 mempty

execCodeGen :: CodeGen a -> CodeGenState
execCodeGen m = execState (runCodeGen m) emptyCodeGen

--- Block Operations

runLLVM :: A.Module -> LLVM a -> A.Module
runLLVM mod (LLVM m) = execState m mod

emptyModule :: BSS.ShortByteString -> A.Module
emptyModule label = defaultModule { moduleName = label }

entry :: CodeGen Name
entry = gets currentBlock

--- Take a base name and add a block with returned qualified name.
addBlock :: BSS.ShortByteString -> CodeGen Name
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
  modify $ \s -> s { blocks = M.insert active new (blocks s) }

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

--- LLVM Actions

addDefn :: Definition -> LLVM ()
addDefn d = do
  defs <- gets moduleDefinitions
  modify $ \s -> s { moduleDefinitions = defs <> [d] }

define :: Type -> Name -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
define retType label argTypes body = addDefn $ 
  GlobalDefinition $ functionDefaults {
    name = label
  , parameters = ([Parameter ty nm [] | (ty, nm) <- argTypes], False)
  , returnType = retType
  , basicBlocks = body
  }

external :: Type -> Name -> [(Type, Name)] -> LLVM ()
external retType label argTypes = addDefn $
  GlobalDefinition $ functionDefaults {
    name = label
  , linkage = External
  , parameters = ([Parameter ty nm [] | (ty, nm) <- argTypes], False)
  , returnType = retType
  , basicBlocks = []
  }

--- Operand Operations

uniqueName :: BSS.ShortByteString -> Names -> (BSS.ShortByteString, Names)
uniqueName n ns =
  case M.lookup n ns of
    Nothing -> (n, M.insert n 1 ns)
    Just i -> (n <> fromString (show i), M.insert n (i + 1) ns)

local :: Type -> Name -> Operand
local t = LocalReference t

localDouble :: Name -> Operand
localDouble = local double

localInt :: Name -> Operand
localInt = local int

localBoolean :: Name -> Operand
localBoolean = local boolean

localV :: S.BuiltinType -> Name -> Operand
localV S.BTInt = localInt
localV S.BTFloat = localDouble
localV S.BTBool = localBoolean

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

--- Floating-point Operations

fadd :: Operand -> Operand -> CodeGen Operand
fadd a b = instr $ A.FAdd NoFastMathFlags a b []

add :: Operand -> Operand -> CodeGen Operand
add a b = instr $ A.Add False False a b []

fsub :: Operand -> Operand -> CodeGen Operand
fsub a b = instr $ A.FSub NoFastMathFlags a b []

sub :: Operand -> Operand -> CodeGen Operand
sub a b = instr $ A.Sub False False a b []

fmul :: Operand -> Operand -> CodeGen Operand
fmul a b = instr $ A.FMul NoFastMathFlags a b []

mul :: Operand -> Operand -> CodeGen Operand
mul a b = instr $ A.Mul False False a b []

fdiv :: Operand -> Operand -> CodeGen Operand
fdiv a b = instr $ A.FDiv NoFastMathFlags a b []

div :: Operand -> Operand -> CodeGen Operand
div a b = instr $ A.SDiv False a b []

fcmp :: FPP.FloatingPointPredicate -> Operand -> Operand -> CodeGen Operand
fcmp cond a b = instr $ A.FCmp cond a b [] 

icmp :: IP.IntegerPredicate -> Operand -> Operand -> CodeGen Operand
icmp cond a b = instr $ A.ICmp cond a b []

flt :: Operand -> Operand -> CodeGen Operand
flt a b = fcmp FPP.ULT a b

lt :: Operand -> Operand -> CodeGen Operand
lt a b = icmp IP.SLT a b

--- Control Operations (Terminators)

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

--- Memory Operations

alloca :: Type -> CodeGen Operand
alloca t = instr $ Alloca t Nothing 0 []

store :: Operand -> Operand -> CodeGen Operand
store ptr val = instr $ Store False ptr val Nothing 0 []

load :: Operand -> CodeGen Operand
load ptr = instr $ Load False ptr Nothing 0 []

--- Compilation

codeGenTop :: (S.Statement S.BuiltinType S.BuiltinType) -> LLVM ()
codeGenTop (S.FuncS (S.FProto n@(S.Var name ty _) args) bodyS bodyE) = do
  define (builtinTypeToType $ S.btLambdaReturnType ty) (varToName n) (V.toList fnargs) bls
  void $ forM bodyS codeGenTop
  where
    fnargs = toSig <$> args
    bls = createBlocks $ execCodeGen $ do
      entry <- addBlock entryBlockName
      void $ setBlock entry
      forM args $ \v@(S.Var a t _) -> do
        var <- alloca (builtinTypeToType t)
        store var $ localV t $ snd $ toSig v
        assign a var
      cgen bodyE >>= ret
codeGenTop (S.ExternS (S.FProto n@(S.Var name ty _) args)) = do
  external (builtinTypeToType $ S.btLambdaReturnType ty) (varToName n) (V.toList fnargs)
  where
    fnargs = toSig <$> args
codeGenTop (S.ExprS e) = do
  define double "main" [] blks -- | FIXME: This shouldn't be `double'.
  where
    blks = createBlocks $ execCodeGen $ do
      entry <- addBlock entryBlockName
      void $ setBlock entry
      cgen e >>= ret

varToName :: S.Var a -> Name
varToName (S.Var name _ _) = Name $ textToSBS $ mangle name -- FIXED mangling issue

textToSBS :: Text -> BSS.ShortByteString
textToSBS = fromString . BS.unpack . TE.encodeUtf8

toSig :: S.SimplyTypedVar -> (Type, Name)
toSig (S.Var x t _) = (builtinTypeToType t, Name $ textToSBS x)

builtinTypeToType :: S.BuiltinType -> Type
builtinTypeToType S.BTFloat = double
builtinTypeToType S.BTInt   = int
builtinTypeToType S.BTBool  = boolean
builtinTypeToType _         = error $ "Cannot convert function types yet."

dispatchBinOp :: Vector Operand -> (Operand -> Operand -> CodeGen Operand) -> CodeGen Operand
dispatchBinOp os bop = assert (length os == 2) $ bop (os V.! 0) (os V.! 1)

staticDispatch :: Show a => S.Var a -> Vector S.BuiltinType -> Vector Operand -> CodeGen Operand
staticDispatch (S.Var "+" _ _) ts os 
  | ts == fromList [S.BTInt, S.BTInt] = dispatchBinOp os add
staticDispatch (S.Var ".+" _ _) ts os 
  | ts == fromList [S.BTFloat, S.BTFloat] = dispatchBinOp os fadd
staticDispatch (S.Var "-" _ _) ts os
  | ts == fromList [S.BTInt, S.BTInt] = dispatchBinOp os sub
staticDispatch (S.Var ".-" _ _) ts os
  | ts == fromList [S.BTFloat, S.BTFloat] = dispatchBinOp os fsub
staticDispatch (S.Var "*" _ _) ts os
  | ts == fromList [S.BTInt, S.BTInt] = dispatchBinOp os mul
staticDispatch (S.Var ".*" _ _) ts os
  | ts == fromList [S.BTFloat, S.BTFloat] = dispatchBinOp os fmul
staticDispatch (S.Var "/" _ _) ts os
  | ts == fromList [S.BTInt, S.BTInt] = dispatchBinOp os Tush.Compile.LLVM.CodeGen.div
staticDispatch (S.Var "./" _ _) ts os
  | ts == fromList [S.BTFloat, S.BTFloat] = dispatchBinOp os fdiv
staticDispatch (S.Var "<" _ _) ts os
  | ts == fromList [S.BTInt, S.BTInt] = dispatchBinOp os lt
staticDispatch (S.Var ".<" _ _) ts os
  | ts == fromList [S.BTFloat, S.BTFloat] = dispatchBinOp os flt
staticDispatch op ts _ = error $ "Could not static dispatch op `" ++ 
                         show op ++ 
                         "' with arg types `" ++ 
                         show ts ++ 
                         "'."

false :: Operand
false = constant $ C.Int 1 0

cgen :: (S.Expression S.BuiltinType) -> CodeGen Operand
cgen (S.LitE (S.FLit n) _) = return $ constant $ C.Float $ F.Double n
cgen (S.LitE (S.ILit n) _) = return $ constant $ C.Int 64 n
cgen (S.VarE (S.Var  v _ _) _) = getVar v >>= load 
cgen (S.CallE (S.VarE fn@(S.Var _ _ True) ftype) args _) = do
  largs <- mapM cgen args
  staticDispatch fn (S.btLambdaArgTypes ftype) largs
cgen (S.CallE (S.VarE fn@(S.Var _ _ False) ftype) args _) = do
  largs <- mapM cgen args
  call (externf (builtinTypeToType $ S.btLambdaReturnType ftype) $ varToName fn) (V.toList largs) 
cgen (S.IfE cond conse anted t) = do
  ifthen <- addBlock "if.then"
  ifelse <- addBlock "if.else"
  ifexit <- addBlock "if.exit"

  cond' <- cgen cond
  test <- icmp IP.NE false cond'
  void $ cbr test ifthen ifelse

  void $ setBlock ifthen
  trval <- cgen conse
  void $ br ifexit
  ifthen' <- getBlock

  void $ setBlock ifelse
  flval <- cgen anted
  void $ br ifexit
  ifelse' <- getBlock

  void $ setBlock ifexit
  phi (builtinTypeToType t) [(trval, ifthen'), (flval, ifelse')]

phi :: Type -> [(Operand, Name)] -> CodeGen Operand
phi t ivs = instr $ Phi t ivs []

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

codeGen :: A.Module -> Vector (S.Statement S.BuiltinType S.BuiltinType) -> IO A.Module
codeGen mod fns = withContext $ \context ->
  withModuleFromAST context newast $ \m -> do
    llstr <- moduleLLVMAssembly m
    BS.putStrLn llstr
    return newast
  where
    modn = mapM codeGenTop fns
    newast = runLLVM mod modn
