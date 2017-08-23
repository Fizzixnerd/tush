{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- | CodeGen2.hs
-- Author: Matt Walker
-- License: https://opensource.org/licenses/BSD-2-Clause
-- Created: Aug 13, 2017
-- Summary: 

module Tush.Compile.LLVM.CodeGen2 where

import ClassyPrelude hiding (throwM)

import Control.Monad.State
import Control.Monad.Except

import Control.Lens

import LLVM.AST as A
import qualified LLVM.AST.Global as G
import qualified LLVM.AST.AddrSpace as AS
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Type as T
import qualified LLVM.AST.IntegerPredicate as IP

import qualified Tush.Syntax as S
import Tush.Typecheck2
import Tush.Expression
import Tush.Compile.LLVM.Types
import Tush.Compile.LLVM.Instructions
import qualified Tush.Compile.LLVM.Prelude as P

import qualified Data.ByteString.Short as BSS
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Encoding as TE
import qualified Data.Map as M
import Data.Maybe
import qualified Data.List as L

execLlvm :: Llvm a -> (Either UnreachableError a, LlvmState)
execLlvm x = runIdentity $ runStateT (runLlvmT (runExceptT x)) P.prelude

constructProgram :: LlvmState -> Program
constructProgram ls = Program $ fromList $ (uncurry constructModule) <$> (ls^.moduleTable.to M.toList)

constructModule :: ModuleName -> ModuleState -> A.Module
constructModule n ms = defaultModule { moduleName = n
                                     , moduleDefinitions = toList $ (ms^.definitions.to toList) <> (GlobalDefinition . (uncurry constructFunction) <$> (ms^.functionTable.to M.toList))
                                     }

constructFunction :: Name -> FunctionState -> A.Global
constructFunction n fs = functionDefaults { G.name = n
                                          , G.parameters = ([Parameter ty nm [] | (nm, ty) <- fs^.fargs.to fromJust], False)
                                          , G.returnType = fs^.returnType.to fromJust
                                          , G.basicBlocks = (uncurry constructBlock) <$> (fs^.blockTable.to M.toList)
                                          }

constructBlock :: Name -> BlockState -> A.BasicBlock
constructBlock n bs = BasicBlock n (reverse $ bs^.stack) (fromJust $ bs^.terminator)

compileabunch = do
  x <- unifyabunch
  case x of
    Right y -> return $ execLlvm (do
                                     mapM generateFromStatement y :: Llvm (Vector ()))
    Left y -> error $ show y

lookupM :: (MonadError UnreachableError m, Ord k) => k -> M.Map k v -> m v
lookupM k m = do
  case lookup k m of
    Just x -> return x
    Nothing -> throwError UnreachableError

getCurrentModule :: ( MonadState LlvmState m
                    , MonadError UnreachableError m ) =>
                    m ModuleState
getCurrentModule = do
  k <- gets $ view currentModule
  t <- gets $ view moduleTable
  lookupM k t

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState { _idx = i
                          , _stack = empty
                          , _terminator = Nothing
                          }

emptyFunction :: FunctionState
emptyFunction = FunctionState { _currentBlock = Name "__entry"
                              , _blockTable = M.fromList [(Name "__entry", emptyBlock 0)]
                              , _returnType = Nothing
                              , _fargs = Nothing
                              , _symbolTable = M.empty
                              , _blockCount = 0
                              , _blockNameTable = M.empty
                              }

textToSBS :: Text -> BSS.ShortByteString
textToSBS = fromString . BS.unpack . TE.encodeUtf8

varToName :: S.Var -> A.Name
varToName (S.Var x _) = Name $ textToSBS $ x

uniqueName :: BSS.ShortByteString
           -> NameTable
           -> (BSS.ShortByteString, NameTable)
uniqueName n ns =
  case M.lookup n ns of
    Nothing -> (n, M.insert n 1 ns)
    Just i -> (n <> fromString (show i), M.insert n (i + 1) ns)

addBlock :: (MonadState LlvmState m, MonadError UnreachableError m) => 
            BSS.ShortByteString -> m Name
addBlock blockName = do
  mn <- gets $ view currentModule
  fn <- gets $ view $ moduleTable.at mn.to fromJust.currentFunction
  ix' <- gets $ view $ moduleTable.at mn.to fromJust.functionTable.at fn
    .to fromJust.blockCount
  ns <- gets $ view $ moduleTable.at mn.to fromJust.functionTable.at fn
    .to fromJust.blockNameTable
  let newBlock = emptyBlock ix'
      (qName, supply) = uniqueName blockName ns
      fun = moduleTable.at mn._Just.functionTable.at fn._Just
  fun.blockTable %= M.insert (Name qName) newBlock
  fun.blockCount += 1
  fun.blockNameTable .= supply
  return (Name qName)

addFunction :: (MonadState LlvmState m, MonadError UnreachableError m) =>
               Maybe BSS.ShortByteString -> m Name
addFunction mFunctionName = do
  mn <- gets $ view currentModule
  ns <- gets $ view $ moduleTable.at mn.to fromJust.functionNameTable
  let mod_ = moduleTable.at mn._Just
  n <- case mFunctionName of
    Nothing -> freshUnname
    Just n' -> do
      let (qName, supply) = uniqueName n' ns
      mod_.functionNameTable .= supply
      return (Name qName)
  mod_.functionTable %= M.insert n emptyFunction
  mod_.functionCount += 1
  return n

functionSetReturnType :: (MonadState LlvmState m, MonadError UnreachableError m) =>
                         A.Type -> m ()
functionSetReturnType x = do
  mn <- gets $ view currentModule
  fn <- gets $ view $ moduleTable.at mn.to fromJust.currentFunction
  moduleTable.at mn._Just.functionTable.at fn._Just.returnType
    .= Just x

functionSetFargs :: (MonadState LlvmState m, MonadError UnreachableError m) =>
                    [(Name, A.Type)] -> m ()
functionSetFargs xs = do
  mn <- gets $ view currentModule
  fn <- gets $ view $ moduleTable.at mn.to fromJust.currentFunction
  moduleTable.at mn._Just.functionTable.at fn._Just.fargs 
    .= (Just xs)

-- populateSymTable :: (MonadState LlvmState m, MonadError UnreachableError m) =>
--                     S.Statement S.Type S.Type -> m ()
-- populateSymTable (S.ExprS e) = populateSymTableE e

-- populateSymTableE :: (MonadState LlvmState m, MonadError UnreachableError m) =>
--                      S.Expression S.Type -> m ()
-- populateSymTableE (S.LamE (S.VarE v _) body _) = do
--   mn <- gets $ view currentModule
--   fn <- gets $ view $ moduleTable.at mn.to fromJust.currentFunction
--   moduleTable.at mn._Just.functionTable.at fn._Just.symbolTable.at v._Just
--     .= 

symTableAssociate :: (MonadState LlvmState m, MonadError UnreachableError m) =>
                     S.Var -> A.Operand -> m ()
symTableAssociate k v = do
  mn <- gets $ view currentModule
  fn <- gets $ view $ moduleTable.at mn.to fromJust.currentFunction
  moduleTable.at mn._Just.functionTable.at fn._Just.symbolTable 
    %= (M.insert k v)

lFuncType :: A.Type -> A.Type
lFuncType funcType = StructureType True [ PointerType 
                                          funcType
                                          (AS.AddrSpace 0)
                                        , PointerType 
                                          (PointerType 
                                           (IntegerType 8) 
                                           (AS.AddrSpace 0)) 
                                          (AS.AddrSpace 0)
                                        ]

i32 :: Integer -> Operand
i32 = ConstantOperand . C.Int 32

i64 :: Integer -> Operand
i64 = ConstantOperand . C.Int 64

makeLFunc :: (MonadState LlvmState m, MonadError UnreachableError m) =>
             Operand -> [Operand] -> m Operand
makeLFunc f es = do
  let (ConstantOperand (C.GlobalReference (PointerType t@(FunctionType _ ts _) (AS.AddrSpace 0)) _)) = f
      ts' = if null ts then [] else L.tail ts
  lfuncAddr <- malloc1 $ lFuncType t
  funcAddr <- getElementPtr lfuncAddr [i64 0, i32 0]
  void $ store funcAddr f
  env <- malloc (T.ptr T.i8) (fromIntegral $ length ts')
  envAddr <- getElementPtr lfuncAddr [i64 0, i32 1]
  void $ store envAddr env
  void $ forM (zip [0..] es) (\(i, e) -> do
                                 --- Gets to here.
                                 eAddr <- getElementPtr envAddr [i64 0, i64 i]
                                 void $ store eAddr e)
  return lfuncAddr

-- | Call an lfunc: a function with an attached env.  If you try to
-- call something that isn't an lfunc, then god help you.
callLFunc :: (MonadState LlvmState m, MonadError UnreachableError m) =>
             Operand -- | lfunc to call
          -> Operand -- | function's logical argument
          -> m Operand
callLFunc lfunc arg = do
  funcAddr <- getElementPtr lfunc [i64 0, i32 0, i64 0]
  funcArgArrayAddr <- getElementPtr lfunc [i64 0, i32 1, i64 0]

  func <- load funcAddr
  funcArgArray <- load funcArgArrayAddr

  let argTypes = case func of
        (LocalReference (PointerType (FunctionType _ argTypes _) _) _) -> argTypes
        (ConstantOperand (C.GlobalReference (PointerType (FunctionType _ argTypes _) _) _)) -> argTypes
        x -> error (show x)
      -- We want the tail because the first arg is the logical
      -- argument
      indexedArgTypes = zip [0..] (if null argTypes then 
                                     [] 
                                   else 
                                     L.tail argTypes)

  typedArgs <- forM indexedArgTypes (\(i, t) -> do
                                        argAddr <- getElementPtr funcArgArray [i64 0, i64 i]
                                        argAddrTyped <- bitcast argAddr t
                                        return argAddrTyped)
  call func (arg : typedArgs)
  
generateFromStatement :: ( MonadState LlvmState m
                         , MonadError UnreachableError m ) =>
                         S.Statement S.Type S.Type -> m ()
generateFromStatement (S.ExprS e) = do
  void $ generateFromExpression e
  
-- | Generate the LLVM bitcode for an expression.  Functions are the
-- hard part: we have to store their lambda-captured arguments and
-- reload them when the function is called.  We do this by storing
-- functions as a tuple: a function, and an array of pointers to
-- environment args.  The length of the array is encoded in the type
-- of the function.
generateFromExpression :: ( MonadState LlvmState m
                          , MonadError UnreachableError m ) => 
                          S.Expression S.Type -> m Operand
generateFromExpression e@(S.LitE _ _) = do
  case e^.S.exprType of
    (S.Type (S.TyBuiltinType bt)) -> generateLiteralConstant bt (e^.to S._litELiteral)
    _ -> throwError UnreachableError
generateFromExpression e@(S.LamE _ _ _) = do
  mn <- gets $ view currentModule
  -- This is the current function name!
  fn <- gets $ view $ moduleTable.at mn.to fromJust.currentFunction

  -- Get the return type and visible argument type of the function.
  (retT, argT) <- case e^.S.exprType of 
                    (S.Type (S.TyLambda l)) -> return ( l^.S.lamReturnType.to typeToType
                                                      , l^.S.lamArgType.to typeToType )
                    _ -> throwError UnreachableError
  
  st <- gets $ view $ moduleTable.at mn.to fromJust.functionTable
    .at fn.to fromJust.symbolTable
  
  -- Now we find the free variables in the function and arrange to
  -- store those for later loading.
  let freeVars = e^.S.lamEBody.to getFreeVars
      freeOperands = (\(S.VarE var t) -> LocalReference (typeToType t) (varToName var)) <$> freeVars
      freeVarsVars = (\(S.VarE var _ ) -> var) <$> freeVars
      freeNames = (\(LocalReference _ n) -> n) <$> freeOperands
  storedOperands <- mstoreList $ toList freeOperands
  let capturedArgs = (\(LocalReference t n) -> (n, t)) <$> storedOperands

  -- START NEW FUNCTION
  -- Add the new function and switch to it.  Then load the stored
  -- variables into their local names.
  funcName <- addFunction Nothing
  setFunction funcName
  let (S.VarE argName t) = e^.to S._lamEArg
  symTableAssociate argName (LocalReference (typeToType t) (varToName argName))
  fbodyBlockName <- addBlock "fbody"
  functionSetReturnType retT
  functionSetFargs $ [(e^.to S._lamEArg.to S._varEVar.to varToName, argT)] <> capturedArgs

  loadedOperands <- forM (zip (fromList storedOperands) freeNames) (uncurry loadInto)
  void $ forM (zip freeVarsVars loadedOperands) (uncurry symTableAssociate)
  void $ br fbodyBlockName

  setBlock fbodyBlockName
  body <- generateFromExpression $ e^.to S._lamEBody
  void $ ret body

  setFunction fn
  -- END NEW FUNCTION
  
  makeLFunc (ConstantOperand $ C.GlobalReference (PointerType (e^.S.exprType.to typeToType) (AS.AddrSpace 0)) funcName) storedOperands
generateFromExpression e@(S.AppE _ _ _) = do
  f <- generateFromExpression $ e^.to S._appEFunc
  arg <- generateFromExpression $ e^.to S._appEArg
  callLFunc f arg
generateFromExpression e@(S.VarE _ _) = do
  mn <- gets $ view currentModule
  fn <- gets $ view $ moduleTable.at mn.to fromJust.currentFunction
  st <- gets $ view $ moduleTable.at mn.to fromJust.functionTable
    .at fn.to fromJust.symbolTable
  case M.lookup (e^.to S._varEVar) st of
    Nothing -> throwError UnreachableError
    (Just o) -> return o
generateFromExpression e@(S.IfE _ _ _ _) = do
  ifthen <- addBlock "if.then"
  ifelse <- addBlock "if.else"
  ifexit <- addBlock "if.exit"

  cond' <- generateFromExpression $ e^.to S._ifEConditional
  test <- icmp IP.NE false cond'
  void $ cbr test ifthen ifelse
  
  void $ setBlock ifthen
  trueVal <- generateFromExpression $ e^.to S._ifEConsequent
  void $ br ifexit
  ifthen' <- getBlock

  void $ setBlock ifelse
  falseVal <- generateFromExpression $ e^.to S._ifEAntecedent
  void $ br ifexit
  ifelse' <- getBlock

  void $ setBlock ifexit
  phi (typeToType $ e^.S.exprType) [(trueVal, ifthen'), (falseVal, ifelse')]
  
false :: Operand
false = ConstantOperand $ C.Int 1 0

getBlock :: MonadState LlvmState m => m Name
getBlock = do
  mn <- gets $ view currentModule
  fn <- gets $ view $ moduleTable.at mn.to fromJust.currentFunction
  gets $ view $ moduleTable.at mn.to fromJust.functionTable.at fn.to fromJust.currentBlock

setBlock :: MonadState LlvmState m => Name -> m ()
setBlock n = do
  mn <- gets $ view currentModule
  fn <- gets $ view $ moduleTable.at mn.to fromJust.currentFunction
  moduleTable.at mn._Just.functionTable.at fn._Just.currentBlock .= n

setFunction :: MonadState LlvmState m => Name -> m ()
setFunction n = do
  mn <- gets $ view currentModule
  moduleTable.at mn._Just.currentFunction .= n
  
generateLiteralConstant :: ( MonadState LlvmState m
                           , MonadError UnreachableError m ) => 
                           S.BuiltinType -> S.Literal -> m Operand
generateLiteralConstant bt (S.ILit n) = assert (bt == S.BTInt) $
  return $ ConstantOperand $ C.Int 64 n

entryBlockName :: BSS.ShortByteString
entryBlockName = "__entry"

define :: ( MonadState LlvmState m
          , MonadError UnreachableError m ) =>
          Global -> m ()
define = addDefinition . GlobalDefinition

addDefinition :: ( MonadState LlvmState m
                 , MonadError UnreachableError m ) =>
                 Definition -> m ()
addDefinition d = do
  mn <- gets $ view currentModule
  moduleTable.at mn._Just.definitions <>= fromList [d]

makeBlock :: MonadError UnreachableError m => Name -> BlockState -> m BasicBlock
makeBlock l (BlockState _ s t) = do
  t' <- makeTerm t
  return $ BasicBlock l (reverse s) t'
  where
    makeTerm (Just x) = return x
    makeTerm Nothing = throwError UnreachableError

