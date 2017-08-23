{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecordWildCards #-}
-- | Instructions.hs
-- Author: Matt Walker
-- License: https://opensource.org/licenses/BSD-2-Clause
-- Created: Aug 14, 2017
-- Summary: 

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Tush.Compile.LLVM.Instructions where

import ClassyPrelude

import Control.Monad.State

import Control.Lens

import Data.List as L

import LLVM.AST as A
import qualified LLVM.AST.FloatingPointPredicate as FPP
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.ParameterAttribute as PA
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.AddrSpace as AS

import Tush.Compile.LLVM.Types

import Data.Maybe

mallocRef :: Operand
mallocRef = ConstantOperand $ C.GlobalReference mallocType (Name "malloc")
  where
    mallocType = FunctionType (PointerType char (AS.AddrSpace 0)) [int] False

malloc :: MonadState LlvmState m =>
          Type -> Integer -> m Operand
malloc t n = do
  p <- getElementPtr (ConstantOperand $ C.Null t) [ConstantOperand $ C.Int 64 1]
  i <- ptrToInt p
  size <- mul i (ConstantOperand $ C.Int 64 n)
  ptr <- call mallocRef [size]
  bitcast ptr (PointerType t (AS.AddrSpace 0))

malloc1 :: ( MonadState LlvmState m
           ) =>
           Type -> m Operand
malloc1 t = malloc t 1

instr :: ( MonadState LlvmState m
         ) => 
         Maybe Name -> Instruction -> m Operand
instr n ins = do
  mn <- gets $ view currentModule
  fn <- gets $ view $ moduleTable.at mn.to fromJust.currentFunction
  bn <- gets $ view $ moduleTable.at mn.to fromJust.functionTable
        .at fn.to fromJust.currentBlock
  n  <- maybe freshUnname return n
  is <- gets $ view $
        moduleTable.at mn.to fromJust.functionTable.at fn
        .to fromJust.blockTable.at bn.to fromJust.stack
  moduleTable.at mn._Just.functionTable.at fn._Just
    .blockTable.at bn._Just.stack .= ((n := ins) : is)
  case ins of
    -- Integer Ops
    Add {..} -> return $ localRef int n
    Mul {..} -> return $ localRef int n
    Sub {..} -> return $ localRef int n
    SDiv {..} -> return $ localRef int n
    ICmp {..} -> return $ localRef bool_ n
    -- Floating Point Ops
    FAdd {..} -> return $ localRef float n
    FSub {..} -> return $ localRef float n
    -- Memory Ops
    Call {function = (Right (ConstantOperand (C.GlobalReference (FunctionType {resultType = retType}) _)))} -> return $ localRef retType n
    Call {function = (Right (LocalReference (PointerType (FunctionType {resultType = retType}) _) _))} -> return $ localRef retType n
    PtrToInt {..} -> return $ localRef int n
    BitCast {..} -> return $ localRef type' n
    GetElementPtr _ (LocalReference t _) indexes _ -> return $ localRef (gepReturnType t indexes) n
    GetElementPtr _ (ConstantOperand (C.Null { C.constantType = t })) indexes _ -> return $ localRef (gepReturnType (PointerType t (AS.AddrSpace 0)) indexes) n
    GetElementPtr _ (ConstantOperand (C.GlobalReference t _)) indexes _ ->
      return $ localRef (gepReturnType (PointerType t (AS.AddrSpace 0)) indexes) n
    Store _ (LocalReference t _) _ _ _ _ -> return $ localRef t n
    Load _ (LocalReference t _) _ _ _ -> return $ localRef t n
    x -> error (show x)

getElementPtr :: (MonadState LlvmState m) => 
                 Operand -> [Operand] -> m Operand
getElementPtr addr indexes = instr Nothing $ A.GetElementPtr False addr indexes []

gepReturnType :: A.Type -> [Operand] -> A.Type
gepReturnType t [] = (PointerType t (AS.AddrSpace 0))
gepReturnType (StructureType _ ts) ((ConstantOperand (C.Int 32 i)):is) = 
  gepReturnType (ts L.!! (fromIntegral i)) is
gepReturnType (PointerType t _) ((ConstantOperand (C.Int 64 _)):is) =
  gepReturnType t is
-- FIXME: This should handle non-constant operands for the pointer
-- case.
gepReturnType x y = error ((show x) <> (show y))

ptrToInt :: (MonadState LlvmState m) => Operand -> m Operand
ptrToInt p = instr Nothing $ A.PtrToInt p int []

bitcast :: (MonadState LlvmState m) => Operand -> A.Type -> m Operand
bitcast x t = instr Nothing $ A.BitCast x t []

fadd :: ( MonadState LlvmState m) => Operand -> Operand -> m Operand
fadd a b = instr Nothing $ A.FAdd NoFastMathFlags a b []

add :: ( MonadState LlvmState m) => Operand -> Operand -> m Operand
add a b = instr Nothing $ A.Add False False a b []

fsub :: ( MonadState LlvmState m) => Operand -> Operand -> m Operand
fsub a b = instr Nothing $ A.FSub NoFastMathFlags a b []

sub :: ( MonadState LlvmState m) => Operand -> Operand -> m Operand
sub a b = instr Nothing $ A.Sub False False a b []

fmul :: ( MonadState LlvmState m) => Operand -> Operand -> m Operand
fmul a b = instr Nothing $ A.FMul NoFastMathFlags a b []

mul :: ( MonadState LlvmState m) => Operand -> Operand -> m Operand
mul a b = instr Nothing $ A.Mul False False a b []

fdiv :: ( MonadState LlvmState m) => Operand -> Operand -> m Operand
fdiv a b = instr Nothing $ A.FDiv NoFastMathFlags a b []

div :: ( MonadState LlvmState m) => Operand -> Operand -> m Operand
div a b = instr Nothing $ A.SDiv False a b []

fcmp :: ( MonadState LlvmState m) => FPP.FloatingPointPredicate -> Operand -> Operand -> m Operand
fcmp cond a b = instr Nothing $ A.FCmp cond a b [] 

icmp :: ( MonadState LlvmState m) => IP.IntegerPredicate -> Operand -> Operand -> m Operand
icmp cond a b = instr Nothing $ A.ICmp cond a b []

flt :: ( MonadState LlvmState m) => Operand -> Operand -> m Operand
flt a b = fcmp FPP.ULT a b

lt :: ( MonadState LlvmState m) => Operand -> Operand -> m Operand
lt a b = icmp IP.SLT a b

--- Control Operations (Terminators)

term :: (MonadState LlvmState m) => 
        Named Terminator -> m (Named Terminator)
term trm = do
  mn <- gets $ view currentModule
  fn <- gets $ view $ moduleTable.at mn.to fromJust.currentFunction
  bn <- gets $ view $ moduleTable.at mn.to fromJust.functionTable.at fn.to fromJust.currentBlock
  moduleTable.at mn._Just.functionTable.at fn._Just.blockTable.at bn._Just.terminator .= (Just trm)
  return trm

br :: ( MonadState LlvmState m) => Name -> m (Named Terminator)
br val = term $ Do $ Br val []

cbr :: ( MonadState LlvmState m) => Operand -> Name -> Name -> m (Named Terminator)
cbr cond tr fl = term $ Do $ CondBr cond tr fl []

ret :: ( MonadState LlvmState m) => Operand -> m (Named Terminator)
ret val = term $ Do $ Ret (Just val) []

toArg :: Operand -> (Operand, [PA.ParameterAttribute])
toArg arg = (arg, [])

call :: ( MonadState LlvmState m) => Operand -> [Operand] -> m Operand
call fn args = instr Nothing $ Call Nothing CC.C [] (Right fn) (toArg <$> args) [] []

--- Weird Stuff

phi :: (MonadState LlvmState m) => Type -> [(Operand, Name)] -> m Operand
phi t ivs = instr Nothing $ Phi t ivs []

--- Memory Operations

alloca :: ( MonadState LlvmState m) => Type -> m Operand
alloca t = instr Nothing $ Alloca t Nothing 0 []

store :: ( MonadState LlvmState m) => Operand -> Operand -> m Operand
store ptr val = instr Nothing $ Store False ptr val Nothing 0 []

load :: ( MonadState LlvmState m) => Operand -> m Operand
load ptr = instr Nothing $ Load False ptr Nothing 0 []

loadInto :: (MonadState LlvmState m) => 
            Operand -> Name -> m Operand
loadInto ptr n = instr (Just n) $ Load False ptr Nothing 0 []

ptrTo :: ( MonadState LlvmState m
         ) =>
         Operand -> m Operand
ptrTo (LocalReference t _) = malloc1 t
ptrTo (ConstantOperand (C.GlobalReference t _)) = malloc1 t

-- | Take a list of operand and return a list of pointers to where
-- they are now stored.
mstoreList :: ( MonadState LlvmState m
             ) =>
             [Operand] -> m [Operand]
mstoreList os = forM os mstore

-- | Store an operand and return the allocated pointer to where it's
-- stored.
mstore :: ( MonadState LlvmState m
          ) =>
          Operand -> m Operand
mstore val = do
  ptr <- ptrTo val
  void $ store ptr val
  return ptr
