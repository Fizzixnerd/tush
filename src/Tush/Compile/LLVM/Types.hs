{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Types.hs
-- Author: Matt Walker
-- License: https://opensource.org/licenses/BSD-2-Clause
-- Created: Aug 14, 2017
-- Summary: 

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Tush.Compile.LLVM.Types where

import ClassyPrelude

import Control.Monad.Except
import Control.Monad.State

import Control.Lens

import LLVM.AST as A

import qualified Tush.Syntax as S

import qualified Data.ByteString.Short as BSS
import Data.Maybe

type ModuleName = BSS.ShortByteString
type UnnameGen = Word
type NameTable = Map BSS.ShortByteString Int
data ModuleState = ModuleState { _currentFunction :: Name
                               , _functionTable :: Map Name FunctionState
                               , _unnameGen :: UnnameGen
                               , _functionCount :: Int
                               , _functionNameTable :: NameTable
                               , _definitions :: Vector Definition
                               } deriving (Eq, Show)

data FunctionState = FunctionState { _currentBlock :: Name
                                   , _blockTable :: Map Name BlockState
                                   , _returnType :: Maybe A.Type
                                   , _fargs :: Maybe [(Name, A.Type)]
                                   , _symbolTable :: Map S.Var Operand
                                   , _blockCount :: Int
                                   , _blockNameTable :: NameTable
                                   } deriving (Eq, Show)

data BlockState = BlockState { _idx :: Int
                             , _stack :: [Named Instruction]
                             , _terminator :: Maybe (Named Terminator)
                             } deriving (Eq, Show)

data LlvmState = LlvmState { _currentModule :: ModuleName
                           , _moduleTable :: Map ModuleName ModuleState
                           } deriving (Eq, Show)

makeLenses ''FunctionState
makeLenses ''ModuleState
makeLenses ''BlockState
makeLenses ''LlvmState

newtype Program = Program { unprogram :: Vector A.Module }
  deriving (Eq, Show)

newtype LlvmT m a = LlvmT { runLlvmT :: StateT LlvmState m a }
  deriving (Functor, Applicative, Monad, MonadState LlvmState)

type Llvm a = (ExceptT UnreachableError (LlvmT Identity)) a

data UnreachableError = UnreachableError deriving (Eq, Ord, Show)
instance Exception UnreachableError

int :: A.Type
int = IntegerType 64

float :: A.Type
float = FloatingPointType DoubleFP

bool_ :: A.Type
bool_ = IntegerType 1

char :: A.Type
char = IntegerType 8

btToType :: S.BuiltinType -> A.Type
btToType S.BTInt   = int
btToType S.BTFloat = float 
btToType S.BTBool  = bool_

typeToType :: S.Type -> A.Type
typeToType (S.Type (S.TyADT adt@(S.ADT S.Product _ _))) = StructureType {
    isPacked = True
  , elementTypes = typeToType <$> (adt^.S.adtTypes.to toList)
  }
typeToType (S.Type (S.TyLambda lam)) = FunctionType {
    resultType = lam^.S.lamReturnType.to typeToType
  , argumentTypes = [lam^.S.lamArgType.to typeToType]
  , isVarArg = False
  }
typeToType (S.Type (S.TyBuiltinType bt)) = btToType bt
typeToType (S.Type (S.TyVar v)) = error "Unreachable"

localRef :: Type -> Name -> Operand
localRef = LocalReference

unname :: UnnameGen -> Name
unname u = UnName u

freshUnname :: ( MonadState LlvmState m ) =>
               m Name
freshUnname = do
  mn <- gets $ view currentModule
  moduleTable.at mn._Just.unnameGen += 1
  gets $ view $ moduleTable.at mn.to fromJust.unnameGen.to unname
