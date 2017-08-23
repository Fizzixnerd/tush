{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Prelude.hs
-- Author: Matt Walker
-- License: https://opensource.org/licenses/BSD-2-Clause
-- Created: Aug 21, 2017
-- Summary: 

module Tush.Compile.LLVM.Prelude where

import LLVM.AST as A
import LLVM.AST.Constant

import ClassyPrelude

import qualified Data.Map as M

import qualified Tush.Syntax as S
import Tush.Compile.LLVM.Types

prelude :: LlvmState
prelude = LlvmState { _currentModule = "__main"
                    , _moduleTable = M.fromList [( "__main"
                                                 , preludeM )]
                    }

preludeM :: ModuleState
preludeM = ModuleState { _currentFunction = "__main"
                       , _functionTable = M.fromList [( "__main"
                                                      , preludeF )]
                       , _unnameGen = 0
                       , _functionCount = 1
                       , _functionNameTable = M.fromList [( "__main"
                                                          , 1 )]
                       , _definitions = preludeD
                       -- FIXME: make this empty and __main.  Also,
                       -- you shouldn't have this contain a module
                       -- directly, but construct one at exec time!
                       -- This is probably here so that you can make
                       -- global definitions, but just store those in
                       -- a Vector or something.
                       }

preludeF :: FunctionState
preludeF = FunctionState { _currentBlock = A.Name "__main"
                         , _blockTable = M.fromList [( A.Name "__main"
                                                     , preludeB )]
                         , _returnType = Nothing
                         , _fargs = Nothing
                         , _symbolTable = preludeST
                         , _blockCount = 1
                         , _blockNameTable = M.fromList [( "__main"
                                                         , 1 )]
                         }

preludeB :: BlockState
preludeB = BlockState { _idx = 0
                      , _stack = []
                      , _terminator = Nothing
                      }

preludeST :: Map S.Var Operand
preludeST = M.fromList [( S.Var "+" S.VClassOperator
                        , ConstantOperand 
                          (GlobalReference 
                           (FunctionType { resultType = IntegerType 64
                                         , argumentTypes = [ IntegerType 64
                                                           , IntegerType 64 ]
                                         , isVarArg = False })
                            "__add")
                        )]

preludeD :: Vector Definition
preludeD = empty
