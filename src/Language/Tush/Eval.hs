{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Language.Tush.Eval where

import ClassyPrelude
import qualified Language.Tush.Syntax as S
import qualified Data.Map as M
import Text.Printf
import Language.Tush.Builtins

type Environment = Map S.Name S.Expression

applyBuiltin :: S.Builtin -> S.Expression -> IO S.Expression
applyBuiltin S.Builtin {..} = _builtinFunc

startEnv :: Environment
startEnv = M.fromList [ (S.NIdentifier $ S.Identifier "run", S.EBuiltin run) ]

eval :: Environment -> S.Builtin
eval env = S.Builtin "eval" $ \case
  S.EName n -> case lookup n env of
    Nothing -> error $ printf "Could not locate %s in environment." (show n)
    Just e  -> return e
  p@(S.EPath _) -> return p
  s@(S.EString _) -> return s
  b@(S.EBuiltin _) -> return b
  v@(S.EVector _) -> return v
  i@(S.EInt _) -> return i
  S.ECall S.Call {..} -> do
    op_ <- applyBuiltin (eval env) _callOperand
    fn <- applyBuiltin (eval env) _callFunc
    case fn of
      S.EBuiltin S.Builtin {..} -> _builtinFunc op_
      _ -> error $ printf "Could not call function %s." (show fn)

