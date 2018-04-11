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
import Language.Tush.Parse

type Environment = Map Text S.Expression

applyBuiltin :: S.Builtin -> S.Expression -> IO S.Expression
applyBuiltin S.Builtin {..} = _builtinFunc

startEnv :: Environment
startEnv = M.fromList [ ("run", S.EBuiltin run)
                      , ("+", S.EBuiltin add)]

eval :: Environment -> S.Builtin
eval env = S.Builtin "eval" $ \case
  S.EName n -> case lookup (S.nameToText n) env of
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

evalText :: Text -> IO S.Expression
evalText t =
  let Right parsed = parse t
  in
    applyBuiltin (eval startEnv) parsed
