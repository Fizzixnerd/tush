{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Language.Tush.Eval where

import ClassyPrelude
import qualified Language.Tush.Syntax as S
import qualified Data.Map as M
import qualified Data.Vector as V
import Text.Printf
import Language.Tush.Builtins
import Language.Tush.Parse

applyBuiltin :: S.Builtin -> S.Expression -> IO S.Expression
applyBuiltin S.Builtin {..} = _builtinFunc

startEnv :: S.Environment
startEnv = M.fromList [ ("run", S.EBuiltin run)
                      , ("+", S.EBuiltin add)
                      , ("-", S.EBuiltin sub)
                      , ("<", S.EBuiltin lt)
                      ]

eval :: S.Environment -> S.Builtin
eval env = S.Builtin "eval" $ \case
  S.EName n -> case lookup (S.nameToText n) env of
    Just e  -> return e
    Nothing -> error $ printf "Could not locate %s in environment: %s." (show n) (show env)
  p@(S.EPath _) -> return p
  s@(S.EString _) -> return s
  b@(S.EBuiltin _) -> return b
  v@(S.EVector _) -> return v
  i@(S.EInt _) -> return i
  b@(S.EBool _) -> return b
  S.EIte (S.Ite i t e) -> do
    i' <- applyBuiltin (eval env) i
    case i' of
      S.EBool (S.TushBool i'') ->
        if i''
        then applyBuiltin (eval env) t
        else applyBuiltin (eval env) e
      x -> error $ printf "Expected Bool in if-then-else expression, got %s" (show x)
  S.EBind S.Bind {..} ->
    case _bindName of
      S.EName n -> do
        let textName = S.nameToText n
        value <- applyBuiltin (eval env) _bindValue
        let newEnv = M.insert textName value env
        applyBuiltin (eval newEnv) _bindBody
      x -> error $ printf "Expected a Name for the variable binding, got %s" (show x)
  (S.ELambda S.Lambda {..}) ->
    case _lambdaArg of
      S.EName _ -> return $ S.EEnvLambda S.EnvLambda { S._envLambdaArg = _lambdaArg
                                                     , S._envLambdaBody = _lambdaBody
                                                     , S._envLambdaEnv = env
                                                     }
      x -> error $ printf "Lambda argument must be a Name, got %s" (show x)
  l@(S.EEnvLambda _) -> return l
  S.ESequence S.Sequence {..} ->
    V.last <$> mapM (applyBuiltin (eval env)) _sequenceExpressions
  S.ECall S.Call {..} -> do
    fn <- applyBuiltin (eval env) _callFunc
    op <- applyBuiltin (eval env) _callOperand
    case fn of
      S.EBuiltin S.Builtin {..} -> _builtinFunc op
      S.EEnvLambda S.EnvLambda {..} -> do
        let _bindName = _envLambdaArg
            _bindBody = _envLambdaBody
            _bindValue = op
        applyBuiltin (eval _envLambdaEnv) $ S.EBind S.Bind {..}
      p@(S.EPath S.Path {..}) ->
        case _pathRelativity of
          S.PATH -> do
            p' <- applyBuiltin run p
            applyBuiltin (eval env) (S.ECall $ S.Call p' op)
          _ -> error $ printf "Could not run non-PATH Path %s" (show p)
      _ -> error $ printf "Could not call %s." (show fn)

evalText :: Text -> IO S.Expression
evalText t =
  let Right parsed = parse t
  in
    applyBuiltin (eval startEnv) parsed
