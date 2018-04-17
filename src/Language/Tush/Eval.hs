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
import Control.Monad.Catch

applyBuiltin :: MonadIO m => S.Builtin -> S.Expression -> m S.Expression
applyBuiltin S.Builtin {..} = liftIO . _builtinFunc

startEnv :: S.Environment
startEnv = M.fromList [ ("run", S.EBuiltin run)
                      , ("+", S.EBuiltin add)
                      , ("-", S.EBuiltin sub)
                      , ("<", S.EBuiltin lt)
                      , ("fst", S.EBuiltin ft)
                      , ("snd", S.EBuiltin sd)
                      , ("==", S.EBuiltin eq)
                      , ("!", S.EBuiltin idx)
                      , ("ls", S.EBuiltin ls)
                      , ("debugShow", S.EBuiltin debugShow)
                      , ("cd", S.EBuiltin cd)
                      , ("moveTo", S.EBuiltin moveTo)
                      , ("rename", S.EBuiltin rename)
                      , ("copy", S.EBuiltin copy)
                      , ("copyDir", S.EBuiltin copyDir)
                      , ("remove", S.EBuiltin remove)
                      , ("removeDir", S.EBuiltin removeDir)
                      ]


evalE :: S.Environment -> S.Builtin
evalE env = S.Builtin "evalE" $ \case
  S.EName n -> case lookup (S.nameToText n) env of
    Just e  -> return e
    Nothing -> throwM S.UndefinedError
      { S._undefinedErrorName = n
      }
  p@(S.EPath _) -> return p
  s@(S.EString _) -> return s
  b@(S.EBuiltin _) -> return b
  v@(S.EVector _) -> return v
  i@(S.EInt _) -> return i
  b@(S.EBool _) -> return b
  c@(S.EChar _) -> return c
  u@(S.EUnit _) -> return u
  e@(S.EError _) -> return e
  S.ETuple (S.TushTuple (x, y)) -> do
    x' <- applyBuiltin (evalE env) x
    y' <- applyBuiltin (evalE env) y
    return $ S.ETuple $ S.TushTuple (x', y')
  S.EIte (S.Ite i t e) -> do
    i' <- applyBuiltin (evalE env) i
    case i' of
      S.EBool (S.TushBool i'') ->
        if i''
        then applyBuiltin (evalE env) t
        else applyBuiltin (evalE env) e
      x -> throwM S.TypeError
        { S._typeErrorExpected = S.TyBool
        , S._typeErrorActual = S.TyUnknown
        , S._typeErrorExpression = x
        }
  S.EBind S.Bind {..} ->
    case _bindName of
      S.EName n -> do
        let textName = S.nameToText n
        value <- applyBuiltin (evalE env) _bindValue
        let newEnv = M.insert textName value env
        applyBuiltin (evalE newEnv) _bindBody
      x -> throwM S.PatternMatchError
           { S._patternMatchErrorPattern = x }
  S.ELambda S.Lambda {..} ->
    case _lambdaArg of
      S.EName _ -> return $ S.EEnvLambda S.EnvLambda { S._envLambdaArg = _lambdaArg
                                                     , S._envLambdaBody = _lambdaBody
                                                     , S._envLambdaEnv = env
                                                     }
      x -> error $ printf "Lambda argument must be a Name, got %s" (show x)
  l@(S.EEnvLambda _) -> return l
  S.ESequence S.Sequence {..} ->
    V.last <$> mapM (applyBuiltin (evalE env)) _sequenceExpressions
  S.ECall S.Call {..} -> do
    fn <- applyBuiltin (evalE env) _callFunc
    op <- applyBuiltin (evalE env) _callOperand
    case fn of
      S.EBuiltin S.Builtin {..} -> _builtinFunc op
      S.EEnvLambda S.EnvLambda {..} -> do
        let _bindName = _envLambdaArg
            _bindBody = _envLambdaBody
            _bindValue = op
        applyBuiltin (evalE _envLambdaEnv) $ S.EBind S.Bind {..}
      p@(S.EPath S.Path {..}) ->
        case _pathRelativity of
          S.PATH -> do
            p' <- applyBuiltin run p
            applyBuiltin (evalE env) (S.ECall $ S.Call p' op)
          _ -> error $ printf "Could not run non-PATH Path %s" (show p)
      _ -> error $ printf "Could not call %s." (show fn)

evalS :: MonadIO m => S.Environment -> S.Statement -> m S.Environment
evalS env statement =
  case statement of
    S.SAssignment S.Assignment {..} -> do
      let name = S.nameToText _assignmentName
      value <- applyBuiltin (evalE env) _assignmentValue
      return $ M.insertWith (\x y -> error $ printf "Tried to bind %s to %s, but already bound to %s." name (show y) (show x)) name value env

evalEText :: MonadIO m => Text -> m S.Expression
evalEText t =
  let Right parsed = parseE t
  in
    applyBuiltin (evalE startEnv) parsed

evalSText :: MonadIO m => Text -> m S.Environment
evalSText t =
  let Right parsed = parseS t
  in
    evalS startEnv parsed

evalLinearProgramText :: MonadIO m => Text -> m S.Environment
evalLinearProgramText t =
  let Right statements = parseWith (many statementP) t
  in
    foldM evalS startEnv (fromList statements :: Vector S.Statement)
