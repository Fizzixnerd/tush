{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Tush.Builtins where

import ClassyPrelude
import qualified System.Process.Typed as P
import qualified Language.Tush.Syntax as S
import Language.Tush.Path
import System.Exit
import Text.Printf

run_ :: MonadIO m => S.Path -> Vector Text -> m Int
run_ p args = do
  p' <- pathToFilePath p
  ec <- P.runProcess $ P.proc p' (toList $ unpack <$> args)
  case ec of
    ExitSuccess -> return 0
    ExitFailure n -> return n

checkRun :: MonadIO m => S.Path -> Vector S.Expression -> m S.TushInt
checkRun p args =
  if all isString args
  then S.TushInt <$> run_ p (expStringToText <$> args)
  else error "`run' is of type Path -> [String] -> ExitCode, but was passed things that are not Strings!"
  where
    isString :: S.Expression -> Bool
    isString (S.EString _) = True
    isString _ = False

    expStringToText :: S.Expression -> Text
    expStringToText (S.EString (S.TushString t)) = t
    expStringToText _ = error "Unreachable: checkRun::expStringToText."

run :: S.Builtin
run = S.Builtin "run" $ \case
  (S.EPath p) -> return $ S.EBuiltin $ S.Builtin "run" $ \case
    S.EVector (S.TushVector args_) -> S.EInt <$> checkRun p args_
    x -> error $ printf "Expected a [String] as the second argument to `run', got %s" (show x)
  _ -> error "Expected a Path as the first argument to `run'."

add :: S.Builtin
add = S.Builtin "+" $ \case
  S.EInt x -> return $ S.EBuiltin $ S.Builtin "+" $ \case
    S.EInt y -> return $ S.EInt $ x + y
    y -> error $ printf "Expected an Int as the second argument to `+', got %s." (show y)
  x -> error $ printf "Expected an Int as the first argument to `+', got %s" (show x)

sub :: S.Builtin
sub = S.Builtin "-" $ \case
  S.EInt x -> return $ S.EBuiltin $ S.Builtin "-" $ \case
    S.EInt y -> return $ S.EInt $ x - y
    y -> error $ printf "Expected an Int as the second argument to `-', got %s." (show y)
  x -> error $ printf "Expected an Int as the first argument to `-', got %s" (show x)
