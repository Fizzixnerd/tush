{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Tush.Builtins where

import ClassyPrelude
import qualified System.Process.Typed as P
import qualified Language.Tush.Syntax as S
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified System.Directory as D
import Language.Tush.Path
import System.Exit
import Text.Printf
import Control.Lens

run_ :: MonadIO m => S.Path -> Vector Text -> m (Int, Text, Text)
run_ p args = do
  p' <- pathToFilePath p
  (ec, out, err) <- P.readProcess $ P.proc p' (toList $ unpack <$> args)
  let outString = pack $ BS.unpack out :: Text
      errString = pack $ BS.unpack err :: Text
  return (case ec of
            ExitSuccess -> 0
            ExitFailure n -> n, outString, errString)

checkRun :: MonadIO m => S.Path -> Vector S.Expression -> m S.TushTuple
checkRun p args =
  if all isString args
  then do
    (ec, out, err) <- run_ p (stringToText <$> args)
    return $ S.TushTuple (S.EInt $ S.TushInt ec, S.ETuple $ S.TushTuple (S.EString $ S.TushString out, S.EString $ S.TushString err))
  else error "`run' is of type Path -> [String] -> Int, but was passed things that are not Strings!"
  where
    isString :: S.Expression -> Bool
    isString (S.EString _) = True
    isString _ = False

    stringToText :: S.Expression -> Text
    stringToText (S.EString (S.TushString t)) = t
    stringToText _ = error "Unreachable: checkRun::expStringToText."

run :: S.Builtin
run = S.Builtin "run" $ \case
  (S.EPath p) -> return $ S.EBuiltin $ S.Builtin "run" $ \case
    S.EVector (S.TushVector args_) -> S.ETuple <$> checkRun p args_
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

lt :: S.Builtin
lt = S.Builtin "<" $ \case
  S.EInt x -> return $ S.EBuiltin $ S.Builtin "<" $ \case
    S.EInt y -> return $ S.EBool $ S.TushBool $ x < y
    y -> error $ printf "Expected an Int as the second argument to `<', got %s" (show y)
  x -> error $ printf "Expected an Int as the first argument to `<', got %s" (show x)

ft :: S.Builtin
ft = S.Builtin "fst" $ \case
  S.ETuple (S.TushTuple (x, _)) -> return x
  t -> error $ printf "Expected a Tuple as the argument to `fst', got %s" (show t)

sd :: S.Builtin
sd = S.Builtin "snd" $ \case
  S.ETuple (S.TushTuple (_, y)) -> return y
  t -> error $ printf "Expected a Tuple as the argument to `fst', got %s" (show t)

eq :: S.Builtin
eq = S.Builtin "==" $ \case
  S.EInt x -> return $ S.EBuiltin $ S.Builtin "==" $ \case
    S.EInt y -> return $ S.EBool $ S.TushBool $ x == y
    y -> error $ printf "Expected an Int as the second argument to `==', got %s" (show y)
  S.EString x -> return $ S.EBuiltin $ S.Builtin "==" $ \case
    S.EString y -> return $ S.EBool $ S.TushBool $ x == y
    y -> error $ printf "Expected a String as the second argument to `==', got %s" (show y)
  S.EChar x -> return $ S.EBuiltin $ S.Builtin "==" $ \case
    S.EChar y -> return $ S.EBool $ S.TushBool $ x == y
    y -> error $ printf "Expected a Char as the second argument to `==', got %s" (show y)
  x -> error $ printf "Expected an Int, Char, or String as the first argument to `==', got %s" (show x)

idx :: S.Builtin
idx = S.Builtin "!" $ \case
  S.EVector (S.TushVector v) -> return $ S.EBuiltin $ S.Builtin "!" $ \case
    S.EInt (S.TushInt j) -> return $ v V.! j
    y -> error $ printf "Expected an Int as the second argument to `!', got %s" (show y)
  S.EString (S.TushString s) -> return $ S.EBuiltin $ S.Builtin "!" $ \case
    S.EInt (S.TushInt j) -> return $ S.EChar $ S.TushChar $ s `T.index` j
    y -> error $ printf "Expected an Int as the second argument to `!', got %s" (show y)
  x -> error $ printf "Expected a String or a Vector as the first argument to `!', got %s" (show x)

ls :: S.Builtin
ls = S.Builtin "ls" $ \case
  S.EPath p ->
    if not $ p ^. S.pathIsDirectory
    then error $ printf "Expected a directory as the argument to `ls', got %s" (show p)
    else do
      p' <- pathToFilePath p
      contents <- D.listDirectory $ unpack p'
      paths <- forM contents $ \fp -> do
        isDir <- D.doesDirectoryExist fp
        let postfix = if isDir then "/" else ""
        let Just path =  relativeFilePathToPath (fp ++ postfix)
            Just path' = pathJoin p path
        return $ S.EPath path'
      return $ S.EVector $ S.TushVector $ fromList paths
  x -> error $ printf "Expected a Path as the argument to `ls', got %s" (show x)
