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
  p' <- S.pathToFilePath p
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
  S.EPath p -> return $ S.EBuiltin $ S.Builtin "run" $ \case
    S.EVector (S.TushVector args_) -> S.ETuple <$> checkRun p args_
    y -> return $ S.EError $ S.Error $ fromString $ printf "Expected a [String] as the second argument to `run', got %s." (show y)
  x -> return $ S.EError $ S.Error $ fromString $ printf "Expected a Path as the first argument to `run', got %s." (show x)

tushError :: S.Builtin
tushError = S.Builtin "error" $ \case
  S.EString (S.TushString s) -> return $ S.EError $ S.Error s
  x -> return $ S.EError $ S.Error $ fromString $ printf "Expected a String as the first argument to `error', got %s." (show x)

add :: S.Builtin
add = S.Builtin "+" $ \case
  S.EInt x -> return $ S.EBuiltin $ S.Builtin "+" $ \case
    S.EInt y -> return $ S.EInt $ x + y
    y -> return $ S.EError $ S.Error $ fromString $ printf "Expected an Int as the second argument to `+', got %s." (show y)
  x -> return $ S.EError $ S.Error $ fromString $ printf "Expected an Int as the first argument to `+', got %s." (show x)

sub :: S.Builtin
sub = S.Builtin "-" $ \case
  S.EInt x -> return $ S.EBuiltin $ S.Builtin "-" $ \case
    S.EInt y -> return $ S.EInt $ x - y
    y -> return $ S.EError $ S.Error $ fromString $ printf "Expected an Int as the second argument to `-', got %s." (show y)
  x -> return $ S.EError $ S.Error $ fromString $ printf "Expected an Int as the first argument to `-', got %s." (show x)

lt :: S.Builtin
lt = S.Builtin "<" $ \case
  S.EInt x -> return $ S.EBuiltin $ S.Builtin "<" $ \case
    S.EInt y -> return $ S.EBool $ S.TushBool $ x < y
    y -> return $ S.EError $ S.Error $ fromString $ printf "Expected an Int as the second argument to `<', got %s." (show y)
  x -> return $ S.EError $ S.Error $ fromString $ printf "Expected an Int as the first argument to `<', got %s." (show x)

ft :: S.Builtin
ft = S.Builtin "fst" $ \case
  S.ETuple (S.TushTuple (x, _)) -> return x
  t -> return $ S.EError $ S.Error $ fromString $ printf "Expected a Tuple as the argument to `fst', got %s." (show t)

sd :: S.Builtin
sd = S.Builtin "snd" $ \case
  S.ETuple (S.TushTuple (_, y)) -> return y
  t -> return $ S.EError $ S.Error $ fromString $ printf "Expected a Tuple as the argument to `fst', got %s." (show t)

eq :: S.Builtin
eq = S.Builtin "==" $ \case
  S.EInt x -> return $ S.EBuiltin $ S.Builtin "==" $ \case
    S.EInt y -> return $ S.EBool $ S.TushBool $ x == y
    y -> return $ S.EError $ S.Error $ fromString $ printf "Expected an Int as the second argument to `==', got %s." (show y)
  S.EString x -> return $ S.EBuiltin $ S.Builtin "==" $ \case
    S.EString y -> return $ S.EBool $ S.TushBool $ x == y
    y -> return $ S.EError $ S.Error $ fromString $ printf "Expected a String as the second argument to `==', got %s." (show y)
  S.EChar x -> return $ S.EBuiltin $ S.Builtin "==" $ \case
    S.EChar y -> return $ S.EBool $ S.TushBool $ x == y
    y -> return $ S.EError $ S.Error $ fromString $ printf "Expected a Char as the second argument to `==', got %s." (show y)
  x -> return $ S.EError $ S.Error $ fromString $ printf "Expected an Int, Char, or String as the first argument to `==', got %s." (show x)

idx :: S.Builtin
idx = S.Builtin "!" $ \case
  S.EVector (S.TushVector v) -> return $ S.EBuiltin $ S.Builtin "!" $ \case
    S.EInt (S.TushInt j) -> return $ v V.! j
    y -> return $ S.EError $ S.Error $ fromString $ printf "Expected an Int as the second argument to `!', got %s." (show y)
  S.EString (S.TushString s) -> return $ S.EBuiltin $ S.Builtin "!" $ \case
    S.EInt (S.TushInt j) -> return $ S.EChar $ S.TushChar $ s `T.index` j
    y -> return $ S.EError $ S.Error $ fromString $ printf "Expected an Int as the second argument to `!', got %s." (show y)
  x -> return $ S.EError $ S.Error $ fromString $ printf "Expected a String or a Vector as the first argument to `!', got %s." (show x)

ls :: S.Builtin
ls = S.Builtin "ls" $ \case
  S.EPath p ->
    if not $ p ^. S.pathIsDirectory
    then return $ S.EError $ S.Error $ fromString $ printf "Expected a directory as the argument to `ls', got %s. (Did you forget the trailing slash?)" (show p)
    else do
      p' <- S.pathToFilePath p
      contents <- D.listDirectory $ unpack p'
      paths <- forM contents $ \fp -> do
        isDir <- D.doesDirectoryExist $ p' </> fp
        let postfix = if isDir then "/" else ""
        let Just path = relativeFilePathToPath (fp ++ postfix)
            Just path' = pathJoin p path
        return $ S.EPath path'
      return $ S.EVector $ S.TushVector $ fromList paths
  x -> return $ S.EError $ S.Error $ fromString $ printf "Expected a Path as the argument to `ls', got %s." (show x)

debugShow :: S.Builtin
debugShow = S.Builtin "debugShow" $ \s -> return $ S.EString $ S.TushString $ fromString $ show s

cd :: S.Builtin
cd = S.Builtin "cd" $ \case
  S.EPath p ->
    if not $ p ^. S.pathIsDirectory
    then return $ S.EError $ S.Error $ fromString $ printf "Expected a directory as the argument to `cd', got %s. (Did you forget the trailing slash?)" (show p)
    else do
      p' <- S.pathToFilePath p
      D.setCurrentDirectory p'
      return $ S.EUnit S.TushUnit
  x -> return $ S.EError $ S.Error $ fromString $ printf "Expected a directory Path as the argument to `cd', got %s." (show x)

moveTo :: S.Builtin
moveTo = S.Builtin "moveTo" $ \case
  S.EPath p -> return $ S.EBuiltin $ S.Builtin "moveTo" $ \case
    S.EPath q ->
      if not $ q ^. S.pathIsDirectory
      then return $ S.EError $ S.Error $ fromString $ printf "Expected a directory Path as the second argument to `moveTo', got %s." (show q)
      else do
        p' <- S.pathToFilePath p
        q' <- S.pathToFilePath $
          let pf = pathFileToPathComponent $ q ^. S.pathFile
          in
            q & S.pathDirectory %~ (\pd -> case pf of
                                             Nothing -> pd
                                             Just pf' -> V.snoc pd pf')
              & S.pathFile .~ p ^. S.pathFile
              & S.pathIsDirectory .~ False
        D.renamePath p' q'
        return $ S.EUnit S.TushUnit
    y -> return $ S.EError $ S.Error $ fromString $ printf "Expected a directory Path as the second argument to `moveTo', got %s." (show y)
  x -> return $ S.EError $ S.Error $ fromString $ printf "Expected a Path as the first argument to `moveTo', got %s." (show x)

rename :: S.Builtin
rename = S.Builtin "rename" $ \case
  S.EPath p -> return $ S.EBuiltin $ S.Builtin "rename" $ \case
    S.EPath q ->
      if p ^. S.pathIsDirectory /= q ^. S.pathIsDirectory
      then return $ S.EError $ S.Error $ fromString $ printf "Mismatch: Both paths must be either directories or not; got %s and %s." (show p) (show q)
      else do
        p' <- S.pathToFilePath p
        q' <- S.pathToFilePath q
        D.renamePath p' q'
        return $ S.EUnit S.TushUnit
    y -> return $ S.EError $ S.Error $ fromString $ printf "Expected a Path as the second argument to `rename', got %s." (show y)
  x -> return $ S.EError $ S.Error $ fromString $ printf "Expected a Path as the first argument to `rename', got %s." (show x)

copy :: S.Builtin
copy = S.Builtin "copy" $ \case
  S.EPath p -> return $ S.EBuiltin $ S.Builtin "copy" $ \case
    S.EPath q ->
      if p ^. S.pathIsDirectory || q ^. S.pathIsDirectory
      then return $ S.EError $ S.Error $ fromString $ printf "`copy' cannot copy directories (%s to %s); try `copyDir'." (show p) (show q)
      else do
        p' <- S.pathToFilePath p
        q' <- S.pathToFilePath q
        D.copyFile p' q'
        return $ S.EUnit S.TushUnit
    y -> return $ S.EError $ S.Error $ fromString $ printf "Expected a Path as the second argument to `copy', got %s." (show y)
  x -> return $ S.EError $ S.Error $ fromString $ printf "Expected a Path as the first argument to `copy', got %s." (show x)

copyDir :: S.Builtin
copyDir = S.Builtin "copyDir" $ \case
  S.EPath p -> return $ S.EBuiltin $ S.Builtin "copyDir" $ \case
    S.EPath q ->
      if not (p ^. S.pathIsDirectory) || not (q ^. S.pathIsDirectory)
      then return $ S.EError $ S.Error $ fromString $ printf "`copyDir' cannot copy non-directories (%s to %s); try `copy'" (show p) (show q)
      else do
        p' <- S.pathToFilePath p
        q' <- S.pathToFilePath q
        void $ P.readProcess $ fromString $ printf "cp -r '%s' '%s'" p' q'
        return $ S.EUnit S.TushUnit
    y -> return $ S.EError $ S.Error $ fromString $ printf "Expected a Path as the second argument to `copyDir', got %s." (show y)
  x -> return $ S.EError $ S.Error $ fromString $ printf "Expected a Path as the first argument to `copyDir', got %s." (show x)

remove :: S.Builtin
remove = S.Builtin "remove" $ \case
  S.EPath p ->
    if p ^. S.pathIsDirectory
    then return $ S.EError $ S.Error $ fromString $ printf "`remove' cannot remove directories (like %s); try `removeDir'." (show p)
    else do
      p' <- S.pathToFilePath p
      D.removeFile p'
      return $ S.EUnit S.TushUnit
  x -> return $ S.EError $ S.Error $ fromString $ printf "Expected a Path as the argument to `remove', got %s." (show x)

removeDir :: S.Builtin
removeDir = S.Builtin "removeDir" $ \case
  S.EPath p ->
    if not $ p ^. S.pathIsDirectory
    then return $ S.EError $ S.Error $ fromString $ printf "`removeDir' cannot remove non-directories (like %s); try `remove'." (show p)
    else do
      p' <- S.pathToFilePath p
      void $ P.readProcess $ fromString $ printf "rm -rf '%s'" p'
      return $ S.EUnit S.TushUnit
  x -> return $ S.EError $ S.Error $ fromString $ printf "Expected a Path as the argument to `removeDir', got %s." (show x)
