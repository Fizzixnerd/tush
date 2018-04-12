{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Tush.Path where

import ClassyPrelude
import qualified Language.Tush.Syntax as S
import qualified System.Process.Typed as P
import Text.Printf
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.List as L

pathComponentToFilePath :: S.PathComponent -> FilePath
pathComponentToFilePath (S.PathComponent pc) = unpack pc

filePathToPathComponent :: String -> S.PathComponent
filePathToPathComponent fp = S.PathComponent $ pack fp

pathToFilePath :: MonadIO m => S.Path -> m FilePath
pathToFilePath S.Path {..} =
  let prefix = case _pathRelativity of
        S.Relative -> "./"
        S.Absolute -> "/"
        S.PATH -> ""
      directory = intersperse "/" $ pathComponentToFilePath <$> _pathDirectory
      file = maybe "" (pathComponentToFilePath . fst) _pathFile
      mExtension = do
        f <- _pathFile
        snd f
      extension = maybe "" (unpack . S._unPathExtension) mExtension
      dottedExtension = if null extension
                        then ""
                        else "." ++ extension
      path = prefix ++ concat directory ++ (if not $ null directory then "/" else "") ++ file ++ dottedExtension
  in
    if _pathRelativity == S.PATH
    then do
      (_, p, _) <- P.readProcess $ fromString $ printf "which %s" (unpack path)
      return $ L.init $ BS.unpack p
    else return path

