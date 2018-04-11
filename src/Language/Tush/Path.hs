{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Tush.Path where

import ClassyPrelude
import Language.Tush.Syntax

pathComponentToFilePath :: PathComponent -> FilePath
pathComponentToFilePath (PathComponent pc) = unpack pc

filePathToPathComponent :: String -> PathComponent
filePathToPathComponent fp = PathComponent $ pack fp

pathToFilePath :: Path -> FilePath
pathToFilePath Path {..} =
  let prefix = if _pathIsRelative then "./" else "/"
      directory = intersperse "/" $ pathComponentToFilePath <$> _pathDirectory
      file = maybe "" (pathComponentToFilePath . fst) _pathFile
      mExtension = do
        f <- _pathFile
        snd f
      extension = maybe "" (unpack . _unPathExtension) mExtension
      dottedExtension = if null extension
                        then ""
                        else "." ++ extension
  in
    prefix ++ concat directory ++ file ++ dottedExtension

