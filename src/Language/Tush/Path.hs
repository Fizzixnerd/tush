{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Tush.Path where

import ClassyPrelude
import Language.Tush.Types
import qualified Data.ByteString.Char8 as BS
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP

pathComponentToFilePath :: PathComponent -> FilePath
pathComponentToFilePath (PCLiteral pc) = BS.unpack pc

filePathToPathComponent :: String -> PathComponent
filePathToPathComponent fp = PCLiteral $ BS.pack fp

pathToFilePath :: Path -> FilePath
pathToFilePath Path {..} =
  let prefix = if _pathIsRelative then "./" else "/"
      directory = intersperse "/" $ pathComponentToFilePath <$> _pathDirectory
      file = maybe "" (pathComponentToFilePath . fst) _pathFile
      mExtension = do
        f <- _pathFile
        snd f
      extension = maybe "" (BS.unpack . unPathExtension) mExtension
  in
    prefix ++ concat directory ++ file ++ extension

