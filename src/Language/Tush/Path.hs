{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Tush.Path where

import ClassyPrelude
import qualified Language.Tush.Syntax as S
import Language.Tush.Parse
import Control.Lens

filePathToPathComponent :: String -> S.PathComponent
filePathToPathComponent fp = S.PathComponent $ pack fp

relativeFilePathToPath :: FilePath -> Maybe S.Path
relativeFilePathToPath fp =
  case parseWith pathP $ fromString ("./" ++ fp) of
    Left _ -> Nothing
    Right p -> Just p

isAbsolute :: S.Path -> Bool
isAbsolute S.Path {..} = _pathRelativity == S.Absolute

isRelative :: S.Path -> Bool
isRelative S.Path {..} = _pathRelativity == S.Relative

isPATH :: S.Path -> Bool
isPATH S.Path {..} = _pathRelativity == S.PATH

pathJoin :: S.Path -> S.Path -> Maybe S.Path
pathJoin x y =
  if not $ x ^. S.pathIsDirectory
  then Nothing
  else
    let _pathRelativity = if isRelative x then S.Relative else S.Absolute
        _pathIsDirectory = y ^. S.pathIsDirectory
        _pathDirectory = (x ^. S.pathDirectory) <> (y ^. S.pathDirectory)
        _pathFile = y ^. S.pathFile
    in
      Just S.Path {..}

