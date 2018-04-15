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

-- | TODO Change this so that it parses the whole String without regard to Tush
-- syntax.
relativeFilePathToPath :: FilePath -> Maybe S.Path
relativeFilePathToPath fp =
  case parseRawPath $ fromString ("./" ++ fp) of
    Left _ -> Nothing
    Right p -> Just p

relativeFilePathToPath' :: FilePath -> Maybe S.Path
relativeFilePathToPath' fp =
  case parseWith pathP $ fromString ("./" ++ fp) of
    Left _ -> Nothing
    Right p -> Just p

isAbsolute :: S.Path -> Bool
isAbsolute S.Path {..} = _pathRelativity == S.Absolute

isRelative :: S.Path -> Bool
isRelative S.Path {..} = _pathRelativity == S.Relative

isPATH :: S.Path -> Bool
isPATH S.Path {..} = _pathRelativity == S.PATH

isHOME :: S.Path -> Bool
isHOME S.Path {..} = _pathRelativity == S.HOME

pathFileToPathComponent :: Maybe (S.PathComponent, Maybe S.PathExtension)
                        -> Maybe S.PathComponent
pathFileToPathComponent Nothing = Nothing
pathFileToPathComponent (Just (pf, Nothing)) = Just pf
pathFileToPathComponent (Just (S.PathComponent pf, Just (S.PathExtension pe))) =
  Just $ S.PathComponent $ pf <> "." <> pe

pathJoin :: S.Path -> S.Path -> Maybe S.Path
pathJoin x y =
  if not $ x ^. S.pathIsDirectory
  then Nothing
  else
    let _pathRelativity = case x ^. S.pathRelativity of
          S.Relative -> S.Relative
          S.Absolute -> S.Absolute
          S.PATH -> error "Unreachable: Path.hs/pathJoin encountered illegal PATH path for first argument."
          S.HOME -> S.HOME
        _pathIsDirectory = y ^. S.pathIsDirectory
        middleBit = case pathFileToPathComponent $ x ^. S.pathFile of
          Nothing -> fromList []
          Just pf -> fromList [pf]
        _pathDirectory = (x ^. S.pathDirectory) <> middleBit <> (y ^. S.pathDirectory)
        _pathFile = y ^. S.pathFile
    in
      Just S.Path {..}

