{-# LANGUAGE NoImplicitPrelude#-}

module Language.Tush where

import ClassyPrelude
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP


pathP = do
  rel <- optional $ MP.char '.'
  let _pathIsRelative = isJust rel
  slash <- MP.char '/'
  _pathDirectory <- fromList <$> some pathComponentP
  undefined

pathComponentP = MP.takeWhile1P (Just "valid path character") $ (not . (== chr '/'))

