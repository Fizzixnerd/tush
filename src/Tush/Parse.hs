-- | Parse.hs
-- Author: Matt Walker
-- License: https://opensource.org/licenses/BSD-2-Clause
-- Created: Mar 14, 2017
-- Summary: 

{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module Tush.Parse where

import ClassyPrelude

import Text.Megaparsec
import Text.Megaparsec.Text

import Tush.Parse.Syntax
import Tush.Parse.Lex
import Tush.Parse.Statement

type TushParseError = ParseError Char Dec

contents :: Parser a -> Parser a
contents p = do
  spaceConsumer
  r <- p
  eof
  return r

toplevel :: Parser (Vector Statement)
toplevel = fromList <$> many statement  

parseStatement :: Text -> Either TushParseError Statement
parseStatement s = parse (contents statement) "<stdin>" s

parseToplevel :: Text -> Either TushParseError (Vector Statement)
parseToplevel s = parse (contents toplevel) "<stdin>" s

-- import Control.Applicative as Ap
-- import Control.Arrow
-- import Control.Monad
-- import Data.Monoid

-- import Data.Functor
-- import qualified Data.Text as T
-- import qualified Data.Map as M
-- import qualified Data.Vector as V

-- import Data.Char

-- import Control.Monad.Identity

-- import Text.Parsec as PC
-- import Text.Parsec.Text
-- import Text.Parsec.Char as PCC

-- newtype RelativePath = RelativePath PathBody deriving (Eq, Show)
-- newtype AbsolutePath = AbsolutePath PathBody deriving (Eq, Show)
-- type PathBody = [PathComponent]
-- type PathComponent = T.Text

-- text :: Stream s m Char => T.Text -> ParsecT s u m T.Text
-- text t = T.pack <$> (string $ T.unpack t)

-- unquotedPathComponent :: Parser PathComponent
-- unquotedPathComponent = T.pack <$> (PC.many
--                                      (satisfy (\c -> 
--                                                  not (isSpace c || c == pathSeparatorChar)) 
--                                        <?> "non-space non-separator."))

-- unquotedPathBody :: Parser PathBody
-- unquotedPathBody = unquotedPathComponent `sepBy1` pathSeparator
  
-- pathSeparatorChar :: Char
-- pathSeparatorChar = '/'

-- pathSeparator :: Parser ()
-- pathSeparator = do
--   char pathSeparatorChar <?> "path separator (i.e., /)."
--   return ()

-- relativePathStarterText :: T.Text
-- relativePathStarterText = "./"

-- relativePathStarter :: Parser ()
-- relativePathStarter = do
--   text relativePathStarterText <?> "relative path starter (i.e., ./)."
--   return ()

-- unquotedRelativePath :: Parser RelativePath
-- unquotedRelativePath = RelativePath <$> (relativePathStarter >> unquotedPathBody)
                                         
-- testUnquotedRealtivePath = sequence $ parseTest unquotedRelativePath <$> [ "./home/matt/whatup"
--                                                                          , "./home/matt/whatup/"
--                                                                          , "home/matt"
--                                                                          , "./home/matt whatup"]
