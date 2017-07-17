{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Tush.Parse.Expr where

import ClassyPrelude

import Data.Text
import Data.Char
import qualified Data.Vector as V

import Text.Megaparsec
import Text.Megaparsec.Prim as P
import Text.Megaparsec.Text
import qualified Text.Megaparsec.Expr as E

import Tush.Parse.Lex
import Tush.Parse.Syntax

expr :: Parser Expr
expr = makeExprParser
