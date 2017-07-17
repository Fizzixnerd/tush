{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Tush.Parse.Lex where

import ClassyPrelude

import Data.Char
import qualified Data.Vector as V

import Text.Megaparsec
import Text.Megaparsec.Prim as P
import Text.Megaparsec.Text
import qualified Text.Megaparsec.Lexer as L

newtype Var = Var Text deriving (Eq, Ord, Show, IsString)
data Extern = Extern deriving (Eq, Show)
data Def = Def deriving (Eq, Show)
data BOp = Add | Sub | Mul | Div deriving (Eq, Ord, Show)
data UOp = Neg deriving (Eq, Ord, Show)
data Terminator = Terminator deriving (Eq, Show)

lineComment :: Parser ()
lineComment = L.skipLineComment "--"

blockComment :: Parser ()
blockComment = L.skipBlockComment "{-" "-}"

spaceConsumer :: Parser ()
spaceConsumer = L.space (void spaceChar) lineComment blockComment

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

var :: Parser Var
var = lexeme $ do
  fst <- letterChar
  rst <- many $ satisfy (\c -> isAlphaNum c || c == '_')
  return $ fromString $ fst : rst

integer :: Parser Integer
integer = lexeme L.integer

floating :: Parser Double
floating = lexeme L.float

reserved :: String -> a -> Parser a
reserved r v = do
  void $ symbol r
  return v

extern :: Parser Extern
extern = reserved "extern" Extern

def :: Parser Def
def = reserved "def" Def

neg :: Parser UOp
neg = reserved "-" Neg

add :: Parser BOp
add = reserved "+" Add

sub :: Parser BOp
sub = reserved "-" Sub

mul :: Parser BOp
mul = reserved "*" Mul

div :: Parser BOp
div = reserved "/" Div

terminator :: Parser Terminator
terminator = reserved ";" Terminator

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

commaSep :: Parser a -> Parser (V.Vector a)
commaSep p = fmap fromList $ sepBy p (symbol ",")

semiSep :: Parser a -> Parser (V.Vector a)
semiSep p = fmap fromList $ sepBy p (symbol ";")
