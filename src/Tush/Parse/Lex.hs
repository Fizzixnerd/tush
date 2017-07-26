{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Tush.Parse.Lex where

import ClassyPrelude

import Tush.Parse.Syntax

import Data.Char
import qualified Data.Vector as V

import Text.Megaparsec as MP
import Text.Megaparsec.Text
import qualified Text.Megaparsec.Lexer as L

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

var :: Parser (Var ())
var = lexeme $ do
  fs <- letterChar
  rs <- many $ satisfy (\c -> isAlphaNum c || c == '_')
  return $ Var (fromString $ fs : rs) ()

simplyTypedVar :: Parser SimplyTypedVar
simplyTypedVar = do
  (Var v _) <- var
  void $ typeAs
  st <- simpleType
  return $ Var v st

simpleType :: Parser BuiltinType
simpleType =     MP.try intType
             <|> MP.try floatType
             <|>        boolType
             <|> MP.try functionType

intType :: Parser BuiltinType
intType = reserved "Int" BTInt

floatType :: Parser BuiltinType
floatType = reserved "Float" BTFloat

boolType :: Parser BuiltinType
boolType = reserved "Bool" BTBool

functionType :: Parser BuiltinType
functionType = do
  args <- parens $ commaSep simpleType
  void arrow
  retType <- simpleType
  return $ BTLambda retType args

integer :: Parser Integer
integer = lexeme L.integer

true :: Parser Bool
true = do
  void $ symbol "true" 
  return True

false :: Parser Bool
false = do
  void $ symbol "false"
  return False

boolean :: Parser Bool
boolean = MP.try true
          <|>    false

if' :: Parser If
if' = reserved "if" If

then' :: Parser Then
then' = reserved "then" Then

else' :: Parser Else
else' = reserved "else" Else

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

not' :: Parser UOp
not' = reserved "not" Not

add :: Parser BOp
add = reserved "+" Add

sub :: Parser BOp
sub = reserved "-" Sub

mul :: Parser BOp
mul = reserved "*" Mul

div :: Parser BOp
div = reserved "/" Div

lt :: Parser BOp
lt = reserved "<" Lt

or' :: Parser BOp
or' = reserved "||" Or

and' :: Parser BOp
and' = reserved "&&" And

terminator :: Parser Terminator
terminator = reserved ";" Terminator

typeAs :: Parser TypeAs
typeAs = reserved ":" TypeAs

arrow :: Parser Arrow
arrow = reserved "->" Arrow

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

commaSep :: Parser a -> Parser (V.Vector a)
commaSep p = fmap fromList $ sepBy p (symbol ",")

semiSep :: Parser a -> Parser (V.Vector a)
semiSep p = fmap fromList $ sepBy p (symbol ";")
