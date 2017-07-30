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


var' :: Parser (Var ())
var' = MP.try operator 
    <|>       var

simplyTypedVar :: Parser SimplyTypedVar
simplyTypedVar = do
  (Var v _ op) <- var
  void $ typeAs
  st <- simpleType
  return $ Var v st op

simpleType :: Parser BuiltinType
simpleType =     MP.try intType
             <|> MP.try floatType
             <|> MP.try boolType -- Used to be switched wrt to the MP.try here... wtf?
             <|>        functionType

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

comma :: Parser Comma
comma = reserved "," Comma

equals :: Parser Equals
equals = reserved "=" Equals

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
