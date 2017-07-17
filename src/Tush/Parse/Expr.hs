{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Tush.Parse.Expr where

import ClassyPrelude

import Data.Text
import Data.Char
import qualified Data.Vector as V

import Text.Megaparsec as MP
import Text.Megaparsec.Prim as P
import Text.Megaparsec.Text
import Text.Megaparsec.Expr

import Tush.Parse.Lex as L
import Tush.Parse.Syntax

iLitE :: Parser Expr
iLitE = LitE <$> ILit <$> integer

fLitE :: Parser Expr
fLitE = LitE <$> FLit <$> floating

callE :: Parser Expr
callE = do
  name <- var
  args <- parens $ commaSep expr
  return $ CallE name args

varE :: Parser Expr
varE = VarE <$> var

exprS :: Parser Statement
exprS = ExprS <$> expr

statement :: Parser Statement
statement =  do
  s <- MP.try definitionS
       <|> exprS
  void terminator
  return s

definitionS :: Parser Statement
definitionS =  MP.try externS
           <|> funcS

fProto :: Parser FProto
fProto = do
  name <- var
  args <- parens $ commaSep var
  return $ FProto name args

externS :: Parser Statement
externS = do
  void extern 
  fp <- fProto
  return $ ExternS fp

funcS :: Parser Statement
funcS = do
  void def
  fp <- fProto
  body <- expr
  return $ FuncS fp body

-- Doesn't yet include funcalls
term :: Parser Expr
term =  MP.try fLitE
    <|> MP.try iLitE
    <|> MP.try callE
    <|> varE
    <|> parens expr

opTable :: [[Operator Parser Expr]]
opTable = [[ prefix neg (\x -> UnOpE Neg x) ]
          ,[ binary mul (\x y -> BinOpE Mul x y)
           , binary L.div (\x y -> BinOpE Div x y) ]
          ,[ binary add (\x y -> BinOpE Add x y)
           , binary sub (\x y -> BinOpE Sub x y) ]]

binary :: Parser a -> (b -> b -> b) -> Operator Parser b
binary p f = InfixL $ f <$ p

prefix :: Parser a -> (b -> b) -> Operator Parser b
prefix p f = Prefix $ f <$ p

expr :: Parser Expr
expr = makeExprParser term opTable <?> "expression"
