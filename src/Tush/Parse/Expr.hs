{-# LANGUAGE NoImplicitPrelude #-}

module Tush.Parse.Expr where

import ClassyPrelude

import Text.Megaparsec as MP
import Text.Megaparsec.Text
import Text.Megaparsec.Expr

import Tush.Parse.Lex as L
import Tush.Parse.Syntax

iLitE :: Parser Expression
iLitE = LitE <$> ILit <$> integer

fLitE :: Parser Expression
fLitE = LitE <$> FLit <$> floating

varE :: Parser Expression
varE = VarE <$> var

callE :: Parser Expression
callE = do
  name <- var
  args <- parens $ commaSep expr
  return $ CallE name args

ifE :: Parser Expression
ifE = do
  void $ if'
  condE <- expr
  void $ then'
  consE <- expr
  void $ else'
  anteE <- expr
  return $ IfE condE consE anteE

term :: Parser Expression
term =  MP.try fLitE
    <|> MP.try iLitE
    <|> MP.try ifE
    <|> MP.try callE
    <|> varE
    <|> parens expr

opTable :: [[Operator Parser Expression]]
opTable = [ [ prefix neg (\x -> UnOpE Neg x) ]
          , [ binary mul (\x y -> BinOpE Mul x y)
            , binary L.div (\x y -> BinOpE Div x y) ]
          , [ binary add (\x y -> BinOpE Add x y)
            , binary sub (\x y -> BinOpE Sub x y) ]
          , [ binary lt (\x y -> BinOpE Lt x y) ] 
          , [ prefix not' (\x -> UnOpE Not x) ]
          , [ binary and' (\x y -> BinOpE And x y) ]
          , [ binary or' (\x y -> BinOpE Or x y) ] ]

binary :: Parser a -> (b -> b -> b) -> Operator Parser b
binary p f = InfixL $ f <$ p

prefix :: Parser a -> (b -> b) -> Operator Parser b
prefix p f = Prefix $ f <$ p

expr :: Parser Expression
expr = makeExprParser term opTable <?> "expression"
