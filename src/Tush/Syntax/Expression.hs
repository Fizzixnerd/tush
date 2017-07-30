{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Tush.Parse.Expr where

import ClassyPrelude

import Text.Megaparsec as MP
import Text.Megaparsec.Text

import Tush.Parse.Lex as L
import Tush.Parse.Syntax

iLitE :: Parser (Expression ())
iLitE = (flip LitE ()) <$> ILit <$> integer

fLitE :: Parser (Expression ())
fLitE = (flip LitE ()) <$> FLit <$> floating

bLitE :: Parser (Expression ())
bLitE = (flip LitE ()) <$> BLit <$> boolean

varE :: Parser (Expression ())
varE = (flip VarE ()) <$> var

operatorE :: Parser (Expression ())
operatorE =  (flip VarE ()) <$> operator

varCallE :: Parser (Expression ())
varCallE = do
  name <- varE
  args <- parens $ commaSep expr
  return $ CallE name args ()

opCallE :: Parser (Expression ())
opCallE = do
  l <- term
  op <- operatorE
  r <- term
  return $ CallE op (fromList [l, r]) ()

ifE :: Parser (Expression ())
ifE = do
  void $ if'
  condE <- expr
  void $ then'
  consE <- expr
  void $ else'
  anteE <- expr
  return $ IfE condE consE anteE ()

forE :: Parser (Expression ())
forE = do
  void for'
  forV <- var
  void equals
  initE <- expr
  void comma
  termE <- expr
  void comma
  incrE <- expr
  void in'
  e <- expr
  return $ ForE forV initE termE incrE e ()
  
term :: Parser (Expression ())
term =  MP.try fLitE
    <|> MP.try iLitE
    <|> MP.try bLitE
    <|> MP.try (parens forE)
    <|> MP.try (parens ifE)
    <|> MP.try varCallE
    <|> MP.try varE
    <|>        parens expr

expr =  MP.try opCallE
    <|> MP.try forE
    <|> MP.try ifE
    <|> MP.try term
