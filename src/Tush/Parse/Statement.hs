{-# LANGUAGE NoImplicitPrelude #-}

module Tush.Parse.Statement where

import ClassyPrelude

import Text.Megaparsec as MP
import Text.Megaparsec.Text

import Tush.Parse.Syntax
import Tush.Parse.Lex
import Tush.Parse.Expr

exprS :: Parser (Statement ())
exprS = ExprS <$> expr

definitionS :: Parser (Statement ())
definitionS =  MP.try externS
           <|> funcS

externS :: Parser (Statement ())
externS = do
  void extern 
  fp <- fProto
  return $ ExternS fp

funcS :: Parser (Statement ())
funcS = do
  void def
  fp <- fProto
  body <- expr
  return $ FuncS fp body

fProto :: Parser (FProto ())
fProto = do
  (SimplyTypedVar name type') <- simplyTypedVar
  args <- parens $ commaSep simplyTypedVar
  return $ FProto (SimplyTypedVar name (BTLambda type' (stvType <$> args))) args

statement :: Parser (Statement ())
statement =  do
  s <- MP.try definitionS
       <|> exprS
  void terminator
  return s

