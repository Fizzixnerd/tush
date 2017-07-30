{-# LANGUAGE NoImplicitPrelude #-}

module Tush.Parse.Statement where

import ClassyPrelude

import Text.Megaparsec as MP
import Text.Megaparsec.Text

import qualified Data.Vector as V

import Tush.Parse.Syntax
import Tush.Parse.Lex
import Tush.Parse.Expr

exprS :: Parser (Statement () BuiltinType)
exprS = do
  e <- expr
  void terminator
  return $ ExprS e

definitionS :: Parser (Statement () BuiltinType)
definitionS =  MP.try externS
           <|>        funcS

externS :: Parser (Statement () BuiltinType)
externS = do
  void extern 
  fp <- fProto
  void terminator
  return $ ExternS fp

funcS :: Parser (Statement () BuiltinType)
funcS = do
  void def
  fp <- fProto
  body <- fromList <$> some statement
  case V.last body of
    (ExprS e) -> return $ FuncS fp (V.init body) e
    _ -> fail "Expected last statement in a function definition to be an Expression."

fProto :: Parser (FProto BuiltinType)
fProto = do
  (Var name type' op) <- simplyTypedVar
  args <- parens $ commaSep simplyTypedVar
  return $ FProto (Var name (BTLambda type' ((\(Var _ t _) -> t) <$> args)) op) args

statement :: Parser (Statement () BuiltinType)
statement =  MP.try definitionS
         <|>        exprS

