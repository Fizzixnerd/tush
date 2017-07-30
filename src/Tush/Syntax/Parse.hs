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

module Tush.Syntax.Parse where

import ClassyPrelude

import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set

import qualified Text.Megaparsec as MP
import Text.Megaparsec.Prim

import qualified Tush.Syntax as S
--import Tush.Syntax.Lex
--import Tush.Syntax.Statement

type TushParseError = MP.ParseError Char MP.Dec

type TushParser = Parsec MP.Dec (Vector S.Token)

satisfy :: (MonadParsec e s m, Token s ~ S.Token) => (S.Token -> Bool) -> m S.Token
satisfy f = token test Nothing
  where
    test x =
      if f x then
        Right x
      else
        Left (Set.singleton (Tokens (x NE.:| [])), mempty, mempty)

tokenP :: S.Token ->  TushParser S.Token
tokenP t = satisfy (== t)

varP :: TushParser (S.Var ())
varP = do
  (S.VarT v) <- satisfy S.isVarT
  return v

typeAsP :: TushParser S.ReservedOp
typeAsP = do
  (S.ReservedOpT ta) <- satisfy S.isTypeAsT
  return ta

typeP :: TushParser S.Type
typeP = do
  

typedVarP :: TushParser (S.Var S.Type)
typedVarP = do
  v <- varP
  void $ typeAsP
  t <- typeP
  

openParenP :: TushParser S.ReservedPunctuation
openParenP = do
  (S.ReservedPunctuationT op) <- satisfy S.isOpenParenT
  return op

commaP :: TushParser S.ReservedPunctuation
commaP = do
  (S.ReservedPunctuationT c) <- satisfy S.isCommaT
  return c

fProtoP :: TushParser (S.FProto S.BuiltinType)
fProtoP = do
  name <- typedVarP
  void $ openParenP
  args <- MP.sepBy typedVarP commaP
  void $ closerParenP
  void $ semicolon
  return $ FProto name (fromList args)

externS :: TushParser (S.Statement () S.BuiltinType)
externS = do
  void $ tokenP (S.ReservedWordT S.Extern)
  fp <- fProtoP
  return $ ExternS fp
  
  

-- contents :: Parser a -> Parser a
-- contents p = do
--   spaceConsumer
--   r <- p
--   eof
--   return r

-- toplevel :: Parser (Vector (Statement () BuiltinType))
-- toplevel = fromList <$> many statement  

-- parseStatement :: Text -> Either TushParseError (Statement () BuiltinType)
-- parseStatement s = parse (contents statement) "<stdin>" s

-- parseToplevel :: Text -> Either TushParseError (Vector (Statement () BuiltinType))
-- parseToplevel s = parse (contents toplevel) "<stdin>" s
