{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiWayIf #-}

module Tush.Parse.Lex2 where

import ClassyPrelude

import Tush.Parse.Syntax

import qualified Data.Char as C
import qualified Data.Map as M

import qualified Text.Megaparsec as MP
import Text.Megaparsec.Text
import qualified Text.Megaparsec.Lexer as L

lexeme :: Parser a -> Parser a
lexeme p = do
  res <- p
  void $ many MP.spaceChar
  return res

comment :: Parser String
comment = MP.between (void $ MP.string "--") MP.newline (many $ MP.noneOf ['\n'])

commentT :: Parser Token
commentT = CommentT <$> fromString <$> comment

reserved :: String -> a -> Parser a
reserved r v = lexeme $ do
  r' <- MP.string r
  return v

reservedWords :: Map Text ReservedWord
reservedWords = M.fromList [ ("if", If)
                         , ("then", Then)
                         , ("else", Else)
                         , ("for", For)
                         , ("in", In)
                         , ("def", Def)
                         , ("extern", Extern) ]

reservedOps :: Map Text ReservedOp
reservedOps = M.fromList [ (",", Comma)
                      , ("->", Arrow)
                      , (";", Terminator)
                      , (":", TypeAs)
                      , ("=", Equals) ]

varT :: Parser Token
varT = MP.label "Identifier" $ lexeme $ do
  fs <- MP.letterChar
  if | C.isUpper fs -> do -- Uppercase => Type
         rs <- many $ MP.satisfy (\c -> C.isAlphaNum c || c == '_')
         return $ TypeT $ Var (fromString $ fs : rs) () VClassType
     | C.isAlpha fs -> do -- Lowercase => Var || ReservedWord
         rs <- many $ MP.satisfy (\c -> C.isAlphaNum c || c == '_')
         let result = fromString $ fs : rs
         case lookup result reservedWords of
           Nothing -> return $ VarT $ Var result () VClassNormal
           Just rw -> return $ ReservedWordT rw
     | otherwise -> do -- Symbol => Op || ReservedOp
         rs <- many $ MP.symbolChar <|> (MP.oneOf ['.', '*', '/', '<', '>'])
         let result = fromString $ fs : rs
         case lookup result reservedOps of
           Nothing -> return $ OpT $ Var result () VClassOperator
           Just ro -> return $ ReservedOpT ro

literalT :: Parser Token
literalT =  MP.try (fmap (LiteralT . ILit) $ lexeme L.integer)
        <|> MP.try (fmap (LiteralT . FLit) $ lexeme L.float)
        <|>        (fmap (LiteralT . BLit) $ lexeme $ do
                       MP.try (reserved "true" True)
                         <|>  (reserved "false" False))

token :: Parser Token
token =  MP.try commentT
     <|> MP.try literalT
     <|> MP.try varT

tokens :: Parser (Vector Token)
tokens = fromList <$> (many token)
