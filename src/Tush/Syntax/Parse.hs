-- | Parse.hs
-- Author: Matt Walker
-- License: https://opensource.org/licenses/BSD-2-Clause
-- Created: Mar 14, 2017
-- Summary: 

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

module Tush.Syntax.Parse where

import ClassyPrelude as CP

import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set

import qualified Text.Megaparsec as MP
import Text.Megaparsec.Text
import Text.Megaparsec.Prim as Prim
import Text.Megaparsec.Pos

import qualified Tush.Syntax as S
import qualified Tush.Syntax.Lex as L
--import Tush.Syntax.Statement

type TushParseError = MP.ParseError S.Token MP.Dec

type TushParser = Parsec MP.Dec (Vector S.Token)

instance Ord s => Stream (Vector s) where
  type Token (Vector s) = s
  uncons = CP.uncons
  updatePos ps p sp t = (sp, sp { sourceColumn = unsafePos $ (unPos $ sourceColumn sp) + 1 })

-- | Combinators

parens = MP.between openParenP closeParenP

satisfy :: (MonadParsec e s m, Token s ~ S.Token) => (S.Token -> Bool) -> m S.Token
satisfy f = Prim.token test Nothing
  where
    test x =
      if f x then
        Right x
      else
        Left (Set.singleton (MP.Tokens (x NE.:| [])), mempty, mempty)

tokenP :: S.Token -> TushParser S.Token
tokenP t = satisfy (== t)

-- | Simple Parsers

ifP :: TushParser S.ReservedWord
ifP = do
  (S.ReservedWordT i) <- tokenP $ S.ReservedWordT S.If
  return i

thenP :: TushParser S.ReservedWord
thenP = do
  (S.ReservedWordT t) <- tokenP $ S.ReservedWordT S.Then
  return t

elseP :: TushParser S.ReservedWord
elseP = do
  (S.ReservedWordT e) <- tokenP $ S.ReservedWordT S.Else
  return e

varP :: TushParser (S.Var (Maybe S.Type))
varP = do
  (S.VarT v) <- satisfy (\x -> S.isVarT x && (not $ S.isOpT x))
  return v

opP :: TushParser (S.Var (Maybe S.Type))
opP = do
  (S.VarT o) <- satisfy (\x -> S.isVarT x && S.isOpT x)
  return o

colonP :: TushParser S.ReservedPunctuation
colonP = do
  (S.ReservedPunctuationT ta) <- satisfy S.isColonT
  return ta

typeLiteralP :: TushParser S.TypeLiteral
typeLiteralP = do
  (S.TypeLiteralT tl) <- satisfy S.isTypeLiteralT 
  return tl

openParenP :: TushParser S.ReservedPunctuation
openParenP = do
  (S.ReservedPunctuationT op) <- satisfy S.isOpenParenT
  return op

closeParenP :: TushParser S.ReservedPunctuation
closeParenP = do
  (S.ReservedPunctuationT cp) <- tokenP (S.ReservedPunctuationT S.CloseParen)
  return cp

equalsP :: TushParser S.ReservedOp
equalsP = do
  (S.ReservedOpT e) <- tokenP (S.ReservedOpT S.Equals)
  return e

commaP :: TushParser S.ReservedPunctuation
commaP = do
  (S.ReservedPunctuationT c) <- satisfy S.isCommaT
  return c

semicolonP :: TushParser S.ReservedPunctuation
semicolonP = do
  (S.ReservedPunctuationT sc) <- tokenP (S.ReservedPunctuationT S.Semicolon)
  return $ sc

literalP :: TushParser S.Literal
literalP = do
  (S.LiteralT l) <- satisfy S.isLiteralT
  return l

externP = tokenP (S.ReservedWordT S.Extern)

-- | Expressions

untyped :: Maybe S.Type
untyped = Nothing

literalE :: TushParser (S.Expression (Maybe S.Type))
literalE = flip S.LitE untyped <$> literalP

varE :: TushParser (S.Expression (Maybe S.Type))
varE = flip S.VarE untyped <$> varP

opE :: TushParser (S.Expression (Maybe S.Type))
opE = flip S.VarE untyped <$> opP

varCallE :: TushParser (S.Expression (Maybe S.Type))
varCallE = do
  name <- varE
  args <- some termE
  return $ S.CallE name (fromList args) untyped

opCallE :: TushParser (S.Expression (Maybe S.Type))
opCallE = do
  l <- termE
  name <- opE
  r <- termE
  return $ S.CallE name (fromList [l, r]) untyped

ifE :: TushParser (S.Expression (Maybe S.Type))
ifE = do
  void $ ifP
  cond <- termE
  void $ thenP
  conse <- exprE
  void $ elseP
  anted <- exprE
  return $ S.IfE cond conse anted untyped

termE =  MP.try literalE
     <|> MP.try varCallE
     <|> MP.try varE
     <|> parens exprE

exprE =  MP.try opCallE
     <|> MP.try ifE
     <|>        termE

-- | Function Prototypes

typedVarP :: TushParser (S.Var S.Type)
typedVarP = do
  v <- varP
  void $ colonP
  tl <- typeLiteralP
  return $ v { S.varType = S.TTypeLiteral tl }
  
fProtoP :: TushParser (S.FProto S.Type)
fProtoP = do
  (S.Var name type' op) <- typedVarP
  args <- fromList <$> many typedVarP
  return $ S.FProto (S.Var name (S.TTypeLiteral $ S.TLBuiltinType (S.BTLambda type' ((\(S.Var _ t _) -> t) <$> args))) op) args

-- | Statements

externS :: TushParser (S.Statement (Maybe S.Type) S.Type)
externS = do
  void $ externP
  fp <- fProtoP
  void $ semicolonP
  return $ S.ExternS fp

funcS :: TushParser (S.Statement (Maybe S.Type) S.Type)
funcS = do
  fp <- fProtoP
  void $ equalsP
  e <- exprE
  void $ semicolonP
  return $ S.FuncS fp mempty e

exprS :: TushParser (S.Statement (Maybe S.Type) S.Type)
exprS = do
  e <- exprE
  void semicolonP
  return $ S.ExprS e

statementS :: TushParser (S.Statement (Maybe S.Type) S.Type)
statementS =  MP.try externS
          <|> MP.try funcS
          <|>        exprS

minitest :: Either TushParseError (S.Statement (Maybe S.Type) S.Type)
minitest = MP.runParser (program statementS) "<tokens>" $ (\(Just x) -> x) $ MP.parseMaybe L.tokens "f : Int x : Int = 2 * x;"

contents :: Parser a -> Parser a
contents p = do
  void $ many MP.spaceChar
  r <- p
  eof
  return r
  
program :: TushParser a -> TushParser a
program p = do
  r <- p
  eof
  return r

toplevel :: TushParser (Vector (S.Statement (Maybe S.Type) S.Type))
toplevel = fromList <$> many statementS

lexTopLevel :: Text -> Either (MP.ParseError Char MP.Dec) (Vector S.Token)
lexTopLevel t = MP.runParser (contents L.tokens) "<stdin>" t

parseStatement :: Vector S.Token -> Either TushParseError (S.Statement (Maybe S.Type) S.Type)
parseStatement s = MP.runParser (program statementS) "<tokens>" s

parseToplevel :: Vector S.Token -> Either TushParseError (Vector (S.Statement (Maybe S.Type) S.Type))
parseToplevel s = MP.runParser (program toplevel) "<tokens>" s
