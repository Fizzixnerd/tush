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
  updatePos _ _ sp _ = (sp, sp { sourceColumn = unsafePos $ (unPos $ sourceColumn sp) + 1 })

-- | Combinators

parens :: TushParser a -> TushParser a
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

varP :: TushParser S.Var
varP = do
  (S.VarT v) <- satisfy (\x -> S.isVarT x && (not $ S.isOpT x))
  return v

opP :: TushParser S.Var
opP = do
  (S.VarT o) <- satisfy (\x -> S.isVarT x && S.isOpT x)
  return o

colonP :: TushParser S.ReservedPunctuation
colonP = do
  (S.ReservedPunctuationT ta) <- satisfy S.isColonT
  return ta

namedTypeP :: TushParser S.ManifestType
namedTypeP = do
  (S.VarT v) <- satisfy isNamedTypeT 
  return $ S.MTyName v
  where isNamedTypeT (S.VarT (S.Var _ S.VClassType)) = True
        isNamedTypeT _ = False

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

arrowP :: TushParser S.ReservedOp
arrowP = do
  (S.ReservedOpT a) <- tokenP (S.ReservedOpT S.Arrow)
  return a

commaP :: TushParser S.ReservedPunctuation
commaP = do
  (S.ReservedPunctuationT c) <- satisfy S.isCommaT
  return c

semicolonP :: TushParser S.ReservedPunctuation
semicolonP = do
  (S.ReservedPunctuationT sc) <- tokenP (S.ReservedPunctuationT S.Semicolon)
  return sc

backslashP :: TushParser S.ReservedPunctuation
backslashP = do
  (S.ReservedPunctuationT bs) <- tokenP (S.ReservedPunctuationT S.Backslash)
  return bs

literalP :: TushParser S.Literal
literalP = do
  (S.LiteralT l) <- satisfy S.isLiteralT
  return l

externP :: TushParser S.Token
externP = tokenP (S.ReservedWordT S.Extern)

-- | Expressions

untyped :: S.PreType
untyped = S.PTyUntyped

literalE :: TushParser (S.Expression S.PreType)
literalE = flip S.LitE untyped <$> literalP

varE :: TushParser (S.Expression S.PreType)
varE = flip S.VarE untyped <$> varP

opE :: TushParser (S.Expression S.PreType)
opE = flip S.VarE untyped <$> opP

varAppE :: TushParser (S.Expression S.PreType)
varAppE = do
  name <- termE
  arg <- termE
  return $ S.AppE name arg untyped

opAppE :: TushParser (S.Expression S.PreType)
opAppE = do
  l <- termE
  name <- opE
  r <- termE
  return $ S.AppE (S.AppE name r untyped) l untyped

ifE :: TushParser (S.Expression S.PreType)
ifE = do
  void $ ifP
  cond <- termE
  void $ thenP
  conse <- exprE
  void $ elseP
  anted <- exprE
  return $ S.IfE cond conse anted untyped

lamE :: TushParser (S.Expression S.PreType)
lamE = do
  void $ backslashP
  arg <- exprE
  void $ arrowP
  body <- exprE
  return $ S.LamE arg body S.PTyUntyped

termE :: TushParser (S.Expression S.PreType)
termE =  MP.try literalE
     <|> MP.try varE
     <|> parens exprE

exprE :: TushParser (S.Expression S.PreType)
exprE =  MP.try opAppE
     <|> MP.try ifE
     <|> MP.try lamE
     <|> MP.try varAppE
     <|>        termE

-- | Function Prototypes

possiblyTypedVarP :: TushParser (S.Expression S.PreType)
possiblyTypedVarP = do
  v <- varP
  void $ colonP
  nt <- optional namedTypeP
  let nt' = maybe S.PTyUntyped S.PTyMType nt
  return $ S.VarE { S._varEVar = v
                  , S._exprType = nt'
                  }

fProtoP :: TushParser (S.FProto S.PreType)
fProtoP = do
  (S.VarE (S.Var retName rop) retType) <- possiblyTypedVarP
  (S.VarE (S.Var argName aop) argType) <- possiblyTypedVarP
  return $ S.FProto { S._fProtoName = 
                      S.VarE (S.Var retName rop) 
                      (S.PTyType (S.TyLambda 
                                   (S.Lambda { S._lamReturnType = retType
                                             , S._lamArgType = argType
                                             })))
                    , S._fProtoArg = S.VarE (S.Var argName aop) argType
                    }

-- | Statements

externS :: TushParser (S.Statement S.PreType S.PreType)
externS = do
  void $ externP
  fp <- fProtoP
  void $ semicolonP
  return $ S.ExternS fp

funcS :: TushParser (S.Statement S.PreType S.PreType)
funcS = do
  fp <- fProtoP
  void $ equalsP
  e <- exprE
  void $ semicolonP
  return $ S.FuncS fp mempty e

exprS :: TushParser (S.Statement S.PreType S.PreType)
exprS = do
  e <- exprE
  void semicolonP
  return $ S.ExprS e

statementS :: TushParser (S.Statement S.PreType S.PreType)
statementS =  MP.try externS
          <|> MP.try funcS
          <|>        exprS

minitest :: Either TushParseError (S.Statement S.PreType S.PreType)
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

toplevel :: TushParser (Vector (S.Statement S.PreType S.PreType))
toplevel = fromList <$> many statementS

lexTopLevel :: Text -> Either (MP.ParseError Char MP.Dec) (Vector S.Token)
lexTopLevel t = MP.runParser (contents L.tokens) "<stdin>" t

parseStatement :: Vector S.Token -> Either TushParseError (S.Statement S.PreType S.PreType)
parseStatement s = MP.runParser (program statementS) "<tokens>" s

parseTopLevel :: Vector S.Token -> Either TushParseError (Vector (S.Statement S.PreType S.PreType))
parseTopLevel s = MP.runParser (program toplevel) "<tokens>" s

lexabunch :: Either (MP.ParseError Char MP.Dec) [Vector S.Token]
lexabunch = forM [-- "\\x -> x;"
                 "(\\x -> x) 3;"
--                 , "f x;"
--                 , "(f x) y;"
                 , "3 + 4;"
--                 , "extern putchar : Int x : Int;"
--                 , "" 
                 ] lexTopLevel

parseabunch :: Either TushParseError ([Vector (S.Statement S.PreType S.PreType)])
parseabunch = case lexabunch of
  Right x -> forM x parseTopLevel
  _ -> error "SUCK A DICK"
