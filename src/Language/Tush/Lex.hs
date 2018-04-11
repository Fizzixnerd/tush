{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Tush.Lex where

import ClassyPrelude

import Data.Void

import qualified Data.Map as M

import Data.Char
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MP
import qualified Data.Vector as V

import qualified Language.Tush.Syntax as S

reservedSyntax :: Map Char S.Token
reservedSyntax = M.fromList [ ('[', S.LBracket)
                            , (']', S.RBracket)
                            , ('(', S.LParen)
                            , (')', S.RParen)
                            , ('\\', S.BSlash)
                            , ('"', S.DoubleQuote)
                            , ('\n', S.Newline) ]

reservedOps :: Map Char S.Token
reservedOps = M.fromList [ ('=', S.Equals)
                         , ('<', S.LAngle)
                         , ('>', S.RAngle)
                         , (':', S.Colon)
                         , ('-', S.Dash)
                         , ('@', S.AtSign)
                         , ('`', S.BackTick)
                         , ('*', S.Asterisk)
                         , ('_', S.Underscore)
                         , ('#', S.Octothorpe)
                         , ('~', S.Tilde)
                         , ('^', S.Caret)
                         , ('/', S.FSlash)
                         , (',', S.Comma)
                         , ('.', S.Period)
                         , ('$', S.DollarSign)
                         , ('%', S.PercentSign)
                         , (';', S.SemiColon)
                         , ('+', S.Plus) ]

isSyntax :: Char -> Bool
isSyntax c = isJust $ lookup c reservedSyntax

isOp :: Char -> Bool
isOp c = isJust $ lookup c reservedOps

isReserved :: Char -> Bool
isReserved c = isOp c || isSyntax c

mkDTokenP :: MP.Parsec (MP.ErrorFancy Void) Text S.Token -> MP.Parsec (MP.ErrorFancy Void) Text S.DToken
mkDTokenP p = do
  MP.SourcePos _ r1 c1 <- MP.getPosition
  x <- p
  MP.SourcePos _ r2 c2 <- MP.getPosition
  let di = S.DebugInfo (MP.unPos r1, MP.unPos c1) (MP.unPos r2, MP.unPos c2)
  return $ S.DebugToken di x

identifierT :: MP.Parsec (MP.ErrorFancy Void) Text S.Token
identifierT = S.TIdentifier . fromString <$> (some $ MP.satisfy $ \c ->
                                                 (not $ isSpace c) && (not $ isReserved c))

dIdentifierT :: MP.Parsec (MP.ErrorFancy Void) Text S.DToken
dIdentifierT = MP.label "Identifier" $ mkDTokenP identifierT

operatorT :: MP.Parsec (MP.ErrorFancy Void) Text S.Token
operatorT = S.TOperator . fromString <$> (some $ MP.satisfy $ \c ->
                                             (not $ isSpace c) && isOp c)

dOperatorT :: MP.Parsec (MP.ErrorFancy Void) Text S.DToken
dOperatorT = MP.label "Operator" $ mkDTokenP operatorT

pathT :: MP.Parsec (MP.ErrorFancy Void) Text S.Token
pathT = do
  rel <- MP.optional $ MP.char '.'
  let isRel = isJust rel
  pcs <- some $ do
    void $ MP.char '/'
    many $ MP.satisfy $ \c -> c /= '/' && (not $ isSpace c) && (not $ isReserved c)
  let isDir = null $ V.last $ fromList pcs
  return $ S.TPath (filter (not . null) $ fromList $ fromString <$> pcs) isRel isDir

dPathT :: MP.Parsec (MP.ErrorFancy Void) Text S.DToken
dPathT = MP.label "Path" $ mkDTokenP pathT

-- | TStrings can't contain double quotes at the moment.
stringT :: MP.Parsec (MP.ErrorFancy Void) Text S.Token
stringT = do
  void $ MP.char '"'
  s <- many $ MP.satisfy (/= '"')
  void $ MP.char '"'
  return $ S.TString $ fromString s

dStringT :: MP.Parsec (MP.ErrorFancy Void) Text S.DToken
dStringT = MP.label "String" $ mkDTokenP stringT

equalsT :: MP.Parsec (MP.ErrorFancy Void) Text S.Token
equalsT = const S.Equals <$> MP.char '='

dEqualsT :: MP.Parsec (MP.ErrorFancy Void) Text S.DToken
dEqualsT = MP.label "'='" $ mkDTokenP equalsT

lparenT :: MP.Parsec (MP.ErrorFancy Void) Text S.Token
lparenT = const S.LParen <$> MP.char '('

dLparenT :: MP.Parsec (MP.ErrorFancy Void) Text S.DToken
dLparenT = MP.label "'('" $ mkDTokenP lparenT

rparenT :: MP.Parsec (MP.ErrorFancy Void) Text S.Token
rparenT = const S.RParen <$> MP.char ')'

dRparenT :: MP.Parsec (MP.ErrorFancy Void) Text S.DToken
dRparenT = MP.label "')'" $ mkDTokenP rparenT

newlineT :: MP.Parsec (MP.ErrorFancy Void) Text S.Token
newlineT = const S.Newline <$> MP.char '\n'

dNewlineT :: MP.Parsec (MP.ErrorFancy Void) Text S.DToken
dNewlineT = MP.label "<Newline>" $ mkDTokenP newlineT

intT :: MP.Parsec (MP.ErrorFancy Void) Text S.Token
intT = S.TInt <$> MP.signed space MP.decimal

dIntT :: MP.Parsec (MP.ErrorFancy Void) Text S.DToken
dIntT = MP.label "Integer" $ mkDTokenP intT

space :: MP.Parsec (MP.ErrorFancy Void) Text ()
space = void $ many $ MP.satisfy $ \c -> isSpace c && (c /= '\n')

dToken :: MP.Parsec (MP.ErrorFancy Void) Text S.DToken
dToken = MP.try dEqualsT
         <|> MP.try dLparenT
         <|> MP.try dRparenT
         <|> MP.try dNewlineT
         <|> MP.try dIntT
         <|> MP.try dStringT
         <|> MP.try dPathT
         <|> MP.try dOperatorT
         <|> dIdentifierT

dTokens :: MP.Parsec (MP.ErrorFancy Void) Text S.TushTokenStream
dTokens = S.TushTokenStream . fromList <$> do
  toks <- many $ space >> dToken
  space
  return toks

lex :: Text -> Either (MP.ParseError Char (MP.ErrorFancy Void)) S.TushTokenStream
lex = MP.runParser dTokens "<tokens>"
