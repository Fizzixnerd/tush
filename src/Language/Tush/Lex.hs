{-# LANGUAGE NoImplicitPrelude #-}

module Language.Tush.Lex where

import ClassyPrelude

import Data.Void

import qualified Data.Map as M

import Data.Char
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP

import qualified Language.Tush.Syntax as S

reservedPunctuation :: Map Char S.Token
reservedPunctuation = M.fromList [ ('=', S.Equals)
                                 , ('<', S.LAngle)
                                 , ('>', S.RAngle)
                                 , ('[', S.LBracket)
                                 , (']', S.RBracket)
                                 , ('{', S.LBrace)
                                 , ('}', S.RBrace)
                                 , (':', S.Colon)
                                 , ('\n',S.Newline)
                                 , ('-', S.Dash)
                                 , ('@', S.AtSign)
                                 , ('`', S.BackTick)
                                 , ('*', S.Asterisk)
                                 , ('_', S.Underscore)
                                 , ('#', S.Octothorpe)
                                 , ('~', S.Tilde)
                                 , ('^', S.Caret)
                                 , ('"', S.DoubleQuote)
                                 , ('/', S.FSlash)
                                 , (',', S.Comma)
                                 , ('.', S.Period)
                                 , ('$', S.DollarSign)
                                 , ('%', S.PercentSign)
                                 , (';', S.SemiColon)
                                 , ('\\',S.BSlash)
                                 , ('+', S.Plus) ]


mkDTokenP :: MP.Parsec (MP.ErrorFancy Void) Text S.Token -> MP.Parsec (MP.ErrorFancy Void) Text S.DToken
mkDTokenP p = do
  MP.SourcePos _ r1 c1 <- MP.getPosition
  x <- p
  MP.SourcePos _ r2 c2 <- MP.getPosition
  let di = S.DebugInfo (MP.unPos r1, MP.unPos c1) (MP.unPos r2, MP.unPos c2)
  return $ S.DebugToken di x

identifierT :: MP.Parsec (MP.ErrorFancy Void) Text S.Token
identifierT = S.TIdentifier . fromString <$> (some $ MP.satisfy $ \c ->
                                                 (not $ isSpace c) && (c /= '='))

dIdentifierT :: MP.Parsec (MP.ErrorFancy Void) Text S.DToken
dIdentifierT = MP.label "Identifier" $ mkDTokenP identifierT

operatorT :: MP.Parsec (MP.ErrorFancy Void) Text S.Token
operatorT = S.TIdentifier . fromString <$> (some $ MP.satisfy $ \c ->
                                               (not $ isSpace c) &&
                                               (c `elem` M.keys reservedPunctuation))

dOperatorT :: MP.Parsec (MP.ErrorFancy Void) Text S.DToken
dOperatorT = MP.label "Operator" $ mkDTokenP operatorT

equalsT :: MP.Parsec (MP.ErrorFancy Void) Text S.Token
equalsT = const S.Equals <$> MP.satisfy (== '=')

dEqualsT :: MP.Parsec (MP.ErrorFancy Void) Text S.DToken
dEqualsT = MP.label "'='" $ mkDTokenP equalsT

dToken :: MP.Parsec (MP.ErrorFancy Void) Text S.DToken
dToken = MP.try dEqualsT
         <|> MP.try dOperatorT
         <|> dIdentifierT

dTokens :: MP.Parsec (MP.ErrorFancy Void) Text S.TushTokenStream
dTokens = S.TushTokenStream . fromList <$> many dToken
