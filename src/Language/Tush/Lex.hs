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

reservedWords :: Map Text S.Token
reservedWords = M.fromList [ ("if", S.If)
                           , ("then", S.Then)
                           , ("else", S.Else)
                           , ("do", S.Do)
                           , ("True", S.TTrue)
                           , ("False", S.TFalse)
                           ]

reservedSyntax :: Map Char S.Token
reservedSyntax = M.fromList [ ('[', S.LBracket)
                            , (']', S.RBracket)
                            , ('(', S.LParen)
                            , (')', S.RParen)
                            , ('\\', S.BSlash)
                            , ('"', S.DoubleQuote)
                            , ('\n', S.Newline)
                            ]

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
                         , ('+', S.Plus)
                         ]

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

reservedWordT :: MP.Parsec (MP.ErrorFancy Void) Text S.Token
reservedWordT = do
  word <- MP.try (MP.string "if" >> return S.If)
          <|> MP.try (MP.string "then" >> return S.Then)
          <|> MP.try (MP.string "else" >> return S.Else)
          <|> MP.try (MP.string "do" >> return S.Do)
  void MP.spaceChar
  return word

dReservedWordT :: MP.Parsec (MP.ErrorFancy Void) Text S.DToken
dReservedWordT = mkDTokenP reservedWordT

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
  relPath <- MP.optional $ MP.eitherP (MP.char '.') (MP.char '!')
  let relativity = case relPath of
        Nothing -> S.Absolute
        (Just (Left _)) -> S.Relative
        (Just (Right _)) -> S.PATH
  pcs <- some $ do
    void $ MP.char '/'
    many $ MP.satisfy $ \c -> c /= '/' && (not $ isSpace c) && (not $ isReserved c)
  let isDir = null $ V.last $ fromList pcs
  return $ S.TPath
    (filter (not . null) $ fromList $ fromString <$> pcs)
    relativity
    isDir

dPathT :: MP.Parsec (MP.ErrorFancy Void) Text S.DToken
dPathT = MP.label "Path" $ mkDTokenP pathT

-- | TStrings can't contain double quotes at the moment. They also span multiple
-- lines. Dunno if this is a good thing... O_o
stringT :: MP.Parsec (MP.ErrorFancy Void) Text S.Token
stringT = do
  void $ MP.char '"'
  s <- many $ MP.satisfy (/= '"')
  void $ MP.char '"'
  return $ S.TString $ fromString s

dStringT :: MP.Parsec (MP.ErrorFancy Void) Text S.DToken
dStringT = MP.label "String" $ mkDTokenP stringT

boolT :: MP.Parsec (MP.ErrorFancy Void) Text S.Token
boolT = do
  tf <- MP.eitherP (MP.string "True") (MP.string "False")
  case tf of
    Left _ -> return S.TTrue
    Right _ -> return S.TFalse

dBoolT :: MP.Parsec (MP.ErrorFancy Void) Text S.DToken
dBoolT = MP.label "Bool" $ mkDTokenP boolT

equalsT :: MP.Parsec (MP.ErrorFancy Void) Text S.Token
equalsT = MP.char '=' >> return S.Equals

dEqualsT :: MP.Parsec (MP.ErrorFancy Void) Text S.DToken
dEqualsT = MP.label "'='" $ mkDTokenP equalsT

lparenT :: MP.Parsec (MP.ErrorFancy Void) Text S.Token
lparenT = MP.char '(' >> return S.LParen

dLparenT :: MP.Parsec (MP.ErrorFancy Void) Text S.DToken
dLparenT = MP.label "'('" $ mkDTokenP lparenT

rparenT :: MP.Parsec (MP.ErrorFancy Void) Text S.Token
rparenT = MP.char ')' >> return S.RParen

dRparenT :: MP.Parsec (MP.ErrorFancy Void) Text S.DToken
dRparenT = MP.label "')'" $ mkDTokenP rparenT

lbracketT :: MP.Parsec (MP.ErrorFancy Void) Text S.Token
lbracketT = MP.char '[' >> return S.LBracket

dLbracketT :: MP.Parsec (MP.ErrorFancy Void) Text S.DToken
dLbracketT = MP.label "'['" $ mkDTokenP lbracketT

rbracketT :: MP.Parsec (MP.ErrorFancy Void) Text S.Token
rbracketT = MP.char ']' >> return S.RBracket

dRbracketT :: MP.Parsec (MP.ErrorFancy Void) Text S.DToken
dRbracketT = MP.label "']'" $ mkDTokenP rbracketT

newlineT :: MP.Parsec (MP.ErrorFancy Void) Text S.Token
newlineT = MP.char '\n' >> return S.Newline

dNewlineT :: MP.Parsec (MP.ErrorFancy Void) Text S.DToken
dNewlineT = MP.label "<Newline>" $ mkDTokenP newlineT

intT :: MP.Parsec (MP.ErrorFancy Void) Text S.Token
intT = S.TInt <$> MP.decimal

dIntT :: MP.Parsec (MP.ErrorFancy Void) Text S.DToken
dIntT = MP.label "Integer" $ mkDTokenP intT

space :: MP.Parsec (MP.ErrorFancy Void) Text ()
space = void $ many $ MP.satisfy $ \c -> isSpace c && (c /= '\n')

dToken :: MP.Parsec (MP.ErrorFancy Void) Text S.DToken
dToken = MP.try dReservedWordT
         <|> MP.try dEqualsT
         <|> MP.try dLparenT
         <|> MP.try dRparenT
         <|> MP.try dLbracketT
         <|> MP.try dRbracketT
         <|> MP.try dNewlineT
         <|> MP.try dIntT
         <|> MP.try dStringT
         <|> MP.try dBoolT
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
