{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Tush.Parse where

import ClassyPrelude
import qualified Text.Megaparsec as MP
import qualified Language.Tush.Syntax as S
import Data.Void
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector as V
import Language.Tush.Lex

type TushParseError = MP.ParseError S.DToken (MP.ErrorFancy Void)

newtype TushParser a = TushParser
  { _unTushParser :: MP.Parsec (MP.ErrorFancy Void) S.TushTokenStream a }
  deriving (Functor, Applicative, Monad, MonadPlus,
            MP.MonadParsec (MP.ErrorFancy Void) S.TushTokenStream, Alternative)

satisfy :: (MP.MonadParsec e s m, MP.Token s ~ S.DToken) =>
           (S.Token -> Bool) -> m S.Token
satisfy f = MP.token test Nothing
  where
    test (y@S.DebugToken { S._dtToken = x }) =
      if f x then
        Right x
      else
        Left (pure (MP.Tokens (y NE.:| [])), mempty)

token :: (MP.MonadParsec e s m, MP.Token s ~ S.DToken) => S.Token -> m S.Token
token t = satisfy (== t)

equalsP :: TushParser ()
equalsP = void (satisfy (== S.Equals)) MP.<?> "'='"

identifierP :: TushParser S.Identifier
identifierP = MP.label "Identifier" $ do
  S.TIdentifier id_ <- satisfy $ \case
    S.TIdentifier _ -> True
    _ -> False
  return $ S.Identifier id_

operatorP :: TushParser S.Operator
operatorP = MP.label "Operator" $ do
  S.TOperator op_ <- satisfy $ \case
    S.TOperator _ -> True
    _ -> False
  return $ S.Operator op_

pathP :: TushParser S.Path
pathP = MP.label "Path" $ do
  S.TPath pcs _pathIsRelative _pathIsDirectory <- satisfy $ \case
    S.TPath {} -> True
    _ -> False
  let pcs' = S.PathComponent <$> pcs
      _pathDirectory = if null pcs'
                       then empty
                       else V.init pcs'
      _pathFile = if null pcs'
                  then Nothing
                  else let f = V.last pcs
                           fparts = fromList $ splitElem '.' f
                           fname = if length fparts == 1
                                   then S.PathComponent $ fparts V.! 0
                                   else S.PathComponent $ concat $ V.init $ V.init $ intersperse "." fparts
                           fext = if length fparts == 1
                                  then Nothing
                                  else Just $ S.PathExtension $ V.last fparts
                       in
                         Just (fname, fext)
  return S.Path {..}

tushStringP :: TushParser S.TushString
tushStringP = MP.label "String" $ do
  S.TString s <- satisfy $ \case
    S.TString _ -> True
    _ -> False
  return $ S.TushString s

tushVectorP :: TushParser S.TushVector
tushVectorP = MP.label "Vector" $ do
  void $ token S.LBracket
  eles <- MP.sepBy expressionP (token S.Comma)
  void $ token S.RBracket
  return $ S.TushVector $ fromList eles

tushIntP :: TushParser S.TushInt
tushIntP = MP.label "Integer" $ do
  S.TInt i <- satisfy $ \case
    S.TInt _ -> True
    _ -> False
  return $ S.TushInt i

callP :: TushParser S.Call
callP = MP.label "Function Call" $ do
  exp1 <- atomicP
  exp2 <- atomicP
  case exp2 of
    S.EName (S.NOperator _) -> return S.Call
                               { S._callFunc = exp2
                               , S._callOperand = exp1
                               }
    _ -> return S.Call
         { S._callFunc = exp1
         , S._callOperand = exp2
         }

nameP :: TushParser S.Name
nameP = MP.label "Name" $
        MP.try (S.NIdentifier <$> identifierP)
        <|> (S.NOperator <$> operatorP)

parensedP :: TushParser S.Expression
parensedP = do
  void $ token S.LParen
  e <- expressionP
  void $ token S.RParen
  return e

atomicP :: TushParser S.Expression
atomicP = MP.try (S.EString <$> tushStringP)
          <|> MP.try (S.EPath <$> pathP)
          <|> MP.try (S.EVector <$> tushVectorP)
          <|> MP.try parensedP
          <|> (S.EName <$> nameP)

expressionP :: TushParser S.Expression
expressionP = MP.label "Expression" $ do
  atomics <- do
    as <- fromList <$> many atomicP
    void $ optional $ token S.Newline
    return as
  when (null atomics) $
    error "Got an empty Expression."
  if length atomics == 1
    then return $ atomics V.! 0
    else return $ V.foldl1' (\exp1 exp2 -> case exp2 of
                                S.EName (S.NOperator _) -> S.ECall S.Call
                                                           { S._callFunc = exp2
                                                           , S._callOperand = exp1
                                                           }
                                _ -> S.ECall S.Call
                                     { S._callFunc = exp1
                                     , S._callOperand = exp2
                                     }) atomics

parseWith :: TushParser a -> Text -> Either (MP.ParseError S.DToken (MP.ErrorFancy Void)) a
parseWith p s =
  let lexed = MP.runParser dTokens "<input>" s
  in
    case lexed of
      Left e -> error $ show e
      Right x -> MP.runParser (_unTushParser p) "<tokens>" x

parse :: Text -> Either (MP.ParseError S.DToken (MP.ErrorFancy Void)) S.Expression
parse = parseWith expressionP
