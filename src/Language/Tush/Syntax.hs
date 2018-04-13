{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Tush.Syntax where

import qualified Text.Megaparsec.Stream as MP
import qualified Text.Megaparsec.Pos as MP
import ClassyPrelude
import Control.Lens
import Data.Data
import qualified Data.Vector as V
import Text.Printf

-- * Parsing Bookkeeping

-- Most of this was taken from the idoc source code.

-- | Type synonym for keeping track of which row we are on.
type Row = Int

-- | Type synonym for keeping track of which column we are on.
type Col = Int

-- | A `Token' with attached debug information; the parser never sees
-- the debug information directly and so doesn't need to worry about
-- it.
data DebugToken d = DebugToken { _dtInfo :: d
                               , _dtToken :: Token
                               }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- | The current debug information kept around so that we can tell the
-- user where an error occurred.  More can be added later without
-- breaking much code.
data DebugInfo = DebugInfo { _diStart :: !(Row, Col)
                           , _diEnd :: !(Row, Col)
                           }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

type DToken = DebugToken DebugInfo
data Relativity = Relative | Absolute | PATH
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
type IsDir = Bool

data Token = TIdentifier Text
           | TOperator Text
           | TPath (Vector Text) Relativity IsDir
           | TString Text
           | TInt Int
           -- symbols and punctuation
           | Equals
           | LAngle
           | RAngle
           | LBracket
           | RBracket
           | LParen
           | RParen
           | Colon
           | Newline
           | Dash
           | AtSign
           | BackTick
           | Asterisk
           | Underscore
           | Octothorpe
           | DoubleQuote
           | Tilde
           | Caret
           | FSlash
           | Comma
           | Period
           | DollarSign
           | PercentSign
           | BSlash
           | Plus
           -- if-then-else (ite)
           | If
           | Then
           | Else
           -- do
           | Do
           | BwdArrow
           | LBrace
           | RBrace
           | Semicolon
           -- let
           | Let
           | In
           -- lambda
           | Lam
           | FwdArrow
           -- bools
           | TTrue
           | TFalse
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- | Newtype around a Vector of `DToken's; represents lexed source.
newtype TushTokenStream = TushTokenStream { unStream :: Vector DToken }
  deriving Show

-- | Megaparsec Stream instance so that this properly works with the
-- rest of the library.
instance MP.Stream TushTokenStream where
  type Token TushTokenStream = DToken
  type Tokens TushTokenStream = Vector DToken
  tokensToChunk _ = fromList
  chunkToTokens _ = toList
  chunkLength _ = length
  advance1 _ _ MP.SourcePos {MP.sourceName = sn} DebugToken { _dtInfo = info } =
    let (r, c) = _diEnd info
    in
      MP.SourcePos { MP.sourceName = sn
                   , MP.sourceLine = MP.mkPos $ fromIntegral r
                   , MP.sourceColumn = MP.mkPos $ fromIntegral c
                   }
  advanceN pxy p_ sp ts = MP.advance1 pxy p_ sp (V.last ts)
  take1_ (TushTokenStream ts) =
    if null ts
    then Nothing
    else Just (V.head ts, TushTokenStream $ V.tail ts)
  takeN_ n tts | n <= 0 = Just (V.empty, tts)
  takeN_ _ (TushTokenStream ts) | V.null ts = Nothing
  takeN_ n (TushTokenStream ts) = Just $ TushTokenStream <$> V.splitAt n ts
  takeWhile_ p_ (TushTokenStream ts) = TushTokenStream <$> V.span p_ ts

-- * Paths
--
-- A Path is a newtype wrapper around a Vector of PathComponents.
--
-- PathComponents are just file/directory name literals at the moment.
--
-- We probably want to include globbing/regexes here somehow in a rational way.
newtype PathComponent = PathComponent Text
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

newtype PathExtension = PathExtension { _unPathExtension :: Text }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Path = Path
  { _pathDirectory :: Vector PathComponent
  , _pathFile :: Maybe (PathComponent, Maybe PathExtension)
  , _pathRelativity :: Relativity
  , _pathIsDirectory :: Bool
  } deriving (Eq, Ord, Show, Data, Typeable, Generic)

newtype Identifier = Identifier { _unIdentifier :: Text }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

newtype Operator = Operator { _unOperator :: Text }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

newtype TushString = TushString { _unTushString :: Text }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Name = NIdentifier Identifier
          | NOperator Operator
          deriving (Eq, Ord, Show, Data, Typeable, Generic)

nameToText :: Name -> Text
nameToText (NIdentifier (Identifier x)) = x
nameToText (NOperator (Operator x)) = x

data Call = Call
  { _callFunc :: Expression
  , _callOperand :: Expression
  } deriving (Show, Typeable, Generic)

newtype TushVector = TushVector { _unTushVector :: Vector Expression }
  deriving (Show, Typeable, Generic)

newtype TushInt = TushInt { _unTushInt :: Int }
  deriving (Eq, Ord, Show, Data, Typeable, Generic, Num)

newtype TushBool = TushBool { _unTushBool :: Bool }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Builtin = Builtin
  { _builtinName :: Text
  , _builtinFunc :: Expression -> IO Expression
  }

instance Show Builtin where
  show Builtin {..} = printf "<<Builtin Function %s>>" (unpack _builtinName)

data Ite = Ite
  { _iteIf   :: Expression
  , _iteThen :: Expression
  , _iteElse :: Expression
  } deriving (Show, Typeable, Generic)

data Bind = Bind
  { _bindName :: Expression
  , _bindValue :: Expression
  , _bindBody :: Expression
  } deriving (Show, Typeable, Generic)

type Environment = Map Text Expression

data Lambda = Lambda
  { _lambdaArg :: Expression
  , _lambdaBody :: Expression
  } deriving (Show, Typeable, Generic)

-- Lambda with attached env
data EnvLambda = EnvLambda
  { _envLambdaArg :: Expression
  , _envLambdaBody :: Expression
  , _envLambdaEnv :: Environment
  } deriving (Show, Typeable, Generic)

newtype Sequence = Sequence
  { _sequenceExpressions :: Vector Expression
  } deriving (Show, Typeable, Generic)

data Expression = ECall Call
                | EName Name
                | EPath Path
                | EString TushString
                | EVector TushVector
                | EInt TushInt
                | EBool TushBool
                | EBuiltin Builtin
                | EIte Ite
                | EBind Bind
                | ELambda Lambda
                | EEnvLambda EnvLambda
                | ESequence Sequence
                deriving (Show, Typeable, Generic)

data Statement = SAssignment Name Expression
  deriving (Show, Typeable, Generic)

mconcat <$> mapM makeLenses
  [ ''Path
  , ''PathExtension
  , ''PathComponent
  , ''DebugInfo
  , ''Identifier
  , ''Operator
  , ''Call
  , ''TushVector
  , ''TushInt
  , ''Ite
  , ''Bind
  , ''Lambda
  ]
