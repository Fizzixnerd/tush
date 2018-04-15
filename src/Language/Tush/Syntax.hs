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
import ClassyPrelude hiding (tshow)
import Control.Lens
import Data.Data
import qualified Data.Vector as V
import qualified Text.PrettyPrint as PP
import Text.Printf
import qualified System.Process.Typed as P
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.List as L
import qualified System.Directory as D


-- | Class for making pretty-printable show instances.
class TushShow a where
  tshow :: a -> IO PP.Doc

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
data Relativity = Relative | Absolute | PATH | HOME
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
type IsDir = Bool

data Token = TIdentifier Text
           | TOperator Text
           | TPath (Vector Text) Relativity IsDir
           | TString Text
           | TInt Int
           | TChar Char
           | TUnit
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
           | Bang
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
-- PathComponents are just file/directory name literals at the moment.
newtype PathComponent = PathComponent Text
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

pathComponentToFilePath :: PathComponent -> FilePath
pathComponentToFilePath (PathComponent pc) = unpack pc

instance TushShow PathComponent where
  tshow (PathComponent pc) = return $ PP.text $ unpack pc

newtype PathExtension = PathExtension { _unPathExtension :: Text }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance TushShow PathExtension where
  tshow (PathExtension pe) = return $ PP.text "." <> (PP.text $ unpack pe)

data Path = Path
  { _pathDirectory :: Vector PathComponent
  , _pathFile :: Maybe (PathComponent, Maybe PathExtension)
  , _pathRelativity :: Relativity
  , _pathIsDirectory :: Bool
  } deriving (Eq, Ord, Show, Data, Typeable, Generic)

pathToFilePathWithHomeAs :: MonadIO m => m FilePath -> Path -> m FilePath
pathToFilePathWithHomeAs x Path {..} = do
  prefix <- case _pathRelativity of
              Relative -> return "./"
              Absolute -> return "/"
              PATH -> return ""
              HOME -> x
  let directory = intersperse "/" $ pathComponentToFilePath <$> _pathDirectory
      file = maybe "" (pathComponentToFilePath . fst) _pathFile
      mExtension = do
        f <- _pathFile
        snd f
      extension = maybe "" (unpack . _unPathExtension) mExtension
      dottedExtension = if null extension
                        then ""
                        else "." ++ extension
      postfix = if _pathIsDirectory then "/" else ""
      path = (prefix </> concat directory) ++ (if not $ null directory then "/" else "") ++ file ++ dottedExtension ++ postfix
  if _pathRelativity == PATH
    then do
      (_, p, _) <- P.readProcess $ fromString $ printf "which %s" (unpack path)
      return $ L.init $ BS.unpack p
    else return path

pathToFilePath :: MonadIO m => Path -> m FilePath
pathToFilePath = pathToFilePathWithHomeAs $ do
  home <- liftIO D.getHomeDirectory
  return $ home ++ "/"

pathToVisualPath :: MonadIO m => Path -> m FilePath
pathToVisualPath = pathToFilePathWithHomeAs (return "~/")

instance TushShow Path where
  tshow p = PP.text <$> pathToVisualPath p

newtype Identifier = Identifier { _unIdentifier :: Text }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance TushShow Identifier where
  tshow (Identifier i) = return $ PP.text $ unpack i

newtype Operator = Operator { _unOperator :: Text }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance TushShow Operator where
  tshow (Operator o) = return $ PP.text $ unpack o

newtype TushString = TushString { _unTushString :: Text }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance TushShow TushString where
  tshow (TushString s) = return $ PP.text $ unpack s

data Name = NIdentifier Identifier
          | NOperator Operator
          deriving (Eq, Ord, Show, Data, Typeable, Generic)

nameToText :: Name -> Text
nameToText (NIdentifier (Identifier x)) = x
nameToText (NOperator (Operator x)) = x

instance TushShow Name where
  tshow n = return $ PP.text $ unpack $ nameToText n

data Call = Call
  { _callFunc :: Expression
  , _callOperand :: Expression
  } deriving (Show, Typeable, Generic)

instance TushShow Call where
  tshow Call {..} = do
    f <- tshow _callFunc
    o <- tshow _callOperand
    return $ f PP.<+> o

newtype TushVector = TushVector { _unTushVector :: Vector Expression }
  deriving (Show, Typeable, Generic)

instance TushShow TushVector where
  tshow (TushVector v) = do
    v' <- forM v tshow
    return $ PP.char '[' <> (PP.hsep $ toList v') <> PP.char ']'

newtype TushTuple = TushTuple { _unTushTuple :: (Expression, Expression) }
  deriving (Show, Typeable, Generic)

instance TushShow TushTuple where
  tshow (TushTuple t) = do
    f <- tshow $ fst t
    s <- tshow $ snd t
    return $ PP.char '(' <> f <> PP.text ", " <> s <> PP.char ')'

newtype TushInt = TushInt { _unTushInt :: Int }
  deriving (Eq, Ord, Show, Data, Typeable, Generic, Num)

instance TushShow TushInt where
  tshow (TushInt i) = return $ PP.int i

newtype TushChar = TushChar { _unTushChar :: Char }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance TushShow TushChar where
  tshow (TushChar c) = return $ PP.text $ show c

data TushUnit = TushUnit
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance TushShow TushUnit where
  tshow _ = return $ PP.text "()"

newtype TushBool = TushBool { _unTushBool :: Bool }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance TushShow TushBool where
  tshow (TushBool b) = return $ PP.text $ show b

data Builtin = Builtin
  { _builtinName :: Text
  , _builtinFunc :: Expression -> IO Expression
  } deriving (Typeable, Generic)

instance Show Builtin where
  show Builtin {..} = printf "<<Builtin Function %s>>" (unpack _builtinName)

instance TushShow Builtin where
  tshow b = return $ PP.text $ show b

data Ite = Ite
  { _iteIf   :: Expression
  , _iteThen :: Expression
  , _iteElse :: Expression
  } deriving (Show, Typeable, Generic)

instance TushShow Ite where
  tshow Ite {..} = do
    i <- tshow _iteIf
    t <- tshow _iteThen
    e <- tshow _iteElse
    return $ (PP.text "if" PP.<+> i)
      PP.$+$ (PP.text "then" PP.<+> t)
      PP.$+$ (PP.text "else" PP.<+> e)

data Bind = Bind
  { _bindName :: Expression
  , _bindValue :: Expression
  , _bindBody :: Expression
  } deriving (Show, Typeable, Generic)

instance TushShow Bind where
  tshow Bind {..} = do
    n <- tshow _bindName
    v <- tshow _bindValue
    b <- tshow _bindBody
    return $ PP.text "let" PP.<+> n PP.<+> PP.char '=' PP.<+> v PP.<+> PP.text "in" PP.<+> b

type Environment = Map Text Expression

data Lambda = Lambda
  { _lambdaArg :: Expression
  , _lambdaBody :: Expression
  } deriving (Show, Typeable, Generic)

instance TushShow Lambda where
  tshow Lambda {..} = do
    a <- tshow _lambdaArg
    b <- tshow _lambdaBody
    return $ (PP.char '\\' <> a) PP.<+> PP.text "->" PP.<+> b

-- Lambda with attached env
data EnvLambda = EnvLambda
  { _envLambdaArg :: Expression
  , _envLambdaBody :: Expression
  , _envLambdaEnv :: Environment
  } deriving (Show, Typeable, Generic)

instance TushShow EnvLambda where
  tshow EnvLambda {..} = do
    a <- tshow _envLambdaArg
    b <- tshow _envLambdaBody
    return $ (PP.char '\\' <> a) PP.<+> PP.text "->" PP.<+> b

newtype Sequence = Sequence
  { _sequenceExpressions :: Vector Expression
  } deriving (Show, Typeable, Generic)

instance TushShow Sequence where
  tshow (Sequence s) = do
    s' <- forM s tshow
    return $ PP.text "do" PP.<+> (PP.braces $ PP.hsep $ toList $ intersperse (PP.text "; ") s')

data Expression = ECall Call
                | EName Name
                | EPath Path
                | EString TushString
                | EVector TushVector
                | ETuple TushTuple
                | EInt TushInt
                | EBool TushBool
                | EChar TushChar
                | EUnit TushUnit
                | EBuiltin Builtin
                | EIte Ite
                | EBind Bind
                | ELambda Lambda
                | EEnvLambda EnvLambda
                | ESequence Sequence
                deriving (Show, Typeable, Generic)

instance TushShow Expression where
  tshow (ECall c) = tshow c
  tshow (EName n) = tshow n
  tshow (EPath p) = tshow p
  tshow (EString s) = tshow s
  tshow (EVector v) = tshow v
  tshow (ETuple t) = tshow t
  tshow (EInt i) = tshow i
  tshow (EBool b) = tshow b
  tshow (EChar c) = tshow c
  tshow (EUnit u) = tshow u
  tshow (EBuiltin b) = tshow b
  tshow (EIte i) = tshow i
  tshow (EBind b) = tshow b
  tshow (ELambda l) = tshow l
  tshow (EEnvLambda e) = tshow e
  tshow (ESequence s) = tshow s

data Statement = SAssignment Assignment
  deriving (Show, Typeable, Generic)

data Assignment = Assignment
                  { _assignmentName :: Name
                  , _assignmentValue :: Expression
                  } deriving (Show, Typeable, Generic)

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
  , ''TushChar
  , ''Ite
  , ''Bind
  , ''Lambda
  , ''Assignment
  ]
