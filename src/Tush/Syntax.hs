{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

module Tush.Syntax where

import Control.Lens

import Text.Printf
import Data.Typeable
import Data.Data

import ClassyPrelude

-- | Vars

data VarClass = VClassNormal
              | VClassOperator
              | VClassType
  deriving (Eq, Ord, Show, Data, Typeable)

data Var = Var { _varName :: Text
               , _varClass :: VarClass
               } deriving (Eq, Ord, Show, Data, Typeable)

makeLenses ''Var

isOp :: Var -> Bool
isOp (Var _ VClassOperator) = True
isOp _ = False

-- | Tokens

data ReservedWord = For 
                  | If 
                  | Then 
                  | Else 
                  | In 
                  | Let 
                  | Def 
                  | Extern
  deriving (Eq, Ord, Show)

data ReservedOp = Arrow
                | Equals
  deriving (Eq, Ord, Show)

data ReservedPunctuation = Comma
                         | Semicolon
                         | Colon
                         | OpenParen
                         | CloseParen
                         | OpenBrace
                         | CloseBrace
                         | OpenBracket
                         | CloseBracket
                         | Backslash
  deriving (Eq, Ord, Show)

data Token = CommentT Text
           | ReservedWordT ReservedWord
           | ReservedOpT ReservedOp
           | ReservedPunctuationT ReservedPunctuation
           | VarT Var
           | LiteralT Literal
           | EofT
  deriving (Eq, Ord, Show)

isVarT :: Token -> Bool
isVarT (VarT _) = True
isVarT _ = False

isOpT :: Token -> Bool
isOpT (VarT v) = isOp v
isOpT _ = False

isOpenParenT :: Token -> Bool
isOpenParenT (ReservedPunctuationT OpenParen) = True
isOpenParenT _ = False

isCommaT :: Token -> Bool
isCommaT (ReservedPunctuationT Comma) = True
isCommaT _ = False

isColonT :: Token -> Bool
isColonT (ReservedPunctuationT Colon) = True
isColonT _ = False

isLiteralT :: Token -> Bool
isLiteralT (LiteralT _) = True
isLiteralT _ = False

-- | Syntax Tree

data Literal = ILit Integer
             | FLit Double
             | BLit Bool 
             deriving (Eq, Ord, Show, Data, Typeable)

--type Env t = Map Var (Expression t)

data Expression t = LitE { _litELiteral :: Literal
                         , _exprType    :: t
                         }
                  | VarE { _varEVar  :: Var
                         , _exprType :: t
                         }
                  | AppE { _appEFunc :: Expression t
                         , _appEArg  :: Expression t
                         , _exprType  :: t
                         }
                  | IfE  { _ifEConditional :: Expression t
                         , _ifEConsequent  :: Expression t
                         , _ifEAntecedent  :: Expression t
                         , _exprType       :: t
                         }
                  | LamE { _lamEArg  :: Expression t
                         , _lamEBody :: Expression t
                         , _exprType :: t
                         }
  deriving ( Eq
           , Ord
           , Show
           , Functor
           , Foldable
           , Traversable
           , Typeable
           , Data )

makeLenses ''Expression

data FProto t = FProto { _fProtoName :: Expression t
                       , _fProtoArg  :: Expression t
                       } deriving ( Eq
                                  , Ord
                                  , Show
                                  , Functor
                                  , Traversable
                                  , Foldable
                                  , Data
                                  , Typeable )

makeLenses ''FProto

data Statement a b = ExprS (Expression b)
                   | FuncS (FProto b) (Vector (Statement a b)) (Expression b)
                   | ExternS (FProto a)
                   deriving ( Eq
                            , Ord
                            , Show
                            , Functor
                            , Foldable
                            , Traversable
                            , Data
                            , Typeable )

isExprS :: Statement a b -> Bool
isExprS (ExprS _) = True
isExprS _ = False

-- | The way types work in tush is like this: You lex then parse the
-- source, and this code is PreTyped.  It might have type annotations
-- and stuff, but it's probably mostly not typed yet.  Anything whose
-- type we don't know is represented by Nothing.  Typechecking
-- proceeds by giving each of these entities either an explicit type,
-- a placeholder "named" type, or a type variable.  It also collects
-- all type definitions to be used during reification (see below).
-- This whole process is called "pretyping" or just "typing".
-- 
-- After all type /definitions/ are known, it is now possible to
-- replace the named types with their actual types.  This process is
-- called "reification".
-- 
-- For definitions, type variables in final form are okay, but for
-- expressions it is not.  Therefore, Statement is of kind * -> * ->
-- *.  The first variable is the type of definitions, which is
-- isomorphic to Either TypeVar Type, and the second variable is the
-- type of expressions, which must be Type ~ BuiltinType.  The process
-- of converting Expressions from Either TypeVar Type to plain Type is
-- called "specialization".

type Kind = ()

data BuiltinType = BTInt
                 | BTFloat
                 | BTBool
                 deriving (Eq, Ord, Show, Typeable, Data)

data ManifestType = MTyName Var 
                  | MTyType (Type' ManifestType)
                  deriving (Eq, Ord, Show, Typeable, Data)

data PreType = PTyType (Type' PreType)
             | PTyMType ManifestType
             | PTyUntyped
             deriving (Eq, Ord, Show, Typeable, Data)

data Lambda t = Lambda { _lamReturnType :: t
                       , _lamArgType :: t
                       } deriving ( Eq
                                  , Ord
                                  , Show
                                  , Functor
                                  , Typeable
                                  , Data
                                  , Traversable
                                  , Foldable )

data ADTClass = Sum | Product deriving (Eq, Ord, Show, Typeable, Data)
data ADT t = ADT { _adtClass :: ADTClass
                 , _adtTypes :: Vector t
                 , _adtName  :: Var
                 } deriving ( Eq
                            , Ord
                            , Show
                            , Functor
                            , Typeable
                            , Data
                            , Traversable
                            , Foldable )

data Type' t = TyADT (ADT t)
             | TyLambda (Lambda t)
             | TyBuiltinType BuiltinType
             | TyVar Var
             | TyBadType
             deriving (Eq, Ord, Show, Typeable, Data)

data Type = Type (Type' Type) deriving (Eq, Ord, Show, Typeable, Data)

makeLenses ''Lambda
makeLenses ''ADT

data TypeConstraint = UnifyWith Type Type deriving (Eq, Ord, Show)
newtype TypeVarCounter = TypeVarCounter Word deriving (Eq, Ord, Show)

-- | Exceptions

data CompilerError = forall e . Exception e => CompilerError e deriving Typeable
instance Exception CompilerError
instance Show CompilerError where show (CompilerError e) = displayException e

data TypeMismatchProblem = TypeMismatchProblem { tmpType :: Type } 
  deriving (Typeable, Show)
instance Exception TypeMismatchProblem where
  toException = toException . CompilerError
  fromException x = do
    CompilerError e <- fromException x
    cast e
  displayException (TypeMismatchProblem {..}) = printf "Could not match types %v for some reason." (show tmpType)

data TypeNotDefined = TypeNotDefined { tndType :: Var }
  deriving (Typeable, Show)
instance Exception TypeNotDefined where
  toException = toException . CompilerError
  fromException x = do
    CompilerError e <- fromException x
    cast e
  displayException (TypeNotDefined {..}) = printf "Type type `%v' is not defined." (show tndType)

data FunctionMisapplied = FunctionMisapplied { fmName :: Var
                                             , fmActualTypes :: Vector Type
                                             , fmExpectedTypes :: Vector Type
                                             } deriving (Typeable, Show)
instance Exception FunctionMisapplied where
  toException = toException . CompilerError
  fromException x = do
    CompilerError e <- fromException x
    cast e
  displayException (FunctionMisapplied {..}) = printf "Function `%v' was applied to `%v', but expected `%v'." (show fmName) (show fmActualTypes) (show fmExpectedTypes)

data VariableNotDefined = VariableNotDefined { vndVar :: Var }
  deriving (Typeable, Show)
instance Exception VariableNotDefined where
  toException = toException . CompilerError
  fromException x = do
    CompilerError e <- fromException x
    cast e
  displayException (VariableNotDefined {..}) = printf "The variable `%v' is not defined." (show vndVar)

data BadTypeError = BadTypeError
  deriving (Typeable, Show)
instance Exception BadTypeError where
  toException = toException . CompilerError
  fromException x = do
    CompilerError e <- fromException x
    cast e
  displayException BadTypeError = printf "Y'all fucked up.  Bad Type Error."
