{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

module Tush.Syntax where

import Text.Printf
import Data.Typeable

import ClassyPrelude

-- | Vars

data VarClass = VClassNormal
              | VClassOperator
  deriving (Eq, Ord, Show)

data Var = Var { varName :: Text
               , varClass :: VarClass
               } deriving (Eq, Ord, Show)

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

data FProto a = FProto { fProtoName :: Expression a
                       , fProtoArgs :: Vector (Expression a)
                       } deriving (Eq, Ord, Show, Functor, Traversable, Foldable)

data Literal = ILit Integer
             | FLit Double
             | BLit Bool 
             deriving (Eq, Ord, Show)

data Expression t = LitE { litELiteral :: Literal
                         , exprType    :: t
                         }
                  | VarE { varEVar  :: Var
                         , exprType :: t
                         }
                  | CallE { callEFunc :: Expression t
                          , callEArg  :: Expression t
                          , exprType  :: t
                          }
                  | IfE { ifEConditional :: Expression t
                        , ifEConsequent  :: Expression t
                        , ifEAntecedent  :: Expression t
                        , exprType       :: t
                        }
                  | FuncE { funcEArg  :: Var
                          , funcEBody :: Expression t
                          , funcEEnv  :: Map Var (Expression t)
                          , exprType  :: t
                          } deriving ( Eq
                                     , Ord
                                     , Show
                                     , Functor
                                     , Foldable
                                     , Traversable )

data Statement a b = ExprS (Expression b)
                   | FuncS (FProto b) (Vector (Statement a b)) (Expression b)
                   | ExternS (FProto a)
                   deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

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
                 deriving (Eq, Ord, Show)

data PreType = PTyManifestType ManifestType | PTyUntyped deriving (Eq, Ord, Show)
data ManifestType = MTyName Var | MTyType Type deriving (Eq, Ord, Show)
data Lambda t = Lambda { lamReturnType :: t
                       , lamArgType :: t
                       } deriving (Eq, Ord, Show, Functor)
data ADT t = ADT { adtClass :: ADTClass
                 , adtTypes :: Vector t
                 , adtName  :: Name
                 } deriving (Eq, Ord, Show, Functor)
type Name = Text
data ADTClass = Sum | Product deriving (Eq, Ord, Show)
data Type = TyADT (ADT Type)
          | TyLambda (Lambda Type)
          | TyBuiltinType BuiltinType
          | TyVar Var
          | TyBadType
          deriving (Eq, Ord, Show)

data TypeConstraint = UnifyWith Type Type deriving (Eq, Ord, Show)
newtype TypeVarCounter = TypeVarCounter Word deriving (Eq, Ord, Show)

-- | Exceptions

data CompilerError = forall e . Exception e => CompilerError e deriving Typeable
instance Exception CompilerError
instance Show CompilerError where show (CompilerError e) = displayException e

data TypeMismatchProblem = TypeMismatchProblem { tmpTypes :: Vector Type } 
  deriving (Typeable, Show)
instance Exception TypeMismatchProblem where
  toException = toException . CompilerError
  fromException x = do
    CompilerError e <- fromException x
    cast e
  displayException (TypeMismatchProblem {..}) = printf "Could not match types %v for some reason." (show tmpTypes)

data TypeNotDefined = TypeNotDefined { tndType :: Var } deriving (Typeable, Show)
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

data VariableNotDefined = VariableNotDefined { vndVar :: Var } deriving (Typeable, Show)
instance Exception VariableNotDefined where
  toException = toException . CompilerError
  fromException x = do
    CompilerError e <- fromException x
    cast e
  displayException (VariableNotDefined {..}) = printf "The variable `%v' is not defined." (show vndVar)

data BadTypeError = BadTypeError deriving (Typeable, Show)
instance Exception BadTypeError where
  toException = toException . CompilerError
  fromException x = do
    CompilerError e <- fromException x
    cast e
  displayException BadTypeError = printf "Y'all fucked up.  Bad Type Error."
