{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

module Tush.Syntax where

import ClassyPrelude

-- | Tokens

data VarClass = VClassNormal
              | VClassOperator
  deriving (Eq, Ord, Show)

data Var a = Var { varName :: Text
                 , varClass :: VarClass
                 } deriving (Eq, Ord, Show)
data FlexibleV
data RigidV
data TypeV
data ValueV

isOp :: Var a -> Bool
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
           | VarT (Var ValueV)
           | TVarT (Var TypeV)
           | LiteralT Literal
           | EofT
  deriving (Eq, Ord, Show)

isVarT :: Token -> Bool
isVarT (VarT _) = True
isVarT _ = False

isOpT :: Token -> Bool
isOpT (VarT v) = isOp v
isOpT _ = False

isTVarT :: Token -> Bool
isTVarT (TVarT _) = True
isTVarT _ = False

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
             | BLit Bool deriving (Eq, Ord, Show)

data Expression a = LitE { litELiteral :: Literal
                         , exprType    :: a
                         }
                  | VarE { varEVar  :: Var ValueV
                         , exprType :: a
                         }
                  | CallE { callEName :: Expression a
                          , callEArgs :: Vector (Expression a)
                          , exprType  :: a
                          }
                  | IfE { ifEConditional :: Expression a
                        , ifEConsequent  :: Expression a
                        , ifEAntecedent  :: Expression a
                        , exprType       :: a
                        } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

exprInfo :: Expression a -> a
exprInfo (LitE _ x) = x
exprInfo (VarE _ x) = x
exprInfo (CallE _ _ x) = x
exprInfo (IfE _ _ _ x) = x

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

newtype PreType = PreType (Maybe ManifestType) deriving (Eq, Ord, Show)
newtype ManifestType = ManifestType (Either (Var TypeV) AbstractType) deriving (Eq, Ord, Show)
newtype AbstractType = AbstractType (Either QuantifiedType ConcreteType) deriving (Eq, Ord, Show)
data Type term sub var = TyADT (ADT sub)
                       | TyLambda (Lambda sub)
                       | TyTerm term
                       | TyVar var
                       deriving (Eq, Ord, Show, Functor)
data Lambda t = Lambda { lamReturnType :: t
                       , lamArgTypes :: Vector t
                       } deriving (Eq, Ord, Show, Functor)
data ADT t = ADT { adtClass :: ADTClass
                 , adtTypes :: Vector t
                 } deriving (Eq, Ord, Show, Functor)
data ADTClass = Sum | Product deriving (Eq, Ord, Show)
newtype QuantifiedType = QuantifiedType (Type () AbstractType (Var FlexibleV)) deriving (Eq, Ord, Show)
newtype ConcreteType = ConcreteType (Type BuiltinType ConcreteType (Var RigidV)) deriving (Eq, Ord, Show)

data Type t = TyADT (Type t)
            | TyLambda (Type t)
            | TyBuiltinType BuiltinType
            | TyVar Var

data TypeConstraint = UnifyWith AbstractType AbstractType deriving (Eq, Ord, Show)
newtype TypeVarCounter = TypeVarCounter Word deriving (Eq, Ord, Show)
