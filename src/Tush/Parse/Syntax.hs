{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tush.Parse.Syntax where

import ClassyPrelude

-- | Tokens

data Extern = Extern deriving (Eq, Ord, Show)
data Def = Def deriving (Eq, Ord, Show)

data If = If deriving (Eq, Ord, Show)
data Then = Then deriving (Eq, Ord, Show)
data Else = Else deriving (Eq, Ord, Show)

data Terminator = Terminator deriving (Eq, Ord, Show)
data TypeAs = TypeAs deriving (Eq, Ord, Show)
data Arrow = Arrow deriving (Eq, Ord, Show)

data Var a = Var { varName :: Text
                 , varInfo :: a
                 }  deriving (Eq, Ord, Show)
type SimplyTypedVar = Var BuiltinType

data BOp = Add | Sub | Mul | Div | Lt | Or | And deriving (Eq, Ord, Show)
data UOp = Neg | Not deriving (Eq, Ord, Show)

-- | Syntax Tree

data FProto a = FProto { fProtoName :: Var a
                       , fProtoArgs :: Vector (Var a)
                       }
              deriving (Eq, Ord, Show)

data Literal = ILit Integer
             | FLit Double
             | BLit Bool deriving (Eq, Ord, Show)

data Expression a = LitE Literal a
                  | BinOpE BOp (Expression a) (Expression a) a
                  | UnOpE  UOp (Expression a) a
                  | VarE (Var a) a
                  | CallE { callEName :: Expression a
                          , callEArgs :: Vector (Expression a)
                          , callEInfo :: a
                          }
                  | IfE { ifEConditional :: Expression a
                        , ifEConsequent  :: Expression a
                        , ifEAntecedent  :: Expression a
                        , ifEInfo        :: a
                        }
                  deriving (Eq, Ord, Show)

data Statement a b = ExprS (Expression a)
                   | FuncS (FProto b) (Vector (Statement a b))
                   | ExternS (FProto b)
                   deriving (Eq, Ord, Show)

data BuiltinType = BTInt
                 | BTFloat
                 | BTBool
                 | BTLambda { btLambdaReturnType :: BuiltinType
                            , btLambdaArgTypes :: (Vector BuiltinType)
                            }
                 deriving (Eq, Ord, Show)
