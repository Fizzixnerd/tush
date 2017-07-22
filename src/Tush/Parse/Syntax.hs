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

newtype Var = Var Text deriving (Eq, Ord, Show, IsString)

data BOp = Add | Sub | Mul | Div | Lt | Or | And deriving (Eq, Ord, Show)
data UOp = Neg | Not deriving (Eq, Ord, Show)


-- | Syntax Tree

data FProto = FProto { fProtoName :: Var
                     , fProtoArgs :: (Vector Var)
                     }
            deriving (Eq, Ord, Show)

data Literal = ILit Integer
             | FLit Double
             | BLit Bool deriving (Eq, Ord, Show)

data Expression = LitE Literal
                | BinOpE BOp Expression Expression
                | UnOpE  UOp Expression
                | VarE Var
                | CallE { callEName :: Var
                        , callEArgs :: (Vector Expression)
                        }
                | IfE { ifEConditional :: Expression
                      , ifEConsequent  :: Expression
                      , ifEAntecedent  :: Expression
                      }
                deriving (Eq, Ord, Show)

data Statement = ExprS Expression
               | FuncS FProto Expression
               | ExternS FProto
               deriving (Eq, Ord, Show)
