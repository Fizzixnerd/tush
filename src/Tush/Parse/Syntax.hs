{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Tush.Parse.Syntax where

import ClassyPrelude

data FProto = FProto Var (Vector Var)
  deriving (Eq, Ord, Show)

newtype Var = Var Text deriving (Eq, Ord, Show, IsString)

data Extern = Extern deriving (Eq, Show)
data Def = Def deriving (Eq, Show)
data BOp = Add | Sub | Mul | Div deriving (Eq, Ord, Show)
data UOp = Neg deriving (Eq, Ord, Show)
data Terminator = Terminator deriving (Eq, Show)

data Lit = ILit Integer
         | FLit Double deriving (Eq, Ord, Show)

data Expr = LitE Lit
          | BinOpE BOp Expr Expr
          | UnOpE UOp Expr
          | VarE Var
          | CallE Var (Vector Expr)
          deriving (Eq, Ord, Show)


data Statement = ExprS Expr
               | FuncS FProto Expr
               | ExternS FProto
               deriving (Eq, Ord, Show)
