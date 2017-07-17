{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Tush.Parse.Syntax where

import ClassyPrelude

-- Op, Var is defined in Lex.
import Tush.Parse.Lex

data FProto = FProto Var (Vector Var)
  deriving (Eq, Ord, Show)

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
