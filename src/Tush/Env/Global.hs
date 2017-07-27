{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Tush.Env.Global where

import ClassyPrelude

import Tush.Parse.Syntax

binop :: Text -> Expression () -> Expression () -> Expression ()
binop sym x y = CallE (VarE (Var sym () True) ()) (fromList [x, y]) ()

add :: Expression () -> Expression () -> Expression ()
add = binop "+"

sub :: Expression () -> Expression () -> Expression ()
sub = binop "-"

mul :: Expression () -> Expression () -> Expression ()
mul = binop "*"

div :: Expression () -> Expression () -> Expression ()
div = binop "/"

lt :: Expression () -> Expression () -> Expression ()
lt  = binop "<"

globals :: Map (Var ()) (Vector (Expression ()) -> Expression ())

bintrinsics :: Map (Var ()) (Expression () -> Expression () -> Expression ())
