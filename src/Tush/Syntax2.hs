{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- | Syntax2.hs
-- Author: Matt Walker
-- License: https://opensource.org/licenses/BSD-2-Clause
-- Created: Aug 12, 2017
-- Summary: Redoing it with TTF

{-# LANGUAGE OverloadedStrings #-}

module Tush.Syntax2 where

import ClassyPrelude

newtype Var = Var { varName :: Text }

class LitSym repr where
  litI :: Int    -> repr Int
  litF :: Double -> repr Double
  litB :: Bool   -> repr Bool
  litS :: Text   -> repr Text

class LamSym repr env where
  lamE :: (env -> repr a -> repr b) -> repr (env -> a -> b)
  appE :: repr (env -> a -> b) -> env -> repr a -> repr b

class IfSym repr where
  ifE :: repr Bool -> repr a -> repr a -> repr a

newtype Tushi a = Tushi { unTushi :: a }

instance LitSym Tushi where
  litI = Tushi
  litF = Tushi
  litB = Tushi
  litS = Tushi

instance LamSym Tushi () where
  lamE f = Tushi $ (\env x -> unTushi $ f env (Tushi x))
  appE (Tushi f) env (Tushi x) = Tushi (f env x)

instance IfSym Tushi where
  ifE (Tushi b) cons_ ante = if b then cons_ else ante
