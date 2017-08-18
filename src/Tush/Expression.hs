-- | Expression.hs
-- Author: Matt Walker
-- License: https://opensource.org/licenses/BSD-2-Clause
-- Created: Aug 13, 2017
-- Summary: 

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tush.Expression where

import ClassyPrelude

import Tush.Syntax

import Data.Generics.Uniplate.Data
import Data.Data

import qualified Data.Set as S

getVars :: Data t => Expression t -> Vector Var
getVars e = fromList $ toList $ S.fromList $ [v | VarE v _ <- universe e]

-- | NOTE: Assumes variables are unique!
getFreeVars :: Data t => Expression t -> Vector Var
getFreeVars e = fromList $ S.toList $ (S.fromList (toList $ getVars e) S.\\ S.fromList (toList $ getBoundVars e))

getBoundVars :: Data t => Expression t -> Vector Var
getBoundVars e = fromList $ toList $ S.fromList $ [v | LamE v _ _ _ <- universe e]
