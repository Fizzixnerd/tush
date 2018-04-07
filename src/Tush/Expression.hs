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

getVars :: (Data t, Ord t) => Expression t -> Set (Expression t)
getVars e = S.fromList $ [v | v@(VarE _ _) <- universe e]

-- | NOTE: Assumes variables are unique!
getFreeVars :: (Ord t, Data t) => Expression t -> Set (Expression t)
getFreeVars e = (S.fromList (toList $ getVars e) S.\\ S.fromList (toList $ getBoundVars e))

getBoundVars :: (Data t, Ord t) => Expression t -> Set (Expression t)
getBoundVars e = S.fromList $ [v | LamE v@(VarE _ _) _ _ <- universe e]
