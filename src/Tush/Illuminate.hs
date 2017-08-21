{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Illuminate.hs
-- Author: Matt Walker
-- License: https://opensource.org/licenses/BSD-2-Clause
-- Created: Aug 20, 2017
-- Summary: 

module Tush.Illuminate where

import ClassyPrelude

import Text.Printf
import Control.Lens
import Control.Monad.State

import Data.Data

import Tush.Syntax

newtype NameCounter = NameCounter Word
  deriving (Eq, Ord, Show)
data IlluminateState = IlluminateState { _nameCounter :: NameCounter }

makeLenses ''IlluminateState

newtype Illumination a = Illumination { unilluminate :: (StateT IlluminateState Identity) a }
  deriving (Functor, Applicative, Monad, MonadState IlluminateState)

runIllumination :: Illumination a -> a
runIllumination x = fst $ runIdentity $ runStateT (unilluminate x) IlluminateState { _nameCounter = NameCounter 0 }

newName :: MonadState IlluminateState m =>
           m Text
newName = do
  (NameCounter nc) <- gets $ view nameCounter
  let name = fromString $ printf "__x%v" (show nc)
  nameCounter .= (NameCounter $ nc + 1)
  return name

-- | Remove variable name shadowing by renaming of shadowing
-- variables.
illuminate :: (MonadState IlluminateState m, Eq t, Data t) =>
              Expression t -> m (Expression t)
illuminate (LamE var arg t) = do
  (var', arg') <- illuminate' var arg
  arg'' <- illuminate arg'
  return $ LamE var' arg'' t
illuminate y@(VarE _ _) = return y
illuminate y@(LitE _ _) = return y
illuminate (AppE arg bdy t) = do
  arg' <- illuminate arg
  bdy' <- illuminate bdy
  return $ AppE arg' bdy' t
illuminate (IfE i th el t) = do
  i' <- illuminate i
  th' <- illuminate th
  el' <- illuminate el
  return $ IfE i' th' el' t

illuminateS (ExprS e) = ExprS <$> illuminate e
illuminateS x = return x

-- | Helper for illuminate that renames all instances of the Var @v@.
illuminate' :: (MonadState IlluminateState m, Eq t, Data t) =>
               Expression t -> Expression t -> m (Expression t, Expression t)
illuminate' v e = do
  name <- rename v
  return $ (name, transform (\x -> if x == v then name else x) e)

rename :: MonadState IlluminateState m =>
          Expression t -> m (Expression t)
rename (VarE (Var _ c) t) = do
  name <- newName
  return (VarE (Var name c) t)
rename x = return x

-- | Reshadow variables, restoring their original names.
eclipse :: Expression t -> Expression t
eclipse = undefined
