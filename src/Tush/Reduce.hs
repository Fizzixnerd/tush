{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Function.hs
-- Author: Matt Walker
-- License: https://opensource.org/licenses/BSD-2-Clause
-- Created: Aug 12, 2017
-- Summary: 

module Tush.Function where

import ClassyPrelude

import Control.Monad.State

import Tush.Syntax

-- | Replace all the VarEs by the actual expression they are bound
-- to.
reduce :: ( MonadState (Env t) m
          , MonadThrow m ) => 
          Expression t -> m (Expression t)
reduce x = do
  env <- get
  case x of
    (LitE _ _) -> return x
    (VarE v _) -> case lookup v env of
                    -- if we don't find it, it could simply be because
                    -- it is a function argument and so isn't bound to
                    -- an expression (yet).
                    Nothing -> return x
                    (Just e) -> return e
    (CallE fn arg t) -> do
      fn' <- reduce fn
      arg' <- reduce arg
      return $ CallE fn' arg' t
    (IfE cond con ante t) -> do
      cond' <- reduce cond
      cons' <- reduce con
      ante' <- reduce ante
      return $ IfE cond' cons' ante' t
    (FuncE arg bod fenv t) -> do
      put fenv
      bod' <- reduce bod
      put env
      return $ FuncE arg bod' fenv t
