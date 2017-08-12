{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

module Tush.Typecheck.Unify where

import ClassyPrelude hiding (throwM)

import Data.Equivalence.Monad
import Control.Monad.Catch.Pure

import Tush.Syntax

isCompatibleWith :: Type -> Type -> Bool
isCompatibleWith (TyADT x) (TyADT y) = x == y
isCompatibleWith (TyADT _) (TyLambda _) = False
isCompatibleWith (TyADT _) (TyBuiltinType _) = False
isCompatibleWith (TyADT _) (TyVar _) = True
isCompatibleWith (TyLambda x) (TyLambda y) = x `isLambdaCompatibleWith` y
isCompatibleWith (TyLambda _) (TyBuiltinType _) = False
isCompatibleWith (TyLambda _) (TyVar _) = True
isCompatibleWith (TyBuiltinType x) (TyBuiltinType y) = x == y
isCompatibleWith (TyBuiltinType _) (TyVar _) = True
isCompatibleWith (TyVar _) (TyVar _) = True
isCompatibleWith TyBadType _ = False
-- Symmetric relation
isCompatibleWith x y = y `isCompatibleWith` x

isLambdaCompatibleWith :: Lambda Type -> Lambda Type -> Bool
isLambdaCompatibleWith (Lambda xret xarg) (Lambda yret yarg) =
  xarg `isCompatibleWith` yarg && xret `isCompatibleWith` yret

compatibleType :: Type -> Type -> Type
compatibleType x y | x `isCompatibleWith` y = 
                       case (x, y) of
                         (x'@(TyADT _), TyADT _) -> x'
                         (x'@(TyADT _), TyVar _) -> x'
                         (TyLambda x', TyLambda y') -> TyLambda $ compatibleLambda x' y'
                         (x'@(TyLambda _), TyVar _) -> x'
                         (x'@(TyBuiltinType _), TyBuiltinType _) -> x'
                         (x'@(TyBuiltinType _), TyVar _) -> x'
                         (x'@(TyVar _), TyVar _) -> x'
                         (x', y') -> compatibleType y' x'
                   | otherwise = TyBadType

compatibleLambda :: Lambda Type -> Lambda Type -> Lambda Type
compatibleLambda (Lambda xret xarg) (Lambda yret yarg) = let
  lrt = compatibleType xret yret
  lat = compatibleType xarg yarg in
  Lambda { lamReturnType = lrt
         , lamArgType    = lat
         }

unify' :: ( MonadEquiv c Type a m
          , MonadThrow m ) => 
          TypeConstraint -> m ()
unify' (UnifyWith TyBadType _) = throwM BadTypeError
unify' (UnifyWith _ TyBadType) = throwM BadTypeError
unify' (UnifyWith x y) = x `equate` y

unify :: ( MonadEquiv (Vector Type) Type Type m
         , MonadThrow m ) =>
         TypeConstraint -> m ()
unify (UnifyWith x y) = do
  -- if these guys are incompatible, then we are fucked either way.
  unless (x `isCompatibleWith` y) $
    throwM $ TypeMismatchProblem (fromList [x, y])
  -- next try to unify their descriptors
  xdesc <- classDesc x
  ydesc <- classDesc y
  unless (xdesc `isCompatibleWith` ydesc) $
    -- The equivalence classes can't unify because of other
    -- constraints
    throwM $ TypeMismatchProblem (fromList [x, y])
  -- These guys can be unified, so do it!
  unify' (UnifyWith x y)

getRestrictedTypes :: MonadEquiv (Vector Type) Type Type m =>
                      Vector Type -> m (Vector Type)
getRestrictedTypes ts = mapM classDesc ts

runTypechecker :: Monad m =>
                  (forall s. EquivT s Type Type m a)
               -> m a
runTypechecker = runEquivT id compatibleType
