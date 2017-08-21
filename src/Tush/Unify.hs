{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

module Tush.Unify where

import ClassyPrelude hiding (throwM)

import Data.Equivalence.Monad as M
import Data.Equivalence.STT
import Control.Monad.Except

import Control.Lens

import Tush.Syntax

isCompatibleWith :: Type -> Type -> Bool
isCompatibleWith (Type (TyADT x)) (Type (TyADT y)) = x == y
isCompatibleWith (Type (TyADT _)) (Type (TyLambda _)) = False
isCompatibleWith (Type (TyADT _)) (Type (TyBuiltinType _)) = False
isCompatibleWith (Type (TyADT _)) (Type (TyVar _)) = True
isCompatibleWith (Type (TyLambda x)) (Type (TyLambda y)) = x `isLambdaCompatibleWith` y
isCompatibleWith (Type (TyLambda _)) (Type (TyBuiltinType _)) = False
isCompatibleWith (Type (TyLambda _)) (Type (TyVar _)) = True
isCompatibleWith (Type (TyBuiltinType x)) (Type (TyBuiltinType y)) = x == y
isCompatibleWith (Type (TyBuiltinType _)) (Type (TyVar _)) = True
isCompatibleWith (Type (TyVar _)) (Type (TyVar _)) = True
isCompatibleWith (Type TyBadType) _ = False
-- Symmetric relation
isCompatibleWith x y = y `isCompatibleWith` x

isLambdaCompatibleWith :: Lambda Type -> Lambda Type -> Bool
isLambdaCompatibleWith (Lambda xret xarg) (Lambda yret yarg) =
  xarg `isCompatibleWith` yarg && xret `isCompatibleWith` yret

merge :: Type -> Type -> Type
merge x y | x `isCompatibleWith` y = 
              case (x, y) of
                (x'@(Type (TyADT _)), Type (TyADT _)) -> x'
                (x'@(Type (TyADT _)), Type (TyVar _)) -> x'
                (Type (TyLambda x'), Type (TyLambda y')) ->
                  let l = mergeLambda x' y' in
                  Type $ TyLambda $ l
                (x'@(Type (TyLambda _)), Type (TyVar _)) -> x'
                (x'@(Type (TyBuiltinType _)), Type (TyBuiltinType _)) -> x'
                (x'@(Type (TyBuiltinType _)), Type (TyVar _)) -> x'
                (x'@(Type (TyVar _)), Type (TyVar _)) -> x'
                (x', y') -> merge y' x'
          | otherwise = Type TyBadType

compatibleType :: (MonadEquiv (Class s Type Type) Type Type m) => 
                  Type -> Type -> m Type
compatibleType x y | x `isCompatibleWith` y = 
                       case (x, y) of
                         (x'@(Type (TyADT _)), Type (TyADT _)) -> return x'
                         (x'@(Type (TyADT _)), Type (TyVar _)) -> return x'
                         (Type (TyLambda x'), Type (TyLambda y')) -> do
                           l <- compatibleLambda x' y'
                           return $ Type $ TyLambda $ l
                         (x'@(Type (TyLambda _)), Type (TyVar _)) -> return x'
                         (x'@(Type (TyBuiltinType _)), Type (TyBuiltinType _)) -> return x'
                         (x'@(Type (TyBuiltinType _)), Type (TyVar _)) -> return x'
                         (x'@(Type (TyVar _)), Type (TyVar _)) -> return x'
                         (x', y') -> compatibleType y' x'
                   | otherwise = return $ Type TyBadType

compatibleLambda :: (MonadEquiv (Class s Type Type) Type Type m) =>
                    Lambda Type -> Lambda Type -> m (Lambda Type)
compatibleLambda (Lambda xret xarg) (Lambda yret yarg) = do
  lrt <- compatibleType xret yret
  lat <- compatibleType xarg yarg
  lrt' <- M.classDesc lrt
  lat' <- M.classDesc lat
  return Lambda { _lamReturnType = lrt'
                , _lamArgType    = lat'
                }

mergeLambda :: Lambda Type -> Lambda Type -> Lambda Type
mergeLambda (Lambda xret xarg) (Lambda yret yarg) = let
  lrt = merge xret yret
  lat = merge xarg yarg in
  Lambda { _lamReturnType = lrt
         , _lamArgType    = lat
         }

unify' :: ( MonadEquiv (Class s Type Type) Type Type m 
          , MonadError SomeException m ) =>
          TypeConstraint -> m ()
unify' (UnifyWith (Type TyBadType) _) = error $ show $ BadTypeError
unify' (UnifyWith _ (Type TyBadType)) = error $ show $ BadTypeError
unify' (UnifyWith x@(Type (TyLambda (Lambda xret xarg))) y@(Type (TyLambda (Lambda yret yarg)))) = do
  unify (UnifyWith xret yret)
  unify (UnifyWith xarg yarg)
  x `M.equate` y
unify' (UnifyWith x y) = x `M.equate` y

unify :: ( MonadEquiv (Class s Type Type) Type Type m, MonadError SomeException m ) =>
         TypeConstraint -> m ()
unify (UnifyWith x y) = do
  -- if these guys are incompatible, then we are fucked either way.
  unless (x `isCompatibleWith` y) $
    throwError $ toException $ TypeMismatchProblem x
  -- next try to unify their descriptors
  xdesc <- M.classDesc x
  ydesc <- M.classDesc y
  unless (xdesc `isCompatibleWith` ydesc) $
    -- The equivalence classes can't unify because of other
    -- constraints
    throwError $ toException $ TypeMismatchProblem xdesc
  -- These guys can be unified, so do it!
  unify' (UnifyWith x y)
  unify' (UnifyWith xdesc ydesc)

getRestrictedTypes :: MonadEquiv s Type Type m =>
                      Vector Type -> m (Vector Type)
getRestrictedTypes ts = mapM (transformM M.classDesc) ts
