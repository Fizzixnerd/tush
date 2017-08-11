{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Tush.Typecheck.Unify where

import ClassyPrelude

import Data.Equivalence.Monad

import Tush.Syntax

-- | Non-equal concrete types cannot be instantiated to the same
-- variable class, nor can they be unified with rigid type variables,
-- nor can rigid type variables be unified with each other.
--
-- Suppose we have (a -> (b -> c) -> d) and we want to unify with (e
-- -> f -> Int).  Then the constraints are d = Int, a = e, f = b -> c.
-- Assuming all type variables are flexible, this is fine.  The final
-- type will be: a -> (b -> c) -> Int, since this is the most general
-- type that fulfills the constraints.  We can then unify this with
-- some other type, say Float -> h.  This would imply Float -> (b ->
-- c) -> Int.  If we then tried to unify with, say, Float -> Float ->
-- Int, we would run into problems.
--
-- We would get Float = Float, (b -> c) = Float, Int = Int for the
-- constraints.  But the second constraint is unsatisfiable.
-- 
-- Let's properly account for functions.
--
-- unify (a -> (b -> c) -> d) with (e -> f -> Int) gives
-- unify a e && unify (b -> c) -> d with f -> Int  gives
-- unify a e && unify (b -> c) f && unify d Int
--
-- unify (a -> b) (c -> d -> e)
-- unify a c && unify b (d -> e)
--
-- As can be seen, we recursively strip the first arguments and unify
-- until we run out, where we then attempt to unify the last argument
-- with the REST of the one who didn't run out.
--
-- This reduces the problem to base cases involving at most ONE
-- function type.  Therefore, abbreviating ConcreteType C and
-- QuantifiedType Q, assuming the first variable is the one which is
-- NOT a function:
--
-- unify (x :: C) (y :: C) = constrain $ x .== y
-- unify (x :: Q) (y :: Q) = if hasRigid x && hasRigid y then fail "" else constrain $ x .== y
-- unify (x :: Q) (y :: C) = if hasRigid x then fail "" else constrain $ x .== y
-- unify (x :: C) (y :: Q) = if hasRigid y then fail "" else constrain $ x .== y
-- 
-- hasRigid returns True when its argument CONTAINS a rigid type
-- variable, otherwise False.
-- 
-- When we write "constrain $ x .== y", it is understood to really
-- mean something like:
--
-- do
--   x' <- getIDOrNew x
--   y' <- getIDOrNew y
--   return $ constrain $ x' .== y'
--
-- Suppose now that everything unifies.  We need to build the actual
-- type.  In the first example above, this would be Float -> (b -> c)
-- -> Int.  This involves replacing flexible type variables with
-- either rigid type variables or concrete types.
--
-- Looking above, the smartest thing to do is this: assume unification
-- is going to succeed.  If it fails, just throwM.  But have unify
-- return not a Bool but the unified type!  Then we can do:
--
-- unify (a -> b -> c) (d -> (e -> f) -> Int)
-- (unify a d) -> (unify b (e -> f)) -> (unify c Int)
-- a -> (e -> f) -> Int
-- YEAH?
--
-- Think hard now: is this right?  This says types can be unified
-- without reference to context.  If I swapped c and Int above, would
-- that still unify?
--
-- NOPE.  MAYBE?
--
-- Let's assume yes.  Because that makes the most sense.  Assuming
-- everything is non-rigid.

name :: AbstractType -> Text
name = fromString . show

isCompatibleWith :: AbstractType -> AbstractType -> Bool
-- concrete types are only compatible if equal.
isCompatibleWith (AbstractType (Right x)) (AbstractType (Right y)) = x == y
isCompatibleWith (AbstractType (Left x)) (AbstractType (Left y)) = x `isQuantifiedCompatibleWith` y
-- we may now only consider times with the left argument is concrete
-- and the right type is quantified:
isCompatibleWith x@(AbstractType (Left _)) y = isCompatibleWith y x
-- lambdas are not compatible with terminators
isCompatibleWith (AbstractType (Right (ConcreteType   (TyADT x)))) 
                 (AbstractType (Left  (QuantifiedType (TyADT y)))) = x `isMixedADTCompatibleWith` y
                                                                       

isQuantifiedCompatibleWith :: QuantifiedType -> QuantifiedType -> Bool
isQuantifiedCompatibleWith (QuantifiedType (TyADT x)) (QuantifiedType (TyADT y)) = x `isQuantifiedADTCompatibleWith` y
isQuantifiedCompatibleWith (QuantifiedType (TyADT x)) (QuantifiedType (TyLambda y)) = False
isQuantifiedCompatibleWith (QuantifiedType (TyADT x)) (QuantifiedType (TyTerm y)) = False
isQuantifiedCompatibleWith (QuantifiedType (TyADT x)) (QuantifiedType (TyVar y)) = True
isQuantifiedCompatibleWith (QuantifiedType (TyLambda x)) (QuantifiedType (TyLambda y)) = x `isLambdaCompatible` y
isQuantifiedCompatibleWith (QuantifiedType (TyLambda x)) (QuantifiedType (TyTerm y)) = False
isQuantifiedCompatibleWith (QuantifiedType (TyLambda x)) (QuantifiedType (TyVar y)) = True
isQuantifiedCompatibleWith (QuantifiedType (TyTerm x)) (QuantifiedType (TyTerm y))

unify :: ( MonadEquiv c AbstractType a m
         , MonadCatch m ) => 
         TypeConstraint -> m ()
unify (UnifyWith x y) = x `equate` y
