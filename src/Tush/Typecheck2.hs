{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Tush.Typecheck2 where

import Control.Monad.State

import ClassyPrelude

import Tush.Syntax

import qualified Data.Map as M

data TypeCheckerState t = TypeCheckerState { contraints :: M.Map TVar (Vector Constraint)
                                           , typeDict :: M.Map NamedType t
                                           , uniification :: 
                                           } deriving (Eq, Ord, Show)

-- | The way we manifest a type is if it is typed we pass it through,
-- but if it isn't typed we tag it with a free flexible type variable.
-- We also build a dictionary of user types.  We have a dictionary
-- from flexible type variables to constraints on those type
-- variables.
manifest :: MonadState (TypeCheckerState ManifestType) m => Statement fp PreType -> m (Statement fp ManifestType)
manifest = error "manifest: Unimplemented"

-- | We then take a second pass and associate to all Names their
-- actual type implementation, or "reify" them.  Errors can be thrown
-- if a type is not in scope at this point.  
reify :: Monad m => ManifestType -> m AbstractType
reify = error "reify: Unimplemented"

-- | Next we take all the flexible type variables and see if we can
-- consistently unify them.  This involves identifying each type to
-- unify and doing so one at a time.
unify :: Monad m => AbstractType -> m AbstractType
unify = error "unify: Unimplemented"

-- | Finally, we specialize every expression on their actual type.
-- And we are done!  (Is this not part of unification?)
specialize :: Monad m => AbstractType -> m ConcreteType
specialize = error "specialize: Unimplemented"
