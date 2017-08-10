{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Tush.Typecheck2 where

import ClassyPrelude

import Text.Printf
import Data.Typeable
import Control.Monad.State
import qualified Data.Map as M

import Tush.Syntax

newTypeVar :: TypeVarCounter -> (QuantifiedType, TypeVarCounter)
newTypeVar (TypeVarCounter x) = ( QuantifiedType $ TVar $ Var (fromString $ printf "__a%v" x) VClassTypeFlexible
                                , TypeVarCounter (x+1) 
                                )

-- | Exceptions

data CompilerError = forall e . Exception e => CompilerError e deriving Typeable
instance Exception CompilerError
instance Show CompilerError where show (CompilerError e) = displayException e

data TypeMismatchProblems = TypeMismatchProblems { tmpTypes :: Vector AbstractType } 
  deriving (Typeable, Show)
instance Exception TypeMismatchProblems where
  toException = toException . CompilerError
  fromException x = do
    CompilerError e <- fromException x
    cast e
  displayException (TypeMismatchProblems {..}) = printf "Could not match types %v for some reason." (show tmpTypes)

data TypeNotDefined = TypeNotDefined { tndType :: NamedType } deriving (Typeable, Show)
instance Exception TypeNotDefined where
  toException = toException . CompilerError
  fromException x = do
    CompilerError e <- fromException x
    cast e
  displayException (TypeNotDefined {..}) = printf "Type type `%v' is not defined." (show tndType)


data FunctionMisapplied = FunctionMisapplied { fmName :: Var
                                             , fmActualTypes :: Vector AbstractType
                                             , fmExpectedTypes :: Vector AbstractType
                                             } deriving (Typeable, Show)
instance Exception FunctionMisapplied where
  toException = toException . CompilerError
  fromException x = do
    CompilerError e <- fromException x
    cast e
  displayException (FunctionMisapplied {..}) = printf "Function `%v' was applied to `%v', but expected `%v'." (show fmName) (show fmActualTypes) (show fmExpectedTypes)

data VariableNotDefined = VariableNotDefined { vndVar :: Var } deriving (Typeable, Show)
instance Exception VariableNotDefined where
  toException = toException . CompilerError
  fromException x = do
    CompilerError e <- fromException x
    cast e
  displayException (VariableNotDefined {..}) = printf "The variable `%v' is not defined." (show vndVar)

data TypeCheckerState = TypeCheckerState { contraints :: Vector TypeConstraint
                                         , typeDict :: M.Map NamedType AbstractType
                                         , typeVarCounter :: TypeVarCounter
                                         , locals :: M.Map Var AbstractType
                                         } deriving (Eq, Ord, Show)

-- | The way we manifest a type is if it is typed we pass it through,
-- but if it isn't typed we tag it with a free flexible type variable.
-- We also build a dictionary of user types.
manifest :: (MonadState TypeCheckerState m, MonadThrow m) =>
            Statement ManifestType PreType -> m (Statement ManifestType ManifestType)
manifest (ExprS e) = ExprS <$> manifestE e
manifest (FuncS fp stmnts e) = do
  fp' <- manifestFP fp
  stmnts' <- mapM manifest stmnts
  e' <- manifestE e
  return $ FuncS fp' stmnts' e'
manifest (ExternS fp) = return $ (ExternS fp)

manifestE :: (MonadState TypeCheckerState m, MonadThrow m) => 
             Expression PreType -> m (Expression ManifestType)
manifestE (LitE l t) = do
  t' <- manifestT t
  return $ LitE l t'
manifestE (VarE v t) = do
  t' <- manifestT t
  return $ VarE v t'
manifestE (CallE n as t) = do
  n' <- manifestE n
  as' <- mapM manifestE as
  t' <- manifestT t
  return $ CallE n' as' t'
manifestE (IfE i th el t) = do
  i' <- manifestE i
  th' <- manifestE th
  el' <- manifestE el
  t' <- manifestT t
  return $ IfE i' th' el' t'

manifestT :: (MonadState TypeCheckerState m, MonadThrow m) => 
             PreType -> m ManifestType
manifestT (PreType (Just x)) = return x
manifestT (PreType Nothing) = do
  tvc <- gets typeVarCounter
  let (tv, tvc') = newTypeVar tvc
  modify (\s -> s { typeVarCounter = tvc' })
  return $ ManifestType $ Right $ AbstractType $ Left tv

manifestFP :: (MonadState TypeCheckerState m, MonadThrow m) => 
              FProto PreType -> m (FProto ManifestType)
manifestFP (FProto n as) = do
  n' <- manifestE n
  as' <- mapM manifestE as
  return $ FProto n' as'

reify' :: (MonadState TypeCheckerState m, MonadThrow m) =>
          Statement ManifestType ManifestType -> m (Statement AbstractType ManifestType)
reify' (ExternS fp) = do
  fp' <- mapM reifyT fp
  return $ ExternS fp'
reify' (ExprS e) = return $ (ExprS e)
reify' (FuncS fp ss e) = do
  ss' <- mapM reify' ss
  return $ FuncS fp ss' e

-- | We then take a second pass and associate to all NamedTypes their
-- actual type implementation, or "reify" them.  Errors can be thrown
-- if a type is not in scope at this point.
reify :: (MonadState TypeCheckerState m, MonadThrow m) =>
         Statement ManifestType ManifestType -> m (Statement AbstractType AbstractType)
reify s = join $ fmap (mapM reifyT) $ (reify' s)

reifyT :: (MonadState TypeCheckerState m, MonadThrow m) => 
          ManifestType -> m (AbstractType)
reifyT (ManifestType (Right at)) = return at
reifyT (ManifestType (Left t)) = do
  td <- gets typeDict
  case lookup t td of
    Nothing -> throwM (TypeNotDefined t)
    Just x  -> return x

buildLocals :: (MonadState TypeCheckerState m, MonadThrow m) => 
               Statement AbstractType AbstractType -> m ()
buildLocals (ExprS _) = return ()
buildLocals (FuncS fp _ _) = buildLocalsFromFProto fp
buildLocals (ExternS fp) = buildLocalsFromFProto fp

buildLocalsFromFProto :: (MonadState TypeCheckerState m, MonadThrow m) => 
                         FProto AbstractType -> m ()
buildLocalsFromFProto (FProto (VarE name t) _) = do
  ls <- gets locals
  modify (\s -> s { locals = M.insert name t ls })
  return ()

constrain :: (MonadState TypeCheckerState m, MonadThrow m) =>
             Expression AbstractType -> m (Vector TypeConstraint)
constrain (LitE _ _) = return empty
constrain (VarE _ _) = return empty
constrain (CallE name args t) = 
  let types = exprType <$> args in
    case t of
      AbstractType (Left (QuantifiedType (TLambda (Lambda {..})))) ->
        constrainTypes name types lamArgTypes
      AbstractType (Right (ConcreteType (TLambda (Lambda {..})))) ->
        constrainTypes name types (AbstractType <$> Right <$> lamArgTypes)
      _ -> throwM (TypeMismatchProblems types)
constrain (IfE i th el t) =
  let ic =(exprType i) `unifyWith` (AbstractType $ Right $ ConcreteType $ TTerm BTBool)
      thc = (exprType th) `unifyWith` (exprType el)
      tc = t `unifyWith` (exprType th)
      tc' = t `unifyWith` (exprType el) in
  return $ fromList [ic, thc, tc, tc']

constrainTypes :: (MonadState TypeCheckerState m, MonadThrow m) => 
                  Expression AbstractType 
               -> Vector AbstractType 
               -> Vector AbstractType 
               -> m (Vector TypeConstraint)
constrainTypes (VarE name _) argTypes lamTypes 
  | length argTypes /= length lamTypes = throwM (FunctionMisapplied name argTypes lamTypes)
  | otherwise = do
      let baseConstraints = fmap (uncurry unifyWith) (zip argTypes lamTypes)
      ls <- gets locals
      localConstraints <- 
            case lookup name ls of
              Nothing -> throwM (VariableNotDefined name)
              Just (AbstractType (Left (QuantifiedType (TLambda localTypes))))
                | length (lamArgTypes localTypes) /= length argTypes -> 
                    throwM (FunctionMisapplied name argTypes (lamArgTypes localTypes))
                | otherwise -> 
                    return $ fmap (uncurry unifyWith) (zip argTypes (lamArgTypes localTypes))
              Just (AbstractType (Right (ConcreteType (TLambda localTypes))))
                | length (lamArgTypes localTypes) /= length argTypes -> 
                    throwM (FunctionMisapplied name argTypes (fmap (AbstractType . Right) (lamArgTypes localTypes)))
                | otherwise -> 
                    return $ fmap (uncurry unifyWith) (zip argTypes (fmap (AbstractType . Right) (lamArgTypes localTypes)))
      return $ baseConstraints <> localConstraints

unifyWith :: AbstractType -> AbstractType -> TypeConstraint
unifyWith = UnifyWith

-- | Next we take all the flexible type variables and see if we can
-- consistently unify them.  This involves identifying each type to
-- unify and doing so one at a time.
unify :: Monad m => AbstractType -> m AbstractType
unify = error "unify: Unimplemented"

-- | Finally, we specialize every expression on their actual type.
-- And we are done!  (Is this not part of unification?)
specialize :: Monad m => AbstractType -> m ConcreteType
specialize = error "specialize: Unimplemented"
