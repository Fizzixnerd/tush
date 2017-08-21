{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tush.Typecheck2 where

import ClassyPrelude

import Text.Printf

import Control.Lens
import Control.Monad.State (MonadState, StateT, runStateT, gets, put, modify)
import Control.Monad.Except
import Data.Equivalence.Monad
import Data.Equivalence.STT
import qualified Data.Map as M
import Data.Maybe
import qualified Data.List as L

import Tush.Syntax
import Tush.Unify
import Tush.Syntax.Parse
import Tush.Illuminate

newTypeVar :: TypeVarCounter -> ( Type' a, TypeVarCounter)
newTypeVar (TypeVarCounter x) = ( TyVar $ Var (fromString $ printf "__a%v" x) VClassNormal
                                , TypeVarCounter (x+1) )


data TypeCheckerState = TypeCheckerState { _constraints :: Vector TypeConstraint
                                         , _typeVarCounter :: TypeVarCounter
                                         , _locals :: Map Var Type
                                         } deriving (Eq, Ord, Show)

makeLenses ''TypeCheckerState

type TypeChecker a = forall s . 
                     (ExceptT SomeException
                       (StateT TypeCheckerState 
                         (EquivT s Type Type Identity))) a

prelude = M.fromList [ (Var "Int" VClassType, Type $ TyBuiltinType BTInt)
                     , (Var "Float" VClassType, Type $ TyBuiltinType BTFloat)
                     , (Var "Bool" VClassType, Type $ TyBuiltinType BTBool)
                     , (Var "+" VClassOperator, Type $ TyLambda $ Lambda (Type $ TyLambda $ Lambda (Type $ TyBuiltinType BTInt) (Type $ TyBuiltinType BTInt)) (Type $ TyBuiltinType BTInt))]

runTypeChecker :: TypeChecker a -> Either SomeException a
runTypeChecker x = runIdentity $
                   (runEquivT id merge) $ (fmap fst) <$>
                   (\y -> runStateT y (TypeCheckerState
                                        empty 
                                        (TypeVarCounter 0) 
                                        prelude)) $
                   runExceptT $ x

manifestabunch = case parseabunch of
                   Right x -> (\y -> runTypeChecker $ forM y manifest) <$> x
                   x -> error (show x)

reifyabunch = do
  x <- manifestabunch
  case x of
    Right y -> return $ runTypeChecker $ mapM reify y
    z -> error $ show z

unifyabunch = case parseabunch of
  Right x -> (\y -> runTypeChecker $ forM y (manifest >=> reify >=> (\z -> do
                                                                        buildLocals z
                                                                        constrain z
                                                                        return z) >=> Tush.Typecheck2.unify)) <$>
              ((\y -> runIllumination $ forM y illuminateS) <$> x)
  x -> error (show x)

-- | The way we manifest a type is if it is typed we pass it through,
-- but if it isn't typed we tag it with a free flexible type variable.
-- We also build a dictionary of user types.
manifest :: (MonadState TypeCheckerState m) =>
            Statement PreType PreType -> m (Statement ManifestType ManifestType)
manifest (ExprS e) = ExprS <$> manifestE e
manifest (FuncS fp stmnts e) = do
  fp' <- manifestFP fp
  stmnts' <- mapM manifest stmnts
  e' <- manifestE e
  return $ FuncS fp' stmnts' e'
manifest (ExternS fp) = do
  fp' <- manifestFP fp
  return $ (ExternS fp')

manifestE :: (MonadState TypeCheckerState m) => 
             Expression PreType -> m (Expression ManifestType)
manifestE (LitE l t) = do
  t' <- manifestT t
  return $ LitE l t'
manifestE (VarE v t) = do
  t' <- manifestT t
  return $ VarE v t'
manifestE (AppE n a t) = do
  n' <- manifestE n
  a' <- manifestE a
  t' <- manifestT t
  return $ AppE n' a' t'
manifestE (LamE v b t) = do
  v' <- manifestE v
  b' <- manifestE b
  t' <- manifestTLambda t
  return $ LamE v' b' t'
manifestE (IfE i th el t) = do
  i' <- manifestE i
  th' <- manifestE th
  el' <- manifestE el
  t' <- manifestT t
  return $ IfE i' th' el' t'

manifestT :: (MonadState TypeCheckerState m) => 
             PreType -> m ManifestType
manifestT (PTyMType x) = return x
manifestT PTyUntyped = do
  tvc <- gets $ view typeVarCounter
  let (tv, tvc') = newTypeVar tvc
  modify (\s -> s { _typeVarCounter = tvc' })
  return $ MTyType tv
manifestT (PTyType (TyADT (ADT c ts n))) = do
  ts' <- mapM manifestT ts
  return $ MTyType (TyADT (ADT c ts' n))
manifestT (PTyType (TyLambda (Lambda r a))) = do
  r' <- manifestT r
  a' <- manifestT a
  return $ MTyType (TyLambda (Lambda r' a'))
manifestT (PTyType (TyBuiltinType b)) = return $ MTyType (TyBuiltinType b)
manifestT (PTyType (TyVar v)) = return $ MTyType (TyVar v)
manifestT (PTyType TyBadType) = return $ MTyType TyBadType

manifestTLambda :: MonadState TypeCheckerState m =>
                   PreType -> m ManifestType
manifestTLambda PTyUntyped = do
  tvc <- gets $ view typeVarCounter
  let (argT, tvc') = newTypeVar tvc
      (retT, tvc'') = newTypeVar tvc'
  typeVarCounter .= tvc''
  return $ MTyType $ TyLambda $ Lambda (MTyType retT) (MTyType argT)
manifestTLambda x = manifestT x

manifestFP :: (MonadState TypeCheckerState m) => 
              FProto PreType -> m (FProto ManifestType)
manifestFP (FProto n a) = do
  n' <- manifestE n
  a' <- manifestE a
  return $ FProto n' a'

reify' :: ( MonadState TypeCheckerState m
          , MonadError SomeException m) =>
          Statement ManifestType ManifestType -> m (Statement Type ManifestType)
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
reify :: ( MonadState TypeCheckerState m
         , MonadError SomeException m) =>
         Statement ManifestType ManifestType -> m (Statement Type Type)
reify s = join $ fmap (mapM reifyT) $ (reify' s)

reifyT' :: ( MonadState TypeCheckerState m
           , MonadError SomeException m) =>
           Type' ManifestType -> m (Type' Type)
reifyT' (TyADT x) = TyADT <$> (mapM reifyT x)
reifyT' (TyLambda x) = TyLambda <$> (mapM reifyT x)
reifyT' (TyBuiltinType x) = return $ TyBuiltinType x
reifyT' (TyVar x) = return $ TyVar x
reifyT' TyBadType = return $ TyBadType

reifyT :: (MonadState TypeCheckerState m, MonadError SomeException m) => 
          ManifestType -> m Type
reifyT (MTyType t) = Type <$> reifyT' t
reifyT (MTyName n) = do
  ls <- gets $ view locals
  case lookup n ls of
    Nothing -> throwError $ toException $ TypeNotDefined n
    Just x  -> return x

buildLocals :: (MonadState TypeCheckerState m) => 
               Statement Type Type -> m ()
-- FIXME: need to build locals from AppE's and LamE's.
buildLocals (ExprS _) = return ()
buildLocals (FuncS fp _ _) = buildLocalsFromFProto fp
buildLocals (ExternS fp) = buildLocalsFromFProto fp

buildLocalsFromFProto :: (MonadState TypeCheckerState m) => 
                         FProto Type -> m ()
buildLocalsFromFProto (FProto (VarE name t) _) = do
  ls <- gets $ view locals
  modify (\s -> s { _locals = M.insert name t ls })
  return ()

constrain :: (MonadState TypeCheckerState m, MonadError SomeException m) =>
             Statement Type Type -> m ()
constrain (ExprS e) = constrainE e
-- FIXME: Probably wrong to do nothing
constrain _ = return ()

constrainE :: (MonadState TypeCheckerState m, MonadError SomeException m) =>
             Expression Type -> m ()
constrainE (LitE value t) = do
  let literalConstraint = case value of
                            ILit _ -> (Type $ TyBuiltinType BTInt) `unifyWith` t
                            FLit _ -> (Type $ TyBuiltinType BTFloat) `unifyWith` t
                            BLit _ -> (Type $ TyBuiltinType BTBool) `unifyWith` t
  constraints <>= (singleton literalConstraint)
constrainE (VarE name t) = do
  ls <- gets $ view locals
  let localsConstraint = (ls^.at name.to (maybe t id)) `unifyWith` t
  constraints <>= (singleton localsConstraint)
constrainE (AppE fn arg t) = do
  ls <- gets $ view locals
  localsConstraints <- case fn of
                         VarE name t' -> 
                           return $ fromList [
                           (ls^.at name.to (maybe (mkLam t (arg^.exprType)) id))
                           `unifyWith`
                           (mkLam t (arg^.exprType)),
                           t' `unifyWith` (ls^.at name.to (maybe (mkLam t (arg^.exprType)) id)),
                           t' `unifyWith` (mkLam t (arg^.exprType))
                           ]
                         LamE arg' body t' ->
                           return $ fromList [
                           t `unifyWith` (body^.exprType)
                           ]
                         AppE fn' arg' t' ->
                           return $ fromList [
                           (fn'^.exprType) `unifyWith` (mkLam (mkLam t (arg^.exprType)) (arg'^.exprType))
                           ]
                         _ -> return empty
  let functionConstraint = singleton $ 
                           (fn^.exprType)
                           `unifyWith`
                           (mkLam t (arg^.exprType))
      constraints' = localsConstraints <> functionConstraint
  constraints <>= constraints'
  constrainE fn
  constrainE arg
  where
    mkLam x y = Type $ TyLambda $ Lambda x y
constrainE (IfE i th el t) = do
  let ic = (i^.exprType) `unifyWith` (Type (TyBuiltinType BTBool))
      thc = (th^.exprType) `unifyWith` (el^.exprType)
      tc = t `unifyWith` (th^.exprType)
      tc' = t `unifyWith` (el^.exprType)
  constraints <>= fromList [ic, thc, tc, tc']
  constrainE i
  constrainE th
  constrainE el
-- FIXME: Probably want to involve the type t somehow...
-- FIXED
constrainE (LamE arg body t) = do
  let Type (TyLambda (Lambda ret arg')) = t
      returnConstraint = singleton $ ret `unifyWith` (body^.exprType)
      argConstraint = singleton $ arg' `unifyWith` (arg^.exprType)
      recursiveConstraint = let argInstances = 
                                  [x | x@(VarE v _) <- universe body, v == (arg^.to _varEVar)]
                            in
                              fromList $ (\x -> (x^.exprType) `unifyWith` (arg^.exprType)) <$> argInstances
  constraints <>= returnConstraint <> argConstraint <> recursiveConstraint
  constrainE arg
  constrainE body

unifyWith :: Type -> Type -> TypeConstraint
unifyWith = UnifyWith

-- | Next we take all the flexible type variables and see if we can
-- consistently unify them.  This involves identifying each type to
-- unify and doing so one at a time.
unify :: ( MonadState TypeCheckerState m
         , MonadEquiv (Class s Type Type) Type Type m 
         , MonadError SomeException m) =>
         Statement Type Type -> m (Statement Type Type)
unify s = do
  let allTypes = getAllTypes s
  cs <- gets $ view constraints
  ClassyPrelude.mapM_ Tush.Unify.unify cs
  restTypes <- getRestrictedTypes allTypes
  let typeMap = M.fromList $ toList (zip allTypes restTypes)
  return $ replaceTypes typeMap s

-- FIXME: Partial
getAllTypes :: Statement Type Type -> Vector Type
getAllTypes (ExprS e) = fromList $ L.foldl' (\acc x -> x:acc) empty e
getAllTypes _ = empty

replaceTypes :: Map Type Type -> Statement Type Type -> Statement Type Type
replaceTypes tm (ExprS e) = ExprS $ runIdentity $ traverse (pure . replaceType tm) e
replaceTypes _ s = s

replaceType :: Map Type Type -> Type -> Type
replaceType tm t = fromJust $ lookup t tm

-- | Finally, we specialize every expression on their actual type.
-- And we are done!  (Is this not part of unification?)
specialize :: Monad m => Type -> m Type
specialize = error "specialize: Unimplemented"
