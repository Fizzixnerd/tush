{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}

module Tush.Typecheck.Typecheck where

import ClassyPrelude

import Data.Typeable

import qualified Control.Monad.State as S
import qualified Control.Monad.Catch.Pure as C

import Tush.Parse.Syntax

import qualified Data.Map as M

-- import Control.Monad.State

type LocalTypes = M.Map (Var ()) BuiltinType
type Typecheck types m a = S.StateT types m a
type SimpleTypecheck a = Typecheck LocalTypes (C.CatchT Identity) a
type LocationData = Text

data TypeError = forall e . Exception e => TypeError e
  deriving Typeable

instance Show TypeError where
  show (TypeError e) = displayException e

instance Exception TypeError

runSimpleTypecheck :: SimpleTypecheck a -> Either SomeException a
runSimpleTypecheck = runIdentity . C.runCatchT . (flip S.evalStateT defaultEnv)

typeErrorToException :: Exception e => e -> SomeException
typeErrorToException = toException . TypeError

typeErrorFromException :: Exception e => SomeException -> Maybe e
typeErrorFromException x = do
  TypeError e <- fromException x
  cast e

data VariableNotFound = VariableNotFound { vnfName :: Text
                                         , vnfLocation :: LocationData }
  deriving (Typeable, Show)

instance Exception VariableNotFound where
  toException = typeErrorToException
  fromException = typeErrorFromException
  displayException (VariableNotFound {..}) = "Could not find variable `" ++ 
                                             show vnfName ++ 
                                             "'.  In location: " ++
                                             show vnfLocation

data TypeMismatch = TypeMismatch { tmExpected :: BuiltinType
                                 , tmActual :: BuiltinType
                                 , tmLocation :: LocationData }
  deriving (Typeable, Show)

instance Exception TypeMismatch where
  toException = typeErrorToException
  fromException = typeErrorFromException
  displayException (TypeMismatch {..}) = "Could not match expected type `" ++
                                         show tmExpected ++
                                         "' with actual type `" ++
                                         show tmActual ++
                                         "'.  In location: " ++
                                         show tmLocation

data InternalTypeCheckerError = InternalTypeCheckerError { itceMessage :: Text
                                                         , itceLocation :: LocationData }
  deriving (Typeable, Show)

instance Exception InternalTypeCheckerError where
  toException = typeErrorToException
  fromException = typeErrorFromException
  displayException (InternalTypeCheckerError {..}) = show itceMessage ++ 
                                                     "  In location: " ++
                                                     show itceLocation

localTypes :: SimpleTypecheck LocalTypes
localTypes = S.get

toText :: Show a => a -> Text
toText = fromString . show

-- | Typechecking specific constructs

typecheckCall :: Expression () -> SimpleTypecheck BuiltinType
typecheckCall c@(CallE (VarE name _) args _) = do
  lt <- localTypes
  case M.lookup name lt of
    Nothing ->  -- couldn't find the function in the locals
      throwM $ VariableNotFound (varName name) (toText c)
    Just (BTLambda returnType expectedTypes) -> do
      actualTypes <- mapM typecheck args
      case find (uncurry (/=)) (zip expectedTypes actualTypes) of
        Nothing -> -- Everything is great, we couldn't find a type inequality!
          return returnType
        Just (et, at) -> throwM $ TypeMismatch et at (toText c)
    Just t -> -- It's not even a lambda...
      throwM $ TypeMismatch (BTLambda BTInt mempty) t ("YA DONE FUCKED UP AND CALLED A NONFUNCTION FAM. " ++ toText c)
      -- FIXME This is just saying Int for the return type, but really it
      -- means it was expecting a function.
typecheckCall e = throwM $ InternalTypeCheckerError ("typecheckCall was called on non-CallE: `" ++
                                                     toText e ++
                                                     "'.  This is a programming error by the compiler writer.") (toText e)

typecheckIf :: Expression () -> SimpleTypecheck BuiltinType
typecheckIf i@(IfE cond conse anted _) = do
  condType <- typecheck cond
  conseType <- typecheck conse
  antedType <- typecheck anted
  if | condType  /= BTBool -> throwM $ TypeMismatch BTBool condType (toText cond)
     | conseType /= antedType -> throwM $ TypeMismatch conseType antedType (toText i)
     | otherwise -> return conseType
typecheckIf e = throwM $ InternalTypeCheckerError ("typecheckIf was called on non-IfE: `" ++
                                                   toText e ++
                                                  "'.  This is a programming error by the compiler writer.") (toText e)

typecheckFor :: Expression () -> SimpleTypecheck BuiltinType
typecheckFor f@(ForE {..}) = do
  initT <- typecheck forEInitializer
  S.withStateT (\s -> M.insert forEVar initT s) $ do
    termT <- typecheck forETerminator
    incrT <- typecheck forEIncrementer
    exprT <- typecheck forEExpression
    if | termT /= BTBool -> throwM $ TypeMismatch BTBool termT (toText forETerminator)
       | initT /= incrT  -> throwM $ TypeMismatch initT incrT (toText f)
       | otherwise -> return exprT
typecheckFor e = throwM $ InternalTypeCheckerError ("typecheckFor was called on non-ForE: `" ++
                                                   toText e ++
                                                   "'.  This is a programming error by the compiler writer.") (toText e)

typecheck :: Expression () -> SimpleTypecheck BuiltinType
typecheck (LitE (BLit _) _) = return BTBool
typecheck (LitE (FLit _) _) = return BTFloat
typecheck (LitE (ILit _) _) = return BTInt
typecheck ve@(VarE v@(Var name _ _) _) = do
  lt <- localTypes
  case M.lookup v lt of
    Nothing -> throwM $ VariableNotFound name (toText ve)
    Just t -> return t
typecheck c@(CallE _ _ _) = typecheckCall c
typecheck i@(IfE _ _ _ _) = typecheckIf i
typecheck f@(ForE _ _ _ _ _ _) = typecheckFor f

-- | Convert Expression () to Expression BuiltinType

simpleTagE :: Expression () -> SimpleTypecheck (Expression BuiltinType)
simpleTagE (LitE (BLit x) _) = return $ LitE (BLit x) BTBool
simpleTagE (LitE (FLit x) _) = return $ LitE (FLit x) BTFloat
simpleTagE (LitE (ILit x) _) = return $ LitE (ILit x) BTInt
simpleTagE v@(VarE (Var name _ op) _) = do
  vType <- typecheck v
  return $ VarE (Var name vType op) vType
simpleTagE c@(CallE fName args _) = do
  retType <- typecheck c
  fName' <- simpleTagE fName
  args' <- mapM simpleTagE args
  return $ CallE fName' args' retType
simpleTagE i@(IfE cond conse ante _) = do
  retType <- typecheck i
  cond' <- simpleTagE cond
  conse' <- simpleTagE conse
  ante' <- simpleTagE ante
  return $ IfE cond' conse' ante' retType
simpleTagE f@(ForE {..}) = do
  exprT <- typecheck f
  initT <- typecheck forEInitializer
  let var' = forEVar { varInfo = initT }
  S.withStateT (\s -> M.insert forEVar initT s) $ do
    init' <- simpleTagE forEInitializer
    term' <- simpleTagE forETerminator
    incr' <- simpleTagE forEIncrementer
    expr' <- simpleTagE forEExpression
    return $ ForE var' init' term' incr' expr' exprT

simpleTagS :: Statement () BuiltinType -> SimpleTypecheck (Statement BuiltinType BuiltinType)
simpleTagS (ExprS e) = do
  e' <- simpleTagE e
  return $ ExprS e'
simpleTagS (ExternS fp) = do
  let (nm, ty) = ((fProtoName fp) { varInfo = () }, varInfo $ fProtoName fp)
  S.modify (M.insert nm ty)
  return $ ExternS fp
simpleTagS (FuncS fp stmnts e) = do
  S.modify (\s -> M.union s $ constructLocalTypesFromFProto fp)
  stmnts' <- mapM simpleTagS stmnts
  S.modify (\s -> M.unions $ s : (toList $ constructLocalTypes <$> stmnts))
  e' <- simpleTagE e
  return $ FuncS fp stmnts' e'

defaultEnv :: LocalTypes
defaultEnv = M.fromList [ (Var "+" () True, BTLambda BTInt (fromList [BTInt, BTInt]))
                        , (Var "-" () True, BTLambda BTInt (fromList [BTInt, BTInt]))
                        , (Var "*" () True, BTLambda BTInt (fromList [BTInt, BTInt]))
                        , (Var "/" () True, BTLambda BTInt (fromList [BTInt, BTInt]))
                        , (Var "<" () True, BTLambda BTBool (fromList [BTInt, BTInt]))
                        , (Var ".+" () True, BTLambda BTFloat (fromList [BTFloat, BTFloat]))
                        , (Var ".-" () True, BTLambda BTFloat (fromList [BTFloat, BTFloat]))
                        , (Var ".*" () True, BTLambda BTFloat (fromList [BTFloat, BTFloat]))
                        , (Var "./" () True, BTLambda BTFloat (fromList [BTFloat, BTFloat]))
                        , (Var ".<" () True, BTLambda BTBool (fromList [BTFloat, BTFloat])) ]

-- | LocalTypes Construction

simplyTypedVarToLocalTypePair :: SimplyTypedVar -> (Var (), BuiltinType)
simplyTypedVarToLocalTypePair stv = (Var (varName stv) () (varIsOperator stv), varInfo stv)

constructLocalTypesFromFProto :: FProto BuiltinType -> LocalTypes
constructLocalTypesFromFProto (FProto name args) = M.fromList $ (simplyTypedVarToLocalTypePair name) : 
                                                                toList (simplyTypedVarToLocalTypePair <$> args)

constructLocalTypesForFuncS :: Statement () BuiltinType -> LocalTypes
constructLocalTypesForFuncS (FuncS fp xs _) = concat $ cons funcLocals $ constructLocalTypes <$> xs
  where
    funcLocals = constructLocalTypesFromFProto fp
constructLocalTypesForFuncS x = error $ "ERROR: COMPILERERROR: Called `constructLocalTypesForFuncS' on non-FuncS: `" ++
                                show x ++
                                "'."

constructLocalTypes :: Statement () BuiltinType -> LocalTypes
constructLocalTypes (ExprS _) = mempty
constructLocalTypes (ExternS (FProto name _)) = M.singleton (fst lp) $ snd lp
  where
    lp = simplyTypedVarToLocalTypePair name
constructLocalTypes (FuncS (FProto name _) _ _) = M.singleton (fst lp) $ snd lp
  where
    lp = simplyTypedVarToLocalTypePair name

