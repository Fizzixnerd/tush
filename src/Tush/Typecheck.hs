{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}

module Tush.Typecheck where

import ClassyPrelude

import Data.Typeable

import qualified Control.Monad.State as S
import qualified Control.Monad.Catch.Pure as C

import Tush.Syntax

import qualified Data.Map as M

-- import Control.Monad.State

type LocalTypes = M.Map (Var ()) Type
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

data TypeMismatch = TypeMismatch { tmExpected :: Type
                                 , tmActual :: Type
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

-- | Reifying named types

-- | Typechecking specific constructs

untype :: Functor f => f (Maybe Type) -> f ()
untype = fmap (const ())

typecheckCall :: Expression (Maybe Type) -> SimpleTypecheck Type
typecheckCall c@(CallE (VarE name _) args _) = do
  lt <- localTypes
  case M.lookup (untype name) lt of
    Nothing ->  -- couldn't find the function in the locals
      throwM $ VariableNotFound (varName name) (toText c)
    Just (TTypeLiteral (TLBuiltinType (BTLambda returnType expectedTypes))) -> do
      actualTypes <- mapM typecheck args
      case find (uncurry (/=)) (zip expectedTypes actualTypes) of
        Nothing -> -- Everything is great, we couldn't find a type inequality!
          return returnType
        Just (et, at) -> throwM $ TypeMismatch et at (toText c)
    Just t -> -- It's not even a lambda...
      throwM $ TypeMismatch (builtinType $ BTLambda (builtinType BTInt) mempty) t ("YA DONE FUCKED UP AND CALLED A NONFUNCTION FAM. " ++ toText c)
      -- FIXME This is just saying Int for the return type, but really it
      -- means it was expecting a function.
typecheckCall e = throwM $ InternalTypeCheckerError ("typecheckCall was called on non-CallE: `" ++
                                                     toText e ++
                                                     "'.  This is a programming error by the compiler writer.") (toText e)

typecheckIf :: Expression (Maybe Type) -> SimpleTypecheck Type
typecheckIf i@(IfE cond conse anted _) = do
  condType <- typecheck cond
  conseType <- typecheck conse
  antedType <- typecheck anted
  if | condType  /= (builtinType BTBool) -> throwM $ TypeMismatch (builtinType BTBool) condType (toText cond)
     | conseType /= antedType -> throwM $ TypeMismatch conseType antedType (toText i)
     | otherwise -> return conseType
typecheckIf e = throwM $ InternalTypeCheckerError ("typecheckIf was called on non-IfE: `" ++
                                                   toText e ++
                                                  "'.  This is a programming error by the compiler writer.") (toText e)

typecheck :: Expression (Maybe Type) -> SimpleTypecheck Type
typecheck (LitE (BLit _) _) = return $ builtinType BTBool
typecheck (LitE (FLit _) _) = return $ builtinType BTFloat
typecheck (LitE (ILit _) _) = return $ builtinType BTInt
typecheck ve@(VarE v@(Var name _ _) _) = do
  lt <- localTypes
  case M.lookup (untype v) lt of
    Nothing -> throwM $ VariableNotFound name (toText ve)
    Just t -> return t
typecheck c@(CallE _ _ _) = typecheckCall c
typecheck i@(IfE _ _ _ _) = typecheckIf i

-- | Convert Expression (Maybe Type) Expression BuiltinType

simpleTagE :: Expression (Maybe Type) -> SimpleTypecheck (Expression Type)
simpleTagE (LitE (BLit x) _) = return $ LitE (BLit x) (builtinType BTBool)
simpleTagE (LitE (FLit x) _) = return $ LitE (FLit x) (builtinType BTFloat)
simpleTagE (LitE (ILit x) _) = return $ LitE (ILit x) (builtinType BTInt)
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

simpleTagS :: Statement (Maybe Type) Type -> SimpleTypecheck (Statement Type Type)
simpleTagS (ExprS e) = do
  e' <- simpleTagE e
  return $ ExprS e'
simpleTagS (ExternS fp) = do
  let (nm, ty) = ((fProtoName fp) { varType = Nothing }, varType $ fProtoName fp)
  S.modify (M.insert (untype nm) ty)
  return $ ExternS fp
simpleTagS (FuncS fp stmnts e) = do
  S.modify (\s -> M.union s $ constructLocalTypesFromFProto fp)
  stmnts' <- mapM simpleTagS stmnts
  S.modify (\s -> M.unions $ s : (toList $ constructLocalTypes <$> stmnts))
  e' <- simpleTagE e
  return $ FuncS fp stmnts' e'

defaultEnv :: LocalTypes
defaultEnv = M.fromList [ (Var "+"  () VClassOperator, builtinType $ BTLambda (builtinType BTInt) (fromList [builtinType BTInt, builtinType BTInt]))
                        , (Var "-"  () VClassOperator, builtinType $ BTLambda (builtinType BTInt) (fromList [builtinType BTInt, builtinType BTInt]))
                        , (Var "*"  () VClassOperator, builtinType $ BTLambda (builtinType BTInt) (fromList [builtinType BTInt, builtinType BTInt]))
                        , (Var "/"  () VClassOperator, builtinType $ BTLambda (builtinType BTInt) (fromList [builtinType BTInt, builtinType BTInt]))
                        , (Var "<"  () VClassOperator, builtinType $ BTLambda (builtinType BTBool) (fromList [builtinType BTInt, builtinType BTInt]))
                        , (Var ".+" () VClassOperator, builtinType $ BTLambda (builtinType BTFloat) (fromList [builtinType BTFloat, builtinType BTFloat]))
                        , (Var ".-" () VClassOperator, builtinType $ BTLambda (builtinType BTFloat) (fromList [builtinType BTFloat, builtinType BTFloat]))
                        , (Var ".*" () VClassOperator, builtinType $ BTLambda (builtinType BTFloat) (fromList [builtinType BTFloat, builtinType BTFloat]))
                        , (Var "./" () VClassOperator, builtinType $ BTLambda (builtinType BTFloat) (fromList [builtinType BTFloat, builtinType BTFloat]))
                        , (Var ".<" () VClassOperator, builtinType $ BTLambda (builtinType BTBool) (fromList [builtinType BTFloat, builtinType BTFloat])) ]

-- | LocalTypes Construction

typedVarToLocalTypePair :: Var Type -> (Var (), Type)
typedVarToLocalTypePair stv = (Var (varName stv) () (varClass stv), varType stv)

constructLocalTypesFromFProto :: FProto Type -> LocalTypes
constructLocalTypesFromFProto (FProto name args) = M.fromList $ (typedVarToLocalTypePair name) : 
                                                                toList (typedVarToLocalTypePair <$> args)

constructLocalTypesForFuncS :: Statement (Maybe Type) Type -> LocalTypes
constructLocalTypesForFuncS (FuncS fp xs _) = concat $ cons funcLocals $ constructLocalTypes <$> xs
  where
    funcLocals = constructLocalTypesFromFProto fp
constructLocalTypesForFuncS x = error $ "ERROR: COMPILERERROR: Called `constructLocalTypesForFuncS' on non-FuncS: `" ++
                                show x ++
                                "'."

constructLocalTypes :: Statement (Maybe Type) Type -> LocalTypes
constructLocalTypes (ExprS _) = mempty
constructLocalTypes (ExternS (FProto name _)) = M.singleton (fst lp) $ snd lp
  where
    lp = typedVarToLocalTypePair name
constructLocalTypes (FuncS (FProto name _) _ _) = M.singleton (fst lp) $ snd lp
  where
    lp = typedVarToLocalTypePair name
