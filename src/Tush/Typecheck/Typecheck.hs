{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Tush.Typecheck.Typecheck where

import ClassyPrelude

import Tush.Parse.Syntax

import qualified Data.Map as M

-- import Control.Monad.State

type LocalTypes = M.Map (Var ()) BuiltinType

-- | Typechecking specific

typecheckCall :: LocalTypes -> Expression () -> BuiltinType
typecheckCall lt (CallE (VarE name _) args _) = case M.lookup name lt of
  Nothing -> error $ "ERROR: TYPEINFERENCE: Unknown function: `" ++
             show name ++
             "'."
  Just t -> case t of
    BTLambda returnType declaredTypes ->
      let actualTypes = (\x -> typecheck lt x) <$> args in
      if declaredTypes == actualTypes then
         returnType -- all is good!
      else
        error $ "ERROR: TYPEINFERENCE: Cannot unify declared arg types `" ++
        show declaredTypes ++
        "' with actual types `" ++
        show actualTypes ++
        "'."
    _ -> error $ "ERROR: TYPEINFERENCE: Cannot call non-Lambda value: `" ++
         show name ++
         "'.  Expected Lambda type but actual type is: `" ++
         show t ++
         "'."
typecheckCall e _ = error $ "ERROR: COMPILERERROR: Tried to call `typeCheckCall' on non-CallE: `" ++
                    show e ++
                    "'."

typecheckIf :: LocalTypes -> Expression () -> BuiltinType
typecheckIf lt i@(IfE cond conse anted _)
  | typecheck lt cond /= BTBool = error $ "ERROR: TYPEINFERENCE: Cannot unify type `" ++
                                  show (typecheck lt cond) ++
                                  "' with type Boolean.  (In the Conditional of the If expression `" ++
                                  show i ++
                                  "'.)"
  | typecheck lt conse /= typecheck lt anted = error $ "ERROR: TYPEINFERENCE: Cannot unify type `" ++ 
                                               show (typecheck lt conse) ++ 
                                               "' with type `" ++ 
                                               show (typecheck lt anted) ++
                                               "'.  (The types of the branches of the If Expression `" ++ 
                                               show i ++ 
                                               "' do not match.)"
  | otherwise = typecheck lt conse
typecheckIf _ e = error $ "ERROR: COMPILERERROR: Tried to `typeCheckIf' on non-IfE `" ++
                  show e ++
                  "'."

typecheck :: LocalTypes -> Expression () -> BuiltinType
typecheck _ (LitE (BLit _) _) = BTBool
typecheck _ (LitE (FLit _) _) = BTFloat
typecheck _ (LitE (ILit _) _) = BTInt
typecheck lt b@(BinOpE op l r _)
  | typecheck lt l /= typecheck lt r = error $ "ERROR: TYPEINFERENCE: Cannot unify type `" ++ 
                                       show (typecheck lt l) ++ 
                                       "' with type `" ++ 
                                       show (typecheck lt r) ++
                                       "'.  In Expression: `" ++ 
                                       show b ++ 
                                       "'."
  -- TODO: Fix the binops that are bool only.
  | op == Lt = BTBool
  | otherwise = typecheck lt l
typecheck lt (UnOpE _ x _) = typecheck lt x
typecheck lt (VarE v _) = case M.lookup v lt of
  Nothing -> error $ "ERROR: TYPEINFERENCE: Unknown variable: `" ++
             show v ++
             "'."
  Just t -> t
typecheck lt c@(CallE _ _ _) = typecheckCall lt c
typecheck lt i@(IfE _ _ _ _) = typecheckIf lt i

-- | Convert Expression () to Expression BuiltinType

simpleTagE :: LocalTypes -> Expression () -> Expression BuiltinType
simpleTagE _ (LitE (BLit x) _) = LitE (BLit x) BTBool
simpleTagE _ (LitE (FLit x) _) = LitE (FLit x) BTFloat
simpleTagE _ (LitE (ILit x) _) = LitE (ILit x) BTInt
simpleTagE lt b@(BinOpE op x y _) = BinOpE op (simpleTagE lt x) (simpleTagE lt y) (typecheck lt b)
simpleTagE lt u@(UnOpE op x _) = UnOpE op (simpleTagE lt x) (typecheck lt u)
simpleTagE lt v@(VarE (Var name _) _) = VarE (Var name (typecheck lt v)) (typecheck lt v)
simpleTagE lt c@(CallE name args _) = CallE (simpleTagE lt name) (simpleTagE lt <$> args) (typecheck lt c)
simpleTagE lt i@(IfE cond conse ante _) = IfE (simpleTagE lt cond) (simpleTagE lt conse) (simpleTagE lt ante) (typecheck lt i)

simpleTagS' :: LocalTypes -> Statement () BuiltinType -> Statement BuiltinType BuiltinType
simpleTagS' lt (ExprS e) = ExprS $ simpleTagE lt e
simpleTagS' _  (ExternS fp) = (ExternS fp)
simpleTagS' lt (FuncS fp stmnts) = FuncS fp $ simpleTagS' lt' <$> stmnts
  where
    lt' = M.union lt $ constructLocalTypesFromFProto fp

simpleTagS :: Statement () BuiltinType -> Statement BuiltinType BuiltinType
simpleTagS = simpleTagS' mempty

-- | LocalTypes Construction

simplyTypedVarToLocalTypePair :: SimplyTypedVar -> (Var (), BuiltinType)
simplyTypedVarToLocalTypePair stv = (Var (varName stv) (), varInfo stv)

constructLocalTypesFromFProto :: FProto BuiltinType -> LocalTypes
constructLocalTypesFromFProto (FProto name args) = M.fromList $ (simplyTypedVarToLocalTypePair name) : 
                                                                toList (simplyTypedVarToLocalTypePair <$> args)

constructLocalTypesForFuncS :: Statement () BuiltinType -> LocalTypes
constructLocalTypesForFuncS (FuncS fp xs) = concat $ cons funcLocals $ constructLocalTypes <$> xs
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
constructLocalTypes (FuncS (FProto name _) _) = M.singleton (fst lp) $ snd lp
  where
    lp = simplyTypedVarToLocalTypePair name

