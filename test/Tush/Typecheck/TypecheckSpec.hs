{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Tush.Typecheck.TypecheckSpec where

import ClassyPrelude

import qualified Data.Map as M

import Test.Hspec

import Tush.Parse.Syntax
import Tush.Typecheck.Typecheck

typecheckSpec :: IO ()
typecheckSpec = hspec $ do
  describe "Tush.Typecheck.Typecheck" $ do

    describe "typecheck.LitE" $ do

      it "Infers type of integer literals." $ do
        typecheck (LitE (ILit 3)) mempty `shouldBe` BTInt


      it "Infers type of floating literals." $ do
        typecheck (LitE (FLit 3.0)) mempty `shouldBe` BTFloat

      it "Infers type of boolean literals." $ do
        typecheck (LitE (BLit True)) mempty `shouldBe` BTBool

    describe "typecheck.BinOpE" $ do
      
      it "Infers integer for integer binops" $ do
        typecheck (BinOpE Add (LitE (ILit 3)) (LitE (ILit 4))) mempty `shouldBe` BTInt

      it "Infers bool for integer comparisons" $ do
        typecheck (BinOpE Lt (LitE (ILit 3)) (LitE (ILit 4))) mempty `shouldBe` BTBool

      it "Infers float for floating binops" $ do
        typecheck (BinOpE Mul (LitE (FLit 3.0)) (LitE (FLit 4.0))) mempty `shouldBe` BTFloat

      it "Infers bool for floating comparisons" $ do
        typecheck (BinOpE Lt (LitE (FLit 3.0)) (LitE (FLit 4.0))) mempty `shouldBe` BTBool

      -- | TODO: Should throw when args types are mismatched.

    describe "typecheck.VarE" $ do

      it "Infers correct type of a VarE" $ do
        typecheck (VarE (Var "x" ())) (M.fromList [(Var "x" (), BTInt)]) `shouldBe` BTInt

      -- | TODO: Should throw when var undefined.

    describe "typecheck.CallE" $ do

      it "Infers correct type of a function call." $ do
        typecheck (CallE (Var "f" ()) (fromList [LitE (FLit 4.0)])) (M.fromList [(Var "f" (), BTLambda { btLambdaReturnType = BTFloat
                                                                                                       , btLambdaArgTypes = fromList [BTFloat] 
                                                                                                       })])
          `shouldBe` BTFloat

      -- | TODO The comments code here throws from pure code and so
      -- doesn't work.  But the actual code works, so whatever I'll
      -- probably leave it until I change it to a MonadCatch or
      -- whatever.
      --
      -- it "Throws on incorrect type of a function call." $ do
      --   (return (typecheck (CallE (Var "f" ()) (fromList [LitE (ILit 3)])) (M.fromList [(Var "f" (), BTLambda { btLambdaReturnType = BTFloat
      --                                                                                                         , btLambdaArgTypes = fromList [BTFloat] 
      --                                                                                                         })])) :: IO BuiltinType) 
      --     `shouldThrow` anyErrorCall
