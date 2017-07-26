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
        typecheck mempty (LitE (ILit 3) ()) `shouldBe` BTInt


      it "Infers type of floating literals." $ do
        typecheck mempty (LitE (FLit 3.0) ()) `shouldBe` BTFloat

      it "Infers type of boolean literals." $ do
        typecheck mempty (LitE (BLit True) ()) `shouldBe` BTBool

    describe "typecheck.BinOpE" $ do
      
      it "Infers integer for integer binops" $ do
        typecheck mempty (BinOpE Add (LitE (ILit 3) ()) (LitE (ILit 4) ()) ()) `shouldBe` BTInt

      it "Infers bool for integer comparisons" $ do
        typecheck mempty (BinOpE Lt (LitE (ILit 3) ()) (LitE (ILit 4) ()) ()) `shouldBe` BTBool

      it "Infers float for floating binops" $ do
        typecheck mempty (BinOpE Mul (LitE (FLit 3.0) ()) (LitE (FLit 4.0) ()) ()) `shouldBe` BTFloat

      it "Infers bool for floating comparisons" $ do
        typecheck mempty (BinOpE Lt (LitE (FLit 3.0) ()) (LitE (FLit 4.0) ()) ()) `shouldBe` BTBool

      -- | TODO: Should throw when args types are mismatched.

    describe "typecheck.VarE" $ do

      it "Infers correct type of a VarE" $ do
        typecheck (M.fromList [(Var "x" (), BTInt)]) (VarE (Var "x" ()) ()) `shouldBe` BTInt

      -- | TODO: Should throw when var undefined.

    describe "typecheck.CallE" $ do

      it "Infers correct type of a function call." $ do
        typecheck (M.fromList [(Var "f" (), BTLambda { btLambdaReturnType = BTFloat
                                                     , btLambdaArgTypes = fromList [BTFloat] 
                                                     })]) (CallE (VarE (Var "f" ()) ()) (fromList [LitE (FLit 4.0) ()]) ()) 
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
