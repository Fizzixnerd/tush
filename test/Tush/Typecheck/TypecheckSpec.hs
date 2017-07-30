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
        runSimpleTypecheck (typecheck (LitE (ILit 3) ())) `shouldBe` BTInt


      it "Infers type of floating literals." $ do
        runSimpleTypecheck (typecheck (LitE (FLit 3.0) ())) `shouldBe` BTFloat

      it "Infers type of boolean literals." $ do
        typecheck mempty (LitE (BLit True) ()) `shouldBe` BTBool

    describe "typecheck.VarE" $ do

      it "Infers correct type of a VarE" $ do
        typecheck (M.fromList [(Var "x" () False, BTInt)]) (VarE (Var "x" () False) ()) `shouldBe` BTInt

      -- | TODO: Should throw when var undefined.

    describe "typecheck.CallE" $ do

      it "Infers correct type of a function call." $ do
        typecheck (M.fromList [(Var "f" () False, BTLambda { btLambdaReturnType = BTFloat
                                                           , btLambdaArgTypes = fromList [BTFloat] 
                                                           })]) (CallE (VarE (Var "f" () False) ()) (fromList [LitE (FLit 4.0) ()]) ()) 
          `shouldBe` BTFloat

    describe "simpleTagE.CallE" $ do

      it "Tags addition of Ints correctly." $ do
        simpleTagE defaultEnv (CallE (VarE (Var "+" () True) ()) (fromList [LitE (ILit 4) (), LitE (ILit 3) ()]) ())
          `shouldBe` (CallE (VarE (Var "+" (BTLambda BTInt (fromList [BTInt, BTInt])) True) (BTLambda BTInt (fromList [BTInt, BTInt]))) (fromList [LitE (ILit 4) BTInt, LitE (ILit 3) BTInt]) BTInt)

      it "Should fail" $ do
        simpleTagE defaultEnv (CallE (VarE (Var "+" () True) ()) (fromList [LitE (FLit 4.0) (), LitE (FLit 3.0) ()]) ())
          `shouldBe` (CallE (VarE (Var "+" (BTLambda BTInt (fromList [BTInt, BTInt])) True) (BTLambda BTInt (fromList [BTInt, BTInt]))) (fromList [LitE (ILit 4) BTInt, LitE (ILit 3) BTInt]) BTInt)

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
