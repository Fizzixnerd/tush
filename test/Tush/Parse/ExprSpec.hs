{-# LANGUAGE OverloadedStrings #-}

module Tush.Parse.ExprSpec where

import Tush.Parse.Expr
import Tush.Parse.Statement
import Tush.Parse.Syntax

import Test.Hspec

import Data.Vector

import Text.Megaparsec

exprSpec :: IO ()
exprSpec = hspec $ do
  describe "Tush.Parse.Expr" $ do
    describe "functionPrototype" $ do

      it "Parses function prototypes with no arguments." $ do
        runParser fProto "" "f : Float ()" `shouldBe` (Right $ FProto (Var "f" (BTLambda BTFloat mempty) False) mempty)

      it "Parses function prototypes with three arguments." $ do
        runParser fProto "" "foo : Bool (bar : Float, baz : Int, qux : (Int) -> Bool)" 
          `shouldBe` (Right $ FProto (Var "foo" (BTLambda BTBool (fromList [ BTFloat
                                                                           , BTInt
                                                                           , BTLambda BTBool (fromList [BTInt])
                                                                           ])) False)
                                     (fromList [ Var "bar" BTFloat False
                                               , Var "baz" BTInt False
                                               , Var "qux" (BTLambda BTBool (fromList [BTInt])) False]))

    describe "externStatement" $ do
      
      it "Parses an extern statement for 'sin(theta)'." $ do
        runParser externS "" "extern sin : Float (theta : Float);" 
          `shouldBe` (Right $ ExternS $ FProto (Var "sin" (BTLambda BTFloat (fromList [BTFloat])) False)
                                               (fromList [Var "theta" BTFloat False]))
      
    describe "functionStatement" $ do
      
      it "Parses a function statement for 'twice(x)'." $ do
        runParser funcS "" "def twice : Float (x : Float) 2 * x;" 
          `shouldBe` (Right $ FuncS (FProto (Var "twice" (BTLambda BTFloat (fromList [BTFloat])) False)
                                            (fromList [Var "x" BTFloat False]))
                                    mempty
                                    (CallE (VarE (Var "*" () True) ()) (fromList [LitE (ILit 2) (), VarE (Var "x" () False) ()]) ()))

    describe "expr" $ do
      
      it "Parses float literals." $ do
        runParser expr "" "1.0" `shouldBe` (Right $ LitE (FLit 1.0) ())

      it "Parses integer literals." $ do
        runParser expr "" "3" `shouldBe` (Right $ LitE (ILit 3) ())

      it "Parses multiplication." $ do
        runParser expr "" "3 * 4" `shouldBe` (Right $ CallE (VarE (Var "*" () True) ()) (fromList [LitE (ILit 3) (), LitE (ILit 4) ()]) ())

      it "Parses division." $ do
        runParser expr "" "1.0 ./ 4.5" `shouldBe` (Right $ CallE (VarE (Var "./" () True) ()) (fromList [LitE (FLit 1.0) (), LitE (FLit 4.5) ()]) ())

      -- it "Parses addition." $ do
      --   runParser expr "" "1.5 + 2.4" `shouldBe` (Right $ BinOpE Add (LitE (FLit 1.5) ()) (LitE (FLit 2.4) ()) ())

      -- it "Parses subtraction." $ do
      --   runParser expr "" "1.5 - 2.4" `shouldBe` (Right $ BinOpE Sub (LitE (FLit 1.5) ()) (LitE (FLit 2.4) ()) ())

      -- it "Parses unary negation." $ do
      --   runParser expr "" "-5" `shouldBe` (Right $ UnOpE Neg (LitE (ILit 5) ()) ())

      -- it "Parses binary operations with parens correctly" $ do
      --   runParser expr "" "(3 + 5) * 2" `shouldBe` (Right $ BinOpE Mul (BinOpE Add (LitE (ILit 3) ()) (LitE (ILit 5) ()) ()) 
      --                                                                              (LitE (ILit 2) ()) ())

      -- it "Parses complicated arithmetic expressions with parens, funcalls, and variable references." $ do
      --   runParser expr "" "5 + 3 * (-x) / f(3)" `shouldBe` (Right $ BinOpE Add (LitE (ILit 5) ())
      --                                                                          (BinOpE Div (BinOpE Mul (LitE (ILit 3) ())
      --                                                                                                  (UnOpE Neg (VarE (Var "x" ()) ()) ()) ())
      --                                                                                      (CallE (VarE (Var "f" ()) ()) (fromList [LitE (ILit 3) ()]) ()) ()) ())

    -- describe "statement" $ do

      -- it "Parses a function statement for 'twice(x)'." $ do
      --   runParser statement "" "def twice(x) 2 * x;" `shouldBe` (Right $ FuncS (FProto "twice" (fromList ["x"])) 
      --                                                                          (BinOpE Mul (LitE $ ILit 2) (VarE "x")))

      -- it "Fails to parse a function statement for 'twice(x)' when you forget the semicolon." $ do
      --   runParser statement "" "def twice(x) 2 * x" `shouldSatisfy` isLeft
