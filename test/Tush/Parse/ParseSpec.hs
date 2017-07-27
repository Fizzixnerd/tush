{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Tush.Parse.ParseSpec where

import ClassyPrelude

import Tush.Parse.Statement
import Tush.Parse.Syntax

import Text.Megaparsec


import Test.Hspec

parseSpec :: IO ()
parseSpec = hspec $ do
  describe "Tush.Parse.Statement" $ do

    describe "funcS" $ do

      it "Parses a very simple function definition." $ do
        runParser funcS "" "def f : Bool () true;" `shouldBe` (Right $ FuncS (FProto (Var "f" (BTLambda BTBool mempty) False) mempty) mempty (LitE (BLit True) ()))
