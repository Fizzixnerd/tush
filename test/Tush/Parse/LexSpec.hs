{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Tush.Parse.LexSpec where

import ClassyPrelude

import Tush.Parse.Syntax
import Tush.Parse.Lex

import Test.Hspec

import Data.Either

import Text.Megaparsec

lexSpec :: IO ()
lexSpec = hspec $ do
  describe "Tush.Parse.Lex" $ do
    describe "var" $ do

      it "Parses single-letter names." $ do
        runParser var "" "x" `shouldBe` (Right $ Var "x" ())

      it "Parses multi-letter names." $ do
        runParser var "" "xyz" `shouldBe` (Right $ Var "xyz" ())

      it "Parses alphanumeric names after first letter." $ do
        runParser var "" "xyz2" `shouldBe` (Right $ Var "xyz2" ())

      it "Fails to parse things which begin with a digit." $ do
        runParser var "" "3xy" `shouldSatisfy` isLeft

    describe "integer" $ do
    
      it "Parses single-digit numbers." $ do
        runParser integer "" "1" `shouldBe` (Right 1)

      it "Parses multi-digit numbers." $ do
        runParser integer "" "123" `shouldBe` (Right 123)

      it "Fails on negative numbers (should be parsed as unary negate on the positive number)." $ do
        runParser integer "" "-1" `shouldSatisfy` isLeft

    describe "floating" $ do
      
      it "Parses '1.0'." $ do
        runParser floating "" "1.0" `shouldBe` (Right 1.0)

      it "Parses scientific notation." $ do
        runParser floating "" "1.23e-45" `shouldBe` (Right 1.23e-45)

      it "Fails on negative numbers (should be parsed as unary negate on the positive number)." $ do
        runParser floating "" "-1.0" `shouldSatisfy` isLeft

    describe "lineComment" $ do
      
      it "Parses the empty comment." $ do
        runParser lineComment "" "--" `shouldBe` (Right ())

      it "Parses the empty comment, but leaves the newline." $ do
        runParser (lineComment >> newline) "" "--\n" `shouldBe` (Right '\n')

      it "Parses a non-empty comment." $ do
        runParser (lineComment >> eof) "" "-- Hello darkness my old friend." `shouldBe` (Right ())
    
    describe "blockComment" $ do
      
      it "Parses block comments." $ do
        runParser blockComment "" "{- hello\n there neighbour -}" `shouldBe` (Right ())

    describe "reserved" $ do
      
      it "Parses reserved words and ops." $ do
        runParser extern "" "extern" `shouldBe` (Right Extern)

    describe "parens" $ do
      
      it "Parses stuff between parens." $ do
        runParser (parens extern) "" "(extern)" `shouldBe` (Right Extern)

    describe "commaSep" $ do
      
      it "Parses comma separated stuff." $ do
        runParser (commaSep extern) "" "extern, extern, extern" `shouldBe` (Right $ fromList [Extern, Extern, Extern])
    
    describe "semiSep" $ do

      it "Parses semicolon separated stuff." $ do
        runParser (semiSep extern) "" "extern; extern; extern" `shouldBe` (Right $ fromList [Extern, Extern, Extern])
