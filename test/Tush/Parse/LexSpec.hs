{-# LANGUAGE OverloadedStrings #-}
module Tush.Parse.LexSpec where

import Tush.Parse.Lex

import Test.Hspec

import Data.Either
import Data.List
import Data.Vector

import Text.Megaparsec
import Text.Megaparsec.Char

lexSpec = hspec $ do
  describe "Tush.Parse.Lex" $ do
    describe "Tush.Parse.Lex.identifier" $ do

      it "Parses single-letter names." $ do
        runParser identifier "" "x" `shouldBe` (Right $ Identifier "x")

      it "Parses multi-letter names." $ do
        runParser identifier "" "xyz" `shouldBe` (Right $ Identifier "xyz")

      it "Parses alphanumeric names after first letter." $ do
        runParser identifier "" "xyz2" `shouldBe` (Right $ Identifier "xyz2")

      it "Fails to parse things which begin with a digit." $ do
        runParser identifier "" "3xy" `shouldSatisfy` isLeft

    describe "Tush.Parse.Lex.integer" $ do
    
      it "Parses single-digit numbers." $ do
        runParser integer "" "1" `shouldBe` (Right 1)

      it "Parses multi-digit numbers." $ do
        runParser integer "" "123" `shouldBe` (Right 123)

      it "Fails on negative numbers (should be parsed as unary negate on the positive number)." $ do
        runParser integer "" "-1" `shouldSatisfy` isLeft

    describe "Tush.Parse.Lex.floating" $ do
      
      it "Parses '1.0'." $ do
        runParser floating "" "1.0" `shouldBe` (Right 1.0)

      it "Parses scientific notation." $ do
        runParser floating "" "1.23e-45" `shouldBe` (Right 1.23e-45)

      it "Fails on negative numbers (should be parsed as unary negate on the positive number)." $ do
        runParser floating "" "-1.0" `shouldSatisfy` isLeft

    describe "Tush.Parse.Lex.lineComment" $ do
      
      it "Parses the empty comment." $ do
        runParser lineComment "" "--" `shouldBe` (Right ())

      it "Parses the empty comment, but leaves the newline." $ do
        runParser (lineComment >> newline) "" "--\n" `shouldBe` (Right '\n')

      it "Parses a non-empty comment." $ do
        runParser (lineComment >> eof) "" "-- Hello darkness my old friend." `shouldBe` (Right ())
    
    describe "Tush.Parse.Lex.blockComment" $ do
      
      it "Parses block comments." $ do
        runParser blockComment "" "{- hello\n there neighbour -}" `shouldBe` (Right ())

    describe "Tush.Parse.Lex.reserved" $ do
      
      it "Parses reserved words and ops." $ do
        runParser extern "" "extern" `shouldBe` (Right Extern)

    describe "Tush.Parse.Lex.parens" $ do
      
      it "Parses stuff between parens." $ do
        runParser (parens extern) "" "(extern)" `shouldBe` (Right Extern)

    describe "Tush.Parse.Lex.commaSep" $ do
      
      it "Parses comma separated stuff." $ do
        runParser (commaSep extern) "" "extern, extern, extern" `shouldBe` (Right $ fromList [Extern, Extern, Extern])
    
    describe "Tush.Parse.Lex.semiSep" $ do

      it "Parses semicolon separated stuff." $ do
        runParser (semiSep extern) "" "extern; extern; extern" `shouldBe` (Right $ fromList [Extern, Extern, Extern])
