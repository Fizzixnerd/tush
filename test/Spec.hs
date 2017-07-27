module Main where

import Test.Hspec

import Tush.Parse.LexSpec
import Tush.Parse.ParseSpec
import Tush.Parse.ExprSpec
import Tush.Typecheck.TypecheckSpec

main :: IO ()
main = do
  lexSpec
  exprSpec
  parseSpec
  typecheckSpec

-- tests :: TestTree
-- tests = testGroup "Tests" [lexSpec, parseSpec, exprSpec, typecheckSpec]
