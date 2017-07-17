module Spec where

import Test.Tasty

import Tush.Parse.LexSpec

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [lexSpec, parseSpec]
