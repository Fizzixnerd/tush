{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Lib where

import ClassyPrelude

import Tush.REPL

foreign import ccall unsafe "io.h putchard" putchard :: Double -> Double

someFunc :: IO ()
someFunc = repl
