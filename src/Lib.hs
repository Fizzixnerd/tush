{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Lib where

import ClassyPrelude

import Tush.Repl

someFunc :: IO ()
someFunc = repl
