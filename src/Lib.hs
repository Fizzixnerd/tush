{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Lib where

import ClassyPrelude

import Tush.REPL

someFunc :: IO ()
someFunc = repl
