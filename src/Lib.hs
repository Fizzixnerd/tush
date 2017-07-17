{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Lib where

import ClassyPrelude

import Tush.REPL

someFunc :: IO ()
someFunc = repl
