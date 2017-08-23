{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Lib where

import ClassyPrelude

import Tush.Repl
import Tush.Compile.LLVM.CodeGen2

someFunc :: IO ()
someFunc = print $ compileabunch
