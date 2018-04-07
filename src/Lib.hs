{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Lib where

import ClassyPrelude

import Tush.Compile.LLVM.CodeGen2
import IPPrint.Colored

someFunc :: IO ()
someFunc = cpprint $ (\(x:xs) -> x) $ compileabunch
