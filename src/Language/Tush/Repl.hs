{-# LANGUAGE NoImplicitPrelude #-}

module Language.Tush.Repl where

import ClassyPrelude
import Language.Tush.Parse
import Language.Tush.Eval

repl :: IO ()
repl = do
  line <- getLine
  let Right parsed = parse line
  x <- applyBuiltin (eval startEnv) parsed
  print x
  repl

