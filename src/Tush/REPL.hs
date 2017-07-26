{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Tush.REPL where

import ClassyPrelude

import Tush.Parse
import Tush.Typecheck.Typecheck
import Tush.Compile.LLVM.CodeGen
import Tush.Compile.LLVM.JIT

import System.Console.Haskeline

import LLVM.AST

process :: Module -> Text -> IO (Maybe Module)
process modo source = do
  let res = parseToplevel source
  case res of
    Left e -> print e >> return Nothing
    Right r -> do
      let 
      ast <- codeGen modo (simpleTagS <$> r)
      return $ Just ast

initModule :: Module
initModule = emptyModule "tush"

repl :: IO ()
repl = runInputT defaultSettings (loop initModule)
  where
    loop mod = do
      minput <- getInputLine "λ "
      case minput of
        Nothing -> outputStrLn "Goodbye."
        Just input -> do
          modn <- liftIO $ process mod (fromString input)
          case modn of
            Just modn -> do
              liftIO $ runJIT modn
              loop modn
            Nothing -> loop mod
