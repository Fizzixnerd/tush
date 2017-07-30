{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Tush.Repl where

import ClassyPrelude as CP

import Tush.Syntax.Parse
import Tush.Typecheck
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
      print r
      let taggedR = runSimpleTypecheck $ mapM simpleTagS r 
      case taggedR of
        Right good -> do
          print good
          ast <- codeGen modo good
          return $ Just ast
        Left e ->
          throw e

initModule :: Module
initModule = emptyModule "tush"

-- def printstar : Int (n : Int) if n < 1 then 0 else printstar(n - 1); putchar(42);


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
