{-# LANGUAGE NoImplicitPrelude #-}

module Language.Tush.Repl where

import ClassyPrelude
import Language.Tush.Parse
import Language.Tush.Eval
import qualified System.Console.Haskeline as HL

repl :: IO ()
repl = HL.runInputT HL.defaultSettings loop
  where
    loop :: HL.InputT IO ()
    loop = do
      line <- HL.getInputLine "λ "
      case line of
        Nothing -> return ()
        Just line' -> do
          let Right parsed = parseE $ fromString line'
          x <- applyBuiltin (evalE startEnv) parsed
          print x
          loop

