{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Tush.REPL where

import ClassyPrelude

import Tush.Parse
import Control.Monad.Trans
import System.Console.Haskeline

process :: Text -> IO ()
process line = do
  let res = parseToplevel line
  case res of
    Left e -> print e
    Right r -> mapM_ print r

repl :: IO ()
repl = runInputT defaultSettings loop
  where
    loop = do
      minput <- getInputLine "λ "
      case minput of
        Nothing -> outputStrLn "Goodbye."
        Just input -> do
          liftIO $ process (fromString input)
          loop
