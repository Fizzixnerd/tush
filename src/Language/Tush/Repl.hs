{-# LANGUAGE NoImplicitPrelude #-}

module Language.Tush.Repl where

import ClassyPrelude
import qualified Language.Tush.Syntax as S
import Text.PrettyPrint
import Language.Tush.Parse
import Language.Tush.Eval
import Control.Exception.Base
import qualified System.Console.Haskeline as HL

repl :: IO ()
repl = HL.runInputT HL.defaultSettings loop
  where
    loop :: HL.InputT IO ()
    loop = HL.catch (do
      line <- HL.getInputLine "λ "
      case line of
        Nothing -> return ()
        Just line' -> do
          let Right parsed = parseE $ fromString line'
          x <- applyBuiltin (evalE startEnv) parsed
          x' <- liftIO $ S.tshow x
          liftIO $ putStrLn $ fromString $ render x'
          loop) (\e -> do
                    liftIO $ putStrLn (fromString $ show (e :: SomeException))
                    loop)

