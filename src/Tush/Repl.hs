{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Tush.Repl where

import ClassyPrelude as CP

import Tush.Illuminate
import Tush.Syntax.Parse
import Tush.Typecheck2
import Tush.Compile.LLVM.CodeGen2
import Tush.Compile.LLVM.JIT
import Tush.Compile.LLVM.Types

import Data.Vector as V

import IPPrint.Colored

import System.Console.Haskeline

import LLVM.AST

instance Monoid (Either UnreachableError ()) where
  mappend x y = x
  mempty = return ()

process :: Text -> IO (Maybe LLVM.AST.Module)
process src = do
  let lexResult = lexTopLevel src in
    case lexResult of
      Left e -> print e >> return Nothing
      Right toks -> 
        let res = parseTopLevel toks in
          case res of
            Left e -> print e >> return Nothing
            Right r -> do
              cpprint r
              let taggedR = runTypeChecker $ CP.mapM (manifest >=> 
                                                      reify >=> 
                                                      (\z -> do
                                                          buildLocals z
                                                          constrain z
                                                          return z) >=>
                                                      unify . 
                                                      runIllumination . 
                                                      illuminateS) r 
              case taggedR of
                Left bad -> error (show bad)
                Right good -> do
                  cpprint good
                  let llvmState = CP.mapM (execLlvm . generateFromStatement) good
                  case llvmState of 
                    (Left bad', _) -> error (show bad')
                    (Right _, good') -> do
                      cpprint good'
                      let program' = (constructProgram <$> good') V.! 0
                      case program' of
                        (Program x) -> return $ x V.!? 0
                        
-- def printstar : Int (n : Int) if n < 1 then 0 else printstar(n - 1); putchar(42);

repl :: IO ()
repl = runInputT defaultSettings loop
  where
    loop = do
      minput <- getInputLine "λ "
      case minput of
        Nothing -> outputStrLn "Goodbye."
        Just input -> do
          modn <- liftIO $ process (fromString input)
          case modn of
            Just modn -> do
              liftIO $ cpprint modn
              liftIO $ runJIT modn
              loop
            Nothing -> loop
