{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Tush.Compile.LLVM.JIT where

import ClassyPrelude
import qualified Data.ByteString.Char8 as BS

import Foreign.Ptr (FunPtr, castFunPtr)

import qualified LLVM.AST as AST
import LLVM.Module
import LLVM.Context
import LLVM.PassManager
import LLVM.Analysis

import qualified LLVM.ExecutionEngine as EE

foreign import ccall "dynamic" haskFun :: FunPtr (IO Int) -> (IO Int)

runJIT :: AST.Module -> IO AST.Module
runJIT mod' = do
  withContext $ \context ->
    withModuleFromAST context mod' $ \m -> do
    writeLLVMAssemblyToFile (File "tush.ll") m
    withPassManager passes $ \pm -> do
      -- verify the AST
      verify m
      -- optimize the module
      void $ runPassManager pm m
      optmod <- moduleAST m
      s <- moduleLLVMAssembly m
      -- jit compile and run the code
      jit context $ \executionEngine ->
        EE.withModuleInEngine executionEngine m $ \ee -> do
          mainfn <- EE.getFunction ee (AST.Name "__main")
          case mainfn of
            Just fn -> do
              res <- run fn
              putStrLn $ "Evaluated to: " ++ (fromString $ show res)
            Nothing -> return ()
      -- print the optimized bitcode
      BS.putStrLn s
      -- return the optimized module
      return optmod

jit :: Context -> (EE.MCJIT -> IO a) -> IO a
jit c = EE.withMCJIT c optlevel model ptrelim fastins
  where
    optlevel = Just 0
    model = Nothing
    ptrelim = Nothing
    fastins = Nothing

run :: FunPtr a -> IO Int
run fn = haskFun (castFunPtr fn :: FunPtr (IO Int))

passes :: PassSetSpec
passes = defaultCuratedPassSetSpec { optLevel = Just 3 }
