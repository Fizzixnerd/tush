{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Tush.Compile.LLVM.CodeGen where

import ClassyPrelude as CP

import Data.ByteString.Short
import Data.Map as M
import qualified Data.Vector as V

import LLVM.AST
import LLVM.AST.Global
import LLVM.AST.Linkage
import LLVM.AST.Constant
import Control.Monad.State

type SymbolTable = Map Text (Vector Operand)
type Names = Map ShortByteString Int

data CodeGenState = CodeGenState {
    currentBlock :: Name
  , blocks :: Map Name BlockState
  , symtab :: SymbolTable
  , blockCount :: Int
  , count :: Word
  , names :: Names
  } deriving Show

data BlockState = BlockState {
    idx :: Int
  , stack :: [Named Instruction]
  , term :: Maybe (Named Terminator)
  } deriving Show

newtype CodeGen a = CodeGen { runCodeGen :: State CodeGenState a }
  deriving (Functor, Applicative, Monad, MonadState CodeGenState)

newtype LLVM a = LLVM (State Module a)
  deriving (Functor, Applicative, Monad, MonadState Module)

double :: Type
double = FloatingPointType DoubleFP

emptyModule :: ShortByteString -> Module
emptyModule label = defaultModule { moduleName = label }

addDefn :: Definition -> LLVM ()
addDefn d = do
  defs <- gets moduleDefinitions
  modify $ \s -> s { moduleDefinitions = defs <> [d] }

define :: Type -> ShortByteString -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
define retType label argTypes body = addDefn $ 
  GlobalDefinition $ functionDefaults {
    name = Name label
  , parameters = ([Parameter ty nm [] | (ty, nm) <- argTypes], False)
  , returnType = retType
  , basicBlocks = body
  }

external :: Type -> ShortByteString -> [(Type, Name)] -> LLVM ()
external retType label argTypes = addDefn $
  GlobalDefinition $ functionDefaults {
    name = Name label
  , linkage = External
  , parameters = ([Parameter ty nm [] | (ty, nm) <- argTypes], False)
  , returnType = retType
  , basicBlocks = []
  }

entry :: CodeGen Name
entry = gets currentBlock

addBlock :: ShortByteString -> CodeGen Name
addBlock blockName = do
  bs <- gets blocks
  ix <- gets blockCount
  ns <- gets names
  let newBlock = emptyBlock ix
      (qName, supply) = uniqueName blockName ns

  modify $ \s -> s { blocks = insert (Name qName) newBlock bs
                   , blockCount = ix + 1
                   , names = supply
                   }
  return (Name qName)

setBlock :: Name -> CodeGen Name
setBlock blockName = do
  modify $ \s -> s { currentBlock = blockName }
  return blockName
  
getBlock :: CodeGen Name
getBlock = gets currentBlock

modifyBlock :: BlockState -> CodeGen ()
modifyBlock new = do
  active <- getBlock
  modify $ \s -> s { blocks = insert active new (blocks s) }

current :: CodeGen BlockState
current = do
  c <- getBlock
  blks <- gets blocks
  case M.lookup c blks of
    Just x -> return x
    Nothing -> error $ "No such block: " <> show c

fresh :: CodeGen Word
fresh = do
  i <- gets count
  modify $ \s -> s { count = i + 1 }
  return $ i + 1

uniqueName :: ShortByteString -> Names -> (ShortByteString, Names)
uniqueName n ns =
  case M.lookup n ns of
    Nothing -> (n, M.insert n 1 ns)
    Just i -> (n <> fromString (show i), M.insert n (i + 1) ns)

local :: Type -> Name -> Operand
local t = LocalReference t

localDouble :: Name -> Operand
localDouble = local double

externf :: Type -> Name -> Operand
externf t = ConstantOperand . GlobalReference t

assign :: Text -> Operand -> CodeGen ()
assign var x = do
  locals <- gets symtab
  modify $ \s -> s { symtab = M.insertWith (<>) var (CP.fromList [x]) locals }

getVar :: Text -> CodeGen Operand
getVar var = do
  locals <- gets symtab
  case M.lookup var locals of
    Just xs -> return (V.head xs)
    Nothing -> error $ "Local variable not in scope: " <> show var
