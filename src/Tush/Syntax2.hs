{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Syntax2.hs
-- Author: Matt Walker
-- License: https://opensource.org/licenses/BSD-2-Clause
-- Created: Aug 12, 2017
-- Summary: Redoing it with TTF

module Tush.Syntax2 where

import Control.Monad.State

import Data.Maybe
import Data.Typeable
import Data.Dynamic

import qualified Data.ByteString.Short as BSS

import LLVM.AST as A
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F

import ClassyPrelude

newtype Var = Var { varName :: Text }

type SymTab = Map Text Dynamic

class LitSym repr where
  litI :: Integer -> repr Env Integer
  litF :: Double  -> repr Env Double
  litB :: Bool    -> repr Env Bool

class VarSym repr where

class LamSym repr where
  lamE :: Typeable a => repr Env b -> repr Env (a -> b)
  appE :: repr Env (a -> b) -> repr Env a -> repr Env b

class IfSym repr where
  ifE :: repr Env Bool -> repr Env a -> repr Env a -> repr Env a

class NumSym repr num where
  add :: repr h num -> repr h num -> repr h num
  sub :: repr h num -> repr h num -> repr h num
  mul :: repr h num -> repr h num -> repr h num

class DivSym repr num where
  div :: repr h num -> repr h num -> repr h num

newtype Tushi h a = Tushi { unTushi :: h -> a }
data Env = Env { nameCounter :: Word
               , environment :: SymTab
               }
freshName x@(Env c _) = (fromString $ ("x" ++ show c), x { nameCounter = c + 1 })

insertEnv :: Typeable v => Text -> v -> Env -> Env
insertEnv k v e = e { environment = insertMap k (toDyn v) (environment e) }

instance LitSym Tushi where
  litI = Tushi . const
  litF = Tushi . const
  litB = Tushi . const

instance LamSym Tushi where
  lamE e = Tushi $ \h -> \x -> let (fn, h') = freshName h in
                                 unTushi e (insertEnv fn x h')
  appE e1 e2 = Tushi $ \h -> (unTushi e1 h) (unTushi e2 h)

instance IfSym Tushi where
  ifE b cons_ ante = Tushi $ \h -> if unTushi b h then 
                                     unTushi cons_ h 
                                   else 
                                     unTushi ante h

instance (Num num) => NumSym Tushi num where
  add x y = Tushi $ \h -> unTushi x h + unTushi y h
  sub x y = Tushi $ \h -> unTushi x h - unTushi y h
  mul (Tushi x) (Tushi y) = Tushi $ x * y

instance DivSym Tushi Integer where
  div (Tushi x) (Tushi y) = Tushi $ x `ClassyPrelude.div` y

instance DivSym Tushi Double where
  div (Tushi x) (Tushi y) = Tushi $ x / y

-- newtype Tush code a = Tush { unTush :: code}

-- type SymbolTable = Map Text (Vector Operand)
-- type Names = Map BSS.ShortByteString Int
-- newtype Stack a = Stack { unStack :: [a] } deriving (Eq, Ord, Show)

-- push :: Stack a -> a -> Stack a
-- push (Stack xs) x = Stack (x : xs)

-- pop :: Stack a -> (Maybe a, Stack a)
-- pop (Stack []) = (Nothing, Stack [])
-- pop (Stack (x:xs)) = (Just x, Stack xs)

-- peek :: Stack a -> Maybe a
-- peek = fst . pop

-- empty :: Stack a -> Bool
-- empty (Stack []) = True
-- empty _ = False

-- data TushState = TushState {
--     currentBlock :: Name
--   , blocks :: Map Name BlockState
--   , env :: SymbolTable
--   , blockCount :: Int
--   , count :: Word
--   , names :: Names
--   , currentExit :: Name
--   , operandStack :: Stack Operand
--   }

-- data BlockState = BlockState {
--     idx :: Int
--   , stack :: Named Instruction
--   , term :: Maybe (Named Terminator)
--   }

-- type M = StateT TushState Identity

-- constant :: C.Constant -> Operand
-- constant = ConstantOperand

-- instance LitSym (Tush (M Operand)) where
--   litI x = Tush $ return $ constant $ C.Int 64 x
--   litF x = Tush $ return $ constant $ C.Float $ F.Double x
--   litB x = Tush $ return $ constant $ C.Int 1 (if x then 1 else 0)

-- instance LamSym (Tush (M ())) where
--   lamE f = Tush $ unTush $ f (Tush $ return ())
--   appE mf mx = Tush $ do
--     unTush mx
--     unTush mf
