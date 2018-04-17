{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable#-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Data.History where

import ClassyPrelude
import Data.Data
import qualified Data.Vector as V

newtype History a = History
  { _unHistory :: Vector a
  } deriving (Eq, Ord, Show, Data, Typeable, Generic)

historyPush :: a -> History a -> History a
historyPush x History {..} = History $ _unHistory `V.snoc` x

historyLookup :: History a -> Int -> Maybe a
historyLookup History {..} i = _unHistory V.!? i

historyEmpty :: History a
historyEmpty = History empty

historyNull :: History a -> Bool
historyNull History {..} = null _unHistory

historyEarliest :: History a -> Maybe a
historyEarliest History {..} = if null _unHistory
                               then Nothing
                               else Just $ V.last _unHistory

historyLatest :: History a -> Maybe a
historyLatest History {..} = if null _unHistory
                             then Nothing
                             else Just $ V.head _unHistory

historyLength :: History a -> Int
historyLength History {..} = length _unHistory
