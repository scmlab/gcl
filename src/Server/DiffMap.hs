--
-- This module provides a data structure 
--  for recording offset changes made by text edittings and easy lookups
--
module Server.DiffMap (DiffMap, empty, insert, lookup, adjust) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Prelude hiding (lookup)

-- A mapping with offsets as keys & accumulated differences as values
newtype DiffMap = DiffMap (IntMap Int)
  deriving (Show)

empty :: DiffMap
empty = DiffMap IntMap.empty

-- Given an offset 'o' and an differnce 'd'
-- Add 'd' to all values whose keys are greater or equal to the offset 'o'
insert :: Int -> Int -> DiffMap -> DiffMap
insert offset diff (DiffMap diffMap) =
    -- use the offset as the pivot, split the mapping into 2 halves 
  let (previousHalf, pivot, nextHalf) = IntMap.splitLookup offset diffMap
      nextHalf' = case pivot of
        -- if the pivot already exists, add'em up
        Just v -> IntMap.insert offset (v + diff) $ IntMap.map (+ diff) nextHalf
        -- else, insert a new difference, with its predecessor added
        Nothing -> 
            let diff' = case IntMap.lookupMax previousHalf of 
                            Nothing -> diff 
                            Just (_, x) -> x + diff 
            in IntMap.insert offset diff' $ IntMap.map (+ diff) nextHalf
   in DiffMap $ IntMap.union previousHalf nextHalf'

-- Given any offset, returns the accumulated difference
lookup :: DiffMap -> Int -> Int 
lookup (DiffMap diffMap) offset = 
    -- use the offset as the pivot, split the mapping into 2 halves 
  let (previousHalf, pivot, _) = IntMap.splitLookup offset diffMap 
   in  case pivot of 
        Just v -> v 
        Nothing -> case IntMap.lookupMax previousHalf of 
                        Nothing -> 0 
                        Just (_, x) -> x

-- Given any offset, returns the adjust offset
adjust :: DiffMap -> Int -> Int 
adjust diffMap offset = offset + lookup diffMap offset 