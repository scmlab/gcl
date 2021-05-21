{-# LANGUAGE FlexibleInstances #-}

module Data.Loc.Selection where

import Data.Loc.Range ( Range(..) )
import qualified Data.Loc.Range as Range
import Data.Loc (posCoff)
import Data.Aeson ( FromJSON(parseJSON), ToJSON (toJSON) )

-- | Represents a cursor selection
--
--  Very much like `Range`, but note that the cursor is placed IN-BETWEEN two characters rather than ON a character
--
--  For example: to represent the selection of "ABC" in "ABCD"
--    charactor offset    :   012 3
--    charactors          :   ABC D   
-------------------------------------------------------
--    Range     of "ABC"  :   ^^^     Range     (Pos ... 0) (Pos ... 2)
--    Selection of "ABC"  :  ^   ^    Selection (Pos ... 0) (Pos ... 2)
--
--  We abuse the first `Pos` to represent what is actually the left  endpoint of that `Pos`
--      and the second `Pos` to represent what is actually the right endpoint of that `Pos`

newtype Selection = Selection { unSelection :: Range }
  deriving (Eq)

instance Show Selection where 
  show (Selection (Range start end)) = show (posCoff start) <> "-" <> show (posCoff end) 

instance FromJSON Selection where
  parseJSON x = Selection <$> parseJSON x

instance ToJSON Selection where
  toJSON = toJSON . unSelection

-- | Calculates the length coverted by a selection
span :: Selection -> Int
span = Range.span . unSelection