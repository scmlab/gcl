module Data.Loc.Util where

import Data.Loc

translate :: Int -> Pos -> Pos
translate n (Pos path ln col offset) = Pos path ln ((col + n) `max` 0) ((offset + n) `max` 0)

translateLoc :: Int -> Int -> Loc -> Loc
translateLoc _ _ NoLoc = NoLoc
translateLoc m n (Loc a b) = Loc (translate m a) (translate n b)

