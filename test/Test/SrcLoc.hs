{-# LANGUAGE OverloadedStrings #-}

module Test.SrcLoc where

import Data.Loc
import Test.Tasty
import Test.Tasty.HUnit
import Data.Loc.Range
import Data.List (sort)
import GCL.Predicate (Origin (AtSkip))

tests :: TestTree
tests = testGroup "Source Location" [compareWithPositionTests, withinTests, withinRangeTests, sortingOriginsTests]

--------------------------------------------------------------------------------

compareWithPositionTests :: TestTree
compareWithPositionTests =
  testGroup
    "compareWithPosition"
    [ testCase "1" $ run 0 (make 10 20) @?= LT,
      testCase "2" $ run 9 (make 10 20) @?= LT,
      testCase "3" $ run 10 (make 10 20) @?= EQ,
      testCase "4" $ run 11 (make 10 20) @?= EQ,
      testCase "5" $ run 15 (make 10 20) @?= EQ,
      testCase "6" $ run 19 (make 10 20) @?= EQ,
      testCase "7" $ run 20 (make 10 20) @?= EQ,
      testCase "8" $ run 21 (make 10 20) @?= GT,
      testCase "9" $ run 22 (make 10 20) @?= GT
    ]
  where
    run :: Int -> Item -> Ordering
    run offset item = compareWithPosition (Pos "" 1 1 offset) item 

--------------------------------------------------------------------------------

withinTests :: TestTree
withinTests =
  testGroup
    "within"
    [ testCase "1" $ run (make 9 20) (make 10 20) @?= False,
      testCase "2" $ run (make 10 21) (make 10 20) @?= False,
      testCase "3" $ run (make 10 20) (make 10 20) @?= True,
      testCase "4" $ run (make 10 20) (make 9 20) @?= True,
      testCase "5" $ run (make 10 20) (make 10 21) @?= True,
      testCase "6" $ run (make 0 1) (make 10 20) @?= False,
      testCase "7" $ run (make 0 15) (make 10 20) @?= False,
      testCase "8" $ run (make 30 40) (make 10 20) @?= False,
      testCase "9" $ run (make 15 40) (make 10 20) @?= False
    ]
    where 
      run :: Item -> Item -> Bool
      run x y = rangeOf x `within` rangeOf y

--------------------------------------------------------------------------------

sortingOriginsTests :: TestTree
sortingOriginsTests =
  testGroup
    "sorting Origins"
    [ testCase "1" $ sort [mk 10 20, mk 20 30, mk 11 19, mk 21 29] @?= [mk 11 19, mk 10 20, mk 21 29, mk 20 30]
    , testCase "2" $ sort [mk 80 184, mk 80 184, mk 92 102, mk 92 102] @?= [mk 92 102, mk 92 102, mk 80 184, mk 80 184]
    , testCase "overlapped 1" $ sort [mk 10 20, mk 15 25, mk 20 30] @?= [mk 10 20, mk 15 25, mk 20 30]
    ]
    where 
      mk :: Int -> Int -> Origin 
      mk a b = AtSkip (Loc (Pos "" 1 (a + 1) a) (Pos "" 1 (b + 1) b))

--------------------------------------------------------------------------------

withinRangeTests :: TestTree
withinRangeTests =
  testGroup
    "withinRange"
    [ testCase "1" $ run (0, 0) (make 10 20) @?= False,
      testCase "2" $ run (0, 8) (make 10 20) @?= False,
      testCase "3" $ run (0, 9) (make 10 20) @?= False,
      testCase "4" $ run (0, 10) (make 10 20) @?= True,
      testCase "5" $ run (0, 11) (make 10 20) @?= True,
      testCase "6" $ run (11, 16) (make 10 20) @?= True,
      testCase "7" $ run (19, 30) (make 10 20) @?= True,
      testCase "8" $ run (20, 30) (make 10 20) @?= True,
      testCase "9" $ run (21, 30) (make 10 20) @?= False,
      testCase "10" $ run (22, 30) (make 10 20) @?= False
    ]
  where
    run :: (Int, Int) -> Item -> Bool
    run (start, end) item = withinRange (Range (Pos "" 1 1 start) (Pos "" 1 1 end)) item

-- | For testing selection related stuff
newtype Item = Item {unItem :: Range}
  deriving (Eq)

instance Show Item where
  show item = case rangeOf item of
    Range start end -> "Item " <> show (posCoff start) <> " " <> show (posCoff end)

make :: Int -> Int -> Item
make start end = Item (Range (Pos "" 1 (start + 1) start) (Pos "" 1 (end + 1) end))

instance Ranged Item where
  rangeOf = unItem

instance Located Item where
  locOf = locOf . unItem