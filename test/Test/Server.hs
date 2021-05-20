{-# LANGUAGE OverloadedStrings #-}

module Test.Server where

import Data.Loc
import Server.DSL (compareWithPosition, withinSelection)
import Syntax.Predicate (PO)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Server" [utilTests]

utilTests :: TestTree
utilTests = testGroup "Utils" [compareWithPositionTests, withinSelectionTests]

--------------------------------------------------------------------------------

-- | For testing selection related stuff
newtype Item = Item {unItem :: Loc}
  deriving (Eq)

instance Show Item where
  show item = case locOf item of
    NoLoc -> "Item"
    Loc start end -> "Item " <> show (posCoff start) <> " " <> show (posCoff end)

make :: Int -> Int -> Item
make start end = Item (Loc (Pos "" 1 1 start) (Pos "" 1 1 end))

instance Located Item where
  locOf = unItem

--------------------------------------------------------------------------------
  

compareWithPositionTests :: TestTree
compareWithPositionTests =
  testGroup
    "compareWithMouse"
    [ testCase "1" $ run 0 (make 10 20) @?= LT,
      testCase "2" $ run 9 (make 10 20) @?= LT,
      testCase "3" $ run 10 (make 10 20) @?= EQ,
      testCase "4" $ run 11 (make 10 20) @?= EQ,
      testCase "5" $ run 15 (make 10 20) @?= EQ,
      testCase "6" $ run 19 (make 10 20) @?= EQ,
      testCase "7" $ run 20 (make 10 20) @?= EQ,
      testCase "8" $ run 21 (make 10 20) @?= EQ,
      testCase "9" $ run 22 (make 10 20) @?= GT
    ]
  where
    run :: Int -> Item -> Ordering
    run = compareWithPosition  

--------------------------------------------------------------------------------

withinSelectionTests :: TestTree
withinSelectionTests =
  testGroup
    "withinMouseSelection"
    [ testCase "1" $ run (0, 0) (make 10 20) @?= False,
      testCase "2" $ run (0, 8) (make 10 20) @?= False,
      testCase "3" $ run (0, 9) (make 10 20) @?= False,
      testCase "4" $ run (0, 10) (make 10 20) @?= True,
      testCase "5" $ run (0, 11) (make 10 20) @?= True,
      testCase "6" $ run (11, 16) (make 10 20) @?= True,
      testCase "7" $ run (19, 30) (make 10 20) @?= True,
      testCase "8" $ run (20, 30) (make 10 20) @?= True,
      testCase "9" $ run (21, 30) (make 10 20) @?= True,
      testCase "10" $ run (22, 30) (make 10 20) @?= False
    ]
  where
    run :: (Int, Int) -> Item -> Bool
    run = withinSelection