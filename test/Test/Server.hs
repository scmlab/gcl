{-# LANGUAGE OverloadedStrings #-}

module Test.Server where

import Data.Loc
import qualified Data.Text.IO as Text
import Pretty (pretty)
import Server.DSL
import Server.Interpreter.Test
import qualified Server.Interpreter.Test as Server
import Syntax.Predicate (PO, Spec (Specification))
import Test.Tasty
import Test.Tasty.HUnit
import Data.Loc.Range

tests :: TestTree
tests = testGroup "Server" [utils, instantiateSpec]

--------------------------------------------------------------------------------

instantiateSpec :: TestTree
instantiateSpec =
  testGroup
    "Instantiate Specs" []
    -- [ run "top level" "spec-qm.gcl"
    -- ]
  where
    run :: String -> FilePath -> TestTree
    run name path =
      testCase name $ do
        let goldenFilePath = "./test/source/golden" <> path <> ".golden"
        let sourceFilePath = "./test/source/" <> path
        -- perform IO to read file
        source <- Text.readFile sourceFilePath

        let makeRange (offsetA, lineA, colA) (offsetB, lineB, colB) =
              Range
                (Pos sourceFilePath lineA colA offsetA)
                (Pos sourceFilePath lineB colB offsetB)
        --
        let (_, trace) = runTest sourceFilePath source $ do
              program <- parseProgram source
              _ <- sweep program
              return ()

        -- let spec = Specification 0 Pred Pred Loc

        trace
          @?= [ CmdGetFilePath,
                CmdEditText (makeRange (0, 1, 1) (0, 1, 1)) "[!\n\n!]",
                CmdGetFilePath,
                CmdEditText (makeRange (6, 3, 3) (6, 3, 3)) "[!\n  \n  !]",
                CmdGetFilePath
              ]

-- -- goldenTest
-- -- assertion no. 1 (passes)
-- 2 + 2 @?= 4
-- -- assertion no. 2 (fails)
-- assertBool "the list is not empty" $ null [1]
-- -- assertion no. 3 (would have failed, but won't be executed because
-- -- the previous assertion has already failed)
-- "foo" @?= "bar"

--------------------------------------------------------------------------------

utils :: TestTree
utils = testGroup "Utils" [compareWithPositionTests, withinSelectionTests]

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
