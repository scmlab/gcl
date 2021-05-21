{-# LANGUAGE OverloadedStrings #-}

module Test.Server where

import Data.Loc
import Data.Loc.Range
import qualified Data.Text.IO as Text
import Pretty (pretty)
import Server.DSL
import Server.Interpreter.Test
import qualified Server.Interpreter.Test as Server
import Syntax.Predicate (PO, Spec (Specification))
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Server" [instantiateSpec]

--------------------------------------------------------------------------------

instantiateSpec :: TestTree
instantiateSpec =
  testGroup
    "Instantiate Specs"
    []
  where
    -- [ run "top level" "spec-qm.gcl"
    -- ]

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
