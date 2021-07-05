{-# LANGUAGE OverloadedStrings #-}

module Test.WP where

import Pretty ()
import Server.DSL (parseProgram, sweep)
import Server.Interpreter.Test
import Test.Util (runGoldenTest)
import Test.Tasty (TestTree, testGroup)
import Prelude hiding (Ordering (..))

tests :: TestTree
tests =
  testGroup
    "WP"
    [ statements,
      issues
    ]

--------------------------------------------------------------------------------

run :: String -> FilePath -> TestTree
run = runGoldenTest "./test/source/WP/" "./test/golden/WP/" "" $ \sourcePath source -> do
  return $
    serializeTestResult $
      runTest sourcePath source $ do
        program <- parseProgram source
        (xs, _, _, _) <- sweep program
        return (Right xs)
    
-- | Expression
statements :: TestTree
statements =
  testGroup
    "statements"
    [ run "empty" "empty.gcl",
      run "skip" "skip.gcl",
      run "abort" "abort.gcl",
      run "assignment" "assignment.gcl",
      run "conditional" "conditional.gcl"
    ]

--------------------------------------------------------------------------------

-- | Issues
issues :: TestTree
issues =
  testGroup
    "issues"
    [ run "issue #2: Postcondition only" "issue2-1.gcl"
    , run "issue #2: Postcondition + precondition" "issue2-2.gcl"
    ]
