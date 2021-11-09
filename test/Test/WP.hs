{-# LANGUAGE OverloadedStrings #-}

module Test.WP where

import           Prelude                 hiding ( Ordering(..) )
import           Pretty                         ( )
import           Server.DSL                     ( Stage(..)
                                                , SweepResult(..)
                                                , parseProgram
                                                , sweep
                                                )
import           Test.Server.Interpreter
import           Test.Tasty                     ( TestTree
                                                , testGroup
                                                )
import           Test.Util                      ( runGoldenTest )

tests :: TestTree
tests = testGroup "WP" [statements, issues]

--------------------------------------------------------------------------------

run :: String -> FilePath -> TestTree
run =
  runGoldenTest "./test/source/WP/" "./test/golden/WP/" ""
    $ \sourcePath source -> do
        return $ serializeTestResult $ runTest sourcePath source $ do
          (concrete, abstract) <- parseProgram source
          stage                <- sweep concrete abstract
          case stage of
            -- SweepFailure errors -> return $ Left errors
            SweepSuccess result -> return (Right (sweepPOs result))

-- | Expression
statements :: TestTree
statements = testGroup
  "statements"
  [ run "empty"       "empty.gcl"
  , run "skip"        "skip.gcl"
  , run "abort"       "abort.gcl"
  , run "assignment"  "assignment.gcl"
  , run "conditional" "conditional.gcl"
  ]

--------------------------------------------------------------------------------

-- | Issues
issues :: TestTree
issues = testGroup
  "issues"
  [ run "issue #2: Postcondition only"           "issue2-1.gcl"
  , run "issue #2: Postcondition + precondition" "issue2-2.gcl"
  ]
