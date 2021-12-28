{-# LANGUAGE OverloadedStrings #-}

module Test.WP where

import           Prelude                 hiding ( Ordering(..) )
import           Pretty                         ( )
import           Server.Pipeline
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
          parsed      <- parse source
          converted   <- convert parsed
          typeChecked <- typeCheck converted
          result      <- sweep typeChecked
          return (Right (sweptPOs result))

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
  [ run "issue #2-1: Postcondition only"           "issue2-1.gcl"
  , run "issue #2-2: Postcondition + precondition" "issue2-2.gcl"
  , run "issue #57: {::} and then {--}" "issue57.gcl"
  ]
