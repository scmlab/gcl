{-# LANGUAGE OverloadedStrings #-}

module Test.WP where

import           Data.Text.Lazy          hiding ( map )
import           Test.Tasty
import           Test.Tasty.HUnit
import           Prelude                 hiding ( Ordering(..) )

import           Syntax.Predicate
import           Syntax.Concrete
import qualified REPL                          as REPL
import           Syntax.Location
import           Data.Loc
import           Error
import           Pretty                         ( )

tests :: TestTree
tests = testGroup "WP 1" [statements, issues]

run :: Text -> IO (Either Error ([PO], [Spec]))
run text =
  REPL.runREPLM
    $   (\(pos, specs) -> (map toNoLoc pos, map toNoLoc specs))
    <$> (   REPL.scan "<test>" (text)
        >>= REPL.parseProgram "<test>"
        >>= REPL.sweep1
        )

--------------------------------------------------------------------------------
-- | Expression

statements :: TestTree
statements = testGroup
  "simple program"
  [ testCase "skip" $ do
    actual <-
      run
        "{ True }   \n\
                  \skip       \n\
                  \{ 0 = 0 }"
    actual @?= Right
      ( [ PO 0
             (assertion true)
             (assertion (number 0 `eqq` number 0))
             (AtSkip NoLoc)
        ]
      , []
      )
  , testCase "abort" $ do
    actual <-
      run
        "{ True }   \n\
                  \abort      \n\
                  \{ True }"
    actual
      @?= Right ([PO 0 (assertion true) (Constant false) (AtAbort NoLoc)], [])
  , testCase "assignment" $ do
    actual <-
      run
        "{ True }   \n\
                  \x := 1     \n\
                  \{ 0 = x }"
    actual @?= Right
      ( [ PO 0
             (assertion true)
             (assertion (number 0 `eqq` number 1))
             (AtAssignment NoLoc)
        ]
      , []
      )
  , testCase "spec" $ do
    actual <-
      run
        "{ True }   \n\
                  \{!       \n\
                  \!}       \n\
                  \{ 0 = 0 }"
    actual @?= Right
      ( []
      , [ Specification 0
                        (Assertion true NoLoc)
                        (assertion (number 0 `eqq` number 0))
                        NoLoc
        ]
      )
  ]



-- assertions :: TestTree
-- assertions = testCase "assertions" $ do
--   actual <- run "{ True }\n{ False }\n{ True }\n"
--   actual @?= Right
--     ( [ PO 0
--                    (Assertion true NoLoc)
--                    (Assertion false NoLoc)
--                    (AtAssertion NoLoc)

--       -- NOTE: missing the second proof obligation

--       -- , Obligation 1
--       --   (Assertion false NoLoc)
--       --   (Assertion true NoLoc)
--       --   (AssertGuaranteed NoLoc)
--       ]
--     , []
--     )

-- if' :: TestTree
-- if' = testGroup
--   "if statements"
--   [ testCase "without precondition" $ do
--       actual <- run "if False -> skip fi\n{ True }\n"
--       actual @?= Right
--         ( [ PO 0
--                        (GuardIf false NoLoc)
--             -- (Conjunct
--             --   [ Assertion true NoLoc
--             --   , Disjunct [ Guard false (IF NoLoc) NoLoc ]
--             --   , Guard false (IF NoLoc) NoLoc
--             --   ])
--                        (Assertion true NoLoc)
--                        (AtSkip NoLoc)
--           ]
--         , []
--         )
--   ]

--------------------------------------------------------------------------------
-- | Issues
issues :: TestTree
issues =
  testGroup
    "issues"
    [ issue2
    ]

issue2 :: TestTree
issue2 =
  testGroup
    "Issue #2"
    [ testCase "Postcondition only" $ do
        actual <-
          run
            "con A, B : Int\n\
            \var x, y, z : Int\n\
            \{ z = A * B }"
        actual
          @?= Right
            ( [],
              []
            )
    , testCase "Postcondition + precondition" $ do
        actual <-
          run
            "con A, B : Int\n\
            \var x, y, z : Int\n\
            \{ True }\n\
            \{ z = A * B }"
        actual
          @?= Right
            ( [],
              []
            )
   
    ]