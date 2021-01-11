{-# LANGUAGE OverloadedStrings #-}

module Test.WP where

import Data.Loc
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Text (Text)
import Data.Text.Lazy (toStrict, fromStrict)
import qualified Data.Text.Lazy.Encoding as LazyText
import Error
import qualified LSP
import Pretty ()
import Syntax.Concrete
import Syntax.Location
import Syntax.Predicate
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (Ordering (..))

tests :: TestTree
tests = testGroup "WP 1" [emptyProg, statements, issues]

run :: Text -> Either Error ([PO], [Spec])
run text = LSP.runM $ do 
  (pos, specs) <- LSP.parseProgram "<test>" text >>= LSP.sweep
  return (map toNoLoc pos, map toNoLoc specs)            

--------------------------------------------------------------------------------

-- |
emptyProg :: TestTree
emptyProg =
  testGroup
    "empty"
    [ testCase "empty" $ do
        let actual = run ""
        actual @?= Right ([], [])
    ]

--------------------------------------------------------------------------------

-- | Expression
statements :: TestTree
statements =
  testGroup
    "simple program"
    [ testCase "skip" $ do
        let actual = run
              "{ True }   \n\
              \skip       \n\
              \{ 0 = 0 }"
        actual
          @?= Right
            ( [ PO
                  0
                  (assertion true)
                  (assertion (number 0 `eqq` number 0))
                  (AtSkip NoLoc)
              ],
              []
            ),
      testCase "abort" $ do
        let actual = run
              "{ True }   \n\
              \abort      \n\
              \{ True }"
        actual
          @?= Right ([PO 0 (assertion true) (Constant false) (AtAbort NoLoc)], []),
      testCase "assignment" $ do
        let actual = run
              "{ True }   \n\
              \x := 1     \n\
              \{ 0 = x }"
        actual
          @?= Right
            ( [ PO
                  0
                  (assertion true)
                  (assertion (number 0 `eqq` number 1))
                  (AtAssignment NoLoc)
              ],
              []
            ),
      testCase "spec" $ do
        let actual = run
              "{ True }   \n\
              \{!       \n\
              \!}       \n\
              \{ 0 = 0 }"
        actual
          @?= Right
            ( [],
              [ Specification
                  0
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
        let actual = run
              "con A, B : Int\n\
              \var x, y, z : Int\n\
              \{ z = A * B }"
        actual
          @?= Right
            ( [PO 0 (Constant true) (assertion (variable "z" `eqq` binary Mul (constant "A") (constant "B"))) (AtAssertion NoLoc)],
              []
            ),
      testCase "Postcondition + precondition" $ do
        let actual = run 
              "con A, B : Int\n\
              \var x, y, z : Int\n\
              \{ True }\n\
              \{ z = A * B }"
        actual
          @?= Right
            ( [PO 0 (assertion true) (assertion (variable "z" `eqq` binary Mul (constant "A") (constant "B"))) (AtAssertion NoLoc)],
              []
            )
    ]
