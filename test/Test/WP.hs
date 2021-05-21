{-# LANGUAGE OverloadedStrings #-}

module Test.WP where

import Data.Loc
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Text.Lazy (fromStrict, toStrict)
import qualified Data.Text.Lazy.Encoding as LazyText
import Error
import GCL.WP (StructWarning)
import Pretty ()
import qualified Server
import Server.DSL (parseProgram, sweep)
import Server.Interpreter.Test
import Syntax.Abstract
import Syntax.Common
import Syntax.Predicate
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (Ordering (..))

tests :: TestTree
tests = testGroup "WP" [emptyProg, statements, issues]

type Result = ((Maybe ([PO], [Spec], [Expr], [StructWarning]), Text), [CmdKind])

run :: Text -> Result
run text = runTest "<test>" text $ parseProgram text >>= sweep

fromPOs :: Text -> [PO] -> Result
fromPOs source pos = ((Just (pos, [], [], []), source), [CmdGetFilePath])

fromSpecs :: Text -> [Spec] -> Result
fromSpecs source specs = ((Just ([], specs, [], []), source), [CmdGetFilePath])

--------------------------------------------------------------------------------

-- |
emptyProg :: TestTree
emptyProg =
  testGroup
    "empty"
    [ testCase "empty" $ do
        let actual = run ""
        actual @?= fromPOs "" []
    ]

--------------------------------------------------------------------------------

-- | Expression
statements :: TestTree
statements =
  testGroup
    "simple program"
    [ testCase "skip" $ do
        let source = 
                "{ True }   \n\
                \skip       \n\
                \{ 0 = 0 }"
        let actual = run source
        actual
          @?= fromPOs source
            [ PO
                0
                (Assertion (Lit (Bol True) (Loc (pos 1 3 2) (pos 1 7 6))) (Loc (pos 1 1 0) (pos 1 9 8)))
                ( Assertion
                    ( Chain
                        (Lit (Num 0) (Loc (pos 3 3 26) (pos 3 4 27)))
                        (EQ (Loc (pos 3 5 28) (pos 3 6 29)))
                        (Lit (Num 0) (Loc (pos 3 7 30) (pos 3 8 31)))
                        (Loc (pos 3 3 26) (pos 3 8 31))
                    )
                    (Loc (pos 3 1 24) (pos 3 10 33))
                )
                (AtSkip (Loc (pos 2 1 12) (pos 2 5 16)))
            ],
      testCase "abort" $
        do
          let source =
                  "{ True }   \n\
                  \abort      \n\
                  \{ True }"
          let actual = run source
          actual
            @?= fromPOs source
              [ PO
                  0
                  (Assertion (Lit (Bol True) (Loc (pos 1 3 2) (pos 1 7 6))) (Loc (pos 1 1 0) (pos 1 9 8)))
                  (Constant (Lit (Bol False) NoLoc))
                  (AtAbort (Loc (pos 2 1 12) (pos 2 6 17)))
              ],
      testCase
        "assignment"
        $ do
          let source =
                  "{ True }   \n\
                  \x := 1     \n\
                  \{ 0 = x }"
          let actual = run source
          actual
            @?= fromPOs source
              [ PO
                  0
                  (Assertion (Lit (Bol True) (Loc (pos 1 3 2) (pos 1 7 6))) (Loc (pos 1 1 0) (pos 1 9 8)))
                  ( Assertion
                      ( Chain
                          (Lit (Num 0) (Loc (pos 3 3 26) (pos 3 4 27)))
                          (EQ (Loc (pos 3 5 28) (pos 3 6 29)))
                          (Lit (Num 1) (Loc (pos 2 6 17) (pos 2 7 18)))
                          (Loc (pos 3 3 26) (pos 3 8 31))
                      )
                      (Loc (pos 3 1 24) (pos 3 10 33))
                  )
                  (AtAssignment (Loc (pos 2 1 12) (pos 2 7 18)))
              ],
      testCase "spec" $ do
        let source =
                "{ True }   \n\
                \[!       \n\
                \!]       \n\
                \{ 0 = 0 }"
        let actual = run source
        actual
          @?= fromSpecs source
            [ Specification
                0
                (Assertion (Lit (Bol True) (Loc (Pos "<test>" 1 3 2) (Pos "<test>" 1 7 6))) (Loc (Pos "<test>" 1 1 0) (Pos "<test>" 1 9 8)))
                ( Assertion
                    ( Chain
                        (Lit (Num 0) (Loc (Pos "<test>" 4 3 34) (Pos "<test>" 4 4 35)))
                        (EQ (Loc (Pos "<test>" 4 5 36) (Pos "<test>" 4 6 37)))
                        (Lit (Num 0) (Loc (Pos "<test>" 4 7 38) (Pos "<test>" 4 8 39)))
                        (Loc (Pos "<test>" 4 3 34) (Pos "<test>" 4 8 39))
                    )
                    (Loc (Pos "<test>" 4 1 32) (Pos "<test>" 4 10 41))
                )
                (Loc (Pos "<test>" 2 1 12) (Pos "<test>" 3 3 24))
            ]
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
        let source =
                "con A, B : Int\n\
                \var x, y, z : Int\n\
                \{ z = A * B }"
        let actual = run source
        actual
          @?= fromPOs source
            [ PO
                0
                (Constant (Lit (Bol True) NoLoc))
                ( Assertion
                    ( Chain
                        (Var (Name "z" (Loc (pos 3 3 35) (pos 3 4 36))) (Loc (pos 3 3 35) (pos 3 4 36)))
                        (EQ (Loc (pos 3 5 37) (pos 3 6 38)))
                        ( App
                            ( App
                                (Op (Mul (Loc (pos 3 9 41) (pos 3 10 42))))
                                (Const (Name "A" (Loc (pos 3 7 39) (pos 3 8 40))) (Loc (pos 3 7 39) (pos 3 8 40)))
                                (Loc (pos 3 7 39) (pos 3 10 42))
                            )
                            (Const (Name "B" (Loc (pos 3 11 43) (pos 3 12 44))) (Loc (pos 3 11 43) (pos 3 12 44)))
                            (Loc (pos 3 7 39) (pos 3 12 44))
                        )
                        (Loc (pos 3 3 35) (pos 3 12 44))
                    )
                    (Loc (pos 3 1 33) (pos 3 14 46))
                )
                (AtAssertion (Loc (pos 3 1 33) (pos 3 14 46)))
            ],
      testCase "Postcondition + precondition" $ do
        let source =
                "con A, B : Int\n\
                \var x, y, z : Int\n\
                \{ True }\n\
                \{ z = A * B }"
        let actual = run source
        actual
          @?= fromPOs source
            [ PO
                0
                (Assertion (Lit (Bol True) (Loc (pos 3 3 35) (pos 3 7 39))) (Loc (pos 3 1 33) (pos 3 9 41)))
                ( Assertion
                    ( Chain
                        (Var (Name "z" (Loc (pos 4 3 44) (pos 4 4 45))) (Loc (pos 4 3 44) (pos 4 4 45)))
                        (EQ (Loc (pos 4 5 46) (pos 4 6 47)))
                        ( App
                            ( App
                                (Op (Mul (Loc (pos 4 9 50) (pos 4 10 51))))
                                (Const (Name "A" (Loc (pos 4 7 48) (pos 4 8 49))) (Loc (pos 4 7 48) (pos 4 8 49)))
                                (Loc (pos 4 7 48) (pos 4 10 51))
                            )
                            (Const (Name "B" (Loc (pos 4 11 52) (pos 4 12 53))) (Loc (pos 4 11 52) (pos 4 12 53)))
                            (Loc (pos 4 7 48) (pos 4 12 53))
                        )
                        (Loc (pos 4 3 44) (pos 4 12 53))
                    )
                    (Loc (pos 4 1 42) (pos 4 14 55))
                )
                (AtAssertion (Loc (pos 3 1 33) (pos 3 9 41)))
            ]
    ]
