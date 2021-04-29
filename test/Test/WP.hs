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
import qualified Server
import qualified Server.CustomMethod as Server
import Pretty ()
import Syntax.Abstract
import Syntax.Common
import Syntax.Predicate
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (Ordering (..))

tests :: TestTree
tests = testGroup "WP" [emptyProg, statements, issues]

run :: Text -> Either Server.Error2 ([PO], [Spec], [StructWarning])
run text = Server.runM $ do
  (pos, specs, warnings) <- Server.parseProgram "<test>" text >>= Server.genPO
  return (pos, specs, warnings)

--------------------------------------------------------------------------------

-- |
emptyProg :: TestTree
emptyProg =
  testGroup
    "empty"
    [ testCase "empty" $ do
        let actual = run ""
        actual @?= Right ([], [], [])
    ]

--------------------------------------------------------------------------------

-- | Expression
statements :: TestTree
statements =
  testGroup
    "simple program"
    [ testCase "skip" $ do
        let actual =
              run
                "{ True }   \n\
                \skip       \n\
                \{ 0 = 0 }"
        actual
          @?= Right
            ( [ PO
                  0
                  (Assertion (Lit (Bol True) (Loc (pos 1 3 2) (pos 1 6 5))) (Loc (pos 1 1 0) (pos 1 8 7)))
                  ( Assertion
                      ( App
                          ( App
                              (Op EQ (Loc (pos 3 5 28) (pos 3 5 28)))
                              (Lit (Num 0) (Loc (pos 3 3 26) (pos 3 3 26)))
                              (Loc (pos 3 3 26) (pos 3 5 28))
                          )
                          (Lit (Num 0) (Loc (pos 3 7 30) (pos 3 7 30)))
                          (Loc (pos 3 3 26) (pos 3 7 30))
                      )
                      (Loc (pos 3 1 24) (pos 3 9 32))
                  )
                  (AtSkip (Loc (pos 2 1 12) (pos 2 4 15)))
              ],
              [],
              []
            ),
      testCase "abort" $
        do
          let actual =
                run
                  "{ True }   \n\
                  \abort      \n\
                  \{ True }"
          actual
            @?= Right
              ( [ PO
                    0
                    (Assertion (Lit (Bol True) (Loc (pos 1 3 2) (pos 1 6 5))) (Loc (pos 1 1 0) (pos 1 8 7)))
                    (Constant (Lit (Bol False) NoLoc))
                    (AtAbort (Loc (pos 2 1 12) (pos 2 5 16)))
                ],
                [],
                []
              ),
      testCase
        "assignment"
        $ do
          let actual =
                run
                  "{ True }   \n\
                  \x := 1     \n\
                  \{ 0 = x }"
          actual
            @?= Right
              ( [ PO
                    0
                    (Assertion (Lit (Bol True) (Loc (pos 1 3 2) (pos 1 6 5))) (Loc (pos 1 1 0) (pos 1 8 7)))
                    ( Assertion
                        ( App
                            ( App
                                (Op EQ (Loc (pos 3 5 28) (pos 3 5 28)))
                                (Lit (Num 0) (Loc (pos 3 3 26) (pos 3 3 26)))
                                (Loc (pos 3 3 26) (pos 3 5 28))
                            )
                            (Lit (Num 1) (Loc (pos 2 6 17) (pos 2 6 17)))
                            (Loc (pos 3 3 26) (pos 3 7 30))
                        )
                        (Loc (pos 3 1 24) (pos 3 9 32))
                    )
                    (AtAssignment (Loc (pos 2 1 12) (pos 2 6 17)))
                ],
                [],
                []
              ),
      testCase "spec" $ do
        let actual =
              run
                "{ True }   \n\
                \[!       \n\
                \!]       \n\
                \{ 0 = 0 }"
        actual
          @?= Right
            ( [],
              [ Specification
                  0
                  (Assertion (Lit (Bol True) (Loc (Pos "<test>" 1 3 2) (Pos "<test>" 1 6 5))) (Loc (Pos "<test>" 1 1 0) (Pos "<test>" 1 8 7)))
                  (Assertion (App (App (Op EQ (Loc (Pos "<test>" 4 5 36) (Pos "<test>" 4 5 36))) (Lit (Num 0) (Loc (Pos "<test>" 4 3 34) (Pos "<test>" 4 3 34))) (Loc (Pos "<test>" 4 3 34) (Pos "<test>" 4 5 36))) (Lit (Num 0) (Loc (Pos "<test>" 4 7 38) (Pos "<test>" 4 7 38))) (Loc (Pos "<test>" 4 3 34) (Pos "<test>" 4 7 38))) (Loc (Pos "<test>" 4 1 32) (Pos "<test>" 4 9 40)))
                  (Loc (Pos "<test>" 2 1 12) (Pos "<test>" 3 2 23))
              ],
              []
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
        let actual =
              run
                "con A, B : Int\n\
                \var x, y, z : Int\n\
                \{ z = A * B }"
        actual
          @?= Right
            ( [ PO
                  0
                  (Constant (Lit (Bol True) NoLoc))
                  ( Assertion
                      ( App
                          ( App
                              (Op EQ (Loc (pos 3 5 37) (pos 3 5 37)))
                              (Var (Name "z" (Loc (pos 3 3 35) (pos 3 3 35))) (Loc (pos 3 3 35) (pos 3 3 35)))
                              (Loc (pos 3 3 35) (pos 3 5 37))
                          )
                          ( App
                              ( App
                                  (Op Mul (Loc (pos 3 9 41) (pos 3 9 41)))
                                  (Const (Name "A" (Loc (pos 3 7 39) (pos 3 7 39))) (Loc (pos 3 7 39) (pos 3 7 39)))
                                  (Loc (pos 3 7 39) (pos 3 9 41))
                              )
                              (Const (Name "B" (Loc (pos 3 11 43) (pos 3 11 43))) (Loc (pos 3 11 43) (pos 3 11 43)))
                              (Loc (pos 3 7 39) (pos 3 11 43))
                          )
                          (Loc (pos 3 3 35) (pos 3 11 43))
                      )
                      (Loc (pos 3 1 33) (pos 3 13 45))
                  )
                  (AtAssertion (Loc (pos 3 1 33) (pos 3 13 45)))
              ],
              [],
              []
            ),
      testCase "Postcondition + precondition" $ do
        let actual =
              run
                "con A, B : Int\n\
                \var x, y, z : Int\n\
                \{ True }\n\
                \{ z = A * B }"
        actual
          @?= Right
            ( [ PO
                  0
                  (Assertion (Lit (Bol True) (Loc (pos 3 3 35) (pos 3 6 38))) (Loc (pos 3 1 33) (pos 3 8 40)))
                  ( Assertion
                      ( App
                          ( App
                              (Op EQ (Loc (pos 4 5 46) (pos 4 5 46)))
                              (Var (Name "z" (Loc (pos 4 3 44) (pos 4 3 44))) (Loc (pos 4 3 44) (pos 4 3 44)))
                              (Loc (pos 4 3 44) (pos 4 5 46))
                          )
                          ( App
                              ( App
                                  (Op Mul (Loc (pos 4 9 50) (pos 4 9 50)))
                                  (Const (Name "A" (Loc (pos 4 7 48) (pos 4 7 48))) (Loc (pos 4 7 48) (pos 4 7 48)))
                                  (Loc (pos 4 7 48) (pos 4 9 50))
                              )
                              (Const (Name "B" (Loc (pos 4 11 52) (pos 4 11 52))) (Loc (pos 4 11 52) (pos 4 11 52)))
                              (Loc (pos 4 7 48) (pos 4 11 52))
                          )
                          (Loc (pos 4 3 44) (pos 4 11 52))
                      )
                      (Loc (pos 4 1 42) (pos 4 13 54))
                  )
                  (AtAssertion (Loc (pos 3 1 33) (pos 3 8 40)))
              ],
              [],
              []
            )
    ]
