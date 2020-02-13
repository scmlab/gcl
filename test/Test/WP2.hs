{-# LANGUAGE OverloadedStrings #-}

module Test.WP2 where

import Data.Text.Lazy
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (Ordering(..))

import Syntax.Predicate
import Syntax.Concrete
import qualified REPL as REPL
import GCL.WP2 (Obligation2(..), ObliOrigin2(..))
-- import GCL.WP2 (Obligation2(..), Specification2(..), ObliOrigin2(..))
-- import Syntax.Location
import Data.Loc
import Error
import Pretty ()

tests :: TestTree
tests = testGroup "Weakest Precondition 2"
  [ statements
  , assertions
  -- , if'
  ]

--------------------------------------------------------------------------------
-- | Expression

statements :: TestTree
statements = testGroup "simple statements"
  [ testCase "skip" $ run' "skip" @?= Right []
  , testCase "abort" $ run' "abort" @?= Right []
  , testCase "assertion" $ run' "{ True }" @?= Right
      [ Obligation 0
          (assertTrue 0 0)
          (assertFalse 1 8)
          (AssertSufficient (pos 1 1 0 <--> pos 1 8 7))
      ]
  , testCase "assignment" $ run' "x := 1" @?= Right
      []
  , testCase "spec" $ run' "{!\n!}" @?= Right
      []
  ]
  where
    run' :: Text -> Either [Error] [Obligation2]
    run' text = REPL.scan "<test>" (text <> "\n{ False }")
                >>= REPL.parseProgram "<test>"
                >>= REPL.sweep2

assertions :: TestTree
assertions = testCase "assertions" $ run "{ True }\n{ False }\n{ True }\n" @?= Right
  [ Obligation 0
      (assertTrue 0 0)
      (assertFalse 1 8)
      (AssertSufficient (pos 1 1 0 <--> pos 1 8 7))
  , Obligation 1
      (assertFalse 1 8)
      (assertTrue 2 19)
      (AssertSufficient (pos 2 1 9 <--> pos 2 9 17))
  ]
--
-- if' :: TestTree
-- if' = testGroup "if statements"
--   [ testCase "without precondition" $ run "if False -> skip fi\n{ True }\n" @?= Right
--     [ Obligation 0
--         (assertTrue 0 0)
--         (assertTrue 0 0)
--         (AroundSkip (pos 1 13 12 <--> pos 1 16 15))
--
--     ]
--
--   ]


run :: Text -> Either [Error] [Obligation2]
run text = REPL.scan "<test>" text
            >>= REPL.parseProgram "<test>"
            >>= REPL.sweep2


(<->) :: Int -> Int -> Loc
(<->) from to = Loc (Pos "<test>" 1 from (from - 1)) (Pos "<test>" 1 to (to - 1))

at :: Int -> Loc
at n = n <-> n

pos :: Int -> Int -> Int -> Pos
pos = Pos "<test>"

assertTrue :: Int -> Int -> Pred
assertTrue rowBefore charBefore =
  Assertion
    (Lit (Bol True) (pos lineNo 3 (colNo + 1) <--> pos lineNo 6 (colNo + 4)))
    (pos lineNo 1 (colNo - 1) <--> pos lineNo 8 (colNo + 6))
  where
    lineNo = rowBefore + 1
    colNo = charBefore + 1

assertFalse :: Int -> Int -> Pred
assertFalse rowBefore charBefore =
  Assertion
    (Lit (Bol False) (pos lineNo 3 (colNo + 2) <--> pos lineNo 7 (colNo + 6)))
    (pos lineNo 1 colNo <--> pos lineNo 9 (colNo + 8))
  where
    lineNo = rowBefore + 1
    colNo = charBefore + 1
