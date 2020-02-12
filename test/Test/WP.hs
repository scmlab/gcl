{-# LANGUAGE OverloadedStrings #-}

module Test.WP where

import Data.Text.Lazy
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (Ordering(..))

import Syntax.Predicate
import Syntax.Concrete
import qualified REPL as REPL
import GCL.WP (Obligation(..), Specification(..))
-- import Syntax.Location
import Data.Loc
import Error
import Pretty ()

tests :: TestTree
tests = testGroup "Weakest Precondition"
  [ simple
  ]

--------------------------------------------------------------------------------
-- | Expression

simple :: TestTree
simple = testGroup "simple program"
  [ testCase "skip" $ run "skip" @?= Right
      (
        Assertion
          (Var (Lower "false" (pos 2 3 7 <--> pos 2 7 11)) (pos 2 3 7 <--> pos 2 7 11))
          (pos 2 1 5 <--> pos 2 9 13)
      , []
      , []
      )
  , testCase "abort" $ run "abort" @?= Right
      ( Constant
          (Lit (Bol False) NoLoc)
      , []
      , []
      )
  , testCase "assertion" $ run "{ True }" @?= Right
      ( Assertion
          (Lit (Bol True) (pos 1 3 2 <--> pos 1 6 5))
          (pos 1 1 0 <--> pos 1 8 7)
      , []
      , []
      )
  , testCase "assignment" $ run "x := 1" @?= Right
      ( Assertion
          (Var (Lower "false" (pos 2 3 9 <--> pos 2 7 13)) (pos 2 3 9 <--> pos 2 7 13))
          (pos 2 1 7 <--> pos 2 9 15)
      , []
      , []
      )
  , testCase "spec" $ run "{!\n!}\n" @?= Right
      ( Assertion
          (Var (Lower "false" (pos 4 3 9 <--> pos 4 7 13)) (pos 4 3 9 <--> pos 4 7 13))
          (pos 4 1 7 <--> pos 4 9 15)
      , []
      , [ Specification
            0
            (Assertion
              (Var (Lower "false" (pos 4 3 9 <--> pos 4 7 13)) (pos 4 3 9 <--> pos 4 7 13))
              (pos 4 1 7 <--> pos 4 9 15))
            (Assertion
              (Var (Lower "false" (pos 4 3 9 <--> pos 4 7 13)) (pos 4 3 9 <--> pos 4 7 13))
              (pos 4 1 7 <--> pos 4 9 15))
            (pos 1 1 0 <--> pos 2 2 4)
        ]
      )
  ]
  where
    run :: Text -> Either [Error] (Pred, [Obligation], [Specification])
    run text = REPL.scan "<test>" (text <> "\n{ false }")
                >>= REPL.parseProgram "<test>"
                >>= REPL.sweep

(<->) :: Int -> Int -> Loc
(<->) from to = Loc (Pos "<test>" 1 from (from - 1)) (Pos "<test>" 1 to (to - 1))

at :: Int -> Loc
at n = n <-> n

pos :: Int -> Int -> Int -> Pos
pos = Pos "<test>"
