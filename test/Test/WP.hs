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
      ( assertFalse 1 4
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
      ( assertFalse 1 6
      , []
      , []
      )
  , testCase "spec" $ run "{!\n!}" @?= Right
      ( assertFalse 2 5
      , []
      , [ Specification
            0
            (assertFalse 2 5)
            (assertFalse 2 5)
            (pos 1 1 0 <--> pos 2 2 4)
        ]
      )
  ]
  where
    -- assertTrue :: Int -> Int -> Pred
    -- assertTrue rowBefore charBefore =
    --   Assertion
    --     (Lit (Bol False) (pos lineNo 3 (colNo + 2) <--> pos lineNo 6 (colNo + 5)))
    --     (pos lineNo 1 colNo <--> pos lineNo 8 (colNo + 7))
    --   where
    --     lineNo = rowBefore + 1
    --     colNo = charBefore + 1

    assertFalse :: Int -> Int -> Pred
    assertFalse rowBefore charBefore =
      Assertion
        (Lit (Bol False) (pos lineNo 3 (colNo + 2) <--> pos lineNo 7 (colNo + 6)))
        (pos lineNo 1 colNo <--> pos lineNo 9 (colNo + 8))
      where
        lineNo = rowBefore + 1
        colNo = charBefore + 1

    run :: Text -> Either [Error] (Pred, [Obligation], [Specification])
    run text = REPL.scan "<test>" (text <> "\n{ False }")
                >>= REPL.parseProgram "<test>"
                >>= REPL.sweep

(<->) :: Int -> Int -> Loc
(<->) from to = Loc (Pos "<test>" 1 from (from - 1)) (Pos "<test>" 1 to (to - 1))

at :: Int -> Loc
at n = n <-> n

pos :: Int -> Int -> Int -> Pos
pos = Pos "<test>"
