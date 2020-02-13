{-# LANGUAGE OverloadedStrings #-}

module Test.WP where

import Data.Text.Lazy hiding (map)
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (Ordering(..))

import Syntax.Predicate
import Syntax.Concrete
import qualified REPL as REPL
import GCL.WP (Obligation(..), Specification(..))
import Syntax.Location
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
      ( assertFalse
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
      ( assertTrue
      , []
      , []
      )
  , testCase "assignment" $ run "x := 1" @?= Right
      ( assertFalse
      , []
      , []
      )
  , testCase "spec" $ run "{!\n!}" @?= Right
      ( assertFalse
      , []
      , [ Specification
            0
            assertFalse
            assertFalse
            NoLoc
        ]
      )
  ]
  where
    run :: Text -> Either [Error] (Pred, [Obligation], [Specification])
    run text = fmap (\(x, y, z) -> (toNoLoc x, map toNoLoc y, map toNoLoc z))
                $ REPL.scan "<test>" (text <> "\n{ False }")
                  >>= REPL.parseProgram "<test>"
                  >>= REPL.sweep

assertTrue :: Pred
assertTrue = Assertion (Lit (Bol True) NoLoc) NoLoc

assertFalse :: Pred
assertFalse = Assertion (Lit (Bol False) NoLoc) NoLoc
