{-# LANGUAGE OverloadedStrings #-}

module Test.WP2 where

import Data.Text.Lazy hiding (map)
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (Ordering(..))

import Syntax.Predicate
import Syntax.Concrete
import qualified REPL as REPL
import GCL.WP2 (Obligation2(..), ObliOrigin2(..))
-- import GCL.WP2 (Obligation2(..), Specification2(..), ObliOrigin2(..))
import Syntax.Location
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
          assertTrue
          assertFalse
          (AssertSufficient NoLoc)
      ]
  , testCase "assignment" $ run' "x := 1" @?= Right
      []
  , testCase "spec" $ run' "{!\n!}" @?= Right
      []
  ]
  where
    run' :: Text -> Either [Error] [Obligation2]
    run' text = fmap (map toNoLoc) $ REPL.scan "<test>" (text <> "\n{ False }")
                >>= REPL.parseProgram "<test>"
                >>= REPL.sweep2

assertions :: TestTree
assertions = testCase "assertions" $ run "{ True }\n{ False }\n{ True }\n" @?= Right
  [ Obligation 0
      assertTrue
      assertFalse
      (AssertSufficient NoLoc)
  , Obligation 1
      assertFalse
      assertTrue
      (AssertSufficient NoLoc)
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
run text = fmap (map toNoLoc) $ REPL.scan "<test>" text
            >>= REPL.parseProgram "<test>"
            >>= REPL.sweep2

assertTrue :: Pred
assertTrue = Assertion (Lit (Bol True) NoLoc) NoLoc

assertFalse :: Pred
assertFalse = Assertion (Lit (Bol False) NoLoc) NoLoc
