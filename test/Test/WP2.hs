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
  , if'
  ]

--------------------------------------------------------------------------------
-- | Expression

statements :: TestTree
statements = testGroup "simple statements"
  [ testCase "skip" $ run "skip\n{ False }" @?= Right []
  , testCase "abort" $ run "abort\n{ False }" @?= Right []
  , testCase "assertion" $ run "{ True }\n{ False }" @?= Right
      [ Obligation 0
          (Assertion true NoLoc)
          (Assertion false NoLoc)
          (AssertSufficient NoLoc)
      ]
  , testCase "assignment" $ run "x := 1\n{ False }" @?= Right
      []
  , testCase "spec" $ run "{!\n!}\n{ False }" @?= Right
      []
  ]

assertions :: TestTree
assertions = testCase "assertions" $ run "{ True }\n{ False }\n{ True }\n" @?= Right
  [ Obligation 0
      (Assertion true NoLoc)
      (Assertion false NoLoc)
      (AssertSufficient NoLoc)
  , Obligation 1
      (Assertion false NoLoc)
      (Assertion true NoLoc)
      (AssertSufficient NoLoc)
  ]

if' :: TestTree
if' = testGroup "if statements"
  [ testCase "without precondition" $ run "if False -> skip fi\n{ True }\n" @?= Right
    [ Obligation 0
        (Conjunct
          [ Assertion true NoLoc
          , Disjunct [ Guard false (IF NoLoc) NoLoc ]
          , Guard false (IF NoLoc) NoLoc
          ])
        (Assertion true NoLoc)
        (AroundSkip NoLoc)
    ]
  ]

run :: Text -> Either [Error] [Obligation2]
run text = fmap (map toNoLoc) $ REPL.scan "<test>" text
            >>= REPL.parseProgram "<test>"
            >>= REPL.sweep2
