{-# LANGUAGE OverloadedStrings #-}

module Test.WeakestPrecond where

import Data.Text.Lazy hiding (map)
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (Ordering(..))

import Syntax.Predicate
import Syntax.Concrete hiding (LoopInvariant)
import qualified REPL as REPL
import GCL.WP2 (Obligation2(..), ObliOrigin2(..), WPTree, WPNode(..))
-- import GCL.WP2 (Obligation2(..), Specification2(..), ObliOrigin2(..))
import Syntax.Location
import Data.Loc
import Error
import Pretty ()

tests :: TestTree
tests = testGroup "Weakest Precondition"
  [
    statements
  -- , assertions
  -- , if'
  -- , loop
  ]

statements :: TestTree
statements = testGroup "simple statements"
  [ testCase "skip" $ run "skip\n{ 0 = 0 }" @?= Right
      [ Leaf (Assertion (number 0 `eqq` number 0) NoLoc)
      , Leaf (Assertion (number 0 `eqq` number 0) NoLoc)
      ]
  , testCase "abort" $ run "abort\n{ True }" @?= Right
      [ Leaf (Constant false)
      , Leaf (Assertion true NoLoc)
      ]
  , testCase "assertion" $ run "{ 0 = 0 }\n{ 0 = 1 }" @?= Right
      [ Leaf (Assertion (number 0 `eqq` number 0) NoLoc)
      , Leaf (Assertion (number 0 `eqq` number 1) NoLoc)
      ]
  , testCase "assignment" $ run "x := 1\n{ 0 = x }" @?= Right
      [ Leaf (Assertion (number 0 `eqq` number 1) NoLoc)
      , Leaf (Assertion (number 0 `eqq` variable "x") NoLoc)
      ]
  , testCase "spec" $ run "{!\n!}\n{ False }" @?= Right
      [ Leaf (Assertion false NoLoc)
      , Leaf (Assertion false NoLoc)
      ]
  ]

-- if' :: TestTree
-- if' = testGroup "if statements"
--   [ testCase "without precondition 1" $ run "if 0 = 0 -> skip fi\n{ 0 = 2 }\n" @?=
--       Right (Guard (number 0 `eqq` number 0) (IF NoLoc) NoLoc)
--   , testCase "without precondition 2" $ run "if 0 = 0 -> skip | 0 = 1 -> abort fi\n{ 0 = 2 }\n" @?=
--       Right (Disjunct
--               [ Guard (number 0 `eqq` number 0) (IF NoLoc) NoLoc
--               , Guard (number 0 `eqq` number 01) (IF NoLoc) NoLoc
--               ])
--   , testCase "with precondition" $ run "{ 0 = 0 }\nif 0 = 1 -> skip fi\n{ 0 = 2 }\n" @?=
--       Right (Assertion (number 0 `eqq` number 0) NoLoc)
--
--   , testCase "nested" $ run "if 0 = 0 -> if 0 = 1 -> skip fi fi\n{ 0 = 2 }\n" @?=
--       Right ( Guard (number 0 `eqq` number 0) (IF NoLoc) NoLoc)
--   ]



--
-- loop :: TestTree
-- loop = testGroup "if statements"
--   [ testCase "loop" $ run "{ 0 = 1 , bnd: A }\ndo 0 = 2 -> skip od\n{ 0 = 0 }\n" @?= Right
--     [ Obligation 0
--         (Conjunct
--           [ LoopInvariant (number 0 `eqq` number 01) NoLoc
--           , Negate (Guard (makePred 2) (LOOP NoLoc) NoLoc)
--           ])
--         (Assertion (number 0 `eqq` number 0) NoLoc)
--         (LoopBase NoLoc)
--     , Obligation 1
--         (Conjunct
--           [ LoopInvariant (number 0 `eqq` number 01) NoLoc
--           , Guard (makePred 2) (LOOP NoLoc) NoLoc
--           ])
--         (LoopInvariant (number 0 `eqq` number 01) NoLoc)
--         (AroundSkip NoLoc)
--     , Obligation 2
--         (Conjunct
--           [ LoopInvariant (number 0 `eqq` number 01) NoLoc
--           , Guard (makePred 2) (LOOP NoLoc) NoLoc
--           ])
--         (Bound (constant "A" `gte` number 0))
--         (LoopTermBase NoLoc)
--     , Obligation 4
--         (Conjunct
--           [ Bound (constant "A" `lt` variable "_bnd3")
--           , LoopInvariant (number 0 `eqq` number 01) NoLoc
--           , Guard (makePred 2) (LOOP NoLoc) NoLoc
--           , Bound (constant "A" `eqq` variable "_bnd3")
--           ])
--         (Bound (constant "A" `lt` variable "_bnd3"))
--         (AroundSkip NoLoc)
--     ]
--   ]


run :: Text -> Either [Error] WPTree
run text = fmap (map toNoLoc) $ REPL.scan "<test>" text
            >>= REPL.parseProgram "<test>"
            >>= REPL.wpTree
