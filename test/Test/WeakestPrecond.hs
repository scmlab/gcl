{-# LANGUAGE OverloadedStrings #-}

module Test.WeakestPrecond where

import Data.Text.Lazy hiding (map)
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (Ordering(..))

import Syntax.Predicate
import Syntax.Concrete hiding (LoopInvariant)
import qualified REPL as REPL
import GCL.WP2 (Obligation2(..), ObliOrigin2(..))
-- import GCL.WP2 (Obligation2(..), Specification2(..), ObliOrigin2(..))
import Syntax.Location
import Data.Loc
import Error
import Pretty ()

tests :: TestTree
tests = testGroup "Weakest Precondition"
  [ statements
  -- , assertions
  , if'
  -- , loop
  ]

makePred :: Int -> Expr
makePred n = App
                (App (Op EQ NoLoc) (Lit (Num 0) NoLoc) NoLoc)
                (Lit (Num n) NoLoc)
                NoLoc

statements :: TestTree
statements = testGroup "simple statements"
  [ testCase "skip" $ run "skip\n{ 0 = 0 }" @?=
      Right (Assertion (makePred 0) NoLoc)
  , testCase "abort" $ run "abort\n{ False }" @?=
      Right (Constant false)
  , testCase "assertion" $ run "{ 0 = 0 }\n{ 0 = 1 }" @?=
      Right (Assertion (makePred 0) NoLoc)
  , testCase "assignment" $ run "x := 1\n{ 0 = x }" @?=
      Right (Assertion (makePred 1) NoLoc)
  , testCase "spec" $ run "{!\n!}\n{ False }" @?=
      Right (Assertion false NoLoc)
  ]

if' :: TestTree
if' = testGroup "if statements"
  [ testCase "without precondition 1" $ run "if 0 = 0 -> skip fi\n{ 0 = 2 }\n" @?=
      Right (Guard (makePred 0) (IF NoLoc) NoLoc)
  , testCase "without precondition 2" $ run "if 0 = 0 -> skip | 0 = 1 -> abort fi\n{ 0 = 2 }\n" @?=
      Right (Disjunct
              [ Guard (makePred 0) (IF NoLoc) NoLoc
              , Guard (makePred 1) (IF NoLoc) NoLoc
              ])
  , testCase "with precondition" $ run "{ 0 = 0 }\nif 0 = 1 -> skip fi\n{ 0 = 2 }\n" @?=
      Right (Assertion (makePred 0) NoLoc)

  , testCase "nested" $ run "if 0 = 0 -> if 0 = 1 -> skip fi fi\n{ 0 = 2 }\n" @?=
      Right (Guard (makePred 0) (IF NoLoc) NoLoc)
  ]



--
-- loop :: TestTree
-- loop = testGroup "if statements"
--   [ testCase "loop" $ run "{ 0 = 1 , bnd: A }\ndo 0 = 2 -> skip od\n{ 0 = 0 }\n" @?= Right
--     [ Obligation 0
--         (Conjunct
--           [ LoopInvariant (makePred 1) NoLoc
--           , Negate (Guard (makePred 2) (LOOP NoLoc) NoLoc)
--           ])
--         (Assertion (makePred 0) NoLoc)
--         (LoopBase NoLoc)
--     , Obligation 1
--         (Conjunct
--           [ LoopInvariant (makePred 1) NoLoc
--           , Guard (makePred 2) (LOOP NoLoc) NoLoc
--           ])
--         (LoopInvariant (makePred 1) NoLoc)
--         (AroundSkip NoLoc)
--     , Obligation 2
--         (Conjunct
--           [ LoopInvariant (makePred 1) NoLoc
--           , Guard (makePred 2) (LOOP NoLoc) NoLoc
--           ])
--         (Bound (constant "A" `gte` number 0))
--         (LoopTermBase NoLoc)
--     , Obligation 4
--         (Conjunct
--           [ Bound (constant "A" `lt` variable "_bnd3")
--           , LoopInvariant (makePred 1) NoLoc
--           , Guard (makePred 2) (LOOP NoLoc) NoLoc
--           , Bound (constant "A" `eqq` variable "_bnd3")
--           ])
--         (Bound (constant "A" `lt` variable "_bnd3"))
--         (AroundSkip NoLoc)
--     ]
--   ]


run :: Text -> Either [Error] Pred
run text = fmap toNoLoc $ REPL.scan "<test>" text
            >>= REPL.parseProgram "<test>"
            >>= REPL.precond2
