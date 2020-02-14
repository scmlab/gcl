{-# LANGUAGE OverloadedStrings #-}

module Test.WP2 where

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
tests = testGroup "Weakest Precondition 2"
  [
  --   statements
  -- , assertions
  -- ,
  -- if'
  loop
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
  [ testCase "without precondition" $ run "if 0 = 0 -> skip fi\n{ 0 = 2 }\n" @?= Right
    [ Obligation 0
        (Conjunct
          [ Assertion (makePred 2) NoLoc
          , Guard (makePred 0) (IF NoLoc) NoLoc
          ])
        (Assertion (makePred 2) NoLoc)
        (AroundSkip NoLoc)
    ]
  , testCase "without precondition 2" $ run "if 0 = 0 -> skip | 0 = 1 -> abort fi\n{ 0 = 2 }\n" @?= Right
    [ Obligation 0
        (Conjunct
          [ Assertion (makePred 2) NoLoc
          , Disjunct
              [ Guard (makePred 0) (IF NoLoc) NoLoc
              , Guard (makePred 1) (IF NoLoc) NoLoc
              ]
          ])
        (Assertion (makePred 2) NoLoc)
        (AroundSkip NoLoc)
    , Obligation 1
        (Conjunct
          [ Constant false
          , Disjunct
              [ Guard (makePred 0) (IF NoLoc) NoLoc
              , Guard (makePred 1) (IF NoLoc) NoLoc
              ]
          ])
        (Constant false)
        (AroundAbort NoLoc)
    ]
  , testCase "with precondition" $ run "{ 0 = 0 }\nif 0 = 1 -> skip fi\n{ 0 = 2 }\n" @?= Right
    [ Obligation 0
        (Assertion (makePred 0) NoLoc)
        (Guard (makePred 1) (IF NoLoc) NoLoc)
        (AssertSufficient NoLoc)
    , Obligation 1
        (Conjunct
          [ Assertion (makePred 2) NoLoc
          , Guard (makePred 1) (IF NoLoc) NoLoc
          ])
        (Assertion (makePred 2) NoLoc)
        (AroundSkip NoLoc)
    ]
  ]

loop :: TestTree
loop = testGroup "if statements"
  [ testCase "loop" $ run "{ 0 = 1 , bnd: A }\ndo 0 = 2 -> skip od\n{ 0 = 0 }\n" @?= Right
    [ Obligation 0
        (Conjunct
          [ LoopInvariant (makePred 1) NoLoc
          , Negate (Guard (makePred 2) (LOOP NoLoc) NoLoc)
          ])
        (Assertion (makePred 0) NoLoc)
        (LoopBase NoLoc)
    , Obligation 1
        (Conjunct
          [ LoopInvariant (makePred 1) NoLoc
          , LoopInvariant (makePred 1) NoLoc
          , Guard (makePred 2) (LOOP NoLoc) NoLoc
          ])
        (LoopInvariant (makePred 1) NoLoc)
        (AroundSkip NoLoc)
    , Obligation 2
        (Conjunct
          [ LoopInvariant (makePred 1) NoLoc
          , Guard (makePred 2) (LOOP NoLoc) NoLoc
          ])
        (Bound (constant "A" `gte` number 0))
        (LoopTermBase NoLoc)
    , Obligation 4
        (Conjunct
          [ Bound (constant "A" `lt` variable "_bnd3")
          , LoopInvariant (makePred 1) NoLoc
          , Guard (makePred 2) (LOOP NoLoc) NoLoc
          , Bound (constant "A" `eqq` variable "_bnd3")
          ])
        (Bound (constant "A" `lt` variable "_bnd3"))
        (AroundSkip NoLoc)
    ]
  ]


run :: Text -> Either [Error] [Obligation2]
run text = fmap (map toNoLoc) $ REPL.scan "<test>" text
            >>= REPL.parseProgram "<test>"
            >>= REPL.sweep2

makePred :: Int -> Expr
makePred n = App
                (App (Op EQ NoLoc) (Lit (Num 0) NoLoc) NoLoc)
                (Lit (Num n) NoLoc)
                NoLoc
