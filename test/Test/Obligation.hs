{-# LANGUAGE OverloadedStrings #-}

module Test.Obligation where

import Data.Text.Lazy hiding (map)
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (Ordering(..))

import Syntax.Predicate
import Syntax.Concrete hiding (LoopInvariant)
import qualified REPL as REPL
import GCL.WP2 (Obligation2(..), ObliOrigin2(..))
-- import GCL.WP2 (Obligation2(..), Specification2(..), ObliOrigin2(..))
import Data.Text.Prettyprint.Doc

import Syntax.Location
import Data.Loc
import Error
import Pretty ()

tests :: TestTree
tests = testGroup "Proof Obligations"
  [ statements
  , assertions
  , if'
  -- , loop
  ]

--------------------------------------------------------------------------------
-- | Expression

statements :: TestTree
statements = testGroup "simple statements"
  [ testCase "skip"
      $ run "skip\n{ False }" @?= poList []
  , testCase "abort"
      $ run "abort\n{ False }" @?= poList []
  , testCase "assertion"
      $ run "{ True }\n{ False }" @?= poList
      [ Obligation 0
          (assertion true)
          (assertion false)
          (AssertSufficient NoLoc)
      ]
  , testCase "assignment"
      $ run "x := 1\n{ False }" @?= poList []
  , testCase "spec"
      $ run "{!\n!}\n{ False }" @?= poList []
  ]

assertions :: TestTree
assertions = testCase "assertions"
  $ run "{ True }\n\
        \{ False }\n\
        \{ True }\n" @?= poList
  [ Obligation 0
      (assertion true)
      (assertion false)
      (AssertSufficient NoLoc)
  , Obligation 1
      (assertion false)
      (assertion true)
      (AssertSufficient NoLoc)
  ]

if' :: TestTree
if' = testGroup "if statements"
  [ testCase "without precondition"
      $ run "if 0 = 0 -> skip fi\n\
            \{ 0 = 2 }\n" @?= poList
      [ Obligation 0
          (Conjunct
            [ assertion (0 === 2)
            , guardIf (0 === 0)
            ])
          (assertion (0 === 2))
          (AroundSkip NoLoc)
      ]
  , testCase "without precondition 2"
      $ run "if 0 = 0 -> skip   \n\
            \ | 0 = 1 -> abort  \n\
            \fi                 \n\
            \{ 0 = 2 }          \n" @?= poList
      [ Obligation 0
          (Conjunct
            [ assertion (0 === 2)
            , Disjunct
                [ guardIf (0 === 0)
                , guardIf (0 === 1)
                ]
            ])
          (assertion (0 === 2))
          (AroundSkip NoLoc)
      , Obligation 1
          (Conjunct
            [ Constant false
            , Disjunct
                [ guardIf (0 === 0)
                , guardIf (0 === 1)
                ]
            ])
          (Constant false)
          (AroundAbort NoLoc)
      ]
  , testCase "with precondition"
      $ run "{ 0 = 0 }\nif 0 = 1 -> skip fi\n{ 0 = 2 }\n" @?= poList
      [ Obligation 0
          (assertion (0 === 0))
          (guardIf (0 === 1))
          (AssertSufficient NoLoc)
      , Obligation 1
          (Conjunct
            [ assertion (0 === 2)
            , guardIf (0 === 1)
            ])
          (assertion (0 === 2))
          (AroundSkip NoLoc)
      ]
  ]

-- loop :: TestTree
-- loop = testGroup "if statements"
--   [ testCase "loop" $ run "{ 0 = 1 , bnd: A }\ndo 0 = 2 -> skip od\n{ 0 = 0 }\n" @?= Right
--     [ Obligation 0
--         (Conjunct
--           [ koopInvariant (0 === 1)
--           , Negate (guardLoop (0 === 2))
--           ])
--         (assertion (0 === 0))
--         (LoopBase NoLoc)
--     , Obligation 1
--         (Conjunct
--           [ koopInvariant (0 === 1)
--           , guardLoop (0 === 2)
--           ])
--         (koopInvariant (0 === 1))
--         (AroundSkip NoLoc)
--     , Obligation 2
--         (Conjunct
--           [ koopInvariant (0 === 1)
--           , guardLoop (0 === 2)
--           ])
--         (Bound (constant "A" `gte` number 0))
--         (LoopTermBase NoLoc)
--     , Obligation 4
--         (Conjunct
--           [ Bound (constant "A" `lt` variable "_bnd3")
--           , koopInvariant (0 === 1)
--           , guardLoop (0 === 2)
--           , Bound (constant "A" `eqq` variable "_bnd3")
--           ])
--         (Bound (constant "A" `lt` variable "_bnd3"))
--         (AroundSkip NoLoc)
--     ]
--   ]


poList :: [Obligation2] -> Either a POList
poList = Right . POList

run :: Text -> Either [Error] POList
run text = fmap (POList . map toNoLoc) $ REPL.scan "<test>" text
            >>= REPL.parseProgram "<test>"
            >>= REPL.sweep2

--------------------------------------------------------------------------------
-- | Wrap [Obligation] in POList for pretty-printing

newtype POList = POList [Obligation2]
  deriving (Eq)

instance Show POList where
  show = show . pretty

instance Pretty POList where
  pretty (POList xs) = "POList" <> indent 1 (prettyList xs)
instance ToNoLoc POList where
  toNoLoc (POList xs) = POList (map toNoLoc xs)
