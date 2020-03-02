{-# LANGUAGE OverloadedStrings #-}

module Test.WP where

import Data.Text.Lazy hiding (map)
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (Ordering(..))

import Syntax.Predicate
import Syntax.Concrete
import qualified REPL as REPL
import GCL.WP
import Syntax.Location
import Data.Loc
import Error
import Pretty ()

tests :: TestTree
tests = testGroup "Weakest Precondition"
  [ simple
  , assertions
  , if'
  ]

--------------------------------------------------------------------------------
-- | Expression

simple :: TestTree
simple = testGroup "simple program"
  [ testCase "skip" $ do
      actual <- run "skip\n{ False }"
      actual @?= Right
          ( []
          , []
          )
  , testCase "abort" $ do
      actual <- run "abort\n{ False }"
      actual @?= Right
          ( []
          , []
          )
  , testCase "assertion" $ do
      actual <- run "{ True }\n{ False }"
      actual @?= Right
          ( []
          , []
          )
  , testCase "assignment" $ do
      actual <- run "x := 1\n{ False }"
      actual @?= Right
        ( []
        , []
        )
  , testCase "spec" $ do
      actual <- run "{!\n!}\n{ False }"
      actual @?= Right
        ( []
        , [ Specification
              0
              (Assertion false NoLoc)
              (Assertion false NoLoc)
              NoLoc
          ]
        )
  ]


run :: Text -> IO (Either Error ([Obligation], [Specification]))
run text = REPL.runREPLM $ (\((_, y), z) -> (map toNoLoc y, map toNoLoc z)) <$>
             (REPL.scan "<test>" (text)
              >>= REPL.parseProgram "<test>"
              >>= REPL.sweep1)

assertions :: TestTree
assertions = testCase "assertions" $ do
  actual <- run "{ True }\n{ False }\n{ True }\n"
  actual @?= Right
    ( [ Obligation 0
        (Assertion true NoLoc)
        (Assertion false NoLoc)
        (AssertGuaranteed NoLoc)

      -- NOTE: missing the second proof obligation

      -- , Obligation 1
      --   (Assertion false NoLoc)
      --   (Assertion true NoLoc)
      --   (AssertGuaranteed NoLoc)
      ]
    , []
    )

if' :: TestTree
if' = testGroup "if statements"
  [ testCase "without precondition" $ do
    actual <- run "if False -> skip fi\n{ True }\n"
    actual @?= Right
      ( [ Obligation 0
          (GuardIf false NoLoc)
          -- (Conjunct
          --   [ Assertion true NoLoc
          --   , Disjunct [ Guard false (IF NoLoc) NoLoc ]
          --   , Guard false (IF NoLoc) NoLoc
          --   ])
          (Assertion true NoLoc)
          (AroundSkip NoLoc)
        ]
      , []
      )
  ]
