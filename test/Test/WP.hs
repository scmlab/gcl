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
  ]

--------------------------------------------------------------------------------
-- | Expression

simple :: TestTree
simple = testGroup "simple program"
  [ testCase "skip" $ run "skip\n{ False }" @?= Right
      ( Assertion false NoLoc
      , []
      , []
      )
  , testCase "abort" $ run "abort\n{ False }" @?= Right
      ( Constant
          (Lit (Bol False) NoLoc)
      , []
      , []
      )
  , testCase "assertion" $ run "{ True }\n{ False }" @?= Right
      ( Assertion true NoLoc
      , []
      , []
      )
  , testCase "assignment" $ run "x := 1\n{ False }" @?= Right
      ( Assertion false NoLoc
      , []
      , []
      )
  , testCase "spec" $ run "{!\n!}\n{ False }" @?= Right
      ( Assertion false NoLoc
      , []
      , [ Specification
            0
            (Assertion false NoLoc)
            (Assertion false NoLoc)
            NoLoc
        ]
      )
  ]


run :: Text -> Either [Error] (Pred, [Obligation], [Specification])
run text = fmap (\(x, y, z) -> (toNoLoc x, map toNoLoc y, map toNoLoc z))
            $ REPL.scan "<test>" (text)
              >>= REPL.parseProgram "<test>"
              >>= REPL.sweep

assertions :: TestTree
assertions = testCase "assertions" $ run "{ True }\n{ False }\n{ True }\n" @?= Right
  ( Assertion true NoLoc
  , [ Obligation 0
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
