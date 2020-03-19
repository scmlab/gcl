{-# LANGUAGE OverloadedStrings #-}

module Test.WP2.PO where

import Data.Text.Lazy hiding (map)
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (Ordering(..))

import Syntax.Predicate
import Syntax.Concrete hiding (LoopInvariant)
import qualified REPL as REPL
-- import GCL.WP2 (Obligation2(..), Spec(..), ObliOrigin2(..))
import Data.Text.Prettyprint.Doc

import Syntax.Location
import Data.Loc
import Error
import Pretty ()

tests :: TestTree
tests = testGroup "WP2 Proof Obligations"
  [
    statements
  , assertions
  , if'
  , loop
  ]

--------------------------------------------------------------------------------
-- | Expression

statements :: TestTree
statements = testGroup "simple statements"
  [ testCase "skip"
      $ run "{ True }     \n\
            \skip         \n\
            \{ 0 = 0 }    \n" @== poList
      [ PO 0
          (assertion true)
          (assertion (0 === 0))
          (AtSkip NoLoc)
      ]
  , testCase "abort"
      $ run "{ True }     \n\
            \abort        \n\
            \{ 0 = 0 }    \n" @== poList
      [ PO 0
          (assertion true)
          (Constant false)
          (AtAbort NoLoc)
      ]
  , testCase "assignment"
      $ run "{ True }     \n\
            \x := 1       \n\
            \{ 0 = x }    \n" @== poList
      [ PO 0
          (assertion true)
          (assertion (number 0 `eqq` number 1))
          (AtAssignment NoLoc)
      ]
  , testCase "spec"
      $ run "{ True }     \n\
            \{!           \n\
            \!}           \n\
            \{ False }    \n" @== poList
      []
  ]

assertions :: TestTree
assertions = testGroup "assertions"
  [ testCase "3 assertions"
      $ run "{ True }     \n\
            \{ False }    \n\
            \{ True }     \n" @== poList
      [ PO 0
          (assertion true)
          (assertion false)
          (AtAssertion NoLoc)
      , PO 1
          (assertion false)
          (assertion true)
          (AtAssertion NoLoc)
      ]
  , testCase "2 assertions"
      $ run "{ 0 = 0 }    \n\
            \{ 0 = 1 }    \n" @== poList
      [ PO 0
          (assertion (0 === 0))
          (assertion (0 === 1))
          (AtAssertion NoLoc)
      ]
  ]

if' :: TestTree
if' = testGroup "if statements"
  [ testCase "without precondition"
      $ run "{ 0 = 0 }              \n\
            \if 0 = 1 -> skip     \n\
            \ | 0 = 2 -> abort    \n\
            \fi                   \n\
            \{ 0 = 3 }            \n" @== poList
        [ PO 0
          (assertion (0 === 0))
          (Disjunct [ guardIf (0 === 1), guardIf (0 === 2) ])
          (AtIf NoLoc)
        , PO 1
          (Conjunct [ assertion (0 === 0), guardIf (0 === 1) ])
          (assertion (0 === 3))
          (AtSkip NoLoc)
        , PO 2
          (Conjunct [ assertion (0 === 0), guardIf (0 === 2) ])
          (Constant false)
          (AtAbort NoLoc)
        ]
  , testCase "without precondition (nested)"
      $ run "{ 0 = 0 }              \n\
            \if 0 = 1 ->            \n\
            \     if 0 = 2 -> skip  \n\
            \     fi                \n\
            \fi                     \n\
            \{ 0 = 3 }\n" @== poList
      [ PO 0
          (assertion (0 === 0))
          (guardIf (0 === 1))
          (AtIf NoLoc)
      , PO 1
          (Conjunct [ assertion (0 === 0), guardIf (0 === 1) ])
          (guardIf (0 === 2))
          (AtIf NoLoc)
      , PO 2
          (Conjunct [ assertion (0 === 0), guardIf (0 === 1), guardIf (0 === 2) ])
          (assertion (0 === 3))
          (AtSkip NoLoc)

      ]
  , testCase "with precondition"
      $ run "{ 0 = 0 }            \n\
             \if 0 = 1 -> skip    \n\
             \ | 0 = 3 -> abort   \n\
             \fi                  \n\
             \{ 0 = 2 }           \n" @== poList
      [ PO 0
          (assertion (0 === 0))
          (Disjunct [ guardIf (0 === 1), guardIf (0 === 3) ])
          (AtIf NoLoc)
      , PO 1
          (Conjunct [ assertion (0 === 0), guardIf (0 === 1) ])
          (assertion (0 === 2))
          (AtSkip NoLoc)
      , PO 2
          (Conjunct [ assertion (0 === 0), guardIf (0 === 3) ])
          (Constant false)
          (AtAbort NoLoc)
      ]
  ]

loop :: TestTree
loop = testGroup "loop statements"
  [ testCase "1 branch"
    $ run "{ 0 = 1 , bnd: A }     \n\
          \do 0 = 2 -> skip       \n\
          \od                     \n\
          \{ 0 = 0 }              \n" @==
      let inv = loopInvariant (0 === 1) "A" in poList
      [ PO 0
          (Conjunct [ inv, Negate (guardLoop (0 === 2)) ])
          (assertion (0 === 0))
          (AtLoop NoLoc)
      , PO 1
          (Conjunct [ inv, guardLoop (0 === 2) ])
          inv
          (AtSkip NoLoc)
      , PO 3
          (Conjunct [ inv , guardLoop (0 === 2) ])
          (boundGTE (variable "_bnd2") (number 0))
          (AtTermination NoLoc)
      , PO 4
          (Conjunct [ Conjunct [ inv , guardLoop (0 === 2) ]
                    , boundEq (constant "A") (variable "_bnd2")
                    ])
          (boundLT (constant "A") (variable "_bnd2"))
          (AtSkip NoLoc)
      ]
  , testCase "2 branches"
    $ run "{ 0 = 1 , bnd: A }       \n\
          \do 0 = 2 -> skip         \n\
          \ | 0 = 3 -> abort od     \n\
          \{ 0 = 0 }\n" @==
      let inv = loopInvariant (0 === 1) "A" in poList
      [ PO 0
          (Conjunct [ inv
                    , Negate (guardLoop (0 === 2))
                    , Negate (guardLoop (0 === 3))
                    ])
          (assertion (0 === 0))
          (AtLoop NoLoc)
      , PO 1
          (Conjunct [ inv, guardLoop (0 === 2) ])
          inv
          (AtSkip NoLoc)
      , PO 2
          (Conjunct [ inv, guardLoop (0 === 3) ])
          (Constant false)
          (AtAbort NoLoc)
      , PO 4
          (Conjunct [ inv
                    , guardLoop (0 === 2)
                    , guardLoop (0 === 3)
                    ])
          (boundGTE (variable "_bnd3") (number 0))
          (AtTermination NoLoc)
      , PO 5
          (Conjunct [ Conjunct [ inv , guardLoop (0 === 2) ]
                    , boundEq (constant "A") (variable "_bnd3")
                    ])
          (boundLT (constant "A") (variable "_bnd3"))
          (AtSkip NoLoc)
      , PO 6
          (Conjunct [ Conjunct [ inv , guardLoop (0 === 3) ]
                    , boundEq (constant "A") (variable "_bnd3")
                    ])
          (Constant false)
          (AtAbort NoLoc)
      ]
  -- , testCase "nested"
  --   $ run "{ 0 = 1 , bnd: A }     \n\
  --         \do 0 = 2 ->            \n\
  --         \    { 0 = 3 , bnd: B } \n\
  --         \    do 0 = 4 -> skip od\n\
  --         \od                     \n\
  --         \{ 0 = 0 }              \n" @==
  --     let inv1 = loopInvariant (0 === 1) "A"
  --         inv2 = loopInvariant (0 === 3) "B"
  --     in poList
  --     [ PO 0
  --         (Conjunct [ inv1, Negate (guardLoop (0 === 2)) ])
  --         (assertion (0 === 0))
  --         (AtLoop NoLoc)
  --     , PO 1
  --         (Conjunct [ inv1, guardLoop (0 === 2) ])
  --         inv1
  --         (AtSkip NoLoc)
  --     ]
  ]


poList :: [PO] -> Either a POList
poList = Right . POList

run :: Text -> IO (Either Error POList)
run text = REPL.runREPLM $ REPL.scan "<test>" text
            >>= REPL.parseProgram "<test>"
            >>= REPL.toStruct
            >>= REPL.sweep2
            >>= return . POList . map toNoLoc . fst

(@==) :: (Eq a, Show a) => IO a -> a -> IO ()
f @== b = do
  a <- f
  a @?= b


--------------------------------------------------------------------------------
-- | Wrap [Obligation] in POList for pretty-printing

newtype POList = POList [PO]
  deriving (Eq)

instance Show POList where
  show = show . pretty

instance Pretty POList where
  pretty (POList xs) = "POList" <> indent 1 (prettyList xs)
instance ToNoLoc POList where
  toNoLoc (POList xs) = POList (map toNoLoc xs)
