{-# LANGUAGE OverloadedStrings #-}

module Test.WeakestPrecond where

import Data.Text.Lazy hiding (map)
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (Ordering(..))

import Syntax.Predicate
import Syntax.Concrete hiding (LoopInvariant)
import qualified REPL as REPL
import GCL.WP2
-- import GCL.WP2 (Obligation2(..), ObliOrigin2(..), Lasagna(..))
import Data.Text.Prettyprint.Doc

-- import GCL.WP2 (Obligation2(..), Specification2(..), ObliOrigin2(..))
import Syntax.Location
import Error
import Pretty ()



tests :: TestTree
tests = testGroup "Weakest Precondition"
  [
    statements
  , assertions
  , if'
  , loop
  ]

statements :: TestTree
statements = testGroup "simple statements"
  [ testCase "skip" $ run "skip\n{ 0 = 0 }" @?= tree
      [ Leaf $ assertion (0 === 0)
      , Leaf $ assertion (0 === 0)
      ]
  , testCase "abort" $ run "abort\n{ True }" @?= tree
      [ Leaf $ Constant false
      , Leaf $ assertion true
      ]
  , testCase "assignment" $ run "x := 1\n{ 0 = x }" @?= tree
      [ Leaf $ assertion (0 === 1)
      , Leaf $ assertion (number 0 `eqq` variable "x")
      ]
  , testCase "spec" $ run "{!\n!}\n{ False }" @?= tree
      [ Leaf $ assertion false
      , Leaf $ assertion false
      ]
  ]

assertions :: TestTree
assertions = testGroup "assertions"
  [ testCase "1 assertion"
    $ run "{ 0 = 0 }" @?= tree
      [ Leaf $ assertion (0 === 0)
      ]
  , testCase "2 assertions"
    $ run "{ 0 = 0 }\n{ 0 = 1 }" @?= tree
      [ Leaf $ assertion (0 === 0)
      , Leaf $ assertion (0 === 1)
      ]
  , testCase "3 assertions"
    $ run "{ 0 = 0 }\n{ 0 = 1 }\n{ 0 = 2 }" @?= tree
      [ Leaf $ assertion (0 === 0)
      , Leaf $ assertion (0 === 1)
      , Leaf $ assertion (0 === 2)
      ]
  ]


if' :: TestTree
if' = testGroup "if statements"
  [ testCase "without precondition"
    $ run "if 0 = 0 -> skip     \n\
          \ | 0 = 1 -> abort    \n\
          \fi                   \n\
          \{ 0 = 2 }            \n" @?= tree
      [ Node (Disjunct [ guardIf (0 === 0) , guardIf (0 === 1) ] )
          [ Tree  [ Leaf $ guardIf (0 === 0)
                  , Leaf $ assertion (0 === 2)
                  ]
          , Tree  [ Leaf $ guardIf (0 === 1)
                  , Leaf $ assertion (0 === 2)
                  ]
          ]
      , Leaf $ assertion (0 === 2)
      ]
  , testCase "without precondition (nested)"
    $ run "if 0 = 0 ->            \n\
          \     if 0 = 1 -> skip  \n\
          \     fi                \n\
          \fi                     \n\
          \{ 0 = 2 }\n" @?= tree
      [ Node (guardIf (0 === 0))
          [ Tree
              [ Node (guardIf (0 === 1))
                  [ Tree
                      [ Leaf $ Conjunct [ guardIf (0 === 0), guardIf (0 === 1) ]
                      , Leaf $ assertion (0 === 2)
                      ]
                  ]
              , Leaf $ assertion (0 === 2)
              ]
          ]
      , Leaf $ assertion (0 === 2)
      ]
  , testCase "with precondition 1"
    $ run "{ 0 = 0 }          \n\
          \if 0 = 1 -> skip fi\n\
          \{ 0 = 2 }          \n" @?= tree
      [ Leaf (assertion (0 === 0))
      , Node (guardIf (0 === 1))
          [ Tree  [ Leaf $ Conjunct [ assertion (0 === 0), guardIf (0 === 1) ]
                  , Leaf $ assertion (0 === 2)
                  ]
          ]
      , Leaf $ assertion (0 === 2)
      ]
  , testCase "with precondition 2"
    $ run "{ 0 = 0 }          \n\
          \if 0 = 1 -> skip   \n\
          \ | 0 = 2 -> abort  \n\
          \fi                 \n\
          \{ 0 = 3 }          \n" @?= tree
      [ Leaf (assertion (0 === 0))
      , Node (Disjunct [ guardIf (0 === 1), guardIf (0 === 2)])
          [ Tree  [ Leaf $ Conjunct [ assertion (0 === 0), guardIf (0 === 1) ]
                  , Leaf $ assertion (0 === 3)
                  ]
          , Tree  [ Leaf $ Conjunct [ assertion (0 === 0), guardIf (0 === 2) ]
                  , Leaf $ assertion (0 === 3)
                  ]
          ]
      , Leaf $ assertion (0 === 3)
      ]
  ]

loop :: TestTree
loop = testGroup "loop statements"
  [ testCase "1 branch"
    $ run "{ 0 = 1 , bnd: A }     \n\
          \do 0 = 2 -> skip       \n\
          \od                     \n\
          \{ 0 = 0 }              \n" @?= tree
      [ Leaf $ loopInvariant (0 === 1)
      , Node (loopInvariant (0 === 1))
          [ Tree
              [ Leaf $ Conjunct [loopInvariant (0 === 1), guardLoop (0 === 2)]
              , Leaf $ loopInvariant (0 === 1)
              ]
          ]
      , Leaf $ assertion (0 === 0)
      ]
  , testCase "2 branches"
    $ run "{ 0 = 1 , bnd: A }       \n\
          \do 0 = 2 -> skip         \n\
          \ | 0 = 3 -> abort od     \n\
          \{ 0 = 0 }\n" @?= tree
      [ Leaf $ loopInvariant (0 === 1)
      , Node (loopInvariant (0 === 1))
          [ Tree
              [ Leaf $ Conjunct [loopInvariant (0 === 1), guardLoop (0 === 2)]
              , Leaf $ loopInvariant (0 === 1)
              ]
          , Tree
              [ Leaf $ Conjunct [loopInvariant (0 === 1), guardLoop (0 === 3)]
              , Leaf $ loopInvariant (0 === 1)
              ]
          ]
      , Leaf $ assertion (0 === 0)
      ]
  , testCase "nested"
    $ run "{ 0 = 1 , bnd: A }       \n\
          \do 0 = 2 ->              \n\
          \   { 0 = 3 , bnd: A }    \n\
          \   do 0 = 4 -> abort od  \n\
          \od                       \n\
          \{ 0 = 0 }\n" @?= tree
      [ Leaf $ loopInvariant (0 === 1)
      , Node (loopInvariant (0 === 1))
          [ Tree
              [ Leaf $ loopInvariant (0 === 3)
              , Node (loopInvariant (0 === 3))
                  [ Tree
                      [ Leaf $ Conjunct [loopInvariant (0 === 3), guardLoop (0 === 4)]
                      , Leaf $ loopInvariant (0 === 3)
                      ]
                  ]
              , Leaf $ loopInvariant (0 === 1)
              ]
          ]
      , Leaf $ assertion (0 === 0)
      ]
  ]

--
--
-- loop :: TestTree
-- loop = testGroup "if statements"
--   [ testCase "loop" $ run "{ 0 = 1 , bnd: A }\ndo 0 = 2 -> skip od\n{ 0 = 0 }\n" @?= Right
--     [ Obligation 0
--         (Conjunct
--           [ LoopInvariant (0 === 01) NoLoc
--           , Negate (Guard (makePred 2) (LOOP NoLoc) NoLoc)
--           ])
--         (Assertion (0 === 0) NoLoc)
--         (LoopBase NoLoc)
--     , Obligation 1
--         (Conjunct
--           [ LoopInvariant (0 === 01) NoLoc
--           , Guard (makePred 2) (LOOP NoLoc) NoLoc
--           ])
--         (LoopInvariant (0 === 01) NoLoc)
--         (AroundSkip NoLoc)
--     , Obligation 2
--         (Conjunct
--           [ LoopInvariant (0 === 01) NoLoc
--           , Guard (makePred 2) (LOOP NoLoc) NoLoc
--           ])
--         (Bound (constant "A" `gte` number 0))
--         (LoopTermBase NoLoc)
--     , Obligation 4
--         (Conjunct
--           [ Bound (constant "A" `lt` variable "_bnd3")
--           , LoopInvariant (0 === 01) NoLoc
--           , Guard (makePred 2) (LOOP NoLoc) NoLoc
--           , Bound (constant "A" `eqq` variable "_bnd3")
--           ])
--         (Bound (constant "A" `lt` variable "_bnd3"))
--         (AroundSkip NoLoc)
--     ]
--   ]

tree :: [WPNode] -> Either a WPTree
tree = Right . Tree

run :: Text -> Either [Error] WPTree
run text = toNoLoc . toWPTree <$> (REPL.scan "<test>" text
            >>= REPL.parseProgram "<test>"
            >>= REPL.structError . runWPM . programToLasagna)

--------------------------------------------------------------------------------
-- | Data structure for extracting WPs from a Lasagna

newtype WPTree = Tree [WPNode]
  deriving (Eq)

instance Show WPTree where
  show = show . pretty

data WPNode = Node Pred [WPTree] | Leaf Pred
  deriving (Eq)

instance Pretty WPTree where
  pretty (Tree xs) = vsep (map pretty xs)
    -- "Tree" <> indent 1 (prettyList xs)
instance ToNoLoc WPTree where
  toNoLoc (Tree xs) = Tree (map toNoLoc xs)

instance Pretty WPNode where
  pretty (Leaf p) = pretty p
  pretty (Node p xs) = pretty p <> line <> vsep (map (\x -> " | " <+> align (pretty x)) xs)
  -- <> indent 4 (vsep (map pretty xs))
instance Show WPNode where
  show = show . pretty
instance ToNoLoc WPNode where
  toNoLoc (Node p xs) = Node (toNoLoc p) (map toNoLoc xs)
  toNoLoc (Leaf p)    = Leaf (toNoLoc p)

toWPTree :: Lasagna -> WPTree
toWPTree (Final p) = Tree [Leaf p]
toWPTree (Layer _ p _ [] xs) =
  let Tree nodes = toWPTree xs
  in  Tree $ Leaf p : nodes
toWPTree (Layer _ p _ branches xs) =
  let Tree nodes = toWPTree xs
  in  Tree $ Node p (map toWPTree branches) : nodes
