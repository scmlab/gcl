{-# LANGUAGE OverloadedStrings #-}

module Test.WP2 where

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
  -- , loop
  ]

statements :: TestTree
statements = testGroup "simple statements"
  [ testCase "skip" $ run "skip\n{ 0 = 0 }" @?= code
    [ Struct
        (Constant true)
        [ Line $ assertion (0 === 0)
        ]
        (assertion (0 === 0))
    ]
  , testCase "abort" $ run "abort\n{ True }" @?= code
      [ Struct
          (Constant true)
          [ Line $ Constant false
          ]
          (assertion true)
      ]
  , testCase "assignment" $ run "x := 1\n{ 0 = x }" @?= code
      [ Struct
          (Constant true)
          [ Line $ assertion (number 0 `eqq` number 1)
          ]
          (assertion (number 0 `eqq` variable "x"))
      ]
  , testCase "spec" $ run "{!\n!}\n{ 0 = 0 }" @?= code
      [ Struct
          (Constant true)
          [ Line $ assertion (0 === 0)
          ]
          (assertion (0 === 0))
      ]
  ]

assertions :: TestTree
assertions = testGroup "assertions"
  [ testCase "1 assertion"
    $ run "{ 0 = 0 }" @?= code
      [ Struct
          (Constant true)
          []
          (assertion (0 === 0))
      ]
  , testCase "2 assertions"
    $ run "{ 0 = 0 }\n{ 0 = 1 }" @?= code
      [ Struct
          (assertion (0 === 0))
          []
          (assertion (0 === 1))
      ]
  , testCase "3 assertions"
    $ run "{ 0 = 0 }\n{ 0 = 1 }\n{ 0 = 2 }" @?= code
      [ Struct
          (assertion (0 === 0))
          []
          (assertion (0 === 1))
      , Struct
          (assertion (0 === 1))
          []
          (assertion (0 === 2))
      ]
  ]


if' :: TestTree
if' = testGroup "if statements"
  [ testCase "without precondition"
    $ run "if 0 = 0 -> skip     \n\
          \ | 0 = 1 -> abort    \n\
          \fi                   \n\
          \{ 0 = 2 }            \n" @?= code
      [ Struct
          (Constant true)
          [ Block (Disjunct [ guardIf (0 === 0) , guardIf (0 === 1) ])
            [ Code
              [ Struct
                  (Conjunct [ guardIf (0 === 0) , Constant true ])
                  [ Line $ assertion (0 === 2)
                  ]
                  (assertion (0 === 2))
              ]
            , Code
              [ Struct
                  (Conjunct [ guardIf (0 === 1) , Constant true ])
                  [ Line $ Constant false
                  ]
                  (assertion (0 === 2))
              ]
            ]
          ]
          (assertion (0 === 2))
      ]
  , testCase "without precondition (nested)"
    $ run "if 0 = 0 ->            \n\
          \     if 0 = 1 -> skip  \n\
          \     fi                \n\
          \fi                     \n\
          \{ 0 = 2 }\n" @?= code
      [ Struct
          (Constant true)
          [ Block (guardIf (0 === 0))
            [ Code
              [ Struct
                  (Conjunct [guardIf (0 === 0), Constant true])
                  [ Block (guardIf (0 === 1))
                    [ Code
                      [ Struct
                          (Conjunct [guardIf (0 === 1), guardIf (0 === 0), Constant true])
                          [ Line (assertion (0 === 2))
                          ]
                          (assertion (0 === 2))
                      ]
                    ]
                  ]
                  (assertion (0 === 2))
              ]
            ]
          ]
          (assertion (0 === 2))
      ]
  , testCase "without precondition (with assertion in branches)"
    $ run "if 0 = 0 ->            \n\
          \    { 0 = 1 }          \n\
          \    skip               \n\
          \fi                     \n\
          \{ 0 = 2 }\n" @?= code
      [ Struct
          (Constant true)
          [ Block (guardIf (0 === 0))
            [ Code
              [ Struct
                  (Conjunct [guardIf (0 === 0), Constant true])
                  []
                  (assertion (0 === 1))
              , Struct
                  (assertion (0 === 1))
                  [ Line $ assertion (0 === 2) ]
                  (assertion (0 === 2))
              ]
            ]
          ]
          (assertion (0 === 2))
      ]
  , testCase "with precondition 1"
    $ run "{ 0 = 0 }          \n\
          \if 0 = 1 -> skip fi\n\
          \{ 0 = 2 }          \n" @?= code
      [ Struct
          (assertion (0 === 0))
          [ Block (guardIf (0 === 1))
            [ Code
              [ Struct
                  (Conjunct [guardIf (0 === 1), assertion (0 === 0)])
                  [ Line $ assertion (0 === 2)
                  ]
                  (assertion (0 === 2))
              ]
            ]
          ]
          (assertion (0 === 2))
      ]
  , testCase "with precondition 2"
    $ run "{ 0 = 0 }          \n\
          \if 0 = 1 -> skip   \n\
          \ | 0 = 2 -> abort  \n\
          \fi                 \n\
          \{ 0 = 3 }          \n" @?= code
      [ Struct
          (assertion (0 === 0))
          [ Block (Disjunct [guardIf (0 === 1), guardIf (0 === 2)])
              [ Code
                  [ Struct
                    (Conjunct [guardIf (0 === 1), assertion (0 === 0)])
                    [ Line $ assertion (0 === 3)]
                    (assertion (0 === 3))
                  ]
              , Code
                [ Struct
                    (Conjunct [guardIf (0 === 2), assertion (0 === 0)])
                    [ Line $ Constant false]
                    (assertion (0 === 3))
                ]
              ]
          ]
          (assertion (0 === 3))
      ]
  ]

-- loop :: TestTree
-- loop = testGroup "loop statements"
--   [ testCase "1 branch"
--     $ run "{ 0 = 1 , bnd: A }     \n\
--           \do 0 = 2 -> skip       \n\
--           \od                     \n\
--           \{ 0 = 0 }              \n" @?= code
--       [ Leaf $ loopInvariant (0 === 1)
--       , Node (loopInvariant (0 === 1))
--           [ Tree
--               [ Leaf $ Conjunct [loopInvariant (0 === 1), guardLoop (0 === 2)]
--               , Leaf $ loopInvariant (0 === 1)
--               ]
--           ]
--       , Leaf $ assertion (0 === 0)
--       ]
--   , testCase "2 branches"
--     $ run "{ 0 = 1 , bnd: A }       \n\
--           \do 0 = 2 -> skip         \n\
--           \ | 0 = 3 -> abort od     \n\
--           \{ 0 = 0 }\n" @?= code
--       [ Leaf $ loopInvariant (0 === 1)
--       , Node (loopInvariant (0 === 1))
--           [ Tree
--               [ Leaf $ Conjunct [loopInvariant (0 === 1), guardLoop (0 === 2)]
--               , Leaf $ loopInvariant (0 === 1)
--               ]
--           , Tree
--               [ Leaf $ Conjunct [loopInvariant (0 === 1), guardLoop (0 === 3)]
--               , Leaf $ loopInvariant (0 === 1)
--               ]
--           ]
--       , Leaf $ assertion (0 === 0)
--       ]
--   , testCase "nested"
--     $ run "{ 0 = 1 , bnd: A }       \n\
--           \do 0 = 2 ->              \n\
--           \   { 0 = 3 , bnd: A }    \n\
--           \   do 0 = 4 -> abort od  \n\
--           \od                       \n\
--           \{ 0 = 0 }\n" @?= code
--       [ Leaf $ loopInvariant (0 === 1)
--       , Node (loopInvariant (0 === 1))
--           [ Tree
--               [ Leaf $ loopInvariant (0 === 3)
--               , Node (loopInvariant (0 === 3))
--                   [ Tree
--                       [ Leaf $ Conjunct [loopInvariant (0 === 3), guardLoop (0 === 4)]
--                       , Leaf $ loopInvariant (0 === 3)
--                       ]
--                   ]
--               , Leaf $ loopInvariant (0 === 1)
--               ]
--           ]
--       , Leaf $ assertion (0 === 0)
--       ]
--   ]

code :: [Struct] -> Either a Code
code = Right . Code

run :: Text -> Either [Error] Code
run text = toNoLoc <$> (REPL.scan "<test>" text
            >>= REPL.parseProgram "<test>"
            >>= REPL.structError . runWPM . programToBlock)

--
-- data Block = Block [Struct]
-- data Struct = Struct Pred [Line]
-- data Line = Line   Pred
--           | Block Pred [Block]


--------------------------------------------------------------------------------
-- | Data structure for extracting WPs from a Lasagna

-- newtype WPTree = Tree [WPNode]
--   deriving (Eq)
--
-- instance Show WPTree where
--   show = show . pretty
--
-- data WPNode = Node Pred [WPTree] | Leaf Pred
--             deriving (Eq)
--
-- instance Pretty WPTree where
--   pretty (Tree xs) = vsep (map pretty xs)
--     -- "Tree" <> indent 1 (prettyList xs)
instance ToNoLoc Code where
  toNoLoc (Code xs) = Code (map toNoLoc xs)
instance ToNoLoc Struct where
  toNoLoc (Struct pre xs post) = Struct (toNoLoc pre) (map toNoLoc xs) (toNoLoc post)
instance ToNoLoc Line where
  toNoLoc (Line p) = Line (toNoLoc p)
  toNoLoc (Block p xs) = Block (toNoLoc p) (map toNoLoc xs)


instance Show Code where
  show = show . pretty
instance Pretty Code where
  pretty (Code xs) = vsep (map pretty xs)
  -- pretty (Block xs) = encloseSep mempty mempty hr (map pretty xs)
  --   where
  --       hr = line <> "----------------" <> line

instance Show Struct where
  show = show . pretty
instance Pretty Struct where
  pretty (Struct pre xs post) =
    "----------------------------------------------------------------" <> line <>
    "*" <+> pretty pre <> line
    <> vsep (map (\x -> "  " <> pretty x) xs) <> line
    <> "*" <+> pretty post

instance Show Line where
  show = show . pretty
instance Pretty Line where
  pretty (Line p) = pretty p
  pretty (Block p xs) = "*" <+> pretty p <> line <> vsep (map (\x -> "  | " <> align (pretty x)) xs)

-- instance Pretty WPNode where
--   pretty (Leaf p) = pretty p
--   pretty (Node p xs) = pretty p <> line <> vsep (map (\x -> " | " <+> align (pretty x)) xs)
--   -- <> indent 4 (vsep (map pretty xs))
-- instance Show WPNode where
--   show = show . pretty
-- instance ToNoLoc WPNode where
--   toNoLoc (Node p xs) = Node (toNoLoc p) (map toNoLoc xs)
--   toNoLoc (Leaf p)    = Leaf (toNoLoc p)
--
-- toWPTree :: Lasagna -> WPTree
-- toWPTree (Final p) = Tree [Leaf p]
-- toWPTree (Layer _ p _ [] xs) =
--   let Tree nodes = toWPTree xs
--   in  Tree $ Leaf p : nodes
-- toWPTree (Layer _ p _ branches xs) =
--   let Tree nodes = toWPTree xs
--   in  Tree $ Node p (map toWPTree branches) : nodes
