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
  [ testCase "skip" $ run "skip\n{ 0 = 0 }" @?= chunks
    [ Chunk
        (Constant true)
        [ Line $ assertion (0 === 0)
        ]
        (assertion (0 === 0))
    ]
  , testCase "abort" $ run "abort\n{ True }" @?= chunks
      [ Chunk
          (Constant true)
          [ Line $ Constant false
          ]
          (assertion true)
      ]
  , testCase "assignment" $ run "x := 1\n{ 0 = x }" @?= chunks
      [ Chunk
          (Constant true)
          [ Line $ assertion (number 0 `eqq` number 1)
          ]
          (assertion (number 0 `eqq` variable "x"))
      ]
  , testCase "spec" $ run "{!\n!}\n{ 0 = 0 }" @?= chunks
      [ Chunk
          (Constant true)
          [ Line $ assertion (0 === 0)
          ]
          (assertion (0 === 0))
      ]
  ]

assertions :: TestTree
assertions = testGroup "assertions"
  [ testCase "1 assertion"
    $ run "{ 0 = 0 }" @?= chunks
      [ Chunk
          (Constant true)
          []
          (assertion (0 === 0))
      ]
  , testCase "2 assertions"
    $ run "{ 0 = 0 }\n{ 0 = 1 }" @?= chunks
      [ Chunk
          (assertion (0 === 0))
          []
          (assertion (0 === 1))
      ]
  , testCase "3 assertions"
    $ run "{ 0 = 0 }\n{ 0 = 1 }\n{ 0 = 2 }" @?= chunks
      [ Chunk
          (assertion (0 === 0))
          []
          (assertion (0 === 1))
      , Chunk
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
          \{ 0 = 2 }            \n" @?= chunks
      [ Chunk
          (Constant true)
          [ Blocks (Disjunct [ guardIf (0 === 0) , guardIf (0 === 1) ])
            [ Block
              [ Chunk
                  (Conjunct [ guardIf (0 === 0) , Constant true ])
                  [ Line $ assertion (0 === 2)
                  ]
                  (assertion (0 === 2))
              ]
            , Block
              [ Chunk
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
          \{ 0 = 2 }\n" @?= chunks
      [ Chunk
          (Constant true)
          [ Blocks (guardIf (0 === 0))
            [ Block
              [ Chunk
                  (Conjunct [guardIf (0 === 0), Constant true])
                  [ Blocks (guardIf (0 === 1))
                    [ Block
                      [ Chunk
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
  , testCase "with precondition 1"
    $ run "{ 0 = 0 }          \n\
          \if 0 = 1 -> skip fi\n\
          \{ 0 = 2 }          \n" @?= chunks
      [ Chunk
          (assertion (0 === 0))
          [ Blocks (guardIf (0 === 1))
            [ Block
              [ Chunk
                  (Conjunct [guardIf (0 === 1), assertion (0 === 0)])
                  [ Line $ assertion (0 === 2)
                  ]
                  (assertion (0 === 2))
              ]
            ]
          ]
          (assertion (0 === 2))
      ]
      -- [ Leaf (assertion (0 === 0))
      -- , Node (guardIf (0 === 1))
      --     [ Tree  [ Leaf $ Conjunct [ assertion (0 === 0), guardIf (0 === 1) ]
      --             , Leaf $ assertion (0 === 2)
      --             ]
      --     ]
      -- , Leaf $ assertion (0 === 2)
      -- ]
  -- , testCase "with precondition 2"
  --   $ run "{ 0 = 0 }          \n\
  --         \if 0 = 1 -> skip   \n\
  --         \ | 0 = 2 -> abort  \n\
  --         \fi                 \n\
  --         \{ 0 = 3 }          \n" @?= chunks
  --     [ Leaf (assertion (0 === 0))
  --     , Node (Disjunct [ guardIf (0 === 1), guardIf (0 === 2)])
  --         [ Tree  [ Leaf $ Conjunct [ assertion (0 === 0), guardIf (0 === 1) ]
  --                 , Leaf $ assertion (0 === 3)
  --                 ]
  --         , Tree  [ Leaf $ Conjunct [ assertion (0 === 0), guardIf (0 === 2) ]
  --                 , Leaf $ assertion (0 === 3)
  --                 ]
  --         ]
  --     , Leaf $ assertion (0 === 3)
  --     ]
  ]

-- loop :: TestTree
-- loop = testGroup "loop statements"
--   [ testCase "1 branch"
--     $ run "{ 0 = 1 , bnd: A }     \n\
--           \do 0 = 2 -> skip       \n\
--           \od                     \n\
--           \{ 0 = 0 }              \n" @?= chunks
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
--           \{ 0 = 0 }\n" @?= chunks
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
--           \{ 0 = 0 }\n" @?= chunks
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

chunks :: [Chunk] -> Either a Block
chunks = Right . Block

run :: Text -> Either [Error] Block
run text = toNoLoc <$> (REPL.scan "<test>" text
            >>= REPL.parseProgram "<test>"
            >>= REPL.structError . runWPM . programToBlock)

--
-- data Block = Block [Chunk]
-- data Chunk = Chunk Pred [Line]
-- data Line = Line   Pred
--           | Blocks Pred [Block]


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
instance ToNoLoc Block where
  toNoLoc (Block xs) = Block (map toNoLoc xs)
instance ToNoLoc Chunk where
  toNoLoc (Chunk pre xs post) = Chunk (toNoLoc pre) (map toNoLoc xs) (toNoLoc post)
instance ToNoLoc Line where
  toNoLoc (Line p) = Line (toNoLoc p)
  toNoLoc (Blocks p xs) = Blocks (toNoLoc p) (map toNoLoc xs)


instance Show Block where
  show = show . pretty
instance Pretty Block where
  pretty (Block xs) = vsep (map pretty xs)
  -- pretty (Block xs) = encloseSep mempty mempty hr (map pretty xs)
  --   where
  --       hr = line <> "----------------" <> line

instance Show Chunk where
  show = show . pretty
instance Pretty Chunk where
  pretty (Chunk pre xs post) =
    "----------------------------------------------------------------" <> line <>
    "*" <+> pretty pre <> line
    <> vsep (map (\x -> "  " <> pretty x) xs) <> line
    <> "*" <+> pretty post

instance Show Line where
  show = show . pretty
instance Pretty Line where
  pretty (Line p) = pretty p
  pretty (Blocks p xs) = "*" <+> pretty p <> line <> vsep (map (\x -> "  | " <> align (pretty x)) xs)

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
