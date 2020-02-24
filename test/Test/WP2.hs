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
  , loop
  ]

statements :: TestTree
statements = testGroup "simple statements"
  [ testCase "skip"
      $ run "{ True }   \n\
            \skip       \n\
            \{ 0 = 0 }" @?= Right
      ( Struct (assertion true)
          [ Line $ assertion (0 === 0) ]
      $ Postcond (assertion (0 === 0))
      )
  , testCase "abort"
      $ run "{ True }   \n\
            \abort      \n\
            \{ True }" @?= Right
      ( Struct (assertion true)
          [ Line $ Constant false ]
      $ Postcond (assertion true)
      )
  , testCase "assignment"
      $ run "{ True }   \n\
            \x := 1     \n\
            \{ 0 = x }" @?= Right
      ( Struct (assertion true)
          [ Line $ assertion (number 0 `eqq` number 1) ]
      $ Postcond (assertion (number 0 `eqq` variable "x"))
      )
  , testCase "spec"
      $ run "{ True }   \n\
            \{!       \n\
            \!}       \n\
            \{ 0 = 0 }" @?= Right
      ( Struct (assertion true)
          [ Line $ assertion (0 === 0) ]
      $ Postcond (assertion (0 === 0))
      )
  ]

assertions :: TestTree
assertions = testGroup "assertions"
  [ testCase "2 assertions"
    $ run "{ 0 = 0 }\n{ 0 = 1 }" @?= Right
      ( Struct (assertion (0 === 0))
        []
      $ Postcond (assertion (0 === 1))
      )
  , testCase "3 assertions"
    $ run "{ 0 = 0 }\n{ 0 = 1 }\n{ 0 = 2 }" @?= Right
      ( Struct (assertion (0 === 0))
        []
      $ Struct (assertion (0 === 1))
        []
      $ Postcond (assertion (0 === 2))
      )
  ]


if' :: TestTree
if' = testGroup "if statements"
  [ testCase "without precondition"
    $ run "{ True }             \n\
          \if 0 = 0 -> skip     \n\
          \ | 0 = 1 -> abort    \n\
          \fi                   \n\
          \{ 0 = 2 }            \n" @?= Right
      ( Struct (assertion true)
          [ Block (Disjunct [ guardIf (0 === 0) , guardIf (0 === 1) ])
            [ Struct (Conjunct [ assertion true, guardIf (0 === 0) ])
              [ Line $ assertion (0 === 2) ]
            $ Postcond (assertion (0 === 2))
            , Struct (Conjunct [ assertion true, guardIf (0 === 1) ])
              [ Line $ Constant false ]
            $ Postcond (assertion (0 === 2))
            ]
          ]
      $ Postcond (assertion (0 === 2))
      )
  , testCase "without precondition (nested)"
    $ run "{ True }               \n\
          \if 0 = 0 ->            \n\
          \     if 0 = 1 -> skip  \n\
          \     fi                \n\
          \fi                     \n\
          \{ 0 = 2 }\n" @?= Right
      ( Struct (assertion true)
          [ Block (guardIf (0 === 0))
            [ Struct (Conjunct [ assertion true, guardIf (0 === 0) ])
              [ Block (guardIf (0 === 1))
                [ Struct (Conjunct [ assertion true, guardIf (0 === 0), guardIf (0 === 1) ])
                  [ Line (assertion (0 === 2)) ]
                $ Postcond (assertion (0 === 2))
                ]
              ]
              $ Postcond (assertion (0 === 2))
            ]
          ]
      $ Postcond (assertion (0 === 2))
      )
  , testCase "without precondition (with assertion in branches)"
    $ run "{ True }               \n\
          \if 0 = 0 ->            \n\
          \    { 0 = 1 }          \n\
          \    skip               \n\
          \fi                     \n\
          \{ 0 = 2 }\n" @?= Right
      ( Struct (assertion true)
        [ Block (guardIf (0 === 0))
          [ Struct (Conjunct [ assertion true, guardIf (0 === 0) ])
            []
          $ Struct (assertion (0 === 1))
            [ Line $ assertion (0 === 2) ]
          $ Postcond (assertion (0 === 2))
          ]
        ]
      $ Postcond (assertion (0 === 2))
      )
  , testCase "with precondition 1"
    $ run "{ 0 = 0 }          \n\
          \if 0 = 1 -> skip fi\n\
          \{ 0 = 2 }          \n" @?= Right
      ( Struct (assertion (0 === 0))
        [ Block (guardIf (0 === 1))
          [ Struct (Conjunct [ assertion (0 === 0), guardIf (0 === 1) ])
            [ Line $ assertion (0 === 2) ]
          $ Postcond (assertion (0 === 2))
          ]
        ]
      $ Postcond (assertion (0 === 2))
      )
  , testCase "with precondition 2"
    $ run "{ 0 = 0 }          \n\
          \if 0 = 1 -> skip   \n\
          \ | 0 = 2 -> abort  \n\
          \fi                 \n\
          \{ 0 = 3 }          \n" @?= Right
      ( Struct (assertion (0 === 0))
        [ Block (Disjunct [guardIf (0 === 1), guardIf (0 === 2)])
          [ Struct (Conjunct [assertion (0 === 0), guardIf (0 === 1)])
            [ Line $ assertion (0 === 3)]
          $ Postcond (assertion (0 === 3))
          , Struct (Conjunct [assertion (0 === 0), guardIf (0 === 2)])
            [ Line $ Constant false ]
          $ Postcond (assertion (0 === 3))
          ]
        ]
      $ Postcond (assertion (0 === 3))
      )
  ]

loop :: TestTree
loop = testGroup "loop statements"
  [ testCase "1 branch"
    $ run "{ 0 = 1 , bnd: A }     \n\
          \do 0 = 2 -> skip       \n\
          \od                     \n\
          \{ 0 = 0 }              \n" @?= Right
      ( Struct (loopInvariant (0 === 1))
        [ Block (loopInvariant (0 === 1))
          [ Struct (Conjunct [loopInvariant (0 === 1), guardLoop (0 === 2)])
            [ Line $ loopInvariant (0 === 1) ]
          $ Postcond (loopInvariant (0 === 1))
          ]
        ]
      $ Postcond (assertion (0 === 0))
      )
  , testCase "2 branches"
    $ run "{ 0 = 1 , bnd: A }       \n\
          \do 0 = 2 -> skip         \n\
          \ | 0 = 3 -> abort od     \n\
          \{ 0 = 0 }\n" @?= Right
      ( Struct (loopInvariant (0 === 1))
        [ Block (loopInvariant (0 === 1))
          [ Struct (Conjunct [loopInvariant (0 === 1), guardLoop (0 === 2)])
            [ Line $ loopInvariant (0 === 1) ]
          $ Postcond (loopInvariant (0 === 1))
          , Struct (Conjunct [loopInvariant (0 === 1), guardLoop (0 === 3)])
            [ Line $ Constant false ]
          $ Postcond (loopInvariant (0 === 1))
          ]
        ]
      $ Postcond (assertion (0 === 0))
      )
  , testCase "nested"
    $ run "{ 0 = 1 , bnd: A }       \n\
          \do 0 = 2 ->              \n\
          \   { 0 = 3 , bnd: A }    \n\
          \   do 0 = 4 -> abort od  \n\
          \od                       \n\
          \{ 0 = 0 }\n" @?= Right
      ( Struct (loopInvariant (0 === 1))
        [ Block (loopInvariant (0 === 1))
          [ Struct (Conjunct [loopInvariant (0 === 1), guardLoop (0 === 2)])
            []
          $ Struct (loopInvariant (0 === 3))
            [ Block (loopInvariant (0 === 3))
              [ Struct (Conjunct [loopInvariant (0 === 3), guardLoop (0 === 4)])
                [ Line $ Constant false ]
              $ Postcond (loopInvariant (0 === 3))
              ]
            ]
          $ Postcond (loopInvariant (0 === 1))
          ]
        ]
      $ Postcond (assertion (0 === 0))
      )
  ]

run :: Text -> Either [Error] Struct
run text = toNoLoc <$> (REPL.scan "<test>" text
            >>= REPL.parseProgram "<test>"
            >>= REPL.structError . runWPM . programToStruct)

--------------------------------------------------------------------------------
-- |

instance ToNoLoc Struct where
  toNoLoc (Struct pre xs ys) = Struct (toNoLoc pre) (map toNoLoc xs) (toNoLoc ys)
  toNoLoc (Postcond post) = Postcond (toNoLoc post)
instance ToNoLoc Line where
  toNoLoc (Line p) = Line (toNoLoc p)
  toNoLoc (Block p xs) = Block (toNoLoc p) (map toNoLoc xs)

instance Show Struct where
  show = show . pretty
instance Pretty Struct where
  pretty (Struct pre xs next) =
    "----------------------------------------------------------------" <> line
    <> "*" <+> pretty pre <> line
    <> vsep (map (\x -> "  " <> pretty x) xs) <> line
    <> pretty next
  pretty (Postcond post) =
    "----------------------------------------------------------------" <> line
    <> "*" <+> pretty post

instance Show Line where
  show = show . pretty
instance Pretty Line where
  pretty (Line p) = pretty p
  pretty (Block p xs) = "*" <+> pretty p <> line <> vsep (map (\x -> "  | " <> align (pretty x)) xs)
