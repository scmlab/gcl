{-# LANGUAGE OverloadedStrings #-}

module Test.WP2.Struct where

import           Data.Loc
import           Data.Text.Lazy          hiding ( map )
import           Test.Tasty
import           Test.Tasty.HUnit
import           Prelude                 hiding ( Ordering(..) )

import           Syntax.Predicate
import           Syntax.Concrete                ( true
                                                , false
                                                , variable
                                                , number
                                                , eqq
                                                , constant
                                                )
import qualified REPL                          as REPL

import           Syntax.Location
import           Error
import           Pretty                         ( )

tests :: TestTree
tests = testGroup "WP2 Struct" [emptyProg, statements, assertions, if', loop]

emptyProg :: TestTree
emptyProg = testGroup
  "empty" 
  [ testCase "empty" $ do
    actual <- run ""
    actual @?= Right Nothing 
  ]
  
statements :: TestTree
statements = testGroup
  "simple statements"
  [ testCase "skip" $ do
    actual <-
      run
        "{ True }   \n\
                    \skip       \n\
                    \{ 0 = 0 }"
    actual @?= Right
      ( Just 
      $ Struct (assertion true) [Skip $ loc (assertion (0 === 0))]
      $ Postcond (assertion (0 === 0))
      )
  , testCase "abort" $ do
    actual <-
      run
        "{ True }   \n\
                    \abort      \n\
                    \{ True }"
    actual @?= Right
      ( Just 
      $ Struct (assertion true) [Abort $ loc (Constant false)]
      $ Postcond (assertion true)
      )
  , testCase "assignment" $ do
    actual <-
      run
        "{ True }   \n\
                    \x := 1     \n\
                    \{ 0 = x }"
    actual @?= Right
      ( Just 
      $ Struct
          (assertion true)
          [ Assign (loc (assertion (number 0 `eqq` number 1)))
                   undefined
                   undefined
          ]
      $ Postcond (assertion (number 0 `eqq` variable "x"))
      )
  , testCase "spec" $ do
    actual <-
      run
        "{ True }   \n\
                    \{!       \n\
                    \!}       \n\
                    \{ 0 = 0 }"
    actual @?= Right
      ( Just 
      $ Struct (assertion true)
               [Spec (loc (assertion true)) (assertion (0 === 0))]
      $ Postcond (assertion (0 === 0))
      )
  ]

assertions :: TestTree
assertions = testGroup
  "assertions"
  [ testCase "2 assertions" $ do
    actual <- run "{ 0 = 0 }\n{ 0 = 1 }"
    actual @?= Right
      (Just $ Struct (assertion (0 === 0)) [] $ Postcond (assertion (0 === 1)))
  , testCase "3 assertions" $ do
    actual <- run "{ 0 = 0 }\n{ 0 = 1 }\n{ 0 = 2 }"
    actual @?= Right
      (Just 
      $ Struct (assertion (0 === 0)) []
      $ Struct (assertion (0 === 1)) []
      $ Postcond (assertion (0 === 2))
      )
  ]


if' :: TestTree
if' = testGroup
  "if statements"
  [ testCase "without precondition" $ do
    actual <-
      run
        "{ True }             \n\
                  \if 0 = 0 -> skip     \n\
                  \ | 0 = 1 -> abort    \n\
                  \fi                   \n\
                  \{ 0 = 2 }            \n"
    actual @?= Right
      ( Just $ Struct
          (assertion true)
          [ If
              (loc $ Disjunct [guardIf (0 === 0), guardIf (0 === 1)])
              [ GdCmd (guardIf (0 === 0))
              $ Struct (Conjunct [assertion true, guardIf (0 === 0)])
                       [Skip $ loc (assertion (0 === 2))]
              $ Postcond (assertion (0 === 2))
              , GdCmd (guardIf (0 === 1))
              $ Struct (Conjunct [assertion true, guardIf (0 === 1)])
                       [Abort $ loc (Constant false)]
              $ Postcond (assertion (0 === 2))
              ]
          ]
      $ Postcond (assertion (0 === 2))
      )
  , testCase "without precondition (nested)" $ do
    actual <-
      run
        "{ True }               \n\
                  \if 0 = 0 ->            \n\
                  \     if 0 = 1 -> skip  \n\
                  \     fi                \n\
                  \fi                     \n\
                  \{ 0 = 2 }\n"
    actual @?= Right
      ( Just $ Struct
          (assertion true)
          [ If
              (loc $ guardIf (0 === 0))
              [ GdCmd (guardIf (0 === 0))
                $ Struct
                    (Conjunct [assertion true, guardIf (0 === 0)])
                    [ If
                        (loc $ guardIf (0 === 1))
                        [ GdCmd (guardIf (0 === 1))
                          $ Struct
                              (Conjunct
                                [ assertion true
                                , guardIf (0 === 0)
                                , guardIf (0 === 1)
                                ]
                              )
                              [Skip $ loc (assertion (0 === 2))]
                          $ Postcond (assertion (0 === 2))
                        ]
                    ]
                $ Postcond (assertion (0 === 2))
              ]
          ]
      $ Postcond (assertion (0 === 2))
      )
  , testCase "without precondition (with assertion in branches)" $ do
    actual <-
      run
        "{ True }               \n\
                  \if 0 = 0 ->            \n\
                  \    { 0 = 1 }          \n\
                  \    skip               \n\
                  \fi                     \n\
                  \{ 0 = 2 }\n"
    actual @?= Right
      ( Just $ Struct
          (assertion true)
          [ If
              (loc $ guardIf (0 === 0))
              [ GdCmd (guardIf (0 === 0))
                $ Struct (Conjunct [assertion true, guardIf (0 === 0)]) []
                $ Struct (assertion (0 === 1))
                         [Skip $ loc (assertion (0 === 2))]
                $ Postcond (assertion (0 === 2))
              ]
          ]
      $ Postcond (assertion (0 === 2))
      )
  , testCase "with precondition 1" $ do
    actual <-
      run
        "{ 0 = 0 }          \n\
                  \if 0 = 1 -> skip fi\n\
                  \{ 0 = 2 }          \n"
    actual @?= Right
      ( Just $ Struct
          (assertion (0 === 0))
          [ If
              (loc $ guardIf (0 === 1))
              [ GdCmd (guardIf (0 === 1))
                $ Struct (Conjunct [assertion (0 === 0), guardIf (0 === 1)])
                         [Skip $ loc (assertion (0 === 2))]
                $ Postcond (assertion (0 === 2))
              ]
          ]
      $ Postcond (assertion (0 === 2))
      )
  , testCase "with precondition 2" $ do
    actual <-
      run
        "{ 0 = 0 }          \n\
                  \if 0 = 1 -> skip   \n\
                  \ | 0 = 2 -> abort  \n\
                  \fi                 \n\
                  \{ 0 = 3 }          \n"
    actual @?= Right
      ( Just $ Struct
          (assertion (0 === 0))
          [ If
              (loc $ Disjunct [guardIf (0 === 1), guardIf (0 === 2)])
              [ GdCmd (guardIf (0 === 1))
              $ Struct (Conjunct [assertion (0 === 0), guardIf (0 === 1)])
                       [Skip $ loc (assertion (0 === 3))]
              $ Postcond (assertion (0 === 3))
              , GdCmd (guardIf (0 === 2))
              $ Struct (Conjunct [assertion (0 === 0), guardIf (0 === 2)])
                       [Abort $ loc (Constant false)]
              $ Postcond (assertion (0 === 3))
              ]
          ]
      $ Postcond (assertion (0 === 3))
      )
  ]

loop :: TestTree
loop = testGroup
  "loop statements"
  [ testCase "1 branch" $ do
    actual <-
      run
        "{ 0 = 1 , bnd: A }     \n\
                  \do 0 = 2 -> skip       \n\
                  \od                     \n\
                  \{ 0 = 0 }              \n"
    actual @?= Right
      ( Just $ Struct
          (loopInvariant (0 === 1) "A")
          [ Do
              (loc $ loopInvariant (0 === 1) "A")
              (constant "A")
              [ GdCmd (guardLoop (0 === 2))
                $ Struct
                    (Conjunct [loopInvariant (0 === 1) "A", guardLoop (0 === 2)]
                    )
                    [Skip (loc $ loopInvariant (0 === 1) "A")]
                $ Postcond (loopInvariant (0 === 1) "A")
              ]
          ]
      $ Postcond (assertion (0 === 0))
      )
  , testCase "2 branches" $ do
    actual <-
      run
        "{ 0 = 1 , bnd: A }       \n\
                  \do 0 = 2 -> skip         \n\
                  \ | 0 = 3 -> abort od     \n\
                  \{ 0 = 0 }\n"
    actual @?= Right
      ( Just $ Struct
          (loopInvariant (0 === 1) "A")
          [ Do
              (loc $ loopInvariant (0 === 1) "A")
              (constant "A")
              [ GdCmd (guardLoop (0 === 2))
              $ Struct
                  (Conjunct [loopInvariant (0 === 1) "A", guardLoop (0 === 2)])
                  [Skip (loc $ loopInvariant (0 === 1) "A")]
              $ Postcond (loopInvariant (0 === 1) "A")
              , GdCmd (guardLoop (0 === 3))
              $ Struct
                  (Conjunct [loopInvariant (0 === 1) "A", guardLoop (0 === 3)])
                  [Abort (loc $ Constant false)]
              $ Postcond (loopInvariant (0 === 1) "A")
              ]
          ]
      $ Postcond (assertion (0 === 0))
      )
  , testCase "nested" $ do
    actual <-
      run
        "{ 0 = 1 , bnd: A }       \n\
        \do 0 = 2 ->              \n\
        \     { 0 = 3 , bnd: A }    \n\
        \     do 0 = 4 -> abort od  \n\
        \od                       \n\
        \{ 0 = 0 }\n"
    actual @?= Right
      ( Just $ Struct
          (loopInvariant (0 === 1) "A")
          [ Do
              (loc $ loopInvariant (0 === 1) "A")
              (constant "A")
              [ GdCmd (guardLoop (0 === 2))
                $ Struct
                    (Conjunct [loopInvariant (0 === 1) "A", guardLoop (0 === 2)]
                    )
                    []
                $ Struct
                    (loopInvariant (0 === 3) "A")
                    [ Do
                        (loc $ loopInvariant (0 === 3) "A")
                        (constant "A")
                        [ GdCmd (guardLoop (0 === 4))
                          $ Struct
                              (Conjunct
                                [ loopInvariant (0 === 3) "A"
                                , guardLoop (0 === 4)
                                ]
                              )
                              [Abort (loc $ Constant false)]
                          $ Postcond (loopInvariant (0 === 3) "A")
                        ]
                    ]
                $ Postcond (loopInvariant (0 === 1) "A")
              ]
          ]
      $ Postcond (assertion (0 === 0))
      )
  ]

loc :: Pred -> L Pred
loc = L NoLoc

run :: Text -> IO (Either Error (Maybe Struct))
run text = REPL.runREPLM $ do 
  struct <- REPL.scan "<test>" text
        >>= REPL.parseProgram "<test>"
        >>= REPL.toStruct
  return $ fmap toNoLoc struct

--------------------------------------------------------------------------------
-- |
--
instance ToNoLoc Struct where
  toNoLoc (Struct pre xs ys) =
    Struct (toNoLoc pre) (map toNoLoc xs) (toNoLoc ys)
  toNoLoc (Postcond post) = Postcond (toNoLoc post)

instance ToNoLoc GdCmd where
  toNoLoc (GdCmd p xs) = GdCmd (toNoLoc p) (toNoLoc xs)

instance ToNoLoc Stmt where
  toNoLoc (Skip  l) = Skip (toNoLoc l)
  toNoLoc (Abort l) = Abort (toNoLoc l)
  toNoLoc (Assign l xs es) =
    Assign (toNoLoc l) (map toNoLoc xs) (map toNoLoc es)
  toNoLoc (Do l bnd xs) = Do (toNoLoc l) (toNoLoc bnd) (map toNoLoc xs)
  toNoLoc (If   l xs  ) = If (toNoLoc l) (map toNoLoc xs)
  toNoLoc (Spec l p   ) = Spec (toNoLoc l) (toNoLoc p)
