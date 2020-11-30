{-# LANGUAGE OverloadedStrings #-}

module Test.Lexer where

import Data.Loc (unLoc)
import Data.Text.Lazy (Text)
import Language.Lexer.Applicative (streamToList)
import Syntax.Parser.Lexer (LexicalError, Tok (..), scan)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Lexer"
    [indentation, programs]

-- helper function
run :: Text -> Either LexicalError [Tok]
run text = map unLoc . streamToList <$> scan "<filepath>" text

indentation :: TestTree
indentation =
  testGroup
    "Indentation"
    [ simpleIndentation,
      complexIndentation,
      overridingTokens,
      nonIndentingTokens,
      guardedCommands
    ]

simpleIndentation :: TestTree
simpleIndentation =
  testGroup
    "Simple"
    [ testCase "top level" $ do
        let actual =
              run
                "skip\n\
                \skip\n\
                \skip\n"
        let expected = Right [TokSkip, TokNewline, TokSkip, TokNewline, TokSkip]
        actual @?= expected,
      testCase "indent" $ do
        let actual =
              run
                "do\n\
                \  skip\n\
                \  skip\n"
        let expected = Right [TokDo, TokIndent, TokSkip, TokNewline, TokSkip, TokDedent]
        actual @?= expected,
      testCase "dedent" $ do
        let actual =
              run
                "do\n\
                \  do\n\
                \    skip\n\
                \skip\n"
        let expected = Right [TokDo, TokIndent, TokDo, TokIndent, TokSkip, TokDedent, TokDedent, TokNewline, TokSkip]
        actual @?= expected,
      testCase "nested" $ do
        let actual =
              run
                "do\n\
                \  do\n\
                \    skip\n\
                \  skip\n\
                \skip\n"
        let expected = Right [TokDo, TokIndent, TokDo, TokIndent, TokSkip, TokDedent, TokNewline, TokSkip, TokDedent, TokNewline, TokSkip]
        actual @?= expected,
      testCase "consecutive dedent before EOF" $ do
        let actual =
              run
                "do\n\
                \  do\n\
                \    skip"
        let expected = Right [TokDo, TokIndent, TokDo, TokIndent, TokSkip, TokDedent, TokDedent]
        actual @?= expected
    ]

complexIndentation :: TestTree
complexIndentation =
  testGroup
    "Complex"
    [ testCase "indent" $ do
        let actual =
              run
                "do\n\
                \  skip\n\
                \  skip\n\
                \od\n"
        let expected = Right [TokDo, TokIndent, TokSkip, TokNewline, TokSkip, TokDedent, TokOd]
        actual @?= expected,
      testCase "dedent" $ do
        let actual =
              run
                "do\n\
                \  do\n\
                \    skip\n\
                \  od\n\
                \od"
        let expected = Right [TokDo, TokIndent, TokDo, TokIndent, TokSkip, TokDedent, TokOd, TokDedent, TokOd]
        actual @?= expected,
      testCase "indent on the same line" $ do
        let actual =
              run
                "do skip\n\
                \   skip\n\
                \od"
        let expected = Right [TokDo, TokIndent, TokSkip, TokNewline, TokSkip, TokDedent, TokOd]
        actual @?= expected,
      testCase "indent on the same line (with newline)" $ do
        let actual =
              run
                "do skip\n\
                \    skip\n\
                \od"
        let expected = Right [TokDo, TokIndent, TokSkip, TokSkip, TokDedent, TokOd]
        actual @?= expected,
      testCase "indent on the same line (broken)" $ do
        let actual =
              run
                "do skip\n\
                \  skip\n\
                \od"
        let expected = Right [TokDo, TokIndent, TokSkip, TokDedent, TokSkip, TokOd]
        actual @?= expected,
      testCase "dedent on the same line" $ do
        let actual =
              run
                "do\n\
                \  skip od"
        let expected = Right [TokDo, TokIndent, TokSkip, TokDedent, TokOd]
        actual @?= expected,
      testCase "dedent but in weird places 1" $ do
        let actual =
              run
                "do\n\
                \  skip\n\
                \       od"
        let expected = Right [TokDo, TokIndent, TokSkip, TokDedent, TokOd]
        actual @?= expected,
      testCase "dedent but in weird places 2" $ do
        let actual =
              run
                "do\n\
                \  skip\n\
                \ od"
        let expected = Right [TokDo, TokIndent, TokSkip, TokDedent, TokDedent, TokOd]
        actual @?= expected,
      testCase "indent and dedent on the same line" $ do
        let actual = run "do skip od"
        let expected = Right [TokDo, TokIndent, TokSkip, TokDedent, TokOd]
        actual @?= expected
    ]

overridingTokens :: TestTree
overridingTokens =
  testGroup
    "Tokens asserting override"
    [ testCase "TokDo" $ do
        let actual =
              run
                "do\n\
                \  skip\n\
                \od"
        let expected = Right [TokDo, TokIndent, TokSkip, TokDedent, TokOd]
        actual @?= expected,
      testCase "TokIf" $ do
        let actual =
              run
                "if\n\
                \  skip\n\
                \fi"
        let expected = Right [TokIf, TokIndent, TokSkip, TokDedent, TokFi]
        actual @?= expected,
      testCase "TokArror" $ do
        let actual =
              run
                "->\n\
                \  skip\n\
                \|"
        let expected = Right [TokArrow, TokIndent, TokSkip, TokDedent, TokGuardBar]
        actual @?= expected
    ]

nonIndentingTokens :: TestTree
nonIndentingTokens =
  testGroup
    "Tokens not expecting indentation"
    [ testCase "assignment" $ do
        let actual =
              run
                "a\n\
                \    :=\n\
                \  1\n"
        let expected = Right [TokLowerName "a", TokAssign, TokInt 1]
        actual @?= expected
    ]

guardedCommands :: TestTree
guardedCommands =
  testGroup
    "Guarded commands"
    [ testCase "single guarded command 1" $ do
        let actual =
              run
                "if True -> skip\n\
                \fi"
        let expected =
              Right
                [ TokIf,
                  TokIndent,
                  TokTrue,
                  TokArrow,
                  TokIndent,
                  TokSkip,
                  TokDedent,
                  TokDedent,
                  TokFi
                ]
        actual @?= expected,
      testCase "single guarded command 2 (dubious)" $ do
        let actual = run "if True -> skip fi"
        let expected =
              Right
                [ TokIf,
                  TokIndent,
                  TokTrue,
                  TokArrow,
                  TokIndent,
                  TokSkip,
                  TokDedent,
                  TokFi,
                  TokDedent
                ]
        actual @?= expected,
      testCase "multiple guarded commands" $ do
        let actual =
              run
                "if True -> skip\n\
                \ | True -> skip\n\
                \fi"
        let expected =
              Right
                [ TokIf,
                  TokIndent,
                  TokTrue,
                  TokArrow,
                  TokIndent,
                  TokSkip,
                  TokDedent,
                  TokGuardBar,
                  TokTrue,
                  TokArrow,
                  TokIndent,
                  TokSkip,
                  TokDedent,
                  TokDedent,
                  TokFi
                ]
        actual @?= expected,
      testCase "multiple guarded command on a single line" $ do
        let actual =
              run
                "if True -> skip | True -> skip\n\
                \ | True -> skip\n\
                \fi"
        let expected =
              Right
                [ TokIf,
                  TokIndent,
                  TokTrue,
                  TokArrow,
                  TokIndent,
                  TokSkip,
                  TokDedent,
                  TokGuardBar,
                  TokTrue,
                  TokArrow,
                  TokIndent,
                  TokSkip,
                  TokDedent,
                  TokGuardBar,
                  TokTrue,
                  TokArrow,
                  TokIndent,
                  TokSkip,
                  TokDedent,
                  TokDedent,
                  TokFi
                ]
        actual @?= expected,
      testCase "nested nightmare" $ do
        let actual =
              run
                "if True -> do False -> skip\n\
                \            | False -> skip\n\
                \           od\n\
                \ | True -> skip\n\
                \fi"
        let expected =
              Right
                [ TokIf,
                  TokIndent,
                  TokTrue,
                  TokArrow,
                  TokIndent,
                  TokDo,
                  TokIndent,
                  TokFalse,
                  TokArrow,
                  TokIndent,
                  TokSkip,
                  TokDedent,
                  TokGuardBar,
                  TokFalse,
                  TokArrow,
                  TokIndent,
                  TokSkip,
                  TokDedent,
                  TokDedent,
                  TokOd,
                  TokDedent,
                  TokGuardBar,
                  TokTrue,
                  TokArrow,
                  TokIndent,
                  TokSkip,
                  TokDedent,
                  TokDedent,
                  TokFi
                ]
        actual @?= expected,
      testCase "nested nightmare 2 (dubious)" $ do
        let actual =
              run
                "if True -> do False -> skip\n\
                \            | False -> skip od\n\
                \ | True -> skip\n\
                \fi"
        let expected =
              Right
                [ TokIf,
                  TokIndent,
                  TokTrue,
                  TokArrow,
                  TokIndent,
                  TokDo,
                  TokIndent,
                  TokFalse,
                  TokArrow,
                  TokIndent,
                  TokSkip,
                  TokDedent,
                  TokGuardBar,
                  TokFalse,
                  TokArrow,
                  TokIndent,
                  TokSkip,
                  TokDedent,
                  TokOd,
                  TokDedent,
                  TokDedent,
                  TokGuardBar,
                  TokTrue,
                  TokArrow,
                  TokIndent,
                  TokSkip,
                  TokDedent,
                  TokDedent,
                  TokFi
                ]
        actual @?= expected,
      testCase "multiple statements in side a guarded command" $ do
        let actual =
              run
                "if True -> skip\n\
                \           skip\n\
                \ | True -> skip\n\
                \fi"
        let expected =
              Right
                [ TokIf,
                  TokIndent,
                  TokTrue,
                  TokArrow,
                  TokIndent,
                  TokSkip,
                  TokNewline,
                  TokSkip,
                  TokDedent,
                  TokGuardBar,
                  TokTrue,
                  TokArrow,
                  TokIndent,
                  TokSkip,
                  TokDedent,
                  TokDedent,
                  TokFi
                ]
        actual @?= expected
    ]

programs :: TestTree
programs =
  testGroup
    "Programs commands"
    [ testCase "empty source file" $ do
        let actual = run ""
        let expected = Right []
        actual @?= expected,
      testCase "programs 1" $ do
        let actual =
              run
                "skip\n\
                \if True -> skip fi"
        let expected =
              Right
                [ TokSkip,
                  TokNewline,
                  TokIf,
                  TokIndent,
                  TokTrue,
                  TokArrow,
                  TokIndent,
                  TokSkip,
                  TokDedent,
                  TokFi,
                  TokDedent
                ]
        actual @?= expected,
      testCase "programs 2" $ do
        let actual =
              run
                "con A, B : Int\n\
                \var x, y : Int\n\
                \{ z = A * B }"
        let expected =
              Right
                [ TokCon,
                  TokUpperName "A",
                  TokComma,
                  TokUpperName "B",
                  TokColon,
                  TokUpperName "Int",
                  TokNewline,
                  TokVar,
                  TokLowerName "x",
                  TokComma,
                  TokLowerName "y",
                  TokColon,
                  TokUpperName "Int",
                  TokNewline,
                  TokBraceStart,
                  TokLowerName "z",
                  TokEQ,
                  TokUpperName "A",
                  TokMul,
                  TokUpperName "B",
                  TokBraceEnd
                ]
        actual @?= expected
    ]
