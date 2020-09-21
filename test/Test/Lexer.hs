{-# LANGUAGE OverloadedStrings #-}

module Test.Lexer where

import Test.Tasty ( testGroup, TestTree )
import Test.Tasty.HUnit ((@?=), testCase)

import Language.Lexer.Applicative (streamToList, TokenStream(TsEof))
import Data.Text.Lazy (Text)
import Data.Loc (unLoc, L)

import Syntax.Parser.Lexer (Tok(..), LexicalError, scan)

tests :: TestTree
tests = testGroup
    "Lexer"
    [indentation, empty]


-- helper function
run :: Text -> Either LexicalError [Tok]
run text = map unLoc  . streamToList <$> scan "<filepath>" text

empty :: TestTree
empty = testCase "Empty" $ do 
    let actual = run ""
    let expected = Right []
    actual @?= expected


indentation :: TestTree
indentation = testGroup "Indentation" 
    [
        testCase "top level" $ do 
        let actual = run    "skip\n\
                            \skip\n\
                            \skip\n"
        let expected = Right [TokSkip, TokNewline, TokSkip, TokNewline, TokSkip, TokNewline]
        actual @?= expected
    ,   testCase "indent" $ do 
        let actual = run    "do\n\
                            \  skip\n\
                            \  skip\n"
        let expected = Right [TokDo, TokIndent, TokSkip, TokNewline, TokSkip, TokDedent, TokNewline]
        actual @?= expected
    ,   testCase "indent on the same line" $ do 
        let actual = run    "do  skip\n\
                            \    skip\n\
                            \    skip\n"
        let expected = Right [TokDo, TokIndent, TokSkip, TokNewline, TokSkip, TokNewline, TokSkip, TokDedent, TokNewline]
        actual @?= expected
    ,   testCase "indent and dedent on the same line" $ do 
        let actual = run    "do  skip\n\
                            \skip\n\
                            \skip\n"
        let expected = Right [TokDo, TokIndent, TokSkip, TokDedent, TokNewline, TokSkip, TokNewline, TokSkip, TokNewline]
        actual @?= expected
    ,   testCase "nested" $ do 
        let actual = run    "do\n\
                            \  do\n\
                            \    skip\n\
                            \  skip\n"
        let expected = Right [TokDo, TokIndent, TokDo, TokIndent, TokSkip, TokDedent, TokNewline, TokSkip, TokDedent, TokNewline]
        actual @?= expected
    ,   testCase "consecutive dedent" $ do 
        let actual = run    "do\n\
                            \  do\n\
                            \    skip\n\
                            \skip\n"
        let expected = Right [TokDo, TokIndent, TokDo, TokIndent, TokSkip, TokDedent, TokDedent, TokNewline, TokSkip, TokNewline]
        actual @?= expected
    ]