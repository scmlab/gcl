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
    [indentation, indentingTokens, nonIndentingTokens, guardedCommands, empty]

-- helper function
run :: Text -> Either LexicalError [Tok]
run text = map unLoc  . streamToList <$> scan "<filepath>" text

empty :: TestTree
empty = testCase "empty source file" $ do 
    let actual = run ""
    let expected = Right []
    actual @?= expected


indentation :: TestTree
indentation = testGroup "Indentation" 
    [   testCase "top level" $ do 
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

indentingTokens :: TestTree
indentingTokens = testGroup "Tokens expecting indentation" 
    [   testCase "TokDo" $ do 
        let actual = run    "do\n\
                            \  skip\n\
                            \  skip\n"
        let expected = Right [TokDo, TokIndent, TokSkip, TokNewline, TokSkip, TokDedent, TokNewline]
        actual @?= expected   
    ,   testCase "TokIf" $ do 
        let actual = run    "if\n\
                            \  skip\n\
                            \  skip\n"
        let expected = Right [TokIf, TokIndent, TokSkip, TokNewline, TokSkip, TokDedent, TokNewline]
        actual @?= expected  
    ]

nonIndentingTokens :: TestTree
nonIndentingTokens = testGroup "Tokens not expecting indentation" 
    [   testCase "assignment" $ do 
        let actual = run    "a\n\
                            \    :=\n\
                            \  1\n"
        let expected = Right [TokLowerName "a", TokAssign, TokInt 1, TokNewline]
        actual @?= expected   
    ]

guardedCommands :: TestTree
guardedCommands = testGroup "Guarded commands" 
    [   testCase "single line" $ do 
        let actual = run    "if True -> skip fi"
        let expected = Right [TokIf, TokIndent, TokTrue, TokArrow, TokSkip, TokFi]
        actual @?= expected      
    ,   testCase "multiline" $ do 
        let actual = run    "if True -> skip\n\
                            \ | True -> skip\n\
                            \fi"
        let expected = Right [TokIf, TokIndent, TokTrue, TokArrow, TokSkip, TokNewline, TokGuardBar, TokTrue, TokArrow, TokSkip, TokDedent, TokNewline, TokFi]
        actual @?= expected     
    ,   testCase "multiple guarded command on a single line" $ do 
        let actual = run    "if True -> skip | True -> skip\n\
                            \ | True -> skip\n\
                            \fi"
        let expected = Right [TokIf, TokIndent, TokTrue, TokArrow, TokSkip, TokGuardBar, TokTrue, TokArrow, TokSkip, TokNewline, TokGuardBar, TokTrue, TokArrow, TokSkip, TokDedent, TokNewline, TokFi]
        actual @?= expected      
    ,   testCase "nested nightmare" $ do 
        let actual = run    "if True -> do False -> skip\n\
                            \            | False -> skip\n\
                            \ | True -> skip\n\
                            \fi"
        let expected = Right [TokIf, TokIndent, TokTrue, TokArrow, TokDo, TokIndent, TokFalse, TokArrow, TokSkip, TokNewline, TokGuardBar, TokFalse, TokArrow, TokSkip, TokDedent, TokNewline, TokGuardBar, TokTrue, TokArrow, TokSkip, TokDedent, TokNewline, TokFi]
        actual @?= expected
    ]