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
    [indentation]
    -- [indentation, indentingTokens, nonIndentingTokens, guardedCommands, empty]

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
                            \  skip\n\
                            \od"
        let expected = Right [TokDo, TokIndent, TokSkip, TokNewline, TokSkip, TokDedent, TokOd]
        actual @?= expected
    ,   testCase "indent on the same line" $ do 
        let actual = run    "do skip\n\
                            \od"
        let expected = Right [TokDo, TokIndent, TokSkip, TokDedent, TokOd]
        actual @?= expected
    ,   testCase "dedent on the same line" $ do 
        let actual = run    "do\n\
                            \  skip od"
        let expected = Right [TokDo, TokIndent, TokSkip, TokDedent, TokOd]
        actual @?= expected
    ,   testCase "indent and dedent on the same line" $ do 
        let actual = run    "do skip od"
        let expected = Right [TokDo, TokIndent, TokSkip, TokDedent, TokOd]
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
    ,   testCase "TokArror" $ do 
        let actual = run    "->\n\
                            \  skip\n\
                            \  skip\n"
        let expected = Right [TokArrow, TokIndent, TokSkip, TokNewline, TokSkip, TokDedent, TokNewline]
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
    [   testCase "single guarded command" $ do 
        let actual = run    "if True -> skip\n\
                            \fi"
        let expected = Right    [   TokIf, TokIndent
                                    ,   TokTrue, TokArrow, TokIndent
                                        , TokSkip, TokDedent, TokDedent, TokNewline
                                ,   TokFi
                                ]
        actual @?= expected
    ,   testCase "multiple guarded commands" $ do 
        let actual = run    "if True -> skip\n\
                            \ | True -> skip\n\
                            \fi"
        let expected = Right    [   TokIf, TokIndent
                                    ,   TokTrue, TokArrow, TokIndent
                                        ,   TokSkip, TokDedent, TokNewline
                                    ,   TokGuardBar, TokTrue, TokArrow, TokIndent
                                        ,   TokSkip, TokDedent, TokDedent, TokNewline
                                ,   TokFi
                                ]
        actual @?= expected     
    -- ,   testCase "multiple guarded command on a single line" $ do 
    --     let actual = run    "if True -> skip | True -> skip\n\
    --                         \ | True -> skip\n\
    --                         \fi"
    --     let expected = Right    [   TokIf, TokIndent
    --                                 ,   TokTrue, TokArrow, TokIndent
    --                                     ,   TokSkip, TokDedent
    --                                 ,   TokGuardBar, TokTrue, TokArrow, TokIndent
    --                                     ,   TokSkip, TokDedent, TokNewline
    --                                 ,   TokGuardBar, TokTrue, TokArrow, TokIndent
    --                                     ,   TokSkip, TokDedent, TokDedent, TokNewline
    --                             ,   TokFi
    --                             ]
    --     actual @?= expected      
    ,   testCase "nested nightmare" $ do 
        let actual = run    "if True -> do False -> skip\n\
                            \            | False -> skip\n\
                            \ | True -> skip\n\
                            \fi"
        let expected = Right    [   TokIf, TokIndent
                                    ,   TokTrue, TokArrow, TokIndent
                                        , TokDo, TokIndent
                                            ,   TokFalse, TokArrow, TokIndent
                                                ,   TokSkip, TokDedent, TokNewline
                                            ,   TokGuardBar, TokFalse, TokArrow, TokIndent
                                                , TokSkip, TokDedent, TokDedent, TokDedent, TokNewline
                                    ,   TokGuardBar, TokTrue, TokArrow, TokIndent
                                        , TokSkip, TokDedent, TokDedent, TokNewline
                                ,   TokFi]
        actual @?= expected
    ,   testCase "multiple statements in side a guarded command" $ do 
        let actual = run    "if True -> skip\n\
                            \           skip\n\
                            \ | True -> skip\n\
                            \fi"
        let expected = Right    [   TokIf, TokIndent
                                    ,   TokTrue, TokArrow, TokIndent
                                        ,   TokSkip, TokNewline, 
                                            TokSkip, TokDedent, TokNewline
                                    ,   TokGuardBar, TokTrue, TokArrow, TokIndent
                                        ,   TokSkip, TokDedent, TokDedent, TokNewline
                                ,   TokFi]
        actual @?= expected     
    
    ]