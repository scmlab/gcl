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
indentation = testGroup "Indentation" [testCase "top level" $ do 
    let actual = run "skip\n  skip\n    skip\n"
    let expected = Right [TokSkip, TokNewlineAndWhitespace 2, TokSkip, TokNewlineAndWhitespace 4,TokSkip, TokNewlineAndWhitespace 0]
    actual @?= expected
    ]