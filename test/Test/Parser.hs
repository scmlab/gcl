{-# LANGUAGE OverloadedStrings #-}

module Test.Parser where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Test.Tasty
import Test.Tasty.HUnit
import Data.Text.Lazy (Text)

import Test.Util
import Syntax.Parser.Lexer (scan)
import qualified Syntax.Parser as Parser
import Syntax.Parser (Parser)
import Syntax.Abstract

parse :: FromConcrete a b => Parser a -> Text -> Maybe b
parse parser text = case scan "<test>" text of
  Left _ -> Nothing
  Right tokens -> case Parser.parse parser "<test>" tokens of
    Left _ -> Nothing
    Right result -> case abstract result of
      Left _ -> Nothing
      Right syntax -> Just syntax

tests :: TestTree
tests = testGroup "Parser"
  [ expression
  ]

data TestCase b = TestCase String Text b

toTestTree :: (Eq b, Show b, FromConcrete a b) => Parser a -> TestCase b -> TestTree
toTestTree parser (TestCase name text expected) = testCase name $ do
  let actual = parse parser text
  actual @?= Just expected

expression :: TestTree
expression = testGroup "Expressions" $ map (toTestTree Parser.expression)
  [ TestCase "literal 1"
      "1"
      $ Lit (Num 1)
  ,  TestCase "literal 2"
      "True"
      $ Lit (Bol True)
  ,  TestCase "literal 3"
      "False"
      $ Lit (Bol False)
  ,  TestCase "variable"
      "x"
      $ Var "x"
  ,  TestCase "constant"
      "X"
      $ Const "X"
  , TestCase "numerical 1"
      "(1 + (1))"
      $ add (Lit (Num 1)) (Lit (Num 1))
  , TestCase "numerical 1"
      "A + X * Y"
      $ add
          (Const "A")
          (mul (Const "X") (Const "Y"))
  ]
  where
    binary :: Op -> Expr -> Expr -> Expr
    binary op a b = App (App (Op op) a) b

    add :: Expr -> Expr -> Expr
    add = binary Add

    sub :: Expr -> Expr -> Expr
    sub = binary Sub

    div :: Expr -> Expr -> Expr
    div = binary Div

    mul :: Expr -> Expr -> Expr
    mul = binary Mul
