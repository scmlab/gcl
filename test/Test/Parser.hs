{-# LANGUAGE OverloadedStrings #-}

module Test.Parser where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Test.Tasty
import Test.Tasty.HUnit
import Data.Text.Lazy (Text)
import Prelude hiding (Ordering(..))

import Test.Util
import Syntax.Parser.Lexer (scan)
import qualified Syntax.Parser as Parser
import Syntax.Parser (Parser)
import Syntax.Abstract

tests :: TestTree
tests = testGroup "Parser"
  [ expression
  , type'
  ]

--------------------------------------------------------------------------------
-- | Helpers

data TestCase b = TestCase String Text b

toTestTree :: (Eq b, Show b, FromConcrete a b) => Parser a -> TestCase b -> TestTree
toTestTree parser (TestCase name text expected) = testCase name $ do
  let actual = parse parser text
  actual @?= Just expected

parse :: FromConcrete a b => Parser a -> Text -> Maybe b
parse parser text = case scan "<test>" text of
  Left _ -> Nothing
  Right tokens -> case Parser.parse parser "<test>" tokens of
    Left _ -> Nothing
    Right result -> case abstract result of
      Left _ -> Nothing
      Right syntax -> Just syntax

--------------------------------------------------------------------------------
-- | Expression

expression :: TestTree
expression = testGroup "Expressions" $ map (toTestTree Parser.expression)
  [ TestCase "literal (numbers)"
      "1"
      $ Lit (Num 1)
  ,  TestCase "literal (True)"
      "True"
      $ Lit (Bol True)
  ,  TestCase "literal (False)"
      "False"
      $ Lit (Bol False)
  ,  TestCase "variable"
      "x"
      $ Var "x"
  ,  TestCase "constant"
      "X"
      $ Const "X"
  , TestCase "numeric 1"
      "(1 + (1))"
      $ bin Add (Lit (Num 1)) (Lit (Num 1))
  , TestCase "numeric 2"
      "A + X * Y"
      $ bin Add
          (Const "A")
          (bin Mul (Const "X") (Const "Y"))
  , TestCase "numeric 3"
      "(A + X) * Y"
      $ bin Mul
          (bin Add (Const "A") (Const "X"))
          (Const "Y")
  , TestCase "relation (EQ)"
      "A = B"
      $ bin EQ (Const "A") (Const "B")
  , TestCase "relation (NEQ)"
      "A /= B"
      $ bin NEQ (Const "A") (Const "B")
  , TestCase "relation (LT)"
      "A < B"
      $ bin LT (Const "A") (Const "B")
  , TestCase "relation (LTE)"
      "A <= B"
      $ bin LTE (Const "A") (Const "B")
  , TestCase "relation (GT)"
      "A > B"
      $ bin GT (Const "A") (Const "B")
  , TestCase "relation (GTE)"
      "A >= B"
      $ bin GTE (Const "A") (Const "B")
  , TestCase "boolean (Conj)"
      "A && B"
      $ bin Conj (Const "A") (Const "B")
  , TestCase "boolean (Disj)"
      "A || B"
      $ bin Disj (Const "A") (Const "B")
  , TestCase "boolean (Implies)"
      "A => B"
      $ bin Implies (Const "A") (Const "B")
  , TestCase "boolean (Neg)"
      "~ A"
      $ un Neg (Const "A")
  , TestCase "boolean 1"
      "A || B => C"
      $ bin Implies
          (bin Disj (Const "A") (Const "B"))
          (Const "C")
  , TestCase "boolean 2"
      "A || (B => C)"
      $ bin Disj
          (Const "A")
          (bin Implies (Const "B") (Const "C"))
  , TestCase "boolean 3"
      "A || B && C"
      $ bin Disj
          (Const "A")
          (bin Conj (Const "B") (Const "C"))
  , TestCase "boolean 4"
      "B && C || A"
      $ bin Disj
          (bin Conj (Const "B") (Const "C"))
          (Const "A")
  , TestCase "function application 1"
      "(f (x)) y"
      $ App
          (App (Var "f") (Var "x"))
          (Var "y")
  , TestCase "function application 2"
      "f (x y)"
      $ App
          (Var "f")
          (App (Var "x") (Var "y"))
  ]
  where
    bin :: Op -> Expr -> Expr -> Expr
    bin op a b = App (App (Op op) a) b

    un :: Op -> Expr -> Expr
    un op a = App (Op op) a

--------------------------------------------------------------------------------
-- | Type

type' :: TestTree
type' = testGroup "Types" $ map (toTestTree Parser.type')
  [ TestCase "base types (Int)"
      "Int"
      $ TBase TInt
  , TestCase "base types (Bool)"
      "Bool"
      $ TBase TBool
  , TestCase "base types (Char)"
      "Char"
      $ TBase TChar
  ]
