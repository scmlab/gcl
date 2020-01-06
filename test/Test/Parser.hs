{-# LANGUAGE OverloadedStrings #-}

module Test.Parser where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Text.Lazy (Text)
import Data.Loc
import Prelude hiding (Ordering(..))

import qualified Data.Text.Lazy.IO as Text

import qualified Syntax.Parser as Parser
import qualified REPL as REPL
import Syntax.Parser (Parser)
import Syntax.Abstract
import Error

tests :: TestTree
tests = testGroup "Parser"
  [ expression
  , type'
  , declaration
  , statement
  , program
  ]

--------------------------------------------------------------------------------
-- | Helpers

data TestCase b
  = RightCase String Text b
  | LeftCase String Text [Error]
  | ReadFile String FilePath (Either [Error] Program)

toTestTree :: (Eq b, Show b, FromConcrete a b) => Parser a -> TestCase b -> TestTree
toTestTree parser (RightCase name text expected) = testCase name $ do
  let actual = parse parser text
  actual @?= Right expected
toTestTree parser (LeftCase name text expected) = testCase name $ do
  let actual = parse parser text
  actual @?= Left expected
toTestTree _ (ReadFile name filepath expected) = testCase name $ do
  text <- Text.readFile filepath
  let actual = parse Parser.program text
  actual @?= expected

parse :: FromConcrete a b => Parser a -> Text -> Either [Error] b
parse parser text = REPL.scan "<test>" text
        >>= REPL.parse parser "<text>"
        >>= REPL.abstract

--------------------------------------------------------------------------------
-- | Expression

expression :: TestTree
expression = testGroup "Expressions" $ map (toTestTree Parser.expression)
  [ RightCase "literal (numbers)"
      "1"
      $ Lit (Num 1)
  , RightCase "literal (True)"
      "True"
      $ Lit (Bol True)
  , RightCase "literal (False)"
      "False"
      $ Lit (Bol False)
  , RightCase "variable"
      "x"
      $ Var "x"
  , RightCase "constant"
      "X"
      $ Const "X"
  , RightCase "numeric 1"
      "(1 + (1))"
      $ bin Add (Lit (Num 1)) (Lit (Num 1))
  , RightCase "numeric 2"
      "A + X * Y"
      $ bin Add
          (Const "A")
          (bin Mul (Const "X") (Const "Y"))
  , RightCase "numeric 3"
      "(A + X) * Y % 2"
      $ bin Mul
          (bin Add (Const "A") (Const "X"))
          (bin Mod (Const "Y") (Lit (Num 2)))
  , RightCase "relation (EQ)"
      "A = B"
      $ bin EQ (Const "A") (Const "B")
  , RightCase "relation (NEQ)"
      "A /= B"
      $ bin NEQ (Const "A") (Const "B")
  , RightCase "relation (LT)"
      "A < B"
      $ bin LT (Const "A") (Const "B")
  , RightCase "relation (LTE)"
      "A <= B"
      $ bin LTE (Const "A") (Const "B")
  , RightCase "relation (GT)"
      "A > B"
      $ bin GT (Const "A") (Const "B")
  , RightCase "relation (GTE)"
      "A >= B"
      $ bin GTE (Const "A") (Const "B")
  , RightCase "boolean (Conj)"
      "A && B"
      $ bin Conj (Const "A") (Const "B")
  , RightCase "boolean (Disj)"
      "A || B"
      $ bin Disj (Const "A") (Const "B")
  , RightCase "boolean (Implies)"
      "A => B"
      $ bin Implies (Const "A") (Const "B")
  , RightCase "boolean (Neg)"
      "~ A"
      $ un Neg (Const "A")
  , RightCase "boolean 1"
      "A || B => C"
      $ bin Implies
          (bin Disj (Const "A") (Const "B"))
          (Const "C")
  , RightCase "boolean 2"
      "A || (B => C)"
      $ bin Disj
          (Const "A")
          (bin Implies (Const "B") (Const "C"))
  , RightCase "boolean 3"
      "A || B && C"
      $ bin Disj
          (Const "A")
          (bin Conj (Const "B") (Const "C"))
  , RightCase "boolean 4"
      "B && C || A"
      $ bin Disj
          (bin Conj (Const "B") (Const "C"))
          (Const "A")
  , RightCase "function application 1"
      "(f (x)) y"
      $ App
          (App (Var "f") (Var "x"))
          (Var "y")
  , RightCase "function application 2"
      "f (x y)"
      $ App
          (Var "f")
          (App (Var "x") (Var "y"))
  , RightCase "mixed 1"
      "X * Y = N"
      $ bin EQ
          (bin Mul (Const "X") (Const "Y"))
          (Const "N")
  , RightCase "mixed 2"
      "X * Y => P = Q"
      $ bin Implies
          (bin Mul (Const "X") (Const "Y"))
          (bin EQ
            (Const "P")
            (Const "Q"))
    , RightCase "mixed 3"
        "X > Y && X > Y"
        $ bin Conj
            (bin GT (Const "X") (Const "Y"))
            (bin GT (Const "X") (Const "Y"))
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
  [ RightCase "base types (Int)"
      "Int"
      $ TBase TInt
  , RightCase "base types (Bool)"
      "Bool"
      $ TBase TBool
  , RightCase "base types (Char)"
      "Char"
      $ TBase TChar
  , RightCase "function types 1"
      "(Char -> (Int))"
      $ TFunc (TBase TChar) (TBase TInt)
  , RightCase "function types 2"
      "(Char -> Int) -> Int"
      $ TFunc (TFunc (TBase TChar) (TBase TInt)) (TBase TInt)
  , RightCase "array"
      "array [0 .. N) of Int"
      $ TArray
          (Interval (Including (Lit (Num 0))) (Excluding (Const "N")))
          (TBase TInt)
  ]

--------------------------------------------------------------------------------
-- | Declaration

declaration :: TestTree
declaration = testGroup "Declarations" $ map (toTestTree Parser.declaration)
  [ RightCase "variable"
      "var x : Int\n"
      $ VarDecl ["x"] (TBase TInt)
  , RightCase "constant"
      "con X, Y : Int\n"
      $ ConstDecl ["X", "Y"] (TBase TInt)
  ]

--------------------------------------------------------------------------------
-- | Statements

statement :: TestTree
statement = testGroup "Statements" $ map (toTestTree Parser.statement)
  [ RightCase "skip"
      "skip\n"
      $ Skip (loc 4)
  , RightCase "abort"
      "abort\n"
      $ Abort (loc 5)
  , RightCase "assert"
      "{ True }\n"
      $ Assert (Lit (Bol True)) (loc 8)
  , LeftCase "spec"
      "?\n"
      $ [ConvertError (DigHole (loc 1))]
  , RightCase "assign"
      "x := 0\n"
      $ Assign ["x"] [Lit (Num 0)] (loc 6)
  , RightCase "assign (parallel)"
      "x, y := 0, 1\n"
      $ Assign ["x", "y"] [Lit (Num 0), Lit (Num 1)] (loc 12)
  ]

  where
    loc :: Int -> Loc
    loc len = Loc (Pos "<test>" 1 1 0) (Pos "<test>" 1 len (len - 1))

--------------------------------------------------------------------------------
-- | Program

program :: TestTree
program = testGroup "Program" $ map (toTestTree Parser.program)
  [ ReadFile "empty" "./test/source/empty.gcl"
      $ Right $ Program [] Nothing
  ]
