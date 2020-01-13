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
-- import Syntax.Abstract
import Syntax.Concrete
  -- hiding (Expr(..), Lit(..), Base(..), Type(..), Interval(..), Endpoint(..), Op(..))
import Error

tests :: TestTree
tests = testGroup "Parser"
  [
  -- expression
  -- ,
  type'
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

toTestTree :: (Eq a, Show a) => Parser a -> TestCase a -> TestTree
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

parse :: Parser a -> Text -> Either [Error] a
parse parser text = REPL.scan "<test>" text
        >>= REPL.parse parser "<text>"
        -- >>= REPL.abstract

--------------------------------------------------------------------------------
-- | Expression

-- expression :: TestTree
-- expression = testGroup "Expressions" $ map (toTestTree Parser.expression)
--   [ RightCase "literal (numbers)"
--       "1"
--       $ Lit (Num 1)
--   , RightCase "literal (True)"
--       "True"
--       $ Lit (Bol True)
--   , RightCase "literal (False)"
--       "False"
--       $ Lit (Bol False)
--   , RightCase "variable"
--       "x"
--       $ Var "x"
--   , RightCase "constant"
--       "X"
--       $ Const "X"
--   , RightCase "numeric 1"
--       "(1 + (1))"
--       $ bin Add (Lit (Num 1)) (Lit (Num 1))
--   , RightCase "numeric 2"
--       "A + X * Y"
--       $ bin Add
--           (Const "A")
--           (bin Mul (Const "X") (Const "Y"))
--   , RightCase "numeric 3"
--       "(A + X) * Y % 2"
--       $ bin Mul
--           (bin Add (Const "A") (Const "X"))
--           (bin Mod (Const "Y") (Lit (Num 2)))
--   , RightCase "relation (EQ)"
--       "A = B"
--       $ bin EQ (Const "A") (Const "B")
--   , RightCase "relation (NEQ)"
--       "A /= B"
--       $ bin NEQ (Const "A") (Const "B")
--   , RightCase "relation (LT)"
--       "A < B"
--       $ bin LT (Const "A") (Const "B")
--   , RightCase "relation (LTE)"
--       "A <= B"
--       $ bin LTE (Const "A") (Const "B")
--   , RightCase "relation (GT)"
--       "A > B"
--       $ bin GT (Const "A") (Const "B")
--   , RightCase "relation (GTE)"
--       "A >= B"
--       $ bin GTE (Const "A") (Const "B")
--   , RightCase "boolean (Conj)"
--       "A && B"
--       $ bin Conj (Const "A") (Const "B")
--   , RightCase "boolean (Disj)"
--       "A || B"
--       $ bin Disj (Const "A") (Const "B")
--   , RightCase "boolean (Implies)"
--       "A => B"
--       $ bin Implies (Const "A") (Const "B")
--   , RightCase "boolean (Neg)"
--       "~ A"
--       $ un Neg (Const "A")
--   , RightCase "boolean 1"
--       "A || B => C"
--       $ bin Implies
--           (bin Disj (Const "A") (Const "B"))
--           (Const "C")
--   , RightCase "boolean 2"
--       "A || (B => C)"
--       $ bin Disj
--           (Const "A")
--           (bin Implies (Const "B") (Const "C"))
--   , RightCase "boolean 3"
--       "A || B && C"
--       $ bin Disj
--           (Const "A")
--           (bin Conj (Const "B") (Const "C"))
--   , RightCase "boolean 4"
--       "B && C || A"
--       $ bin Disj
--           (bin Conj (Const "B") (Const "C"))
--           (Const "A")
--   , RightCase "function application 1"
--       "(f (x)) y"
--       $ App
--           (App (Var "f") (Var "x"))
--           (Var "y")
--   , RightCase "function application 2"
--       "f (x y)"
--       $ App
--           (Var "f")
--           (App (Var "x") (Var "y"))
--   , RightCase "mixed 1"
--       "X * Y = N"
--       $ bin EQ
--           (bin Mul (Const "X") (Const "Y"))
--           (Const "N")
--   , RightCase "mixed 2"
--       "X * Y => P = Q"
--       $ bin Implies
--           (bin Mul (Const "X") (Const "Y"))
--           (bin EQ
--             (Const "P")
--             (Const "Q"))
--     , RightCase "mixed 3"
--         "X > Y && X > Y"
--         $ bin Conj
--             (bin GT (Const "X") (Const "Y"))
--             (bin GT (Const "X") (Const "Y"))
--   ]
--   where
--     bin :: Op -> Expr -> Expr -> Expr
--     bin op a b = App (App (Op op) a) b
--
--     un :: Op -> Expr -> Expr
--     un op a = App (Op op) a

--------------------------------------------------------------------------------
-- | Type

type' :: TestTree
type' = testGroup "Types" $ map (toTestTree Parser.type')
  [ RightCase "base types (Int)"
      "Int"
      $ TBase TInt (1 <-> 3)
  , RightCase "base types (Bool)"
      "Bool"
      $ TBase TBool (1 <-> 4)
  , RightCase "base types (Char)"
      "Char"
      $ TBase TChar (1 <-> 4)
  , RightCase "function types 1"
      "(Char -> (Int))"
      $ TFunc (TBase TChar (2 <-> 5)) (TBase TInt (11 <-> 13)) (2 <-> 13)
  , RightCase "function types 2"
      "(Char -> Int) -> Int"
      $ TFunc
          (TFunc
            (TBase TChar (2 <-> 5)) (TBase TInt (10 <-> 12)) (2 <-> 12))
          (TBase TInt (18 <-> 20))
          (2 <-> 20)
  , RightCase "array"
      "array [0 .. N) of Int"
      $ TArray
          (Interval
            (Including (Lit (Num 0) (at 8)))
            (Excluding (Const (Upper "N" (at 13)) (at 13)))
            (7 <-> 14))
          (TBase TInt (19 <-> 21))
          (1 <-> 21)
  ]

--------------------------------------------------------------------------------
-- | Declaration

declaration :: TestTree
declaration = testGroup "Declarations" $ map (toTestTree Parser.declaration)
  [ RightCase "variable"
      "var x : Int\n"
      $ VarDecl
          [Lower "x" (at 5)]
          (TBase TInt (9 <-> 11))
          (1 <-> 11)
  , RightCase "constant"
      "con X, Y : Int\n"
      $ ConstDecl
          [Upper "X" (at 5), Upper "Y" (at 8)]
          (TBase TInt (12 <-> 14))
          (1 <-> 14)

  ]

--------------------------------------------------------------------------------
-- | Statements

statement :: TestTree
statement = testGroup "Statements" $ map (toTestTree Parser.statement)
  [ RightCase "skip"
      "skip\n"
      $ Skip (1 <-> 4)
  , RightCase "abort"
      "abort\n"
      $ Abort (1 <-> 5)
  , RightCase "assert"
      "{ True }\n"
      $ Assert
          (Lit (Bol True) (3 <-> 6))
          (1 <-> 8)
  , RightCase "assign"
      "x := 0\n"
      $ Assign
          [Lower "x" (at 1)]
          [Lit (Num 0) (at 6)]
          (1 <-> 6)
  , RightCase "assign (parallel)"
      "x, y := 0, 1\n"
      $ Assign
          [Lower "x" (at 1), Lower "y" (at 4)]
          [Lit (Num 0) (at 9), Lit (Num 1) (at 12)]
          (1 <-> 12)
  ]

(<->) :: Int -> Int -> Loc
(<->) from to = Loc (Pos "<test>" 1 from (from - 1)) (Pos "<test>" 1 to (to - 1))

at :: Int -> Loc
at n = n <-> n


--------------------------------------------------------------------------------
-- | Program

program :: TestTree
program = testGroup "Program" $ map (toTestTree Parser.program)
  [ ReadFile "empty" "./test/source/empty.gcl"
      $ Right $ Program [] [] NoLoc
  ]
