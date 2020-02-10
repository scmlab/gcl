{-# LANGUAGE OverloadedStrings #-}

module Test.Parser where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Text.Lazy (Text)
import Data.Loc
import Prelude hiding (Ordering(..))
import Text.Megaparsec (eof)

import qualified Data.Text.Lazy.IO as Text

import qualified Syntax.Parser as Parser
import qualified REPL as REPL
import Syntax.Parser (Parser)
import Syntax.Concrete
import Error

tests :: TestTree
tests = testGroup "Parser"
  [ expression
  , type'
  , declaration
  , statement
  , statements
  , program
  ]

--------------------------------------------------------------------------------
-- | Helpers

data TestCase b
  = RightCase String Text b
  | LeftCase String Text [Error]
  | LeftCase2 String Text
  | ReadFile String FilePath (Either [Error] Program)

toTestTree :: (Eq a, Show a) => Parser a -> TestCase a -> TestTree
toTestTree parser (RightCase name text expected) = testCase name $ do
  let actual = parse (parser <* eof) text
  actual @?= Right expected
toTestTree parser (LeftCase name text expected) = testCase name $ do
  let actual = parse (parser <* eof) text
  actual @?= Left expected
toTestTree parser (LeftCase2 name text) = testCase name $ do
  let actual = parse (parser <* eof) text
  case actual of
    Left _ -> assertBool "" True
    _      -> assertFailure "expecting a Left value"
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

expression :: TestTree
expression = testGroup "Expressions" $ map (toTestTree Parser.expression)
  [ RightCase "hole"
      "?"
      $ Hole (at 1)
  , RightCase "literal (numbers)"
      "1"
      $ Lit (Num 1) (at 1)
  , RightCase "literal (True)"
      "True"
      $ Lit (Bol True) (1 <-> 4)
  , RightCase "literal (False)"
      "False"
      $ Lit (Bol False) (1 <-> 5)
  , RightCase "variable"
      "x"
      $ var "x" (at 1)
  , RightCase "conant"
      "X"
      $ con "X" (at 1)
  , RightCase "numeric 1"
      "(1 + (1))"
      $ bin Add (at 4)
          (Lit (Num 1) (at 2)) (2 <-> 4)
          (Lit (Num 1) (6 <-> 8)) (1 <-> 9)
  , RightCase "numeric 2"
      "A + X * Y"
      $ bin Add (at 3)
          (con "A" (at 1)) (1 <-> 3)
          (bin Mul (at 7)
            (con "X" (at 5)) (5 <-> 7)
            (con "Y" (at 9)) (5 <-> 9)) (1 <-> 9)
  , RightCase "numeric 3"
      "(A + X) * Y % 2"
      $ bin Mul (at 9)
          (bin Add (at 4)
            (con "A" (at 2)) (2 <-> 4)
            (con "X" (at 6)) (1 <-> 7)) (1 <-> 9)
          (bin Mod (at 13)
            (con "Y" (at 11)) (11 <-> 13)
            (Lit (Num 2) (at 15)) (11 <-> 15)) (1 <-> 15)
  , RightCase "relation (EQ)"
      "A = B"
      $ bin EQ (at 3)
          (con "A" (at 1)) (1 <-> 3)
          (con "B" (at 5)) (1 <-> 5)
  , RightCase "relation (NEQ)"
      "A /= B"
      $ bin NEQ (3 <-> 4)
          (con "A" (at 1)) (1 <-> 4)
          (con "B" (at 6)) (1 <-> 6)
  , RightCase "relation (LT)"
      "A < B"
      $ bin LT (at 3)
          (con "A" (at 1)) (1 <-> 3)
          (con "B" (at 5)) (1 <-> 5)
  , RightCase "relation (LTE)"
      "A <= B"
      $ bin LTE (3 <-> 4)
          (con "A" (at 1)) (1 <-> 4)
          (con "B" (at 6)) (1 <-> 6)
  , RightCase "relation (GT)"
      "A > B"
      $ bin GT (at 3)
          (con "A" (at 1)) (1 <-> 3)
          (con "B" (at 5)) (1 <-> 5)
  , RightCase "relation (GTE)"
      "A >= B"
      $ bin GTE (3 <-> 4)
          (con "A" (at 1)) (1 <-> 4)
          (con "B" (at 6)) (1 <-> 6)
  , RightCase "boolean (Conj)"
      "A && B"
      $ bin Conj (3 <-> 4)
          (con "A" (at 1)) (1 <-> 4)
          (con "B" (at 6)) (1 <-> 6)
  , RightCase "boolean (Disj)"
      "A || B"
      $ bin Disj (3 <-> 4)
          (con "A" (at 1)) (1 <-> 4)
          (con "B" (at 6)) (1 <-> 6)
  , RightCase "boolean (Implies)"
      "A => B"
      $ bin Implies (3 <-> 4)
          (con "A" (at 1)) (1 <-> 4)
          (con "B" (at 6)) (1 <-> 6)
  , RightCase "boolean (Neg)"
      "~ A"
      $ un Neg (at 1)
          (con "A" (at 3)) (1 <-> 3)
  , RightCase "boolean 1"
      "A || B => C"
      $ bin Implies (8 <-> 9)
          (bin Disj (3 <-> 4)
              (con "A" (at 1)) (1 <-> 4)
              (con "B" (at 6)) (1 <-> 6)) (1 <-> 9)
          (con "C" (at 11)) (1 <-> 11)
  , RightCase "boolean 2"
      "A || (B => C)"
      $ bin Disj (3 <-> 4)
          (con "A" (at 1)) (1 <-> 4)
          (bin Implies (9 <-> 10)
            (con "B" (at 7)) (7 <-> 10)
            (con "C" (at 12)) (6 <-> 13)) (1 <-> 13)
  , RightCase "boolean 3"
      "A || B && C"
      $ bin Disj (3 <-> 4)
          (con "A" (at 1)) (1 <-> 4)
          (bin Conj (8 <-> 9)
            (con "B" (at 6)) (6 <-> 9)
            (con "C" (at 11)) (6 <-> 11)) (1 <-> 11)
  , RightCase "boolean 4"
      "B && C || A"
      $ bin Disj (8 <-> 9)
          (bin Conj (3 <-> 4)
              (con "B" (at 1)) (1 <-> 4)
              (con "C" (at 6)) (1 <-> 6)) (1 <-> 9)
          (con "A" (at 11)) (1 <-> 11)
  , RightCase "quant 1"
      "<| (+) i : i > 0 : f i |>"
      $ Quant
          (Op Add (4 <-> 6))
          [Lower "i" (at 8)]
          (bin GT (at 14)
              (var "i" (at 12)) (12 <-> 14)
              (Lit (Num 0) (at 16)) (12 <-> 16))
          (App (var "f" (at 20)) (var "i" (at 22)) (20 <-> 22))
          (1 <-> 25)
  , RightCase "function application 1"
      "(f (x)) y"
      $ App
          (App
            (var "f" (at 2))
            (Var (Lower "x" (at 5)) (4 <-> 6))
            (1 <-> 7))
          (var "y" (at 9))
          (1 <-> 9)
  , RightCase "function application 2"
      "f (x y)"
      $ App
          (var "f" (at 1))
          (App
            (var "x" (at 4))
            (var "y" (at 6))
            (3 <-> 7))
          (1 <-> 7)
  , RightCase "mixed 1"
      "X * Y = N"
      $ bin EQ (at 7)
          (bin Mul (at 3)
            (con "X" (at 1)) (1 <-> 3)
            (con "Y" (at 5)) (1 <-> 5)) (1 <-> 7)
          (con "N" (at 9)) (1 <-> 9)
  , RightCase "mixed 2"
      "X * Y => P = Q"
      $ bin Implies (7 <-> 8)
          (bin Mul (at 3)
            (con "X" (at 1)) (1 <-> 3)
            (con "Y" (at 5)) (1 <-> 5)) (1 <-> 8)
          (bin EQ (at 12)
            (con "P" (at 10)) (10 <-> 12)
            (con "Q" (at 14)) (10 <-> 14)) (1 <-> 14)
    , RightCase "mixed 3"
        "X > Y && X > Y"
        $ bin Conj (7 <-> 8)
            (bin GT (at 3)
              (con "X" (at 1)) (1 <-> 3)
              (con "Y" (at 5)) (1 <-> 5)) (1 <-> 8)
            (bin GT (at 12)
              (con "X" (at 10)) (10 <-> 12)
              (con "Y" (at 14)) (10 <-> 14)) (1 <-> 14)
  ]
  where
    con :: Text -> Loc -> Expr
    con t l = Const (Upper t l) l

    var :: Text -> Loc -> Expr
    var t l = Var (Lower t l) l

    bin :: Op -> Loc -> Expr -> Loc -> Expr -> Loc -> Expr
    bin op opLoc a aLoc b bLoc = App (App (Op op opLoc) a aLoc) b bLoc

    un :: Op -> Loc -> Expr -> Loc -> Expr
    un op opLoc a aLoc = App (Op op opLoc) a aLoc

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
      $ TFunc (TBase TChar (2 <-> 5)) (TBase TInt (10 <-> 14)) (1 <-> 15)
  , RightCase "function types 2"
      "(Char -> Int) -> Int"
      $ TFunc
          (TFunc
            (TBase TChar (2 <-> 5)) (TBase TInt (10 <-> 12)) (1 <-> 13))
          (TBase TInt (18 <-> 20))
          (1 <-> 20)
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
  , RightCase "conant"
      "con X, Y : Int\n"
      $ ConstDecl
          [Upper "X" (at 5), Upper "Y" (at 8)]
          (TBase TInt (12 <-> 14))
          (1 <-> 14)

  ]

--------------------------------------------------------------------------------
-- | Statements

statement :: TestTree
statement = testGroup "Single statement" $ map (toTestTree Parser.statement)
  [ RightCase "skip"
      "skip"
      $ Skip (1 <-> 4)
  , RightCase "abort"
      "abort"
      $ Abort (1 <-> 5)
  , RightCase "assert"
      "{ True }"
      $ Assert
          (Lit (Bol True) (3 <-> 6))
          (1 <-> 8)
  , RightCase "assign"
      "x := 0"
      $ Assign
          [Lower "x" (at 1)]
          [Lit (Num 0) (at 6)]
          (1 <-> 6)
  , RightCase "assign (parallel)"
      "x, y := 0, 1"
      $ Assign
          [Lower "x" (at 1), Lower "y" (at 4)]
          [Lit (Num 0) (at 9), Lit (Num 1) (at 12)]
          (1 <-> 12)
  , RightCase "loop"
      "if True -> skip fi"
      $ If
          [GdCmd
            (Lit (Bol True) (4 <-> 7))
            [Skip (12 <-> 15)]
            (4 <-> 15)
          ]
          (1 <-> 18)
  , RightCase "loop"
      "{ True , bnd: a }"
      $ LoopInvariant
          (Lit (Bol True) (3 <-> 6))
          (Var (Lower "a" (at 15)) (at 15))
          (1 <-> 17)
  , RightCase "loop"
      "do True -> skip od"
      $ Do
          [GdCmd
            (Lit (Bol True) (4 <-> 7))
            [Skip (12 <-> 15)]
            (4 <-> 15)
          ]
          (1 <-> 18)
  ]

(<->) :: Int -> Int -> Loc
(<->) from to = Loc (Pos "<test>" 1 from (from - 1)) (Pos "<test>" 1 to (to - 1))

at :: Int -> Loc
at n = n <-> n

statements :: TestTree
statements = testGroup "Multiple statements" $ map (toTestTree Parser.statements)
  [ RightCase "separated by newlines 1"
      "skip\nskip"
      [ Skip $ pos 1 1 0 <--> pos 1 4 3
      , Skip $ pos 2 1 5 <--> pos 2 4 8]
  , RightCase "separated by newlines 2"
      "skip\n\nskip\n"
      [ Skip $ pos 1 1 0 <--> pos 1 4 3
      , Skip $ pos 3 1 6 <--> pos 3 4 9]
  , RightCase "separated by semicolons 1"
      "skip;skip"
      [ Skip $ pos 1 1 0 <--> pos 1 4 3
      , Skip $ pos 1 6 5 <--> pos 1 9 8]
  , RightCase "separated by semicolons 2"
      "skip;\nskip"
      [ Skip $ pos 1 1 0 <--> pos 1 4 3
      , Skip $ pos 2 1 6 <--> pos 2 4 9]
  , RightCase "separated by semicolons 4"
      "skip;"
      [ Skip $ pos 1 1 0 <--> pos 1 4 3]
  , RightCase "separated by semicolons 3"
      "skip;\nskip;"
      [ Skip $ pos 1 1 0 <--> pos 1 4 3
      , Skip $ pos 2 1 6 <--> pos 2 4 9]
  , LeftCase2 "separated by a space"
      "skip abort"
  ]

pos :: Int -> Int -> Int -> Pos
pos = Pos "<test>"


--------------------------------------------------------------------------------
-- | Program

program :: TestTree
program = testGroup "Program" $ map (toTestTree Parser.program)
  [ ReadFile "empty" "./test/source/empty.gcl"
      $ Right $ Program [] [] NoLoc
  ]
