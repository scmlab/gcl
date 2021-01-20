{-# LANGUAGE OverloadedStrings #-}

module Test.Concrete2 where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Lazy (fromStrict, toStrict)
import Data.Text.Prettyprint.Doc hiding (removeTrailingWhitespace)
import Data.Text.Prettyprint.Doc.Render.Text
  ( renderLazy,
  )
import Error
import qualified LSP
import Pretty (renderStrict)
import Pretty.Util (PrettyWithLoc (prettyWithLoc))
import qualified Syntax.Concrete2 as Concrete2
import Syntax.Location
import Syntax.Parser (Parser)
import qualified Syntax.Parser as Parser
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (Ordering (..), lines)

tests :: TestTree
tests = testGroup "Prettifier" [expression, type', declaration, statement]

--------------------------------------------------------------------------------

render :: Pretty a => Parser a -> Text -> Text
render parser raw =
  let expr = LSP.runM $ LSP.scan "<test>" raw >>= LSP.parse parser "<test>"
   in renderStrict $ pretty expr

isomorphic :: Pretty a => Parser a -> Text -> Assertion 
isomorphic parser raw = removeTrailingWhitespace (render parser raw) @?= removeTrailingWhitespace raw
  where 
    removeTrailingWhitespace :: Text -> Text 
    removeTrailingWhitespace = Text.unlines . map Text.stripEnd . Text.lines

-- | Expression
expression :: TestTree
expression =
  testGroup
    "Expressions"
    [ testCase "literal (numbers)" $ run "1",
      testCase "literal (True)" $ run "True",
      testCase "literal (False)" $ run "False",
      testCase "variable" $ run "   x",
      testCase "constant" $ run " ( X)",
      testCase "numeric 1" $ run "(1   \n  +  \n  (   \n   1))",
      testCase "numeric 2" $ run "A + X * Y",
      testCase "numeric 3" $ run "(A + X) * Y % 2",
      testCase "relation (EQ)" $ run "A = B",
      testCase "relation (NEQ)" $ run "A /= B",
      testCase "relation (LT)" $ run "A < B",
      testCase "relation (LTE)" $ run "A <= B",
      testCase "relation (GT)" $ run "A > B",
      testCase "relation (LTE)" $ run "A >= B",
      testCase "boolean (Conj)" $ run "A && B",
      testCase "boolean (Disj)" $ run "A || B",
      testCase "boolean (Implies)" $ run "A => B",
      testCase "boolean (Neg)" $ run "~ A",
      testCase "boolean 1" $ run "A || B => C",
      testCase "boolean 2" $ run "A || (B => C)",
      testCase "boolean 3" $ run "A || B && C",
      testCase "boolean 4" $ run "B && C || A",
      testCase "quant 1" $ run "<| + i : i > 0 : f i |>",
      testCase "quant 2" $ run "⟨     + i :   i > 0   : f i ⟩",
      testCase "function application 1" $ run "(f   (  x      )) y",
      testCase "function application 2" $ run "f (x y)",
      testCase "mixed 1" $ run "X * Y = N",
      testCase "mixed 2" $ run "X * Y => P = Q",
      testCase "mixed 3" $ run "X > Y && X > Y",
      testCase "mixed 4" $ run "X > (Y) && (X) > Y",
      testCase "mixed 5" $ run "1 + 2 * (3) - 4",
      testCase "mixed 6" $ run "1 + 2 * 3 = 4",
      testCase "mixed 7" $ run "1 > 2 = True",
      testCase "mixed 8" $ run "(1 + 2) * (3) = (4)",
      testCase "mixed 9" $ run "3 / (2 + X)",
      testCase "mixed 10" $ run "3 / 2 + X"
    ]
  where
    run = isomorphic Parser.expression

--------------------------------------------------------------------------------

-- | Type
type' :: TestTree
type' =
  testGroup
    "Types"
    [ testCase "base types (Int)" $ run "Int",
      testCase "base types (Bool)" $ run "  ((Bool))",
      testCase "base types (Char)" $ run "Char",
      testCase "function types 1" $ run "(Char -> (Int))",
      testCase "function types 2" $ run "( Char ->      Int) -> Int",
      testCase "function types (with newlines everywhere)" $
        run
          "(Char \n\
          \   ->\n\
          \   (\n\
          \           Int))",
      testCase "array" $ run "array [0 .. N) of Int"
    ]  
  where
    run = isomorphic Parser.type'

--------------------------------------------------------------------------------

-- | Declaration
declaration :: TestTree
declaration =
  testGroup "Declarations"
    [ testCase "variable" $ run "var   x     :   ( Int)",
      testCase "variable (with newlines in between)" $
        run
          "var\n\
          \ x \n\
          \   : Int\n", 
      testCase "variable with properties" $ run "var x : Int  {    True \n }",
      testCase "constant" $ run "con X , Z,B, Y : Int",
      testCase "let binding" $ run " let  X   i  =  N  >   (0)  "
    ]
  where
    run = isomorphic Parser.declaration

--------------------------------------------------------------------------------

-- | Statements
statement :: TestTree
statement =
  testGroup "Single statement"
    [ testCase "abort" $ run "     abort",
      testCase "skip" $ run "  skip",
      testCase "assertion" $ run "{ \n True   }",
      testCase "assignment" $ run "x := 0",
      testCase "assignment (parallel)" $ run "x, y := 0, 1",
      testCase "conditional 1" $ run "if True -> skip fi",
      testCase "conditional 2" $ run "if True -> skip\n | False -> abort fi",
      testCase "loop invariant" $ run "{ True , bnd: a }",
      testCase "loop body" $ run "do True -> skip od"
    ]
  where
    run = isomorphic Parser.statement
