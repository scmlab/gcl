{-# LANGUAGE OverloadedStrings #-}

module Test.Parser where
import Test.Util (goldenFileTest, parseTest, removeTrailingWhitespace)
import Test.Tasty (TestTree, testGroup)
import Data.Text.Prettyprint.Doc (Pretty)
import Syntax.Parser (Parser, runParse, pProgram, pDeclaration, pType, pExpr, pBlockDeclaration, pStmt)
import Syntax.Parser.Lexer ( scn )
import Data.Text (Text)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Pretty (toText)


tests :: TestTree
tests = testGroup "Parser" [expression, type', declaration, statement, parseError, golden]

--------------------------------------------------------------------------------

-- | Expression
expression :: TestTree
expression =
  testGroup
    "Expressions"
    [ testCase "literal (numbers)" $ run "1",
      testCase "literal (True)" $ run "True",
      testCase "literal (False)" $ run "False",
      testCase "literal (Char)" $ run "'a'",
      testCase "variable" $ run "x",
      testCase "constant" $ run "(X)",
      testCase "numeric 1" $ run "(1   \n  +  \n  (   \n   1))",
      testCase "numeric 2" $ run "A + X * Y",
      testCase "numeric 3" $ run "(A + X) * Y % 2",
      testCase "equivalent (EQProp)" $ run
        "a + b + c\n\
        \≡ a + b + d\n\
        \≡ a + b * e",
      testCase "equivalent (EQPropU)" $ run
        "a + b + c\n\
        \<=> a + b + d\n\
        \<=> a + b * e",
      testCase "relation (EQ)" $ run "A = B",
      testCase "relation (NEQ)" $ run "A /= B",
      testCase "relation (LT)" $ run "A < B",
      testCase "relation (LTE)" $ run "A <= B",
      testCase "relation (GT)" $ run "A > B",
      testCase "relation (GTE)" $ run "A >= B",
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
      testCase "quant 3" $ run "⟨ max i j : 0 ≤ i < j < n : A i - A j ⟩",
      testCase "quant 4" $ run "<| + i : 0 <= i < k : F i |>",
      testCase "quant 5" $ run "x = <| + i : 0 <= i < k : F i |>",
      testCase "quant 6 (hash)" $ run "x = <| # i : 0 < i < n : F i > 0 |>",
      testCase "function application 1" $ run "(f   (  x      )) y",
      testCase "function application 2" $ run "f (x y)",
      testCase "array indexing (app)" $ run "A i",
      testCase "array indexing (bracket) : A[i]" $ run "A[i]",
      testCase "array indexing (bracket) : A[A[i]]" $ run "A[A[i]]",
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
    run = parserIso (scn >> pExpr)
    
--------------------------------------------------------------------------------

-- | Type
type' :: TestTree
type' =
  testGroup
    "Types"
    [ testCase "base types (Int)" $ run "Int",
      testCase "base types (Bool)" $ run "(Bool)",
      testCase "base types (Bool)" $ run "((Bool))",
      testCase "base types (Char)" $ run "Char",
      testCase "function types 1" $ run "(Char -> (Int   ))",
      testCase "function types 2" $ run "( Char →      Int) -> Int",
      testCase "function types (with newlines everywhere)" $
        run
          "(Char \n\
          \   ->\n\
          \   (\n\
          \           Int))",
      testCase "array 1" $ run "array [0 .. N  )   of    Int",
      testCase "array 2" $ run "array (   0   ..  N   ] of Int",
      testCase "array 3" $ run "array [  0 .. N  ] of     Int"
    ]
  where
    run = parserIso (scn >> pType)

--------------------------------------------------------------------------------

-- | Declaration
declaration :: TestTree
declaration =
  testGroup
    "Declarations"
    [ testCase "variable" $ run "var   x     :   ( Int)",
      testCase "variable keyword collision 1" $ run "var iff : Int",
      testCase "variable keyword collision 2" $ run "var fif : Int",
      testCase "variable keyword collision 3" $ run "var doo : Int",
      testCase "variable keyword collision 4" $ run "var odd : Int",
      testCase "variable (with newlines in between)" $
        run
          "var\n\
          \  x \n\
          \   : Int\n",
      testCase "variable with properties" $ run "var x : Int  {    True \n }",
      testCase "constant" $ run "con X , Z,B, Y : Int",
      testCase "constant keyword collision 1" $ run "con Falsee : Int",
      testCase "constant keyword collision 2" $ run "con Trueu : Int",
      testCase "constant keyword collision 3" $ run "con Intt : Int",
      testCase "constant keyword collision 4" $ run "con Boola : Int",
      testCase "let binding" $ run "let  X   i  =  N  >   (0)  ",
      testCase "block declaration 1" $ runBlock
        "{:\n\
        \   A, B : Int\n\
        \:}",
      testCase "block declaration 1" $ runBlock
        "{:\n\
        \   A, B : Int\n\
        \     A > 0\n\
        \:}",
      testCase "block declaration 1" $ runBlock
        "{:\n\
        \   A, B : Int {A > 0}\n\
        \:}",
      testCase "block declaration 2" $ runBlock
        "{:\n\
        \   A, B : Int\n\
        \     {A > 0}\n\
        \:}",
      testCase "block declaration 3" $ runBlock
        "{:\n\
        \   A, B : Int\n\
        \     {A > 0}\n\
        \   F : Int -> Int -> Int\n\
        \:}"
      
    ]
  where
    run = parserIso pDeclaration
    runBlock = parserIso pBlockDeclaration

--------------------------------------------------------------------------------

-- | Statements
statement :: TestTree
statement =
  testGroup
    "Single statement"
    [ testCase "abort" $ run "abort",
      testCase "skip" $ run "skip",
      testCase "assertion" $ run "{ \n True   }",
      testCase "assignment" $ run "x := 0",
      testCase "assignment (parallel)" $ run "x   , y  := 0    ,    1",
      testCase "conditional 1" $ run "if True -> skip fi",
      testCase "conditional 2" $ run "if True ->    skip   \n | False -> abort \nfi",
      testCase "loop invariant" $ run "{ True ,     bnd      : a  }",
      testCase "loop body 1" $ run "do True -> skip od",
      testCase "loop body 2" $ run "do True    →       skip od",
      testCase "indentifier include keyword" $ run "{ Falsee }"
    ]
  where
    run = parserIso pStmt

--------------------------------------------------------------------------------

-- | Parse Error
parseError :: TestTree
parseError =
  testGroup
    "Parse error"
    [
      testCase "constant keyword collision" $ runDeclaration
        "con False : Int"
        "[ ( <test>:1:5\n, using keyword as variable name\n ) ]\n",
      testCase "variable keyword collision" $ runDeclaration
        "var if : Int"
        "[ ( <test>:1:5\n, using keyword as variable name\n ) ]\n",
      testCase "quant with parentheses" $ runExpr
        "<| (+) i : i > 0 : f i |>"
        "[ ( <test>:1:5\n, unexpected \"+) i \"\nexpecting expression\n ) ]\n",
      testCase "array" $ runType
        "array (  0 .. (Int) ) of \n Int"
        "[ ( <test>:1:16\n, using keyword as variable name\n ) ]\n"
    ]
  where
    runDeclaration = parserCompare pDeclaration
    runType = parserCompare pType
    runExpr = parserCompare pExpr


--------------------------------------------------------------------------------

-- | Golden Tests
golden :: TestTree
golden =
  testGroup
    "Program"
    [ parserGolden "empty" "./test/source/" "empty.gcl",
      parserGolden "2" "./test/source/" "2.gcl",
      parserGolden "comment" "./test/source/" "comment.gcl",
      parserGolden "issue 1" "./test/source/" "issue1.gcl",
      -- parserGolden "issue 14" "./test/source/" "issue14.gcl",
      -- parserGolden "no-decl" "./test/source/" "no-decl.gcl",
      parserGolden "no-stmt" "./test/source/" "no-stmt.gcl",
      parserGolden "assign" "./test/source/" "assign.gcl",
      parserGolden "quant 1" "./test/source/" "quant1.gcl",
      parserGolden "spec" "./test/source/" "spec.gcl",
      parserGolden "gcd" "./test/source/examples/" "gcd.gcl"
    ]

parserGolden :: String -> FilePath -> FilePath -> TestTree
parserGolden name filePath fileName =
  goldenFileTest ".ast.golden" name filePath fileName runFile

runFile :: (FilePath, Text) -> Text
runFile (filePath, source) = toText $ runParse pProgram filePath source

parserCompare :: Pretty a => Parser a -> Text -> Text -> Assertion
parserCompare parser actual expected = (removeTrailingWhitespace . toText . parseTest parser) actual @?= removeTrailingWhitespace expected

parserIso :: Pretty a => Parser a -> Text -> Assertion
parserIso parser raw = parserCompare parser raw raw

