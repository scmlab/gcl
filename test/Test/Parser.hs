{-# LANGUAGE OverloadedStrings #-}

module Test.Parser where
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Text.Prettyprint.Doc      ( Pretty )
import           Pretty                         ( toByteString
                                                , toText
                                                )
import           Syntax.Parser
import           Syntax.Parser.Lexer
import           Test.Tasty                     ( TestTree
                                                , testGroup
                                                )
import           Test.Tasty.HUnit               ( (@?=)
                                                , Assertion
                                                , testCase
                                                )
import           Test.Util                      ( parseTest
                                                , removeTrailingWhitespace
                                                , runGoldenTest
                                                )
import           Syntax.Concrete
import           Syntax.Parser.Util
import           Control.Applicative.Combinators

tests :: TestTree
tests = testGroup
  "Parser"
  [ expression
  , pattern'
  , type'
  , definition
  , declaration
  , statement
  , parseError
  , golden
  ]

--------------------------------------------------------------------------------

-- | Expression
expression :: TestTree
expression = testGroup
  "Expressions"
  [ testCase "literal (numbers)" $ run "1"
  , testCase "literal (True)" $ run "True"
  , testCase "literal (False)" $ run "False"
  , testCase "literal (Char)" $ run "'a'"
  , testCase "variable 1" $ run "x"
  , testCase "variable 2" $ run "Falsee"
  , testCase "constant 1" $ run "(X)"
  , testCase "constant 2" $ run "Intt"
  , testCase "numeric 1" $ run "(1   \n  +  \n  (   \n   1))"
  , testCase "numeric 2" $ run "A + X * Y"
  , testCase "numeric 3" $ run "(A + X) * Y % 2"
  , testCase "equivalent (EQProp)"
    $ run "a + b + c\n\
        \≡ a + b + d\n\
        \≡ a + b * e"
  , testCase "equivalent (EQPropU)"
    $ run "a + b + c\n\
        \<=> a + b + d\n\
        \<=> a + b * e"
  , testCase "chain op (EQ)" $ run "A = B"
  , testCase "chain op (NEQ)" $ run "A /= B"
  , testCase "chain op (NEQU)" $ run "A ≠ B"
  , testCase "chain op (LT)" $ run "A < B"
  , testCase "chain op (LTE)" $ run "A <= B"
  , testCase "chain op (LTEU)" $ run "A ≤ B"
  , testCase "chain op (GT)" $ run "A > B"
  , testCase "chain op (GTE)" $ run "A >= B"
  , testCase "chain op (GTEU)" $ run "A ≥ B"
  , testCase "arith op (Conj)" $ run "A && B"
  , testCase "arith op (ConjU)" $ run "A ∧ B"
  , testCase "arith op (Disj)" $ run "A || B"
  , testCase "arith op (DisjU)" $ run "A ∨ B"
  , testCase "arith op (Imply)" $ run "A => B"
  , testCase "arith op (ImplyU)" $ run "A ⇒ B"
  , testCase "arith op (Neg)" $ run "~ A"
  , testCase "arith op (NegU)" $ run "¬ A"
  , testCase "arith op (max)" $ run "A ↑ B"
  , testCase "arith op (min)" $ run "A ↓ B"
  , testCase "arith op (exp)" $ run "A ^ B"
  , testCase "arith op combined 1" $ run "A || B => C"
  , testCase "arith op combined 2" $ run "A || (B => C)"
  , testCase "arith op combined 3" $ run "A || B && C"
  , testCase "arith op combined 4" $ run "B && C || A"
  , testCase "quant 1" $ run "<| + i : i > 0 : f i |>"
  , testCase "quant 2" $ run "⟨     + i :   i > 0   : f i ⟩"
  , testCase "quant 3" $ run "⟨ max i j : 0 ≤ i < j < n : A i - A j ⟩"
  , testCase "quant 4" $ run "<| + i : 0 <= i < k : F i |>"
  , testCase "quant 5" $ run "x = <| + i : 0 <= i < k : F i |>"
  , testCase "quant 6 (sum)" $ run "x = <| + i : 0 < i < n : i |>\n"
  , testCase "quant 7 (pi)" $ run "x = <| * i : 0 < i < n : i |>"
  , testCase "quant 8 (forall)" $ run "x = <| && i : 0 < i < n : i > 0 |>"
  , testCase "quant 9 (exists)" $ run "x = <| || i : 0 < i < n : i > 0 |>"
  , testCase "quant 10 (hash)" $ run "x = <| # i : 0 < i < n : F i > 0 |>"
  , testCase "function application 1" $ run "(f   (  x      )) y"
  , testCase "function application 2" $ run "f (x y)"
  , testCase "array indexing (app)" $ run "A i"
  , testCase "array indexing (bracket) 1 : A[i]" $ run "A[i]"
  , testCase "array indexing (bracket) 2 : A[A[i]]" $ run "A[A[i]]"
  , testCase "array indexing (bracket) 3 : A[i][j]" $ run "A[i][j]"
  , testCase "array indexing (bracket) 4 : (A[i])[j][k]" $ run "(A[i])[j][k]"
  , testCase "array indexing (bracket) 5 : A[i][j][k]" $ run "A[i][j][k]"
  , testCase "mixed 1" $ run "X * Y = N"
  , testCase "mixed 2" $ run "X * Y => P = Q"
  , testCase "mixed 3" $ run "X > Y && X > Y"
  , testCase "mixed 4" $ run "X > (Y) && (X) > Y"
  , testCase "mixed 5" $ run "1 + 2 * (3) - 4"
  , testCase "mixed 6" $ run "1 + 2 * 3 = 4"
  , testCase "mixed 7" $ run "1 > 2 = True"
  , testCase "mixed 8" $ run "(1 + 2) * (3) = (4)"
  , testCase "mixed 9" $ run "3 / (2 + X)"
  , testCase "mixed 10" $ run "3 / 2 + X"
  , testCase "case of 1" $ run "case x of\n\
    \  Nothing -> 0\n"
  , testCase "case of 2" $ run "case x of\n\
    \  Just y -> 0\n"
  , testCase "case of 3"
    $ run "case x of\n\
        \    Just y -> 0\n\
        \    Nothing -> 1"
  ]
  where run = parserIso (scn >> pExpr)


--------------------------------------------------------------------------------
-- | Pattern

pattern' :: TestTree
pattern' = testGroup
  "Pattern"
  [ testCase "pattern (binder)" $ run "a"
  , testCase "pattern (wildcard)" $ run "_"
  , testCase "pattern (constructor)" $ run "Just (Just a)"
  , testCase "pattern (parenthesis 1)" $ run "(_)"
  , testCase "pattern (parenthesis 2)" $ run "(a)"
  , testCase "pattern (parenthesis 3)" $ run "(Just (Just a))"
  ]
  where run = parserIso (scn >> pPattern)

--------------------------------------------------------------------------------

-- | Type
type' :: TestTree
type' = testGroup
  "Types"
  [ testCase "base types (Int)" $ run "Int"
  , testCase "base types (Bool)" $ run "(Bool)"
  , testCase "base types (Bool)" $ run "((Bool))"
  , testCase "base types (Char)" $ run "Char"
  , testCase "function types 1" $ run "(Char -> (Int   ))"
  , testCase "function types 2" $ run "( Char →      Int) -> Int"
  , testCase "function types (with newlines everywhere)"
    $ run
        "(Char \n\
          \   ->\n\
          \   (\n\
          \           Int))"
  , testCase "array 1" $ run "array [0 .. N  )   of    Int"
  , testCase "array 2" $ run "array (   0   ..  N   ] of Int"
  , testCase "array 3" $ run "array [  0 .. N  ] of     Int"
  , testCase "type decl" $ run "List a"
  ]
  where run = parserIso (scn >> pType)

--------------------------------------------------------------------------------
-- | Definition
definition :: TestTree
definition = testGroup
  "Definitions"
  [ testCase "type definition 1" $ run "{:\ndata List a = Nil | Con a\n:}"
  , testCase "type definition 2" $ run "{:\ndata List a = Node (List a)\n:}"
  , testCase "definition 1" $ run "{:\n\
        \   A, B : Int\n\
        \:}"
  , testCase "definition (with prop) 1"
    $ run "{:\n\
        \   A, B : Int {A > 0}\n\
        \:}"
  , testCase "definition (with prop) 2" $ run
    "{:\n\
        \   A, B : Int\n\
        \     { A > 0 }\n\
        \:}"
  , testCase "definition 3"
    $ run
        "{:\n\
        \   A, B : Int\n\
        \     {A > 0}\n\
        \   F : Int -> Int -> Int\n\
        \:}"
  , testCase "definition 4"
    $ run
        "{:\n\
        \   A, B : Int\n\
        \     {A > 0}\n\
        \   A = 1\n\
        \   F : Int -> Int -> Int\n\
        \   F x y = x\n\
        \:}"
  , testCase "definition 5"
    $ run "{:\n\
        \   A = 5\n\
        \   F a b = a + b\n\
        \:}"
  , testCase "definition 6"
    $ run
        "{:\n\
        \   A = 5\n\
        \   F a b = a + b\n\
        \   B, C : Int\n\
        \:}"
  ]
  where run = parserIso pDefinitionBlock
--------------------------------------------------------------------------------

-- | Declaration
declaration :: TestTree
declaration = testGroup
  "Declarations"
  [ testCase "variable" $ run "var   x     :   ( Int)"
  , testCase "variable keyword collision 1" $ run "var iff : Int"
  , testCase "variable keyword collision 2" $ run "var fif : Int"
  , testCase "variable keyword collision 3" $ run "var doo : Int"
  , testCase "variable keyword collision 4" $ run "var odd : Int"
  , testCase "variable (with newlines in between)"
    $ run "var\n\
          \  x \n\
          \   : Int\n"
  , testCase "variable with properties" $ run "var x : Int  {    True \n }"
  , testCase "constant" $ run "con X , Z,B, Y : Int"
  , testCase "constant keyword collision 1" $ run "con Falsee : Int"
  , testCase "constant keyword collision 2" $ run "con Trueu : Int"
  , testCase "constant keyword collision 3" $ run "con Intt : Int"
  , testCase "constant keyword collision 4" $ run "con Boola : Int"
  ]
  where run = parserIso pDeclaration

--------------------------------------------------------------------------------

-- | Statements
statement :: TestTree
statement = testGroup
  "Single statement"
  [ testCase "abort" $ run "abort"
  , testCase "skip" $ run "skip"
  , testCase "assignment" $ run "x := 0"
  , testCase "assignment (parallel)" $ run "x   , y  := 0    ,    1"
  , testCase "array assignment" $ run "x[i] := e"
  , testCase "assertion" $ run "{ \n True   }"
  , testCase "loop invariant" $ run "{ True ,     bnd      : a  }"
  , testCase "conditional 1" $ run "if True -> skip fi"
  , testCase "conditional 2"
    $ run "if True ->    skip   \n | False -> abort \nfi"
  , testCase "loop body 1" $ run "do True -> skip od"
  , testCase "loop body 2" $ run "do True    →       skip od"
  , testCase "spec QM" $ run "?"
  , testCase "spec 1" $ run "[!!]"
  , testCase "spec 2" $ run "[!\n   !]"
  , testCase "proof" $ run "{-   -}"
  , testCase "alloc 1" $ run "p := new(e1)"
  , testCase "alloc 2" $ run "p := new(e1, e2)"
  , testCase "alloc 3" $ run "p := new(e1, e2, e3)"
  , testCase "hlookup" $ run "x := *e"
  , testCase "hmutate" $ run "*e1 := e2"
  , testCase "dispose" $ run "dispose e"
  , testCase "block 1" $ run "|[]|"
  , testCase "block 2" $ run "|[ x := y ]|"
  ]
  where run = parserIso pStmt

--------------------------------------------------------------------------------

-- | Parse Error
parseError :: TestTree
parseError = testGroup
  "Parse error"
  [ testCase "constant keyword collision" $ runDeclaration
    "con False : Int"
    "[(<test>:1:5, using keyword as variable name )]\n"
  , testCase "variable keyword collision" $ runDeclaration
    "var if : Int"
    "[(<test>:1:5, using keyword as variable name )]\n"
  , testCase "quant with parentheses" $ runExpr
    "<| (+) i : i > 0 : f i |>"
    "[(<test>:1:5, unexpected \"+) i \" expecting expression )]\n"
  , testCase "array" $ runType
    "array (  0 .. (Int) ) of \n Int"
    "[(<test>:1:16, using keyword as variable name )]\n"
  ]
 where
  runDeclaration = parserCompare pDeclaration
  runType        = parserCompare pType
  runExpr        = parserCompare pExpr


--------------------------------------------------------------------------------

-- | Golden Tests
golden :: TestTree
golden = testGroup
  "Program"
  [ parserGolden ""          "empty"    "empty.gcl"
  , parserGolden ""          "2"        "2.gcl"
  , parserGolden ""          "comment"  "comment.gcl"
  , parserGolden ""          "issue 1"  "issue1.gcl"
  , parserGolden ""          "issue 14" "issue14.gcl"
  , parserGolden ""          "no-decl"  "no-decl.gcl"
  , parserGolden ""          "no-stmt"  "no-stmt.gcl"
  , parserGolden ""          "assign"   "assign.gcl"
  , parserGolden ""          "quant 1"  "quant1.gcl"
  , parserGolden ""          "spec"     "spec.gcl"
  , parserGolden "examples/" "gcd"      "gcd.gcl"
  , parserGolden "examples/" "proof"    "proof.gcl"
  , parserGolden "examples/" "block"    "block.gcl"
  ]

parserGolden :: String -> FilePath -> FilePath -> TestTree
parserGolden dirName =
  runGoldenTest ("./test/source/" <> dirName)
                ("./test/golden/" <> dirName)
                ".ast"
    $ \sourcePath source -> do
        return $ toByteString $ runParse pProgram sourcePath source

parserShow :: Show a => Parser a -> Text -> Text -> Assertion
parserShow parser actual expected =
  (Text.pack . show . parseTest parser) actual @?= expected

parserCompare :: Pretty a => Parser a -> Text -> Text -> Assertion
parserCompare parser actual expected =
  (removeTrailingWhitespace . toText . parseTest parser) actual
    @?= removeTrailingWhitespace expected

parserIso :: Pretty a => Parser a -> Text -> Assertion
parserIso parser raw = parserCompare parser raw raw
