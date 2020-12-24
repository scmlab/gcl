{-# LANGUAGE OverloadedStrings #-}

module Test.Parser where

import qualified Data.ByteString as Strict
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy.Char8
  ( unpack,
  )
import Data.Loc
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.Encoding as Text
import qualified Data.Text.Lazy.IO as Text
import Data.Text.Prettyprint.Doc.Render.Text
  ( renderLazy,
  )
import Error
import qualified LSP
import Pretty
import Syntax.Concrete
import Syntax.Parser (Parser)
import qualified Syntax.Parser as Parser
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.Golden.Advanced
import Test.Tasty.HUnit
import Text.Megaparsec (eof)
import Prelude hiding (Ordering (..))

tests :: TestTree
tests =
  testGroup
    "Parser"
    [expression, type', declaration, statement, statements, program]

--------------------------------------------------------------------------------

-- | Helpers
data TestCase b
  = RightCase String Text b
  | LeftCase String Text Error
  | LeftCase2 String Text
  | ReadFile String FilePath (Either Error Program)

toTestTree :: (Eq a, Show a) => Parser a -> TestCase a -> TestTree
toTestTree parser (RightCase name text expected) = testCase name $ do
  actual <- parse (parser <* eof) text
  actual @?= Right expected
toTestTree parser (LeftCase name text expected) = testCase name $ do
  actual <- parse (parser <* eof) text
  actual @?= Left expected
toTestTree parser (LeftCase2 name text) = testCase name $ do
  actual <- parse (parser <* eof) text
  case actual of
    Left _ -> assertBool "" True
    _ -> assertFailure "expecting a Left value"
toTestTree _ (ReadFile name filePath expected) = testCase name $ do
  text <- Text.readFile filePath
  actual <- parse Parser.program text
  actual @?= expected

parse :: Parser a -> Text -> IO (Either Error a)
parse parser text =
  LSP.runM $ LSP.scan "<test>" text >>= LSP.parse parser "<text>"

-- >>= LSP.abstract

--------------------------------------------------------------------------------

-- | Expression
expression :: TestTree
expression =
  testGroup "Expressions" $
    map
      (toTestTree Parser.expression)
      [ RightCase "hole" "?" $ Hole (at 1),
        RightCase "literal (numbers)" "1" $ Lit (Num 1) (at 1),
        RightCase "literal (True)" "True" $ Lit (Bol True) (1 <-> 4),
        RightCase "literal (False)" "False" $ Lit (Bol False) (1 <-> 5),
        RightCase "variable" "x" $ var "x" (at 1),
        RightCase "conant" "X" $ con "X" (at 1),
        RightCase
          "numeric 1"
          "(1 \n\
          \ +\n\
          \ (\n\
          \   1))"
          $ bin
            Add
            (pos 2 2 5 <--> pos 2 2 5)
            (Lit (Num 1) (at 2))
            (pos 1 2 1 <--> pos 2 2 5)
            (Lit (Num 1) (pos 3 2 8 <--> pos 4 5 14))
            (pos 1 1 0 <--> pos 4 6 15),
        RightCase "numeric 2" "A + X * Y" $
          bin
            Add
            (at 3)
            (con "A" (at 1))
            (1 <-> 3)
            (bin Mul (at 7) (con "X" (at 5)) (5 <-> 7) (con "Y" (at 9)) (5 <-> 9))
            (1 <-> 9),
        RightCase "numeric 3" "(A + X) * Y % 2" $
          bin
            Mul
            (at 9)
            (bin Add (at 4) (con "A" (at 2)) (2 <-> 4) (con "X" (at 6)) (1 <-> 7))
            (1 <-> 9)
            ( bin
                Mod
                (at 13)
                (con "Y" (at 11))
                (11 <-> 13)
                (Lit (Num 2) (at 15))
                (11 <-> 15)
            )
            (1 <-> 15),
        RightCase "relation (EQ)" "A = B" $
          bin EQ (at 3) (con "A" (at 1)) (1 <-> 3) (con "B" (at 5)) (1 <-> 5),
        RightCase "relation (NEQ)" "A /= B" $
          bin
            NEQ
            (3 <-> 4)
            (con "A" (at 1))
            (1 <-> 4)
            (con "B" (at 6))
            (1 <-> 6),
        RightCase "relation (LT)" "A < B" $
          bin LT (at 3) (con "A" (at 1)) (1 <-> 3) (con "B" (at 5)) (1 <-> 5),
        RightCase "relation (LTE)" "A <= B" $
          bin
            LTE
            (3 <-> 4)
            (con "A" (at 1))
            (1 <-> 4)
            (con "B" (at 6))
            (1 <-> 6),
        RightCase "relation (GT)" "A > B" $
          bin GT (at 3) (con "A" (at 1)) (1 <-> 3) (con "B" (at 5)) (1 <-> 5),
        RightCase "relation (GTE)" "A >= B" $
          bin
            GTE
            (3 <-> 4)
            (con "A" (at 1))
            (1 <-> 4)
            (con "B" (at 6))
            (1 <-> 6),
        RightCase "boolean (Conj)" "A && B" $
          bin
            Conj
            (3 <-> 4)
            (con "A" (at 1))
            (1 <-> 4)
            (con "B" (at 6))
            (1 <-> 6),
        RightCase "boolean (Disj)" "A || B" $
          bin
            Disj
            (3 <-> 4)
            (con "A" (at 1))
            (1 <-> 4)
            (con "B" (at 6))
            (1 <-> 6),
        RightCase "boolean (Implies)" "A => B" $
          bin
            Implies
            (3 <-> 4)
            (con "A" (at 1))
            (1 <-> 4)
            (con "B" (at 6))
            (1 <-> 6),
        RightCase "boolean (Neg)" "~ A" $ un Neg (at 1) (con "A" (at 3)) (1 <-> 3),
        RightCase "boolean 1" "A || B => C" $
          bin
            Implies
            (8 <-> 9)
            ( bin
                Disj
                (3 <-> 4)
                (con "A" (at 1))
                (1 <-> 4)
                (con "B" (at 6))
                (1 <-> 6)
            )
            (1 <-> 9)
            (con "C" (at 11))
            (1 <-> 11),
        RightCase "boolean 2" "A || (B => C)" $
          bin
            Disj
            (3 <-> 4)
            (con "A" (at 1))
            (1 <-> 4)
            ( bin
                Implies
                (9 <-> 10)
                (con "B" (at 7))
                (7 <-> 10)
                (con "C" (at 12))
                (6 <-> 13)
            )
            (1 <-> 13),
        RightCase "boolean 3" "A || B && C" $
          bin
            Disj
            (3 <-> 4)
            (con "A" (at 1))
            (1 <-> 4)
            ( bin
                Conj
                (8 <-> 9)
                (con "B" (at 6))
                (6 <-> 9)
                (con "C" (at 11))
                (6 <-> 11)
            )
            (1 <-> 11),
        RightCase "boolean 4" "B && C || A" $
          bin
            Disj
            (8 <-> 9)
            ( bin
                Conj
                (3 <-> 4)
                (con "B" (at 1))
                (1 <-> 4)
                (con "C" (at 6))
                (1 <-> 6)
            )
            (1 <-> 9)
            (con "A" (at 11))
            (1 <-> 11),
        RightCase "quant 1" "<| (+) i : i > 0 : f i |>" $
          Quant
            (Op Add (4 <-> 6))
            [Name "i" (at 8)]
            ( bin
                GT
                (at 14)
                (var "i" (at 12))
                (12 <-> 14)
                (Lit (Num 0) (at 16))
                (12 <-> 16)
            )
            (App (var "f" (at 20)) (var "i" (at 22)) (20 <-> 22))
            (1 <-> 25),
        RightCase "function application 1" "(f (x)) y" $
          App
            (App (var "f" (at 2)) (Var (Name "x" (at 5)) (4 <-> 6)) (1 <-> 7))
            (var "y" (at 9))
            (1 <-> 9),
        RightCase "function application 2" "f (x y)" $
          App
            (var "f" (at 1))
            (App (var "x" (at 4)) (var "y" (at 6)) (3 <-> 7))
            (1 <-> 7),
        RightCase "mixed 1" "X * Y = N" $
          bin
            EQ
            (at 7)
            (bin Mul (at 3) (con "X" (at 1)) (1 <-> 3) (con "Y" (at 5)) (1 <-> 5))
            (1 <-> 7)
            (con "N" (at 9))
            (1 <-> 9),
        RightCase "mixed 2" "X * Y => P = Q" $
          bin
            Implies
            (7 <-> 8)
            (bin Mul (at 3) (con "X" (at 1)) (1 <-> 3) (con "Y" (at 5)) (1 <-> 5))
            (1 <-> 8)
            ( bin
                EQ
                (at 12)
                (con "P" (at 10))
                (10 <-> 12)
                (con "Q" (at 14))
                (10 <-> 14)
            )
            (1 <-> 14),
        RightCase "mixed 3" "X > Y && X > Y" $
          bin
            Conj
            (7 <-> 8)
            (bin GT (at 3) (con "X" (at 1)) (1 <-> 3) (con "Y" (at 5)) (1 <-> 5))
            (1 <-> 8)
            ( bin
                GT
                (at 12)
                (con "X" (at 10))
                (10 <-> 12)
                (con "Y" (at 14))
                (10 <-> 14)
            )
            (1 <-> 14)
      ]

con :: Text -> Loc -> Expr
con t l = Const (Name t l) l

var :: Text -> Loc -> Expr
var t l = Var (Name t l) l

bin :: Op -> Loc -> Expr -> Loc -> Expr -> Loc -> Expr
bin op opLoc a aLoc = App (App (Op op opLoc) a aLoc)

un :: Op -> Loc -> Expr -> Loc -> Expr
un op opLoc = App (Op op opLoc)

--------------------------------------------------------------------------------

-- | Type
type' :: TestTree
type' =
  testGroup "Types" $
    map
      (toTestTree Parser.type')
      [ RightCase "base types (Int)" "Int" $ TBase TInt (1 <-> 3),
        RightCase "base types (Bool)" "Bool" $ TBase TBool (1 <-> 4),
        RightCase "base types (Char)" "Char" $ TBase TChar (1 <-> 4),
        RightCase "function types 1" "(Char -> (Int))" $
          TFunc (TBase TChar (2 <-> 5)) (TBase TInt (10 <-> 14)) (1 <-> 15),
        RightCase "function types 2" "(Char -> Int) -> Int" $
          TFunc
            (TFunc (TBase TChar (2 <-> 5)) (TBase TInt (10 <-> 12)) (1 <-> 13))
            (TBase TInt (18 <-> 20))
            (1 <-> 20),
        RightCase
          "function types (with newlines everywhere)"
          "(Char \n\
          \   ->\n\
          \   (\n\
          \           Int))"
          $ TFunc (TBase TChar (2 <-> 5)) (TBase TInt (pos 3 4 16 <--> pos 4 15 32)) (pos 1 1 0 <--> pos 4 16 33),
        RightCase "array" "array [0 .. N) of Int" $
          TArray
            ( Interval
                (Including (Lit (Num 0) (at 8)))
                (Excluding (Const (Name "N" (at 13)) (at 13)))
                (7 <-> 14)
            )
            (TBase TInt (19 <-> 21))
            (1 <-> 21)
      ]

--------------------------------------------------------------------------------

-- | Declaration
declaration :: TestTree
declaration =
  testGroup "Declarations" $
    map
      (toTestTree Parser.declaration)
      [ RightCase "variable" "var x : Int\n" $
          VarDecl [Name "x" (at 5)] (TBase TInt (9 <-> 11)) Nothing (1 <-> 11),
        RightCase
          "variable (with newlines in between)"
          "var\n\
          \ x \n\
          \   : Int\n"
          $ VarDecl [Name "x" (pos 2 2 5 <--> pos 2 2 5)] (TBase TInt (pos 3 6 13 <--> pos 3 8 15)) Nothing (pos 1 1 0 <--> pos 3 8 15),
        RightCase "variable with properties" "var x : Int { True }\n" $
          VarDecl
            [Name "x" (at 5)]
            (TBase TInt (9 <-> 11))
            (Just (Lit (Bol True) (15 <-> 18)))
            (1 <-> 20),
        RightCase "constant" "con X, Y : Int\n" $
          ConstDecl
            [Name "X" (at 5), Name "Y" (at 8)]
            (TBase TInt (12 <-> 14))
            Nothing
            (1 <-> 14),
        RightCase "let binding" "let X i = N > 0\n" $
          LetDecl
            (Name "X" (at 5))
            ["i"]
            ( bin
                GT
                (at 13)
                (con "N" (at 11))
                (11 <-> 13)
                (Lit (Num 0) (at 15))
                (11 <-> 15)
            )
            (1 <-> 15)
      ]

--------------------------------------------------------------------------------

-- | Statements
statement :: TestTree
statement =
  testGroup "Single statement" $
    map
      (toTestTree Parser.statement)
      [ RightCase "skip" "skip" $ Skip (1 <-> 4),
        RightCase "abort" "abort" $ Abort (1 <-> 5),
        RightCase "assert" "{ True }" $
          Assert (Lit (Bol True) (3 <-> 6)) (1 <-> 8),
        RightCase "assign" "x := 0" $
          Assign [Name "x" (at 1)] [Lit (Num 0) (at 6)] (1 <-> 6),
        RightCase "assign (parallel)" "x, y := 0, 1" $
          Assign
            [Name "x" (at 1), Name "y" (at 4)]
            [Lit (Num 0) (at 9), Lit (Num 1) (at 12)]
            (1 <-> 12),
        RightCase "selection" "if True -> skip fi" $
          If
            [GdCmd (Lit (Bol True) (4 <-> 7)) [Skip (12 <-> 15)] (4 <-> 15)]
            (1 <-> 18),
        RightCase
          "selection 2"
          "if True -> skip\n\
          \ | False -> abort fi"
          $ If
            [ GdCmd (Lit (Bol True) (4 <-> 7)) [Skip (12 <-> 15)] (4 <-> 15),
              GdCmd (Lit (Bol False) (pos 2 4 19 <--> pos 2 8 23)) [Abort (pos 2 13 28 <--> pos 2 17 32)] (pos 2 4 19 <--> pos 2 17 32)
            ]
            (pos 1 1 0 <--> pos 2 20 35),
        RightCase "loop 1" "{ True , bnd: a }" $
          LoopInvariant
            (Lit (Bol True) (3 <-> 6))
            (Var (Name "a" (at 15)) (at 15))
            (1 <-> 17),
        RightCase "loop 2" "do True -> skip od" $
          Do
            [GdCmd (Lit (Bol True) (4 <-> 7)) [Skip (12 <-> 15)] (4 <-> 15)]
            (1 <-> 18)
      ]

(<->) :: Int -> Int -> Loc
(<->) from to =
  Loc (Pos "<test>" 1 from (from - 1)) (Pos "<test>" 1 to (to - 1))

at :: Int -> Loc
at n = n <-> n

statements :: TestTree
statements =
  testGroup "Multiple statements" $
    map
      (toTestTree Parser.statements1)
      [ RightCase
          "separated by newlines 1"
          "skip\nskip"
          [Skip $ pos 1 1 0 <--> pos 1 4 3, Skip $ pos 2 1 5 <--> pos 2 4 8],
        RightCase
          "separated by newlines 2"
          "skip\n\nskip\n"
          [Skip $ pos 1 1 0 <--> pos 1 4 3, Skip $ pos 3 1 6 <--> pos 3 4 9],
        RightCase
          "separated by newlines 3"
          "a := <| (+) i : 0 : 0 |>\na := 0"
          [ Assign
              [Name "a" $ pos 1 1 0 <--> pos 1 1 0]
              [ Quant
                  (Op Add $ pos 1 9 8 <--> pos 1 11 10)
                  [Name "i" $ pos 1 13 12 <--> pos 1 13 12]
                  (Lit (Num 0) $ pos 1 17 16 <--> pos 1 17 16)
                  (Lit (Num 0) $ pos 1 21 20 <--> pos 1 21 20)
                  (pos 1 6 5 <--> pos 1 24 23)
              ]
              (pos 1 1 0 <--> pos 1 24 23),
            Assign
              [Name "a" $ pos 2 1 25 <--> pos 2 1 25]
              [Lit (Num 0) $ pos 2 6 30 <--> pos 2 6 30]
              $ pos 2 1 25
                <--> pos 2 6 30
          ],
        -- , RightCase
        --     "separated by semicolons 1"
        --     "skip;skip"
        --     [Skip $ pos 1 1 0 <--> pos 1 4 3, Skip $ pos 1 6 5 <--> pos 1 9 8]
        -- , RightCase
        --     "separated by semicolons 2"
        --     "skip;\nskip"
        --     [Skip $ pos 1 1 0 <--> pos 1 4 3, Skip $ pos 2 1 6 <--> pos 2 4 9]
        -- , RightCase "separated by semicolons 4"
        --             "skip;"
        --             [Skip $ pos 1 1 0 <--> pos 1 4 3]
        -- , RightCase
        --     "separated by semicolons 3"
        --     "skip;\nskip;"
        --     [Skip $ pos 1 1 0 <--> pos 1 4 3, Skip $ pos 2 1 6 <--> pos 2 4 9]
        LeftCase2 "separated by a space" "skip abort"
      ]

pos :: Int -> Int -> Int -> Pos
pos = Pos "<test>"

--------------------------------------------------------------------------------


-- | Program
program :: TestTree
program =
  testGroup
    "Program"
    [ ast "empty" "./test/source/empty.gcl",
      ast "quant 1" "./test/source/quant1.gcl",
      ast "no-decl" "./test/source/no-decl.gcl",
      ast "no-stmt" "./test/source/no-stmt.gcl",
      ast "2" "./test/source/2.gcl",
      ast "issue 1" "./test/source/issue1.gcl",
      ast "issue 14" "./test/source/issue14.gcl",
      ast "comment" "./test/source/comment.gcl"
    ]
  where
    ast :: String -> FilePath -> TestTree
    ast name filePath =
      goldenTest
        name
        (readFile (filePath ++ ".ast.golden"))
        (readFile filePath)
        compare
        update

    readFile :: FilePath -> IO (FilePath, ByteString)
    readFile filePath = do
      raw <- BS.readFile filePath
      return (filePath, raw)

    compare ::
      (FilePath, ByteString) -> (FilePath, ByteString) -> IO (Maybe String)
    compare (_, expected) (filePath, actual) = do
      actual <- parseProgramAndRender (filePath, actual)
      if expected == actual
        then return Nothing
        else return (Just $ "expected:\n" ++ unpack expected ++ "\n------------\nactual: \n" ++ unpack actual)

    update :: (FilePath, ByteString) -> IO ()
    update (filePath, input) = do
      result <- parseProgramAndRender (filePath, input)
      let newPath = filePath ++ ".golden"
      createDirectoriesAndWriteFile newPath result

    parseProgramAndRender :: (FilePath, ByteString) -> IO ByteString
    parseProgramAndRender (filePath, input) =
      Text.encodeUtf8
          . renderLazy
          . layoutCompact
          . pretty
          <$> parseProgram (filePath, input)

    parseProgram :: (FilePath, ByteString) -> IO (Either Error Program)
    parseProgram (filePath, raw) = LSP.runM $ LSP.scan filePath (Text.decodeUtf8 raw) >>= LSP.parse Parser.program filePath
