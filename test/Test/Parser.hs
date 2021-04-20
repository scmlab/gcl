{-# LANGUAGE OverloadedStrings #-}

module Test.Parser where

import Data.Text.Prettyprint.Doc ( Pretty(pretty), defaultLayoutOptions, layoutPretty )
import Data.Text.Prettyprint.Doc.Render.Text ( renderLazy )

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.Encoding as Text

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS8

import Error ( Error(..) )
import qualified LSP 

import Syntax.Parser
import Syntax.Parser.Lexer
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex

import Test.Tasty.Golden.Advanced (goldenTest)
import Test.Tasty.Golden (createDirectoriesAndWriteFile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Data.Char (isSpace)
import Data.Int (Int64)

import Prelude hiding (compare)
import Control.Monad.Except (runExcept, withExcept, liftEither)
import Syntax.Parser.Token
import Syntax.Concrete (Type(..), TBase (..),ToAbstract (toAbstract), Program (..), Expr(..))
import Data.Loc (Located(locOf))
import Text.Megaparsec (Stream(reachOffset), setOffset, getOffset, MonadParsec(updateParserState, getParserState, observing, lookAhead, try, eof), State(State, statePosState, stateInput, stateOffset), getInput, getSourcePos)
import Control.Monad.Combinators (optional, many, (<|>))
import qualified Data.Ord as Ord
import Control.Monad (void)
import Syntax.Parser.Util (parser, (↓), getCurLoc)

tests :: TestTree
-- tests = testGroup "Prettifier" [myTest]
tests = testGroup "Prettifier" [expression, type', declaration, statement, parseError, golden]

--------------------------------------------------------------------------------

parse :: Parser a -> Text -> Either Error a
parse parser = 
  runExcept . withExcept SyntacticError . liftEither . runParse parser "<test>" 

render :: Pretty a => Either Error a -> Text
render = renderLazy . layoutPretty defaultLayoutOptions . pretty


isomorphic :: Pretty a => Parser a -> Text -> Assertion
isomorphic parser raw = removeWhitespace (render (parse parser raw)) @?= removeWhitespace raw
  where
    removeWhitespace :: Text -> Text
    removeWhitespace = Text.unlines . map Text.stripEnd . Text.lines


compare :: Pretty a => Parser a -> Text -> Text -> Assertion
compare parser actual expected = removeWhitespace (render (parse parser actual)) @?= removeWhitespace expected
  where
    removeWhitespace :: Text -> Text
    removeWhitespace = Text.unlines . map Text.stripEnd . Text.lines


myTest :: TestTree
myTest = 
  testGroup
    "parse test"
    [
      testCase "1" $ run
        "do x = 0 -> \n\
        \  skip\n\
        \  skip\n\
        \ | y = 0 -> \n\
        \  skip \n\
        \  skip \n\
        \od"

      -- testCase "2" $ run
      --   "= +"
    ]
    where
      run t = show (parse pStmt t) @?= ""
      quantWrap = (↓) pQuant scn
      wrap = (↓) (do
        v <- pVar
        eq <- notFollowedBySymbol lexEQ
        q <- pQuant 
        return (App (App (Op eq) v) q)) scn

      pNotFollowed = (↓) (do
          notFollowedBySymbol lexEQ
        ) scn

     -- dawn surround ritual toward fun planet affair friend edge soap news marble 

      

-- | Expression
expression :: TestTree
expression =
  testGroup
    "Expressions"
    [ testCase "literal (numbers)" $ run "1",
      testCase "literal (True)" $ run "True",
      testCase "literal (False)" $ run "False",
      testCase "variable" $ run "   x",
      testCase "constant" $ run " (X)",
      testCase "numeric 1" $ run "(1   \n  +  \n  (   \n   1))",
      testCase "numeric 2" $ run "A + X * Y",
      testCase "numeric 3" $ run "(A + X) * Y % 2",
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
    run = isomorphic (scn >> pExpr)

--------------------------------------------------------------------------------

-- | Type
type' :: TestTree
type' =
  testGroup
    "Types"
    [ testCase "base types (Int)" $ run "Int",
      testCase "base types (Bool)" $ run "(Bool)",
      testCase "base types (Bool)" $ run "  ((Bool))",
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
      testCase "array 3" $ run "array [  0 .. N  ] of     Int",
      testCase "array 4" $ run' 
        "array (  0 .. (Int) ) of \n Int" 
        "Error Syntactic Error [(<test>:1:16, using keyword as variable name )]\n"
    ]
  where
    run = isomorphic (scn >> pType)
    run' = compare (scn >> pType)

--------------------------------------------------------------------------------

-- | Declaration
declaration :: TestTree
declaration =
  testGroup
    "Declarations"
    [ testCase "variable" $ run "var   x     :   ( Int)",
      testCase "variable keyword collision 1" $ 
        run' "var if : Int" "Error Syntactic Error [(<test>:1:5, using keyword as variable name )]\n",
      testCase "variable keyword collision 2" $ run "var iff : Int",
      testCase "variable (with newlines in between)" $
        run
          "var\n\
          \  x \n\
          \   : Int\n",
      testCase "variable with properties" $ run "var x : Int  {    True \n }",
      testCase "constant" $ run "con X , Z,B, Y : Int",
      testCase "constant keyword collision 1" $ run' "con False : Int" "Error Syntactic Error [(<test>:1:5, using keyword as variable name )]\n",
      testCase "constant keyword collision 2" $ run "con Falsee : Int",
      testCase "let binding" $ run " let  X   i  =  N  >   (0)  "
    ]
  where
    run = isomorphic pDeclaration
    run' = compare pDeclaration

--------------------------------------------------------------------------------

-- | Statements
statement :: TestTree
statement =
  testGroup
    "Single statement"
    [ testCase "abort" $ run "     abort",
      testCase "skip" $ run "  skip",
      testCase "assertion" $ run "{ \n True   }",
      testCase "assignment" $ run "x := 0",
      testCase "assignment (parallel)" $ run "x   , y  := 0    ,    1",
      testCase "conditional 1" $ run "if True -> skip fi",
      testCase "conditional 2" $ run "if True ->    skip   \n | False -> abort \nfi",
      testCase "loop invariant" $ run "{ True ,     bnd      : a  }",
      testCase "loop body 1" $ run "do True -> skip od",
      testCase "loop body 2" $ run "do True    →       skip od"
    ]
  where
    run = isomorphic pStmt

--------------------------------------------------------------------------------

-- | Parse Error
parseError :: TestTree
parseError =
  testGroup
    "Parse error"
    [ testCase "quant with parentheses" $ run "<| (+) i : i > 0 : f i |>" "Error Syntactic Error [(<test>:1:5, unexpected \"+) i \" expecting expression )]\n"
    ]
  where
    run = compare pExpr


--------------------------------------------------------------------------------

-- | Golden Tests
golden :: TestTree
golden =
  testGroup
    "Program"
    [ ast "empty" "./test/source/" "empty.gcl",
      ast "2" "./test/source/" "2.gcl",
      ast "comment" "./test/source/" "comment.gcl",
      ast "issue 1" "./test/source/" "issue1.gcl",
      ast "issue 14" "./test/source/" "issue14.gcl",
      ast "no-decl" "./test/source/" "no-decl.gcl",
      ast "no-stmt" "./test/source/" "no-stmt.gcl",
      ast "assign" "./test/source/" "assign.gcl",
      ast "quant 1" "./test/source/" "quant1.gcl",
      ast "spec" "./test/source/" "spec.gcl",
      ast "gcd" "./test/source/examples/" "gcd.gcl"
    ]
  where
    suffixGolden :: FilePath -> FilePath
    suffixGolden filePath = filePath ++ ".ast.golden"

    ast :: String -> FilePath -> FilePath -> TestTree
    ast name filePath fileName =
      goldenTest
        name
        (readFile (filePath ++ "golden/") (fileName ++ ".ast.golden"))
        (readFile filePath fileName)
        compareAndReport
        update

    readFile :: FilePath -> FilePath -> IO (FilePath, FilePath, ByteString)
    readFile filePath fileName = do
      raw <- BS.readFile (filePath ++ fileName)
      return (filePath, fileName, raw)

    compareAndReport ::
      (FilePath, FilePath, ByteString) -> (FilePath, FilePath, ByteString) -> IO (Maybe String)
    compareAndReport (expectedPath, expectedFileName, expected) (actualPath, actualFileName, actualRaw) = do
      let actual = run actualRaw
      if removeTrailingWhitespace expected == removeTrailingWhitespace actual
        then return Nothing
        else do
          -- BS8.putStrLn expected
          -- BS8.putStrLn actual
          return $
            Just $
              "expected (" ++ expectedPath ++ expectedFileName ++ ", " ++ show (length (BS8.unpack expected)) ++ " chars):\n" ++ BS8.unpack expected ++ "\n------------\n"
                ++ "actual ("
                ++ actualPath ++ actualFileName
                ++ ", "
                ++ show (length (BS8.unpack actual))
                ++ " chars): \n"
                ++ BS8.unpack actual


    removeTrailingWhitespace :: ByteString -> ByteString
    removeTrailingWhitespace = BS8.unlines . map stripEnd . BS8.lines
      where 
        lastNonSpaceCharIndex :: ByteString -> Int64
        lastNonSpaceCharIndex = fromInteger . fst . BS8.foldl (\(acc, index) char -> if isSpace char then (acc, succ index) else (succ index, succ index)) (0, 0)

        stripEnd :: ByteString -> ByteString
        stripEnd s = BS8.take (lastNonSpaceCharIndex s) s

    update :: (FilePath, FilePath, ByteString) -> IO ()
    update (filePath, fileName, input) = do
      createDirectoriesAndWriteFile (filePath ++ "golden/" ++ fileName ++ ".ast.golden") (run input)

    run :: ByteString -> ByteString
    run = Text.encodeUtf8 . render . parse pProgram . Text.decodeUtf8

