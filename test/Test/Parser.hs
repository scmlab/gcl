{-# LANGUAGE OverloadedStrings #-}

module Test.Parser where

import qualified Data.ByteString as Strict
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy.Char8
  ( unpack,
  )
import qualified Data.ByteString.Lazy.Char8 as BS8
import Data.Loc
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Text.Lazy (fromStrict, toStrict)
import qualified Data.Text.Lazy.Encoding as LazyText
-- import Data.Text.Lazy (Text)
import Data.Text.Prettyprint.Doc.Render.Text
  ( renderLazy,
  )
import Error
import qualified LSP
import Pretty
import Syntax.Concrete2
import Syntax.Parser (Parser)
import qualified Syntax.Parser as Parser
import Test.Tasty
import Test.Tasty.Golden (createDirectoriesAndWriteFile)
import Test.Tasty.Golden.Advanced
import Test.Tasty.HUnit
import Text.Megaparsec (eof)
import Prelude hiding (Ordering (..))

tests :: TestTree
tests =
  testGroup
    "Parser" []
--     [expression1, type', declaration, statement, programGolden]

-- --------------------------------------------------------------------------------

-- -- | Expression
-- expression1 :: TestTree
-- expression1 =
--   testGroup
--     "Expressions 1"
--     [ testCase "hole" $ run "?" @?= "{!!}", -- hole in expressions, not spec
--       testCase "literal (numbers)" $ run "1" @?= "1",
--       testCase "literal (True)" $ run "True" @?= "True",
--       testCase "literal (False)" $ run "False" @?= "False",
--       testCase "variable" $ run "x" @?= "x",
--       testCase "constant" $ run "X" @?= "X",
--       testCase "numeric 1" $
--         run
--           "(1 \n\
--           \ +\n\
--           \ (\n\
--           \   1))"
--           @?= "1\n\
--               \ +\n\
--               \ 1",
--       testCase "numeric 2" $ run "A + X * Y" @?= "A + X * Y",
--       testCase "numeric 3" $ run "(A + X) * Y % 2" @?= "(A + X) * Y % 2",
--       testCase "relation (EQ)" $ run "A = B" @?= "A = B",
--       testCase "relation (NEQ)" $ run "A /= B" @?= "A ≠ B",
--       testCase "relation (LT)" $ run "A < B" @?= "A < B",
--       testCase "relation (LTE)" $ run "A <= B" @?= "A ≤ B",
--       testCase "relation (GT)" $ run "A > B" @?= "A > B",
--       testCase "relation (LTE)" $ run "A >= B" @?= "A ≥ B",
--       testCase "boolean (Conj)" $ run "A && B" @?= "A ∧ B",
--       testCase "boolean (Disj)" $ run "A || B" @?= "A ∨ B",
--       testCase "boolean (Implies)" $ run "A => B" @?= "A → B",
--       testCase "boolean (Neg)" $ run "~ A" @?= "¬ A",
--       testCase "boolean 1" $ run "A || B => C" @?= "A ∨ B → C",
--       testCase "boolean 2" $ run "A || (B => C)" @?= "A ∨ (B → C)",
--       testCase "boolean 3" $ run "A || B && C" @?= "A ∨ B ∧ C",
--       testCase "boolean 4" $ run "B && C || A" @?= "B ∧ C ∨ A",
--       testCase "quant 1" $ run "<| (+) i : i > 0 : f i |>" @?= "⟨i : i > 0 : f i ⟩",
--       testCase "function application 1" $ run "(f (x)) y" @?= "f  x   y",
--       testCase "function application 2" $ run "f (x y)" @?= "f  x y",
--       testCase "mixed 1" $ run "X * Y = N" @?= "X * Y = N",
--       testCase "mixed 2" $ run "X * Y => P = Q" @?= "X * Y → P = Q",
--       testCase "mixed 3" $ run "X > Y && X > Y" @?= "X > Y ∧ X > Y"
--     ]
--   where
--     run :: Text -> Text
--     run text =
--       let expr = LSP.runM $ LSP.scan "<test>" text >>= LSP.parse Parser.expression "<test>" :: Either Error Expr
--        in renderStrict $ pretty expr

-- expression2 :: TestTree
-- expression2 =
--   testGroup
--     "Expressions 2"
--     [ testCase "1" $ run "X > Y && X > Y" @?= "X > Y ∧ X > Y",
--       testCase "2" $ run "1 + 2 * 3 - 4" @?= "1 + 2 * 3 - 4",
--       testCase "3" $ run "1 + 2 * 3 = 4" @?= "1 + 2 * 3 = 4",
--       testCase "4" $ run "1 > 2 = True" @?= "1 > 2 = True",
--       testCase "5" $ run "(1 + 2) * 3 = (4)" @?= "(1 + 2) * 3 = 4",
--       testCase "6" $ run "3 / (2 + X)" @?= "3 / (2 + X)",
--       testCase "7" $ run "3 / 2 + X" @?= "3 / 2 + X"
--     ]
--   where
--     run :: Text -> Text
--     run text =
--       let expr = LSP.runM $ LSP.scan "<test>" text >>= LSP.parse Parser.expression "<test>" :: Either Error Expr
--        in renderStrict $ pretty expr

-- --------------------------------------------------------------------------------

-- -- | Type
-- type' :: TestTree
-- type' =
--   testGroup
--     "Types"
--     [ testCase "base types (Int)" $ run "Int" @?= "Int",
--       testCase "base types (Bool)" $ run "Bool" @?= "Bool",
--       testCase "base types (Char)" $ run "Char" @?= "Char",
--       testCase "function types 1" $ run "(Char -> (Int))" @?= "Char  → Int",
--       testCase "function types 2" $ run "(Char -> Int) -> Int" @?= "Char  → Int  → Int",
--       testCase "function types (with newlines everywhere)" $
--         run
--           "(Char \n\
--           \   ->\n\
--           \   (\n\
--           \           Int))"
--           @?= "Char\n\n → Int",
--       testCase "array" $ run "array [0 .. N) of Int" @?= "array [0 .. N) of Int"
--     ]
--   where
--     run :: Text -> Text
--     run text =
--       let expr = LSP.runM $ LSP.scan "<test>" text >>= LSP.parse Parser.type' "<test>" :: Either Error Type
--        in renderStrict $ pretty expr

-- --------------------------------------------------------------------------------

-- -- | Declaration
-- declaration :: TestTree
-- declaration =
--   testGroup "Declarations"
--     [ testCase "variable" $ run "var x : Int" @?= "var x : Int",
--       testCase "variable (with newlines in between)" $
--         run
--           "var\n\
--           \ x \n\
--           \   : Int\n"
--           @?= "var x\n   : Int",
--       testCase "variable with properties" $ run "var x : Int { True }" @?= "var x : Int { True }",
--       testCase "constant" $ run "con X, Y : Int" @?= "con X, Y : Int",
--       testCase "let binding" $ run "let X i = N > 0" @?= "let X i = N > 0"
--     ]
--   where
--     run :: Text -> Text
--     run text =
--       let expr = LSP.runM $ LSP.scan "<test>" text >>= LSP.parse Parser.declaration "<test>" :: Either Error Declaration
--        in renderStrict $ pretty expr

-- --------------------------------------------------------------------------------

-- -- | Statements
-- statement :: TestTree
-- statement =
--   testGroup "Single statement"
--     [ testCase "abort" $ run "abort" @?= "abort",
--       testCase "skip" $ run "skip" @?= "skip",
--       testCase "assertion" $ run "{ True }" @?= "{ True }",
--       testCase "assignment" $ run "x := 0" @?= "x := 0",
--       testCase "assignment (parallel)" $ run "x, y := 0, 1" @?= "x, y := 0, 1",
--       testCase "conditional 1" $ run "if True -> skip fi" @?= "if True  ->skip\nfi",
--       testCase "conditional 2" $ run "if True -> skip\n | False -> abort fi" @?= "if True  ->skip\n | False  ->abort\nfi",
--       testCase "loop invariant" $ run "{ True , bnd: a }" @?= "{ True , bnd: a }",
--       testCase "loop body" $ run "do True -> skip od" @?= "do True  ->skip\nod"
--     ]
--   where
--     run :: Text -> Text
--     run text =
--       let expr = LSP.runM $ LSP.scan "<test>" text >>= LSP.parse Parser.statement "<test>" :: Either Error Stmt
--        in renderStrict $ pretty expr

-- -- statements :: TestTree
-- -- statements =
-- --   testGroup "Multiple statements" $
-- --     map
-- --       (toTestTree Parser.statements1)
-- --       [ RightCase
-- --           "separated by newlines 1"
-- --           "skip\nskip"
-- --           [Skip $ pos 1 1 0 <--> pos 1 4 3, Skip $ pos 2 1 5 <--> pos 2 4 8],
-- --         RightCase
-- --           "separated by newlines 2"
-- --           "skip\n\nskip\n"
-- --           [Skip $ pos 1 1 0 <--> pos 1 4 3, Skip $ pos 3 1 6 <--> pos 3 4 9],
-- --         RightCase
-- --           "separated by newlines 3"
-- --           "a := <| (+) i : 0 : 0 |>\na := 0"
-- --           [ Assign
-- --               [Name "a" $ pos 1 1 0 <--> pos 1 1 0]
-- --               [ Quant
-- --                   (Op Sum $ pos 1 9 8 <--> pos 1 11 10)
-- --                   [Name "i" $ pos 1 13 12 <--> pos 1 13 12]
-- --                   (Lit (Num 0) $ pos 1 17 16 <--> pos 1 17 16)
-- --                   (Lit (Num 0) $ pos 1 21 20 <--> pos 1 21 20)
-- --                   (pos 1 6 5 <--> pos 1 24 23)
-- --               ]
-- --               (pos 1 1 0 <--> pos 1 24 23),
-- --             Assign
-- --               [Name "a" $ pos 2 1 25 <--> pos 2 1 25]
-- --               [Lit (Num 0) $ pos 2 6 30 <--> pos 2 6 30]
-- --               $ pos 2 1 25
-- --                 <--> pos 2 6 30
-- --           ],
-- --         -- , RightCase
-- --         --     "separated by semicolons 1"
-- --         --     "skip;skip"
-- --         --     [Skip $ pos 1 1 0 <--> pos 1 4 3, Skip $ pos 1 6 5 <--> pos 1 9 8]
-- --         -- , RightCase
-- --         --     "separated by semicolons 2"
-- --         --     "skip;\nskip"
-- --         --     [Skip $ pos 1 1 0 <--> pos 1 4 3, Skip $ pos 2 1 6 <--> pos 2 4 9]
-- --         -- , RightCase "separated by semicolons 4"
-- --         --             "skip;"
-- --         --             [Skip $ pos 1 1 0 <--> pos 1 4 3]
-- --         -- , RightCase
-- --         --     "separated by semicolons 3"
-- --         --     "skip;\nskip;"
-- --         --     [Skip $ pos 1 1 0 <--> pos 1 4 3, Skip $ pos 2 1 6 <--> pos 2 4 9]
-- --         LeftCase2 "separated by a space" "skip abort"
-- --       ]
-- --   where 
-- --     run :: Text -> Text
-- --     run text =
-- --       let expr = LSP.runM $ LSP.scan "<test>" text >>= LSP.parse Parser.statement "<test>" :: Either Error Stmt
-- --        in renderStrict $ pretty expr

-- --------------------------------------------------------------------------------

-- -- | Golden tests for programs
-- programGolden :: TestTree
-- programGolden =
--   testGroup
--     "Program"
--     [ ast "empty" "./test/source/empty.gcl",
--       ast "2" "./test/source/2.gcl",
--       ast "comment" "./test/source/comment.gcl",
--       ast "issue 1" "./test/source/issue1.gcl",
--       ast "issue 14" "./test/source/issue14.gcl",
--       ast "no-decl" "./test/source/no-decl.gcl",
--       ast "no-stmt" "./test/source/no-stmt.gcl",
--       ast "quant 1" "./test/source/quant1.gcl",
--       ast "spec" "./test/source/spec.gcl"
--     ]
--   where
--     suffixGolden :: FilePath -> FilePath
--     suffixGolden filePath = filePath ++ ".ast.golden"

--     ast :: String -> FilePath -> TestTree
--     ast name filePath =
--       goldenTest
--         name
--         (readFile (suffixGolden filePath))
--         (readFile filePath)
--         compareAndReport
--         update

--     readFile :: FilePath -> IO (FilePath, ByteString)
--     readFile filePath = do
--       raw <- BS.readFile filePath
--       return (filePath, raw)

--     compareAndReport ::
--       (FilePath, ByteString) -> (FilePath, ByteString) -> IO (Maybe String)
--     compareAndReport (expectedPath, expected) (actualPath, actualRaw) = do
--       let actual = run (actualPath, actualRaw)
--       if expected == actual
--         then return Nothing
--         else do
--           -- BS8.putStrLn expected
--           -- BS8.putStrLn actual
--           return $
--             Just $
--               "expected (" ++ expectedPath ++ ", " ++ show (length (unpack expected)) ++ " chars):\n" ++ unpack expected ++ "\n------------\n"
--                 ++ "actual ("
--                 ++ actualPath
--                 ++ ", "
--                 ++ show (length (unpack actual))
--                 ++ " chars): \n"
--                 ++ unpack actual

--     update :: (FilePath, ByteString) -> IO ()
--     update (filePath, input) = do
--       createDirectoriesAndWriteFile (suffixGolden filePath) (run (filePath, input))

--     run :: (FilePath, ByteString) -> ByteString
--     run (filePath, input) =
--       LazyText.encodeUtf8
--         . renderLazy
--         . layoutCompact
--         . pretty
--         $ parseProgram (filePath, input)

--     parseProgram :: (FilePath, ByteString) -> Either Error Program
--     parseProgram (filePath, raw) = LSP.runM $ LSP.scan filePath (toStrict $ LazyText.decodeUtf8 raw) >>= LSP.parse Parser.program filePath
