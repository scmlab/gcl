{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Test.Type where

import Data.Loc (Loc(..))
import qualified Data.Map as Map
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Util (goldenFileTest, parseTest)
import Test.Tasty.HUnit (testCase, (@?=), Assertion)
import Control.Monad.Except (foldM, runExcept, withExcept)
import GCL.Type
    ( TM, inferExpr, inferDecl, checkType, checkStmt, checkProg )
import GCL.Common ( Env, emptyEnv )
import Syntax.Concrete.ToAbstract ( ToAbstract(toAbstract) )
import Syntax.Abstract
    ( Lit(..),
      Expr(..),
      Type(..),
      TBase(..),
      Interval(..),
      Endpoint(..) )
import Syntax.Common ( ArithOp, Name(Name) )
import Syntax.Parser (runParse, pExpr, pProgram, Parser, pStmt, pType, pDeclaration, pBlockDeclaration)
import Pretty ( Pretty(pretty), toText )
import Error (Error(..))
import Data.Map (Map)
import Control.Monad.State (evalStateT)

tests :: TestTree
tests = testGroup "Type" [exprTests, typeTests, stmtTests, declarationTests, blockDeclarationTests, fileTests]

exprTests :: TestTree
exprTests =
  testGroup
    "Infer Expr"
    [
      testCase "Lit 1" $
        exprCheck "0" "Int",
      testCase "Lit 2" $
        exprCheck "True" "Bool",
      testCase "Lit 3" $
        exprCheck "False" "Bool",
      testCase "Lit 4" $
        exprCheck "'a'" "Char",
      testCase "Chain 1" $
        exprCheck "i = j" "Bool",
      testCase "Chain 2" $
        exprCheck "i < j <= k" "Bool",
      testCase "Chain 3" $
        exprCheck "i = j >= k" "Bool",
      testCase "Chain 4" $
        exprCheck "i >= j <= k" "Bool",
      testCase "Chain 5" $
        exprCheck "b = (i < j)" "Bool",
      testCase "Arith 1" $
        exprCheck "i + j" "Int",
      testCase "Arith 2" $
        exprCheck "i - j" "Int",
      testCase "Arith 3" $
        exprCheck "i * j" "Int",
      testCase "Arith 4" $
        exprCheck "i / j" "Int",
      testCase "Arith 5" $
        exprCheck "i % j" "Int",
      testCase "Arith 6" $
        exprCheck "p ⇒ q" "Bool",
      testCase "Arith 7" $
        exprCheck "p ∧ q" "Bool",
      testCase "Arith 8" $
        exprCheck "p ∨ q" "Bool",
      testCase "Arith 9" $
        exprCheck "p ¬ q" "Bool",
      testCase "Arith 10" $
        exprCheck "i ↑ j" "Int",
      testCase "Arith 11" $
        exprCheck "i ↓ j" "Int",
      testCase "Combined Op 1" $
        exprCheck "i = j ∧ i = k" "Bool",
      testCase "Combined Op 2" $
        exprCheck "i = j ∧ i ≤ k" "Bool",
      -- testCase "Arr App 1" $
      --   exprCheck "Arr" "array [ 0 .. N ) of Int",
      testCase "Arr App 2" $
        exprCheck "Arr[i]" "Int",
      testCase "Arr App 3" $
        exprCheck "Arr[Arr[i]]" "Int",
      testCase "Func App 1" $
        exprCheck "P i" "Bool",
      testCase "Func App 2" $
        exprCheck "P i ∨ P j" "Bool",
      testCase "Func App 3" $
        exprCheck "P i => P j" "Bool",
      testCase "Func App 4" $
        exprCheck "Max i j" "Bool",
      testCase "Func App 5" $
        exprCheck "Q i" "Bool",
      testCase "Func App 6" $
        exprCheck "Q b" "Bool",
      -- testCase "Hole" $
      --   exprCheck "_" "TVar",
      testCase "Quant" $
        exprCheck "<| + i : 0 ≤ i < N : F i |>" "Int"
    ]

typeTests :: TestTree
typeTests = 
  testGroup 
    "Check Type" 
    [
      testCase "TBase 1" $
        typeCheck' "Int",
      testCase "TBase 2" $
        typeCheck' "Bool",
      testCase "TBase 3" $
        typeCheck' "Char",
      testCase "TArray 1" $
        typeCheck' "array (0 .. N] of Int",
      testCase "TArray 2" $
        typeCheck' "array (0 .. 5) of Bool",
      testCase "TArray 3" $
        typeCheck' "array [0 .. N) of Char",
      testCase "TFunc 1" $
        typeCheck' "Int -> Bool",
      testCase "TFunc 2" $
        typeCheck' "Int -> Int -> Int -> Bool",
      testCase "TFunc 3" $
        typeCheck' "Char -> Bool -> Int -> array [0 .. N) of Int",
      testCase "TVar" $
        typeCheck' "T"
    ]

stmtTests :: TestTree
stmtTests =
  testGroup
    "Check stmt"
    [
      testCase "skip" $
        stmtCheck' "skip",
      testCase "abort" $
        stmtCheck' "abort",
      testCase "assign 1" $
        stmtCheck' "i := j",
      testCase "assign 2" $
        stmtCheck' "i, j, k := 0, 0, 0", -- NOTE : not sure if assign should work this way
      testCase "assert 1" $
        stmtCheck' "{ A = B }",
      testCase "assert 2" $
        stmtCheck' "{ P i }",
      -- testCase "assert 3" $
      --   stmtCheck' "{ P }"
      testCase "loop invariant" $
        stmtCheck' "{ i >= 0, bnd: N }",
      -- testCase "loop invariant" $
      --   stmtCheck' "{A, bnd : N}",
      -- testCase "loop invariant" $
      --   stmtCheck' "{i >= 0, bnd : P}"
      testCase "loop 1" $
        stmtCheck'
          "do i /= N ->\n\
          \   skip\n\
          \od",
      testCase "loop 2" $
        stmtCheck'
          "do i /= N ->\n\
          \   i := i + 1\n\
          \   skip\n\
          \od",
      testCase "loop 3" $
        stmtCheck'
          "do i /= N ->\n\
          \   i := i + 1\n\
          \   skip\n\
          \ | j /= N ->\n\
          \   j := j + 1\n\
          \   skip\n\
          \od",
      testCase "loop 4" $
        stmtCheck'
          "if i /= N ->\n\
          \   i := i + 1\n\
          \   skip\n\
          \ | j /= N ->\n\
          \   j := j + 1\n\
          \   skip\n\
          \fi",
      testCase "spec 1" $
        stmtCheck' "[!   !]",
      testCase "spec 2" $
        stmtCheck' "[! asdff !]",
      testCase "spec 3" $
        stmtCheck'
          "[!\n\
          \asdfasdf\n\
          \!]",
      testCase "spec 4" $
        stmtCheck'
          "[!\n\
          \asdfasdf\n\
          \   !]"
      -- testCase "proof" $
      --   stmtCheck' ""
    ]
declarationTests :: TestTree
declarationTests =
  testGroup
    "Check Declaration"
    [
      testCase "const declaration" $
        declarationCheck "con C : Int" "[ ( C\n, Int ) ]",
      testCase "const declaration w/ prop" $
        declarationCheck "con C : Int { C > 0 }" "[ ( C\n, Int ) ]",
      testCase "var declaration" $
        declarationCheck "var x : Bool" "[ ( x\n, Bool ) ]",
      testCase "var declaration w/ prop" $
        declarationCheck "var x : Bool { x = True }" "[ ( x\n, Bool ) ]",
      testCase "let declaration 1" $
        declarationCheck "let N = 5" "[ ( N\n, Int ) ]",
      testCase "let declaration 2" $
        declarationCheck "let G i j = i + j" "[ ( G\n, Int → Int → Int ) ]"
    ]

blockDeclarationTests :: TestTree
blockDeclarationTests = 
  testGroup 
    "" 
    [
      testCase "block declaration 1" $
        blockDeclarationCheck 
        "{:\n\
        \  A, B : Int\
        \:}" 
        "[ ( A\n, Int )\n, ( B\n, Int ) ]",
      testCase "block declaration 2" $
        blockDeclarationCheck
        "{:\n\
        \  A, B : Int { A = 0 }\
        \:}"
        "[ ( A\n, Int )\n, ( B\n, Int ) ]",
      testCase "block declaration 3" $
        blockDeclarationCheck
        "{:\n\
        \  A, B : Int\n\
        \    A = 0\n\
        \:}"
        "[ ( A\n, Int )\n, ( B\n, Int ) ]",
      testCase "block declaration 4" $
        blockDeclarationCheck
        "{:\n\
        \  A, B : Int\n\
        \    A = 0\n\
        \  F : Int -> Int -> Int\n\
        \  P : Char -> Bool\n\
        \:}"
        "[ ( A\n, Int )\n, ( B\n, Int )\n, ( F\n, Int → Int → Int )\n, ( P\n, Char → Bool ) ]"
    ]

programTest :: TestTree
programTest = 
  testGroup
    "Check program"
    [
      testCase "program check 1" $
        programCheck
          "var i, j : Int\n\
          \let P x = i = j\n\
          \{ P 1 }\n\
          \"
    ]

fileTests :: TestTree
fileTests =
  testGroup
    "Check file"
    [ 
      typeCheckFile "2" "./test/source/" "2.gcl",
      typeCheckFile "quant1" "./test/source/" "quant1.gcl"
      -- typeCheckFile "mss" "./test/source/" "mss.gcl",
      -- typeCheckFile "posnegpairs" "./test/source/examples/" "posnegpairs.gcl"
    ]

typeCheckFile :: String -> FilePath -> FilePath -> TestTree
typeCheckFile name filePath fileName =
  goldenFileTest ".tc.golden" name filePath fileName fileCheck

fileCheck :: (FilePath, Text) -> Text
fileCheck (filepath, source) = toText result
  where
    result = case runParse pProgram filepath source of
      Left errors -> Left (map SyntacticError errors)
      Right ast -> case runExcept (toAbstract ast) of
        Left _ -> Left [Others "Should dig hole"]
        Right prog -> runExcept $ withExcept (pure . TypeError) $ checkProg prog

tint :: Type
tint = TBase TInt NoLoc

tbool :: Type
tbool = TBase TBool NoLoc

tchar :: Type
tchar = TBase TChar NoLoc

tvar :: Text -> Type
tvar x = TVar (name' x) NoLoc

tarr :: Endpoint -> Endpoint -> Type -> Type
tarr e1 e2 t = TArray (interval e1 e2) t NoLoc

interval :: Endpoint -> Endpoint -> Interval
interval e1 e2 = Interval e1 e2 NoLoc

tfunc :: Type -> Type -> Type
tfunc t1 t2 = TFunc t1 t2 NoLoc

litNum :: Int -> Expr
litNum i = Lit (Num i) NoLoc

litBool :: Bool -> Expr
litBool b = Lit (Bol b) NoLoc

litChar :: Char -> Expr
litChar c = Lit (Chr c) NoLoc

cons :: Text -> Expr
cons name = Const (Name name NoLoc) NoLoc

app :: Expr -> Expr -> Expr
app e1 e2 = App e1 e2 NoLoc

op :: ArithOp -> Expr
op = Op

var :: Text -> Expr
var t = Var (Name t NoLoc) NoLoc

name' :: Text -> Name
name' t = Name t NoLoc 

env :: Env Type
env =
    Map.fromList
      [
        (name' "A" , tint),
        (name' "B", tint),
        (name' "N", tint),
        (name' "Arr",tarr (Including (litNum 0)) (Excluding (cons "N")) tint),
        (name' "P", tfunc tint tbool),
        (name' "Q", tfunc (tvar "?m") tbool),
        (name' "F", tfunc tint tint),
        (name' "Max", tfunc tint (tfunc tint tbool)),
        (name' "i", tint),
        (name' "j", tint),
        (name' "k", tint),
        (name' "b", tbool),
        (name' "p", tbool),
        (name' "q", tbool),
        (name' "r", tbool)
      ]

runParser :: ToAbstract a b => Parser a -> Text -> Either (Either [Error] Loc) b
runParser p t =
  case runExcept . toAbstract <$> parseTest p t of
    Left errs -> Left $ Left $ map SyntacticError errs
    Right (Left loc) -> Left . Right $ loc
    Right (Right expr) -> Right expr

check ::
  (Env Type -> a -> TM b) ->
  Env Type -> a -> Either Error b
check checker env' e =
  case runExcept (evalStateT (checker env' e) 0) of
    Left err -> Left . TypeError $ err
    Right x -> Right x

exprCheck :: Text -> Text -> Assertion
exprCheck t1 t2 =
  toText (check inferExpr env <$> runParser pExpr t1) @?= t2

typeCheck :: Text -> Text -> Assertion
typeCheck t1 t2 =
  toText (check checkType env <$> runParser pType t1) @?= t2

typeCheck' :: Text -> Assertion
typeCheck' t = typeCheck t "()"

stmtCheck :: Text -> Text -> Assertion
stmtCheck t1 t2 =
  toText (check checkStmt env <$> runParser pStmt t1) @?= t2

stmtCheck' :: Text -> Assertion
stmtCheck' t = stmtCheck t "()"

instance (Pretty a, Pretty b) => Pretty (Map a b) where
  pretty m = pretty $ Map.toList m

declarationCheck :: Text -> Text -> Assertion 
declarationCheck t1 t2 =
  toText (check inferDecl emptyEnv <$> runParser pDeclaration t1) @?= t2

declarationCheck' :: Text -> Text -> Assertion 
declarationCheck' t1 t2 =
  toText (check inferDecl env <$> runParser pDeclaration t1) @?= t2

envCheck :: Text -> Assertion 
envCheck t =
  toText env @?= t

blockDeclarationCheck :: Text -> Text -> Assertion
blockDeclarationCheck t1 t2 =
  -- toString (map (check inferDecl emptyEnv) <$> runParser pBlockDeclaration t1) @?= t2
  toText wrap @?= t2
  where
    wrap = do
      ds <- runParser pBlockDeclaration t1
      foldM (\envM d -> 
        case envM of
          Left err -> return (Left err)
          Right env' -> return (check inferDecl env' d)
          ) (Right emptyEnv) ds
  
programCheck :: Text -> Assertion
programCheck t1 = 
  toText wrap @?= "()"
  where
    -- wrap :: Either Error ()
    wrap = do
      prog <- runParser pProgram t1
      case runExcept (checkProg prog) of
        Left err -> Left . Left $ [TypeError err]
        Right x -> Right x