{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE FlexibleContexts #-}
module Test.Type where

import Data.Loc (Loc(..), Pos(..))
import qualified Data.Map as Map
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Util (goldenFileTest, parseTest, render)
import Test.Tasty.HUnit (testCase, (@?=), Assertion)
import Control.Monad.Except (runExcept, withExcept, liftEither)
import GCL.Type (TypeEnv(TypeEnv), TypeError (UnifyFailed, NotInScope), SubstT, emptySubstT, Scheme (ForallV), runInfer, lookupEnv, checkProg, runSolver', inferExpr, runSolver, infer, TM, checkStmt)
import Syntax.Concrete.ToAbstract ( ToAbstract(toAbstract) )
import Syntax.Abstract
    ( Lit(..),
      Expr(Var, Lit, Const, App, Op),
      Type(TArray, TBase, TFunc),
      TBase(TBool, TChar, TInt),
      Interval(..),
      Endpoint(..) )
import Syntax.Common ( ArithOp, Name(Name) )
import Syntax.Parser (runParse, pExpr, pProgram, Parser, pStmt)
import Pretty ()
import Error (Error(..))
import Data.Text.Prettyprint.Doc.Internal (layoutCompact, Pretty (pretty))
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)

tests :: TestTree
tests = testGroup "Type" [exprTests, stmtTests, fileTests]

exprTests :: TestTree
exprTests =
  testGroup
    "infer Expr"
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
      testCase "Arr App 1" $
        exprCheck "Arr" "array [ 0 .. N ) of Int",
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
      -- testCase "Hole" $
      --   exprCheck "_" "TVar",
      testCase "Quant" $
        exprCheck "<| + i : 0 ≤ i < N : F i |>" "Int"
    ]

stmtTests :: TestTree
stmtTests = 
  testGroup 
    "check stmt" 
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

fileTests :: TestTree
fileTests =
  testGroup
    "Type Check"
    [ typeCheckFile "2" "./test/source/" "2.gcl",
      typeCheckFile "quant1" "./test/source/" "quant1.gcl",
      typeCheckFile "mss" "./test/source/" "mss.gcl",
      typeCheckFile "posnegpairs" "./test/source/examples/" "posnegpairs.gcl"
    ]

typeCheckFile :: String -> FilePath -> FilePath -> TestTree
typeCheckFile name filePath fileName =
  goldenFileTest ".tc.golden" name filePath fileName fileCheck

fileCheck :: (FilePath, Text) -> Text
fileCheck (filepath, source) = renderStrict . layoutCompact . pretty $ result
  where
    result = case runParse pProgram filepath source of
      Left err -> Left (SyntacticError err)
      Right ast -> case runExcept (toAbstract ast) of
        Left err -> Left (Others "Should dig hole")
        Right prog -> runExcept $ withExcept TypeError $ checkProg prog

tint :: Type
tint = TBase TInt NoLoc

tbool :: Type
tbool = TBase TBool NoLoc

tchar :: Type
tchar = TBase TChar NoLoc

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

env :: TypeEnv
env =
  TypeEnv $
    Map.fromList
      [ 
        ("A", ForallV [] tint),
        ("B", ForallV [] tint),
        ("N", ForallV [] tint),
        ("Arr", ForallV [] (tarr (Including (litNum 0)) (Excluding (cons "N")) tint)),
        ("P", ForallV [] (tfunc tint tbool)),
        ("F", ForallV [] (tfunc tint tint)),
        ("Max", ForallV [] (tfunc tint (tfunc tint tbool))),
        ("i", ForallV [] tint),
        ("j", ForallV [] tint),
        ("k", ForallV [] tint)
      ]

runParser :: ToAbstract a b => Parser a -> Text -> Either (Either Error Loc) b
runParser p t = 
  case runExcept . toAbstract <$> parseTest p t of
    Left errs -> Left . Left . SyntacticError $ errs
    Right (Left loc) -> Left . Right $ loc
    Right (Right expr) -> Right expr

runInferExpr :: TypeEnv -> Expr -> Either Error Type
runInferExpr env expr = 
  case runExcept (inferExpr env expr) of
    Left err -> Left . TypeError $ err
    Right (ForallV _ t) -> Right t

exprCheck :: Text -> Text -> Assertion
exprCheck t1 t2 = 
  render (runInferExpr env <$> runParser pExpr t1) @?= t2

typeCheck' :: 
  (TypeEnv -> a -> TM ()) ->
  TypeEnv -> a -> Either Error ()
typeCheck' check env e = 
  case runExcept (check env e) of
    Left err -> Left . TypeError $ err
    Right _ -> Right ()

stmtCheck :: Text -> Text -> Assertion
stmtCheck t1 t2 =
  render (typeCheck' checkStmt env <$> runParser pStmt t1) @?= t2

stmtCheck' :: Text -> Assertion
stmtCheck' t = stmtCheck t "()"