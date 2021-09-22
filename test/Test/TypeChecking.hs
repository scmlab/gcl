{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Test.TypeChecking where

import           Control.Monad.Except           ( runExcept )
import           Control.Monad.State            ( evalStateT )
import           Data.Either                    ( partitionEithers )
import           Data.Loc                       ( Loc(..) )
import qualified Data.Map                      as Map
import           Data.Map                       ( Map )
import           Data.Text                      ( Text )
import           Error                          ( Error(..) )
import           GCL.Type                       ( Environment(..)
                                                , TM
                                                , checkEnvironment
                                                , checkProg
                                                , checkStmt
                                                , checkType
                                                , defnsAndDeclsToEnv
                                                , inferExpr
                                                , runTM
                                                )
import           Pretty                         ( Pretty(pretty)
                                                , toByteString
                                                , toText
                                                )
import           Syntax.Abstract                ( Defns(Defns)
                                                , Endpoint(..)
                                                , Expr(..)
                                                , Interval(..)
                                                , Lit(..)
                                                , TBase(..)
                                                , Type(..)
                                                )
import           Syntax.Abstract.Util           ( extractQDCons )
import           Syntax.Common                  ( Name(Name)
                                                , Op
                                                )
import           Syntax.Concrete                ( ToAbstract(toAbstract) )
import           Syntax.ConstExpr               ( pickLetBindings )
import           Syntax.Parser                  ( Parser
                                                , pBlockDeclaration
                                                , pDeclaration
                                                , pExpr
                                                , pProgram
                                                , pStmt
                                                , pType
                                                , runParse
                                                )
import           Test.Tasty                     ( TestTree
                                                , testGroup
                                                )
import           Test.Tasty.HUnit               ( (@?=)
                                                , Assertion
                                                , testCase
                                                )
import           Test.Util                      ( parseTest
                                                , runGoldenTest
                                                )

tests :: TestTree
tests = testGroup
  "Type"
  [ exprTests
  , typeTests
  , stmtTests
  , declarationTests
  , blockDeclarationTests
  , fileTests
  ]

exprTests :: TestTree
exprTests = testGroup
  "Infer Expr"
  [ testCase "Lit 1" $ exprCheck "0" "Int"
  , testCase "Lit 2" $ exprCheck "True" "Bool"
  , testCase "Lit 3" $ exprCheck "False" "Bool"
  , testCase "Lit 4" $ exprCheck "'a'" "Char"
  , testCase "Chain 1" $ exprCheck "i = j" "Bool"
  , testCase "Chain 2" $ exprCheck "i < j <= k" "Bool"
  , testCase "Chain 3" $ exprCheck "i = j >= k" "Bool"
  , testCase "Chain 4" $ exprCheck "i >= j <= k" "Bool"
  , testCase "Chain 5" $ exprCheck "b = (i < j)" "Bool"
  , testCase "Arith 1" $ exprCheck "i + j" "Int"
  , testCase "Arith 2" $ exprCheck "i - j" "Int"
  , testCase "Arith 3" $ exprCheck "i * j" "Int"
  , testCase "Arith 4" $ exprCheck "i / j" "Int"
  , testCase "Arith 5" $ exprCheck "i % j" "Int"
  , testCase "Arith 6" $ exprCheck "p ⇒ q" "Bool"
  , testCase "Arith 7" $ exprCheck "p ∧ q" "Bool"
  , testCase "Arith 8" $ exprCheck "p ∨ q" "Bool"
  , testCase "Arith 9" $ exprCheck "p ¬ q" "Bool"
  , testCase "Arith 10" $ exprCheck "i ↑ j" "Int"
  , testCase "Arith 11" $ exprCheck "i ↓ j" "Int"
  , testCase "Combined Op 1" $ exprCheck "i = j ∧ i = k" "Bool"
  , testCase "Combined Op 2" $ exprCheck "i = j ∧ i ≤ k" "Bool"
  ,
      -- testCase "Arr App 1" $
      --   exprCheck "Arr" "array [ 0 .. N ) of Int",
    testCase "Arr App 2" $ exprCheck "Arr[i]" "Int"
  , testCase "Arr App 3" $ exprCheck "Arr[Arr[i]]" "Int"
  , testCase "Func App 1" $ exprCheck "P i" "Bool"
  , testCase "Func App 2" $ exprCheck "P i ∨ P j" "Bool"
  , testCase "Func App 3" $ exprCheck "P i => P j" "Bool"
  , testCase "Func App 4" $ exprCheck "Max i j" "Bool"
  , testCase "Func App 5" $ exprCheck "Q i" "Bool"
  , testCase "Func App 6" $ exprCheck "Q b" "Bool"
  ,
      -- testCase "Hole" $
      --   exprCheck "_" "TVar",
    testCase "Quant 1" $ exprCheck "<| + i : 0 ≤ i < N : F i |>" "Int"
  , testCase "Quant 2" $ exprCheck "<| ∃ c : c = 'a' : G c |>" "Bool"
  ]

typeTests :: TestTree
typeTests = testGroup
  "Check Type"
  [ testCase "TBase 1" $ typeCheck' "Int"
  , testCase "TBase 2" $ typeCheck' "Bool"
  , testCase "TBase 3" $ typeCheck' "Char"
  , testCase "TArray 1" $ typeCheck' "array (0 .. N] of Int"
  , testCase "TArray 2" $ typeCheck' "array (0 .. 5) of Bool"
  , testCase "TArray 3" $ typeCheck' "array [0 .. N) of Char"
  , testCase "TFunc 1" $ typeCheck' "Int -> Bool"
  , testCase "TFunc 2" $ typeCheck' "Int -> Int -> Int -> Bool"
  , testCase "TFunc 3"
    $ typeCheck' "Char -> Bool -> Int -> array [0 .. N) of Int"
  --, testCase "TVar" $ typeCheck' "T"
  ]

stmtTests :: TestTree
stmtTests = testGroup
  "Check stmt"
  [ testCase "skip" $ stmtCheck' "skip"
  , testCase "abort" $ stmtCheck' "abort"
  , testCase "assign 1" $ stmtCheck' "i := j"
  , testCase "assign 2" $ stmtCheck' "i, j, k := 0, 0, 0"
  , -- NOTE : not sure if assign should work this way
    testCase "assert 1" $ stmtCheck' "{ A = B }"
  , testCase "assert 2" $ stmtCheck' "{ P i }"
  ,
      -- testCase "assert 3" $
      --   stmtCheck' "{ P }"
    testCase "loop invariant" $ stmtCheck' "{ i >= 0, bnd: N }"
  ,
      -- testCase "loop invariant" $
      --   stmtCheck' "{A, bnd : N}",
      -- testCase "loop invariant" $
      --   stmtCheck' "{i >= 0, bnd : P}"
    testCase "loop 1"
    $ stmtCheck' "do i /= N ->\n\
          \   skip\n\
          \od"
  , testCase "loop 2"
    $ stmtCheck'
        "do i /= N ->\n\
          \   i := i + 1\n\
          \   skip\n\
          \od"
  , testCase "loop 3"
    $ stmtCheck'
        "do i /= N ->\n\
          \   i := i + 1\n\
          \   skip\n\
          \ | j /= N ->\n\
          \   j := j + 1\n\
          \   skip\n\
          \od"
  , testCase "loop 4"
    $ stmtCheck'
        "if i /= N ->\n\
          \   i := i + 1\n\
          \   skip\n\
          \ | j /= N ->\n\
          \   j := j + 1\n\
          \   skip\n\
          \fi"
  , testCase "spec 1" $ stmtCheck' "[!   !]"
  , testCase "spec 2" $ stmtCheck' "[! asdff !]"
  , testCase "spec 3" $ stmtCheck' "[!\n\
          \asdfasdf\n\
          \!]"
  , testCase "spec 4"
    $ stmtCheck' "[!\n\
          \asdfasdf\n\
          \   !]"
      -- testCase "proof" $
      --   stmtCheck' ""
  ]
declarationTests :: TestTree
declarationTests = testGroup
  "Check Declaration"
  [ testCase "const declaration"
    $ declarationCheck "con C : Int" "Environment[(C, Int)][][]"
  , testCase "const declaration w/ prop"
    $ declarationCheck "con C : Int { C > 0 }" "Environment[(C, Int)][][]"
  , testCase "var declaration"
    $ declarationCheck "var x : Bool" "Environment[(x, Bool)][][]"
  , testCase "var declaration w/ prop"
    $ declarationCheck "var x : Bool { x = True }" "Environment[(x, Bool)][][]"
  , testCase "type declaration" $ declarationCheck
    "data T a = Nil | Con a"
    "Environment[(Con, TVar a → T a), (Nil, T a)][(T, (T a, [Nil , Con (TVar a)]))][]"
  ]

blockDeclarationTests :: TestTree
blockDeclarationTests = testGroup
  ""
  [ testCase "block declaration 1" $ blockDeclarationCheck
    "{:\n\
        \  A, B : Int\
        \:}"
    "Environment[(A, Int), (B, Int)][][]"
  , testCase "block declaration 2" $ blockDeclarationCheck
    "{:\n\
        \  A, B : Int { A = 0 }\
        \:}"
    "Environment[(A, Int), (B, Int)][][]"
  , testCase "block declaration 3" $ blockDeclarationCheck
    "{:\n\
        \  A, B : Int\n\
        \    A = 0\n\
        \:}"
    "Environment[(A, Int), (B, Int)][][]"
  , testCase "block declaration 4" $ blockDeclarationCheck
    "{:\n\
        \  A, B : Int\n\
        \    A = 0\n\
        \  F : Int -> Int -> Int\n\
        \  P : Char -> Bool\n\
        \:}"
    "Environment[(A, Int), (B, Int), (F, Int → Int → Int), (P, Char → Bool)][][]"
  , testCase "block declaration 5" $ blockDeclarationCheck
    "{:\n\
        \   N = 5\n\
        \:}"
    "Environment[(N, Int)][][(N, 5)]"
  , testCase "block declaration 6" $ blockDeclarationCheck
    "{:\n\
        \    G i j = i + j\n\
        \:}"
    "Environment[(G, Int → Int → Int)][][(G, λ i → λ j → i + j)]"
  ]

programTest :: TestTree
programTest = testGroup
  "Check program"
  [ testCase "program check 1"
      $ programCheck
          "var i, j : Int\n\
          \{: P x = i = j :}\n\
          \{ P 1 }\n\
          \"
  ]

fileTests :: TestTree
fileTests = testGroup
  "Check file"
  [ typeCheckFile "" "2"      "2.gcl"
  , typeCheckFile "" "quant1" "quant1.gcl"
      -- typeCheckFile "mss" "./test/source/" "mss.gcl",
      -- typeCheckFile "posnegpairs" "./test/source/examples/" "posnegpairs.gcl"
  ]

typeCheckFile :: String -> FilePath -> FilePath -> TestTree
typeCheckFile dirName =
  runGoldenTest ("./test/source/" <> dirName)
                ("./test/golden/" <> dirName)
                ".tc"
    $ \filepath source -> do

        let result = case runParse pProgram filepath source of
              Left  errors -> Left (map SyntacticError errors)
              Right ast    -> case runExcept (toAbstract ast) of
                Left  _    -> Left [Others "Should dig hole"]
                Right prog -> case runTM (checkProg prog) of
                  Left  errors -> Left [TypeError errors]
                  Right val    -> Right val
        return $ toByteString result

fileCheck :: (FilePath, Text) -> Text
fileCheck (filepath, source) = toText result
 where
  result = case runParse pProgram filepath source of
    Left  errors -> Left (map SyntacticError errors)
    Right ast    -> case runExcept (toAbstract ast) of
      Left  _    -> Left [Others "Should dig hole"]
      Right prog -> case runTM (checkProg prog) of
        Left  errors -> Left [TypeError errors]
        Right val    -> Right val

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

op :: Op -> Expr
op = Op

var :: Text -> Expr
var t = Var (Name t NoLoc) NoLoc

name' :: Text -> Name
name' t = Name t NoLoc

env :: Environment
env = Environment
  { envLocalDefns   = Map.fromList
    [ (name' "A"  , tint)
    , (name' "B"  , tint)
    , (name' "N"  , tint)
    , (name' "Arr", tarr (Including (litNum 0)) (Excluding (cons "N")) tint)
    , (name' "P"  , tfunc tint tbool)
    , (name' "Q"  , tfunc (tvar "?m") tbool)
    , (name' "F"  , tfunc tint tint)
    , (name' "G"  , tfunc tchar tbool)
    , (name' "Max", tfunc tint (tfunc tint tbool))
    , (name' "i"  , tint)
    , (name' "j"  , tint)
    , (name' "k"  , tint)
    , (name' "b"  , tbool)
    , (name' "p"  , tbool)
    , (name' "q"  , tbool)
    , (name' "r"  , tbool)
    ]
  , envTypeDefns    = mempty
  , envLocalContext = mempty
  }

runParser :: ToAbstract a b => Parser a -> Text -> Either [Error] b
runParser p t = case runExcept . toAbstract <$> parseTest p t of
  Left  errs         -> Left $ map SyntacticError errs
  Right (Left  loc ) -> Left [Others (show loc)]
  Right (Right expr) -> Right expr

check :: (Environment -> a -> TM b) -> Environment -> a -> Either [Error] b
check checker env' e = case runExcept (evalStateT (checker env' e) 0) of
  Left  err -> Left [TypeError err]
  Right x   -> Right x

exprCheck :: Text -> Text -> Assertion
exprCheck t1 t2 = toText (runParser pExpr t1 >>= check inferExpr env) @?= t2

typeCheck :: Text -> Text -> Assertion
typeCheck t1 t2 = toText (runParser pType t1 >>= check checkType env) @?= t2

typeCheck' :: Text -> Assertion
typeCheck' t = typeCheck t "()"

stmtCheck :: Text -> Text -> Assertion
stmtCheck t1 t2 = toText (runParser pStmt t1 >>= check checkStmt env) @?= t2

stmtCheck' :: Text -> Assertion
stmtCheck' t = stmtCheck t "()"


declarationCheck :: Text -> Text -> Assertion
declarationCheck t1 t2 = toText wrap @?= t2
 where
  wrap = do
    (typeDefns, ds) <- partitionEithers . (: []) <$> runParser pDeclaration t1
    let ds' = ds <> foldMap extractQDCons typeDefns
    return $ check
      (\_ decls -> do
        env' <- defnsAndDeclsToEnv (Defns typeDefns mempty) decls
        checkEnvironment env'
        return env'
      )
      mempty
      ds'

envCheck :: Text -> Assertion
envCheck t = toText env @?= t

blockDeclarationCheck :: Text -> Text -> Assertion
blockDeclarationCheck t1 t2 = toText wrap @?= t2
 where
  wrap = do
    (ds, defs) <- runParser pBlockDeclaration t1
    let defns = Defns [] (pickLetBindings defs)
    return $ check (\_ decls -> defnsAndDeclsToEnv defns decls) mempty ds

programCheck :: Text -> Assertion
programCheck t1 = toText wrap @?= "()"
 where
    -- wrap :: Either Error ()
  wrap = do
    prog <- runParser pProgram t1
    case runTM (checkProg prog) of
      Left  err -> Left [TypeError err]
      Right x   -> Right x

instance (Pretty a, Pretty b) => Pretty (Map a b) where
  pretty m = pretty $ Map.toList m

instance Pretty Environment where
  pretty Environment {..} =
    "Environment"
      <> pretty envLocalDefns
      <> pretty envTypeDefns
      <> pretty envLocalContext
