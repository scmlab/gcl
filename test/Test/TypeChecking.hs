{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Test.TypeChecking where

import           Control.Monad.Except           ( runExcept )
import           Control.Monad.State.Lazy
import           Data.Loc                       ( Loc(..) )
import qualified Data.Map                      as Map
import           Data.Map                       ( Map )
import           Data.Text                      ( Text )
import           Error                          ( Error(..) )
import           GCL.Type                       ( Index(..)
                                                , InferType(..)
                                                , ScopeTree(..)
                                                , ScopeTreeZipper(..)
                                                , TypeCheckable(..)
                                                , TypeDefnInfo(..)
                                                , TypeInfo(..)
                                                , runInferType
                                                , runTypeCheck
                                                )
import           Pretty                         ( Pretty(pretty)
                                                , hsep
                                                , punctuate
                                                , toByteString
                                                , toText
                                                , vsep
                                                )
import qualified Server.IntervalMap            as IntervalMap
import           Syntax.Abstract
import           Syntax.Common                  ( Name(Name)
                                                , Op
                                                )
import           Syntax.Concrete                ( ToAbstract(toAbstract) )
import           Syntax.Parser                 ( Parser )
import qualified Syntax.Parser                as Parser
import           Test.Tasty                     ( TestTree
                                                , testGroup
                                                )
import           Test.Tasty.HUnit               ( (@?=)
                                                , Assertion
                                                , testCase
                                                )
import           Test.Util                      ( runGoldenTest )

tests :: TestTree
tests = testGroup
  "Type"
  [ exprTests
  , typeTests
  , stmtTests
  , declarationTests
  , definitionTests
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
  -- , testCase "Arith 9" $ exprCheck "p ¬ q" "Bool"
  , testCase "Arith 10" $ exprCheck "i ↑ j" "Int"
  , testCase "Arith 11" $ exprCheck "i ↓ j" "Int"
  , testCase "Arith 12" $ exprCheck "-i" "Int"
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
  ,
      -- testCase "Hole" $
      --   exprCheck "_" "TVar",
    testCase "Quant 1" $ exprCheck "<| + i : 0 ≤ i < N : F i |>" "Int"
  , testCase "Quant 2" $ exprCheck "<| ∃ c : c = 'a' : G c |>" "Bool"
  , testCase "Case of 1"
    $ exprCheck "case x of\n\
    \  Just y -> y\n\
    \  Nothing -> 0" "Int"
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
  [ testCase "const declaration" $ declarationCheck "con C : Int" "()"
  , testCase "const declaration w/ prop"
    $ declarationCheck "con C : Int { C > 0 }" "()"
  , testCase "var declaration" $ declarationCheck "var x : Bool" "()"
  , testCase "var declaration w/ prop"
    $ declarationCheck "var y : Bool { y = True }" "()"
  ]

definitionTests :: TestTree
definitionTests = testGroup
  ""
  [ testCase "type definition"
    $ definitionCheck "{:\n data T a = Nil | Con a\n:}" "()"
  , testCase "definition 1"
    $ definitionCheck "{:\n\
         \  A, B : Int\
         \:}" "()"
  , testCase "block declaration 2" $ definitionCheck
    "{:\n\
         \  A, B : Int { A = 0 }\
         \:}"
    "()"
  , testCase "block declaration 3" $ definitionCheck
    "{:\n\
         \  A, B : Int\n\
         \    { A = 0 }\n\
         \:}"
    "()"
  , testCase "block declaration 4" $ definitionCheck
    "{:\n\
         \  A, B : Int\n\
         \    { A = 0 }\n\
         \  F : Int -> Int -> Int\n\
         \  P : Char -> Bool\n\
         \:}"
    "()"
  , testCase "block declaration 5" $ definitionCheck
    "{:\n\
         \   N = 5\n\
         \   N = 6\n\
         \:}"
    "()"
  , testCase "definition 6"
    $ definitionCheck "{:\n\
         \    G i j = i + j\n\
         \:}" "()"
  , testCase "definition 7" $ definitionCheck
    "{:\n\
         \   data Maybe a = Just a | Nothing\n\
         \   G : Maybe a -> Int\n\
         \   G x = case x of\n\
         \             Just y -> 1\n\
         \             Nothing -> 0\n\
         \   A = 5\n\
         \   F a b = a + b\n\
         \:}"
    "()"
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

        let result = case Parser.scanAndParse Parser.program filepath source of
              Left  err -> Left [ParseError err]
              Right ast -> case runExcept (toAbstract ast) of
                Left  _    -> Left [Others "Should dig hole"]
                Right prog -> case runTypeCheck prog of
                  Left  errors -> Left [TypeError errors]
                  Right _      -> Right ()
        return $ toByteString result

fileCheck :: (FilePath, Text) -> Text
fileCheck (filepath, source) = toText result
 where
  result = case Parser.scanAndParse Parser.program filepath source of
    Left  err -> Left [ParseError err]
    Right ast -> case runExcept (toAbstract ast) of
      Left  _    -> Left [Others "Should dig hole"]
      Right prog -> case runTypeCheck prog of
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

tmetavar :: Text -> Type
tmetavar x = TMetaVar (name' x)

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

index :: Text -> Index
index = Index . name'

env :: Map Index TypeInfo
env = Map.fromList
  [ (index "A", ConstTypeInfo tint)
  , (index "B", ConstTypeInfo tint)
  , (index "N", ConstTypeInfo tint)
  , ( index "Arr"
    , ConstTypeInfo (tarr (Including (litNum 0)) (Excluding (cons "N")) tint)
    )
  , (index "P"  , ConstTypeInfo (tfunc tint tbool))
  , (index "F"  , ConstTypeInfo (tfunc tint tint))
  , (index "G"  , ConstTypeInfo (tfunc tchar tbool))
  , (index "Max", ConstTypeInfo (tfunc tint (tfunc tint tbool)))
  , (index "i"  , VarTypeInfo tint)
  , (index "j"  , VarTypeInfo tint)
  , (index "k"  , VarTypeInfo tint)
  , (index "b"  , VarTypeInfo tbool)
  , (index "p"  , VarTypeInfo tbool)
  , (index "q"  , VarTypeInfo tbool)
  , (index "r"  , VarTypeInfo tbool)
  , (index "x", VarTypeInfo (TCon (name' "Maybe") [name' "a"] NoLoc))
  , ( index "Just"
    , TypeDefnCtorInfo (tfunc tint (TCon (name' "Maybe") [name' "a"] NoLoc))
    )
  , (index "Nothing", TypeDefnCtorInfo (TCon (name' "Maybe") [name' "a"] NoLoc))
  ]

runParser :: ToAbstract a b => Parser a -> Text -> Either [Error] b
runParser p t =
  case runExcept . toAbstract <$> Parser.scanAndParse p "<test>" t of
    Left  err          -> Left [ParseError err]
    Right (Left  loc ) -> Left [Others (show loc)]
    Right (Right expr) -> Right expr

check :: TypeCheckable a => Map Index TypeInfo -> a -> Either [Error] ()
check env' e =
  case
      runExcept
        (execStateT
          (typeCheck e)
          ( 0
          , ScopeTreeZipper (ScopeTree mempty mempty) []
          , ScopeTreeZipper (ScopeTree env' mempty)   []
          )
        )
    of
      Left  err -> Left [TypeError err]
      Right _   -> Right ()

inferCheck :: InferType a => Map Index TypeInfo -> a -> Either [Error] Type
inferCheck env' e =
  case
      runExcept
        (evalStateT
          (runInferType e)
          ( 0
          , ScopeTreeZipper (ScopeTree mempty mempty) []
          , ScopeTreeZipper (ScopeTree env' mempty)   []
          )
        )
    of
      Left  err -> Left [TypeError err]
      Right x   -> Right (snd x)

exprCheck :: Text -> Text -> Assertion
exprCheck t1 t2 =
  toText (runParser Parser.expression t1 >>= inferCheck env) @?= t2

typeCheckAssert :: Text -> Text -> Assertion
typeCheckAssert t1 t2 = toText (runParser Parser.type' t1 >>= check env) @?= t2

typeCheck' :: Text -> Assertion
typeCheck' t = typeCheckAssert t "()"

stmtCheck :: Text -> Text -> Assertion
stmtCheck t1 t2 = toText (runParser Parser.statements t1 >>= check env) @?= t2

stmtCheck' :: Text -> Assertion
stmtCheck' t = stmtCheck t "()"

declarationCheck :: Text -> Text -> Assertion
declarationCheck t1 t2 =
  toText (runParser Parser.program t1 >>= check mempty) @?= t2

envCheck :: Text -> Assertion
envCheck t = toText env @?= t

definitionCheck :: Text -> Text -> Assertion
definitionCheck t1 t2 =
  toText (runParser Parser.program t1 >>= check mempty) @?= t2

programCheck :: Text -> Assertion
programCheck t1 =
  toText (runParser Parser.program t1 >>= check mempty) @?= "()"

instance (Pretty a, Pretty b) => Pretty (Map a b) where
  pretty m = "[" <> hsep (punctuate "," (map pretty (Map.toList m))) <> "]"

instance Pretty TypeDefnInfo where
  pretty (TypeDefnInfo ns) =
    "TypeDefnInfo " <> hsep (punctuate "," (map pretty ns))

instance Pretty TypeInfo where
  pretty (TypeDefnCtorInfo t) = "TypeDefnCtorInfo " <> pretty t
  pretty (ConstTypeInfo    t) = "ConstTypeInfo " <> pretty t
  pretty (VarTypeInfo      t) = "VarTypeInfo " <> pretty t

instance Pretty a => Pretty (ScopeTree a) where
  pretty ScopeTree {..} =
    "GlobalScope " <> pretty globalScope <> "\nLocalScopes " <> vsep
      (punctuate "," (map pretty (IntervalMap.toList localScopes)))

instance Pretty a => Pretty (ScopeTreeZipper a) where
  pretty ScopeTreeZipper {..} = "Cursor " <> pretty cursor

instance Pretty Index where
  pretty (Index n  ) = pretty n
  pretty (Hole  rng) = pretty rng
