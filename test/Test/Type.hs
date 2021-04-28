{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Test.Type where

import Control.Monad.Except
import Control.Monad.State hiding (guard)
import qualified Data.Bifunctor
import qualified Data.ByteString.Lazy as BS
import Data.Loc (Loc (Loc, NoLoc), Pos (Pos))
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import Data.Text.Prettyprint.Doc.Internal
import Data.Text.Prettyprint.Doc.Render.Text
import Error
import GCL.Type
import qualified LSP
import qualified LSP.CustomMethod as LSP
import qualified LSP.Monad as LSP
import Syntax.Abstract
import Syntax.Common
import Syntax.Concrete (ToAbstract (toAbstract))
import Syntax.Parser (Parser, pExpr, runParse)
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.Golden.Advanced
import Test.Tasty.HUnit

tests :: TestTree
-- tests = testGroup "Type" [inferTests]
tests = testGroup "Type" [unifyTests, typeCheckTests]

unifyTests :: TestTree
unifyTests =
  testGroup
    "unify"
    [ testCase "TBase 1" $
        mapM_ (\t -> actual t t @?= Right emptySubstT) [TBase TInt NoLoc, TBase TBool NoLoc, TBase TChar NoLoc],
      testCase "TBase 2" $
        mapM_ (\(t1, t2) -> actual t1 t2 @?= Left (UnifyFailed t1 t2 NoLoc)) [(TBase TInt NoLoc, TBase TBool NoLoc), (TBase TInt NoLoc, TBase TChar NoLoc), (TBase TBool NoLoc, TBase TChar NoLoc)]
    ]

tint :: Type
tint = TBase TInt NoLoc

tbool :: Type
tbool = TBase TBool NoLoc

tarr :: Endpoint -> Endpoint -> Type -> Type
tarr e1 e2 t = TArray (interval e1 e2) t NoLoc

interval :: Endpoint -> Endpoint -> Interval
interval e1 e2 = Interval e1 e2 NoLoc

litNum :: Int -> Expr
litNum i = Lit (Num i) NoLoc

cons :: Text -> Expr
cons name = Const (Name name NoLoc) NoLoc

app :: Expr -> Expr -> Expr
app e1 e2 = App e1 e2 NoLoc

op :: Op -> Expr
op = Op

var :: Text -> Expr
var t = Var (Name t NoLoc) NoLoc

env :: TypeEnv
env =
  TypeEnv $
    Map.fromList
      [ ("N", ForallV [] tint),
        ("F", ForallV [] (tarr (Including (litNum 0)) (Excluding (cons "N")) tint)),
        ("i", ForallV [] tint),
        ("j", ForallV [] tint)
      ]

inferTests :: TestTree
inferTests =
  testGroup
    "infer"
    [ testCase "F" $
        (runExcept . withExcept TypeError . runInfer env)
          (lookupEnv (Name "F" (Loc (Pos "<test>" 1 1 0) (Pos "<test>" 1 1 0))))
          @?= Left (TypeError (NotInScope "F" NoLoc)),
      testCase "F" $
        run' "F",
      testCase "F i" $
        run' "F i",
      testCase "F i < 0" $
        run "F i < 0 ",
      testCase "F i ≤ 0 ∧ F j ≥ 0" $
        run "F i ≤ 0 ∧ F j ≥ 0"
    ]
  where
    parse parser =
      runExcept . withExcept SyntacticError . liftEither . runParse parser "<test>"
    run' t = do
      let res = case parse (runExcept . toAbstract <$> pExpr) t of
            Right (Right expr) -> do
              (t, cs) <- runInfer env (infer expr)
              runSolver cs
            Right (Left loc) -> error "Unexpanded hole \"?\" in the program"
            Left err -> liftEither $ Left (NotInScope "" NoLoc)
       in res @?= liftEither (Left (NotInScope " test " NoLoc))
    run t =
      let res =
            case parse (runExcept . toAbstract <$> pExpr) t of
              Right (Right expr) -> runExcept . withExcept TypeError . inferExpr env $ expr
              Right (Left loc) -> error "Unexpanded hole \"?\" in the program"
              Left err -> Left err
       in res @?= Left (TypeError (NotInScope "" NoLoc))

actual :: Type -> Type -> Either TypeError SubstT
actual t1 t2 = runSolver' [(t1, t2)]

typeCheckTests :: TestTree
typeCheckTests =
  testGroup
    "Type Check"
    [ typeCheckGolden "2" "./test/source/" "2.gcl",
      typeCheckGolden "quant1" "./test/source/" "quant1.gcl",
      typeCheckGolden "mss" "./test/source/" "mss.gcl",
      typeCheckGolden "posnegpairs" "./test/source/examples/" "posnegpairs.gcl"
    ]

typeCheckGolden :: String -> FilePath -> FilePath -> TestTree
typeCheckGolden name filePath fileName =
  goldenTest
    name
    ((expectedPath,expectedFileName,) <$> Text.readFile (expectedPath ++ expectedFileName))
    ((filePath,fileName,) <$> Text.readFile (filePath ++ fileName))
    compareAndReport
    update
  where
    expectedPath = filePath ++ "golden/"
    expectedFileName = fileName ++ ".tc.golden"

typeCheck :: (FilePath, Text) -> Text
typeCheck (filepath, source) = renderStrict . layoutCompact . pretty $ result
  where
    result =
      case LSP.runM (LSP.parseProgram filepath source) of
        Left err -> Left err
        Right prog -> LSP.runM . withExcept TypeError $ checkProg prog

compareAndReport :: (FilePath, FilePath, Text) -> (FilePath, FilePath, Text) -> IO (Maybe String)
compareAndReport (expectedPath, _, expectedRes) (actualPath, fileName, actualRaw) = do
  let actualRes = typeCheck (actualPath ++ fileName, actualRaw)
  if expectedRes == actualRes
    then return Nothing
    else
      return . Just $
        "expected: \n\t" ++ Text.unpack expectedRes ++ "\n------------\n"
          ++ "actual: \n\t"
          ++ Text.unpack actualRes

update :: (FilePath, FilePath, Text) -> IO ()
update (filePath, fileName, input) = createDirectoriesAndWriteFile (filePath ++ "golden/" ++ fileName ++ ".tc.golden") result
  where
    result = BS.fromStrict . Text.encodeUtf8 . renderStrict . layoutCompact . pretty $ input