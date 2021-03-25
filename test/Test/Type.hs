{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Test.Type where

import Control.Monad.State hiding (guard)
import Control.Monad.Except
import Data.Loc ( Loc(Loc, NoLoc), Pos(Pos) )
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Encoding as Text
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import Data.Text.Prettyprint.Doc.Internal
import Data.Text.Prettyprint.Doc.Render.Text
import qualified Data.Map as Map
import GCL.Type
import Error
import LSP
import Syntax.Abstract
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.Golden.Advanced
import Test.Tasty.HUnit
import qualified Data.Bifunctor
import Syntax.Parser (Parser, runParse, pExpr)
import Syntax.Concrete (ToAbstract(toAbstract))

tests :: TestTree
tests = testGroup "Type" [inferTests]
-- tests = testGroup "Type" [unifyTests, typeCheckTests]


unifyTests :: TestTree 
unifyTests = 
  testGroup 
    "unify" 
    [ 
      testCase "TBase 1" $ 
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

cons :: LT.Text -> Expr
cons name = Const (Name name NoLoc) NoLoc

app :: Expr -> Expr -> Expr
app e1 e2 = App e1 e2 NoLoc

op :: Op -> Expr
op o = Op o NoLoc

var :: LT.Text -> Expr
var t = Var (Name t NoLoc) NoLoc

env :: TypeEnv
env = TypeEnv $ Map.fromList [
   ("N", ForallV [] tint), 
   ("F", ForallV [] (tarr (Including (litNum 0)) (Excluding (cons "N")) tint)),
   ("i", ForallV [] tint),
   ("j", ForallV [] tint)
  ]

inferTests :: TestTree
inferTests = 
  testGroup
    "infer"
    [
      testCase "F" $
        (runExcept . withExcept TypeError . runInfer env)
         (lookupEnv (Name "F" (Loc (Pos "<test>" 1 1 0) (Pos "<test>" 1 1 0))))
         @?= Left NotLoaded ,
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
          let res = case parse (toAbstract <$> pExpr) t of
                Right expr -> do
                    (t, cs) <- runInfer env (infer expr)
                    runSolver cs
                Left err ->  liftEither $ Left (NotInScope "" NoLoc)
            in res @?= liftEither (Left (NotInScope " test " NoLoc))
      run t = 
        let res = 
              case parse (toAbstract <$> pExpr) t of
                Right expr -> runExcept . withExcept TypeError . inferExpr env $ expr
                Left err -> Left err
        in res @?= Left NotLoaded 
      

actual :: Type -> Type -> Either TypeError SubstT
actual t1 t2 = runSolver' [(t1, t2)]

typeCheckTests :: TestTree
typeCheckTests = 
  testGroup "Type Check" 
    [
      typeCheckGolden "2" "./test/source/2.gcl",
      typeCheckGolden "quant1" "./test/source/quant1.gcl",
      typeCheckGolden "mss" "./test/source/mss.gcl",
      typeCheckGolden "posnegpairs" "./test/source/examples/posnegpairs.gcl"
    ]

typeCheckGolden :: String -> FilePath -> TestTree 
typeCheckGolden name filePath = 
  goldenTest
    name
    ((expectedPath,) <$> Text.readFile expectedPath)
    ((filePath,) <$> Text.readFile filePath)
    compareAndReport
    update
    where
      expectedPath = filePath ++ ".tc.golden"

typeCheck :: (FilePath, Text) -> Text
typeCheck (filepath, source) = renderStrict . layoutCompact . pretty $ result
  where 
    result = 
      case runM (parseProgram filepath source) of
        Left err -> Left err
        Right prog -> runM . withExcept TypeError $ checkProg prog

compareAndReport :: (FilePath, Text) -> (FilePath, Text) -> IO (Maybe String)
compareAndReport (expectedPath, expectedRes) (actualPath, actualRaw) = do
  let actualRes = typeCheck (actualPath, actualRaw)
  if expectedRes == actualRes
    then 
      return Nothing
    else 
      return . Just $
        "expected: \n\t" ++ Text.unpack expectedRes ++ "\n------------\n" 
        ++ "actual: \n\t" ++ Text.unpack actualRes

update :: (FilePath, Text) -> IO ()
update (filePath, input) = createDirectoriesAndWriteFile (filePath ++ ".tc.golden") result
  where 
    result = BS.fromStrict . Text.encodeUtf8 . renderStrict . layoutCompact . pretty $ input