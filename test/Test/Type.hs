{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Test.Type where

import Control.Monad.State hiding (guard)
import Data.Loc ( Loc(NoLoc) )
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Encoding as Text
import Data.Text (Text)
import Data.Text.Prettyprint.Doc.Internal
import Data.Text.Prettyprint.Doc.Render.Text
import GCL.Type
import LSP
import Syntax.Abstract
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.Golden.Advanced
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Type" [unifyTests, typeCheckTests]


unifyTests :: TestTree 
unifyTests = 
  testGroup 
    "unify" 
    [ 
      testCase "TBase 1" $ 
        mapM_ (\t -> actual t t @?= Right t) [TBase TInt, TBase TBool, TBase TChar],
      testCase "TBase 2" $
        mapM_ (\(t1, t2) -> actual t1 t2 @?= Left (UnifyFailed t1 t2 NoLoc)) [(TBase TInt, TBase TBool), (TBase TInt, TBase TChar), (TBase TBool, TBase TChar)],
      testCase "TFunc 1" $
        actual' [("x", TBase TInt), ("y", TBase TBool)] (TFunc (TBase TInt) (TBase TBool)) (TFunc (TVar "x") (TVar "y")) @?= Right (TFunc (TBase TInt) (TBase TBool)),
      testCase "TVar 1" $
        actual (TVar "x") (TVar "y") @?= Right (TVar "y"),
      testCase "TVar 2" $
        actual' [("x", TBase TInt)] (TVar "x") (TBase TInt) @?= Right (TBase TInt)
    ]

actual :: Type -> Type -> Either TypeError Type
actual t1 t2 = runTM $ unify NoLoc t1 t2

actual' :: SubstT -> Type -> Type -> Either TypeError Type 
actual' sub t1 t2 = runTM $ do
  (theta, i) <- get
  put (sub ++ theta, i)
  unify NoLoc t1 t2

typeCheckTests :: TestTree
typeCheckTests = 
  testGroup "Type Check" 
    [
      typeCheckGolden "2" "./test/source/2.gcl"
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
    result = runTM $ do
      case runM (parseProgram filepath source) of
        Left err -> return Nothing
        Right prog -> Just <$> checkProg prog
          -- (theta, i) <- get
          -- return (Just (theta, i))

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