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
import Syntax.Concrete
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.Golden.Advanced
import Test.Tasty.HUnit

tests :: TestTree
-- tests = testGroup "Type" [unifyTests]
tests = testGroup "Type" [unifyTests, typeCheckTests]


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

actual :: Type -> Type -> Either TypeError SubstT
actual t1 t2 = runSolver [(t1, t2)]

typeCheckTests :: TestTree
typeCheckTests = 
  testGroup "Type Check" 
    [
      typeCheckGolden "2" "./test/source/2.gcl"
--       typeCheckGolden "let binding" "./test/source/let.gcl",
--       typeCheckGolden "factor" "./test/source/examples/factor.gcl"
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
        Left err -> return ()
        Right prog -> runM $ checkProg prog

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