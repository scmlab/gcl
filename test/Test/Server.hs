{-# LANGUAGE OverloadedStrings #-}

module Test.Server
  ( tests
  ) where

import           Data.Loc.Range                 ( rangeOf )
import           Data.Maybe                     ( listToMaybe )
import           Error                          ( Error(Others) )
import           GCL.Predicate.Util             ( specPayloadWithoutIndentation
                                                )
import           Server.Pipeline
import           Server.Handler.CustomMethod    ( handleRefine )
import           Test.Server.Interpreter
import           Test.Tasty
import           Test.Util                      ( runGoldenTest )

tests :: TestTree
tests = testGroup
  "Server"
  [instantiateSpec, specPayloadWithoutIndentationTests, refineSpecsTest]

--------------------------------------------------------------------------------

instantiateSpec :: TestTree
instantiateSpec = testGroup
  "Instantiate Specs"
  [ run "top level 1" "spec-qm-1.gcl"
  , run "top level 2" "spec-qm-2.gcl"
  , run "indented 1"  "spec-qm-3.gcl"
  , run "indented 2"  "spec-qm-4.gcl"
  --, run "complex 1"   "spec-qm-5.gcl"
  ]
 where
  run :: String -> FilePath -> TestTree
  run =
    runGoldenTest "./test/source/Server/" "./test/golden/Server/" ""
      $ \sourcePath source -> do
          return $ serializeTestResult $ runTest sourcePath source $ do
            parsed      <- parse source
            converted   <- convert parsed
            typeChecked <- typeCheck converted
            Right <$> sweep typeChecked


specPayloadWithoutIndentationTests :: TestTree
specPayloadWithoutIndentationTests = testGroup
  "Testing specPayloadWithoutIndentation"
  [run "mulitline 1" "spec-payload-1.gcl"]
 where
  run :: String -> FilePath -> TestTree
  run =
    runGoldenTest "./test/source/Server/" "./test/golden/Server/" ""
      $ \sourcePath source -> do
          return $ serializeTestResult $ runTest sourcePath source $ do
            parsed      <- parse source
            converted   <- convert parsed
            typeChecked <- typeCheck converted
            result      <- sweep typeChecked
            let specs = sweptSpecs result
            return $ Right $ map
              ( map (\x -> "\"" <> x <> "\"")
              . specPayloadWithoutIndentation source
              )
              specs


refineSpecsTest :: TestTree
refineSpecsTest = testGroup
  "Refine Specs"
  [ run "multiline, top-level"                  "spec-refine-1.gcl"
  , run "multiline, top-level, indented"        "spec-refine-2.gcl"
  , run "multiline, top-level, poorly indented" "spec-refine-3.gcl"
  , run "multiline, 1 ? in IF, 1"               "spec-refine-4.gcl"
  , run "multiline, 2 ? in IF, 2"               "spec-refine-5.gcl"
  , run "multiline, 2 ? in IF, 3"               "spec-refine-6.gcl"
  ]
 where
  run :: String -> FilePath -> TestTree
  run =
    runGoldenTest "./test/source/Server/" "./test/golden/Server/" ""
      $ \sourcePath source -> do
          return $ serializeTestResult $ runTest sourcePath source $ do
            parsed      <- parse source
            converted   <- convert parsed
            typeChecked <- typeCheck converted
            result      <- sweep typeChecked
            let specs = sweptSpecs result
            case listToMaybe specs of
              Just spec -> do
                let range = rangeOf spec
                resKind <- handleRefine range
                return $ Right resKind
              Nothing -> return $ Left [Others "cannot find any specs"]
