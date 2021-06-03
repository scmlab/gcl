{-# LANGUAGE OverloadedStrings #-}

module Test.Server (tests, runGoldenTest) where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding as Text
import Server.DSL
import Server.Interpreter.Test
import Test.Tasty
import qualified Test.Tasty.Golden as Golden
import Data.Text (Text)
import GCL.Predicate.Util (specPayloadWithoutIndentation)

tests :: TestTree
tests = testGroup "Server" [instantiateSpec, specPayloadWithoutIndentationTests]

--------------------------------------------------------------------------------

instantiateSpec :: TestTree
instantiateSpec =
  testGroup
    "Instantiate Specs"
    [ run "top level 1" "spec-qm-1.gcl"
    , run "top level 2" "spec-qm-2.gcl"
    , run "indented 1" "spec-qm-3.gcl"
    , run "indented 2" "spec-qm-4.gcl"
    , run "complex 1" "spec-qm-5.gcl"
    ]
  where
    run :: String -> FilePath -> TestTree
    run = runGoldenTest "Server/assets/" $ \sourcePath source -> do
      return $ serializeTestResult $ runTest sourcePath source $ do
            program <- parseProgram source
            Right <$> sweep program


specPayloadWithoutIndentationTests :: TestTree
specPayloadWithoutIndentationTests =
  testGroup
    "Testing specPayloadWithoutIndentation"
    [ run "mulitline 1" "spec-payload-1.gcl"
    ]
  where
    run :: String -> FilePath -> TestTree
    run = runGoldenTest "Server/assets/" $ \sourcePath source -> do
      return $ serializeTestResult $ runTest sourcePath source $ do
            program <- parseProgram source
            (_, specs, _, _) <- sweep program
            return $ Right $ map (map (\x -> "\"" <> x <> "\"") . specPayloadWithoutIndentation source) specs


runGoldenTest :: FilePath -> (FilePath -> Text -> IO ByteString) -> String -> FilePath -> TestTree
runGoldenTest dir test name path = do
  let goldenPath = "./test/Test/" <> dir <> path <> ".golden"
  let sourcePath = "./test/Test/" <> dir <> path
  Golden.goldenVsStringDiff name (\ref new -> ["diff", "-u", ref, new]) goldenPath $ do
    source <- Text.decodeUtf8 . BSL.toStrict <$> BSL.readFile sourcePath
    test sourcePath source 

