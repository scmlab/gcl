{-# LANGUAGE OverloadedStrings #-}

module Test.Server (tests) where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Loc
import Data.Loc.Range
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.Encoding as Text
import qualified Data.Text.Prettyprint.Doc as Doc
import qualified Data.Text.Prettyprint.Doc.Render.Text as Doc
import Pretty (pretty)
import Server.DSL
import Server.Interpreter.Test
import qualified Server.Interpreter.Test as Server
import Syntax.Predicate (PO, Spec (Specification))
import Test.Tasty
import Test.Tasty.Golden (createDirectoriesAndWriteFile)
import qualified Test.Tasty.Golden as Golden
import Test.Tasty.HUnit
import qualified Data.Text as StrictText

tests :: TestTree
tests = testGroup "Server" [instantiateSpec]

--------------------------------------------------------------------------------

instantiateSpec :: TestTree
instantiateSpec =
  testGroup
    "Instantiate Specs"
    [ run "top level" "spec-qm.gcl"
    ]
  where
    run :: String -> FilePath -> TestTree
    run = runGoldenTest "Server/assets/" $ \sourcePath -> do
      source <- Text.readFile sourcePath
      let makeRange (offsetA, lineA, colA) (offsetB, lineB, colB) =
            Range
              (Pos sourcePath lineA colA offsetA)
              (Pos sourcePath lineB colB offsetB)
      --
      let testResult = runTest sourcePath source $ do
            program <- parseProgram source
            _ <- sweep program
            return ()

      let makeRange (offsetA, lineA, colA) (offsetB, lineB, colB) =
            Range
              (Pos sourcePath lineA colA offsetA)
              (Pos sourcePath lineB colB offsetB)

      -- see if the traces are right
      let traces = StrictText.pack $ show 
            [ CmdGetFilePath,
              CmdEditText (makeRange (0, 1, 1) (1, 1, 2)) "[!\n\n!]",
              CmdGetFilePath
            ]

      return $ Text.encodeUtf8 $ Text.fromStrict $ StrictText.pack $ show testResult

runGoldenTest :: FilePath -> (FilePath -> IO ByteString) -> String -> FilePath -> TestTree
runGoldenTest dir test name path = do
  let goldenPath = "./test/Test/" <> dir <> path <> ".golden"
  let sourcePath = "./test/Test/" <> dir <> path
  Golden.goldenVsStringDiff name (\ref new -> ["diff", "-u", ref, new]) goldenPath (test sourcePath)
