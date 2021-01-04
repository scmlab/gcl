{-# LANGUAGE OverloadedStrings #-}

module Test.ProofObligation where

import qualified Data.ByteString as Strict
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy.Char8
  ( unpack,
  )
import Data.Loc
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Text (Text)
import Data.Text.Lazy (toStrict, fromStrict)
import qualified Data.Text.Lazy.Encoding as LazyText
import Data.Text.Prettyprint.Doc.Render.Text
  ( renderLazy,
  )
import Error
import qualified LSP
import Pretty
import Syntax.Concrete
import Syntax.Parser (Parser)
import qualified Syntax.Parser as Parser
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.Golden.Advanced
import Test.Tasty.HUnit
import Text.Megaparsec (eof)
import Prelude hiding (Ordering (..))

-- | Golden tests for programs 
tests :: TestTree
tests =
  testGroup
    "Proof Obligations"
    [ ast "empty" "./test/source/empty.gcl"
    , ast "examples/mss" "./test/source/examples/mss.gcl"
      -- ast "quant 1" "./test/source/quant1.gcl",
      -- ast "no-decl" "./test/source/no-decl.gcl",
      -- ast "no-stmt" "./test/source/no-stmt.gcl",
      -- ast "2" "./test/source/2.gcl",
      -- ast "issue 1" "./test/source/issue1.gcl",
      -- ast "issue 14" "./test/source/issue14.gcl",
      -- ast "comment" "./test/source/comment.gcl"
    , ast "spec" "./test/source/spec.gcl"
    ]
  where
    sufffixGolden :: FilePath -> FilePath
    sufffixGolden filePath = filePath ++ ".po.golden"

    ast :: String -> FilePath -> TestTree
    ast name filePath =
      goldenTest
        name
        (readFile (sufffixGolden filePath))
        (readFile filePath)
        compare
        update

    readFile :: FilePath -> IO (FilePath, ByteString)
    readFile filePath = do
      raw <- BS.readFile filePath
      return (filePath, raw)

    compare ::
      (FilePath, ByteString) -> (FilePath, ByteString) -> IO (Maybe String)
    compare (_, expected) (filePath, actual) = do
      let result = run (filePath, actual)
      if expected == actual
        then return Nothing
        else return (Just $ "expected:\n" ++ unpack expected ++ "\n------------\nactual: \n" ++ unpack actual)

    update :: (FilePath, ByteString) -> IO ()
    update (filePath, input) = do
      let result = run (filePath, input)
      createDirectoriesAndWriteFile (sufffixGolden filePath) result

    run :: (FilePath, ByteString) -> ByteString
    run (filePath, raw) = 
      let result = LSP.runM $ do 
            tokens <- LSP.scan filePath (toStrict $ LazyText.decodeUtf8 raw)
            program <- LSP.parse Parser.program filePath tokens
            (pos, _) <- LSP.sweep program
            return pos
          render = LazyText.encodeUtf8
                    . renderLazy
                    . layoutCompact
                    . pretty
      in render result

    parseProgram :: (FilePath, ByteString) -> Either Error Program
    parseProgram (filePath, raw) = LSP.runM $ LSP.scan filePath (toStrict $ LazyText.decodeUtf8 raw) >>= LSP.parse Parser.program filePath
