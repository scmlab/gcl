module Test.Util where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Test.Tasty (TestTree)
import Prelude hiding (readFile)
import Pretty (Pretty, toString)
import Syntax.Parser (Parser, runParse)
import Syntax.Parser.Util (SyntacticError)
import Data.ByteString.Lazy (ByteString)
import qualified Test.Tasty.Golden as Golden
import qualified Data.ByteString.Lazy as BSL

import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

-- goldenFileTest :: String -> 
--   String -> 
--   FilePath -> 
--   FilePath -> 
--   ((FilePath, Text) -> Text) -> TestTree
-- goldenFileTest suffix name filePath fileName run = 
--   goldenTest
--     name
--     (readFile (filePath ++ "golden/") (fileName ++ suffix))
--     (readFile filePath fileName)
--     (compareAndReport run)
--     (update suffix)

-- readFile :: FilePath -> FilePath -> IO (FilePath, FilePath, Text)
-- readFile filePath fileName = do
--   raw <- Text.readFile (filePath ++ fileName)
--   return (filePath, fileName, raw)

-- compareAndReport :: 
--   ((FilePath, Text) -> Text) ->
--   (FilePath, FilePath, Text) -> 
--   (FilePath, FilePath, Text) -> 
--   IO (Maybe String)
-- compareAndReport 
--   run
--   (expectedPath, expectedFileName, expectedRes) 
--   (actualPath, actualFileName, actualRaw) = do
--   let actualRes = run (actualPath ++ actualFileName, actualRaw)
--   if removeTrailingWhitespace expectedRes == removeTrailingWhitespace actualRes
--     then return Nothing
--     else
--       return . Just $
--         "expected (" 
--         ++ expectedPath ++ expectedFileName ++ ", " 
--         ++ show (Text.length expectedRes) ++ " chars):\n" 
--         ++ Text.unpack expectedRes 
--         ++ "\n------------\n"
--         ++ "actual ("
--         ++ actualPath ++ actualFileName ++ ", "
--         ++ show (Text.length actualRes) ++ " chars): \n"
--         ++ Text.unpack actualRes

-- update :: String -> (FilePath, FilePath, Text) -> IO ()
-- update suffix (filePath, fileName, input) = createDirectoriesAndWriteFile (filePath ++ "golden/" ++ fileName ++ suffix) result
--   where
--     result = BS.fromStrict . Text.encodeUtf8 . renderStrict . layoutCompact . pretty $ input

removeTrailingWhitespace :: Text -> Text
removeTrailingWhitespace = Text.unlines . map Text.stripEnd . Text.lines

parseTest :: Parser a -> Text -> Either [SyntacticError] a
parseTest parser = runParse parser "<test>"


runGoldenTest :: FilePath -> FilePath -> FilePath -> (FilePath -> Text -> IO ByteString) -> String -> FilePath -> TestTree
runGoldenTest sourceDir goldenDir ext test name fileName = do
  let goldenPath = goldenDir <> fileName <> ext <> ".golden"
  let sourcePath = sourceDir <> fileName
  Golden.goldenVsStringDiff name (\ref new -> ["diff", "-u", ref, new]) goldenPath $ do
    sourceText <- Text.decodeUtf8 . BSL.toStrict <$> BSL.readFile sourcePath
    test sourcePath sourceText

{- |
  Pretty a
  => FilePath -- source directory
  -> FilePath -- golden file directory
  -> FilePath -- generated log directory
  -> FilePath -- ext
  -> (FilePath -> Text -> a) -- test 
  -> String   -- test name
  -> FilePath -- the specific source file
  -> TestTree
-}
runGoldenTestWithGeneratedLog 
  :: Pretty a  => FilePath -> FilePath -> FilePath -> FilePath -> (FilePath -> Text -> a) -> String -> FilePath -> TestTree
runGoldenTestWithGeneratedLog sourceDir goldenDir genDir ext test name fileName =
  let goldenPath = goldenDir <> fileName <> ext <> ".golden"
      sourcePath = sourceDir <> fileName
      genPath = genDir <> fileName  <> ".txt"
  in
    Golden.goldenVsFile name goldenPath genPath $ do
      sourceText <- Text.decodeUtf8 . BSL.toStrict <$> BSL.readFile sourcePath
      createAndWriteFile genPath $ toString $ test sourcePath sourceText


-- taken from https://stackoverflow.com/questions/58682357/how-to-create-a-file-and-its-parent-directories-in-haskellv
createAndWriteFile :: FilePath -> String -> IO ()
createAndWriteFile path content = do
  createDirectoryIfMissing True $ takeDirectory path
  writeFile path content