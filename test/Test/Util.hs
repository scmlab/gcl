module Test.Util where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Test.Tasty (TestTree)
import Prelude hiding (readFile)
-- Commented for developing Parser2
-- import Syntax.Parser (Parser, runParse)
-- import Syntax.Parser.Util (SyntacticError)
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

-- Commented for developing Parser2
-- parseTest :: Parser a -> Text -> Either [SyntacticError] a
-- parseTest parser = runParse parser "<test>"


runGoldenTest :: FilePath -> FilePath -> FilePath -> (FilePath -> Text -> IO ByteString) -> String -> FilePath -> TestTree
runGoldenTest sourceDir goldenDir ext test name fileName = do
  let goldenPath = goldenDir <> fileName <> ext <> ".golden"
  let sourcePath = sourceDir <> fileName
  Golden.goldenVsStringDiff name (\ref new -> ["diff", "-u", ref, new]) goldenPath $ do
    sourceText <- Text.decodeUtf8 . BSL.toStrict <$> BSL.readFile sourcePath
    test sourcePath sourceText

{- | added one more argument to 'runGoldenTest': the first argument, receiving the test module name.
  Example usage: 
    @runGoldenTest ".\/test\/source\/TheTestModule/" ".\/test\/golden\/TheTestModule\/" "" ...@
    becomes:
    @runGoldenTestWithLog \"TheTestModule\" ".\/test\/source\/TheTestModule\/" ".\/test\/golden\/TheTestModule/" "" ...@
-}
runGoldenTestWithLog :: FilePath -> FilePath -> FilePath -> FilePath -> (FilePath -> Text -> IO ByteString) -> String -> FilePath -> TestTree
runGoldenTestWithLog modName sourceDir goldenDir ext test name fileName =
  let goldenPath = goldenDir <> fileName <> ext <> ".golden"
      sourcePath = sourceDir <> fileName
      genPath = logPath modName fileName
  in
    Golden.goldenVsFile name goldenPath genPath $ do
      sourceText <- Text.decodeUtf8 . BSL.toStrict <$> BSL.readFile sourcePath
      str <- test sourcePath sourceText
      loggingBS modName fileName str

-- | Receiving a module name, a file name and the content to log. The exact logging directoy is defined by 'logPath'.
logging :: Show a => FilePath -> FilePath -> a -> IO ()
logging modName fileName content = do
  let path = logPath modName fileName
  createDirectoryIfMissing True $ takeDirectory path
  writeFile path (show content)


-- | The ByteString version of logging. Adapting with previous uses of ByteStings (such as 'runGoldenTest').
loggingBS :: FilePath -> FilePath -> ByteString -> IO ()
loggingBS modName fileName content = do
  let path = logPath modName fileName
  createDirectoryIfMissing True $ takeDirectory path
  BSL.writeFile path content

-- | Defines the exact log directory.
logPath :: FilePath -> FilePath -> FilePath
logPath modName fileName = "./temp-test-generation/" <> modName <> "/" <> fileName  <> ".log"