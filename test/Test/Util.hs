module Test.Util where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Data.ByteString.Lazy as BS
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import Data.Text.Prettyprint.Doc.Internal (layoutCompact, Pretty (pretty))
import Test.Tasty.Golden.Advanced (goldenTest)
import Test.Tasty.Golden (createDirectoriesAndWriteFile)
import Test.Tasty (TestTree)
import Prelude hiding (readFile)
import Error (Error)
import Pretty ()
import Syntax.Parser (Parser, runParse)
import Syntax.Parser.Util (SyntacticError)

goldenFileTest :: String -> 
  String -> 
  FilePath -> 
  FilePath -> 
  ((FilePath, Text) -> Text) -> TestTree
goldenFileTest suffix name filePath fileName run = 
  goldenTest
    name
    (readFile (filePath ++ "golden/") (fileName ++ suffix))
    (readFile filePath fileName)
    (compareAndReport run)
    (update suffix)

readFile :: FilePath -> FilePath -> IO (FilePath, FilePath, Text)
readFile filePath fileName = do
  raw <- Text.readFile (filePath ++ fileName)
  return (filePath, fileName, raw)

compareAndReport :: 
  ((FilePath, Text) -> Text) ->
  (FilePath, FilePath, Text) -> 
  (FilePath, FilePath, Text) -> 
  IO (Maybe String)
compareAndReport 
  run
  (expectedPath, expectedFileName, expectedRes) 
  (actualPath, actualFileName, actualRaw) = do
  let actualRes = run (actualPath ++ actualFileName, actualRaw)
  if removeTrailingWhitespace expectedRes == removeTrailingWhitespace actualRes
    then return Nothing
    else
      return . Just $
        "expected (" 
        ++ expectedPath ++ expectedFileName ++ ", " 
        ++ show (Text.length expectedRes) ++ " chars):\n" 
        ++ Text.unpack expectedRes 
        ++ "\n------------\n"
        ++ "actual ("
        ++ actualPath ++ actualFileName ++ ", "
        ++ show (Text.length actualRaw) ++ " chars): \n"
        ++ Text.unpack actualRaw

update :: String -> (FilePath, FilePath, Text) -> IO ()
update suffix (filePath, fileName, input) = createDirectoriesAndWriteFile (filePath ++ "golden/" ++ fileName ++ suffix) result
  where
    result = BS.fromStrict . Text.encodeUtf8 . renderStrict . layoutCompact . pretty $ input

removeTrailingWhitespace :: Text -> Text
removeTrailingWhitespace = Text.unlines . map Text.stripEnd . Text.lines

parseTest :: Parser a -> Text -> Either [SyntacticError] a
parseTest parser = runParse parser "<test>"