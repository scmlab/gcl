module Test.Util where

import           Data.ByteString.Lazy           ( ByteString )
import qualified Data.ByteString.Lazy          as BSL
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text
import           Prelude                 hiding ( readFile )
import           Pretty                         ( )
import           Test.Tasty                     ( TestTree )
import qualified Test.Tasty.Golden             as Golden

removeTrailingWhitespace :: Text -> Text
removeTrailingWhitespace = Text.unlines . map Text.stripEnd . Text.lines

runGoldenTest
  :: FilePath
  -> FilePath
  -> FilePath
  -> (FilePath -> Text -> IO ByteString)
  -> String
  -> FilePath
  -> TestTree
runGoldenTest sourceDir goldenDir ext test name path = do
  let goldenPath = goldenDir <> path <> ext <> ".golden"
  let sourcePath = sourceDir <> path
  Golden.goldenVsStringDiff name
                            (\ref new -> ["diff", "-u", ref, new])
                            goldenPath
    $ do
        source <- Text.decodeUtf8 . BSL.toStrict <$> BSL.readFile sourcePath
        test sourcePath source

