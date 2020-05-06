import           Test.Tasty

import qualified Test.Parser                   as Parser
import qualified Test.Pretty                   as Pretty
import qualified Test.WP                       as WP
import qualified Test.WP2                      as WP2

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [Parser.tests, Pretty.tests, WP2.tests, WP.tests]
