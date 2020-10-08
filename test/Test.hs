import qualified Test.Lexer as Lexer
import qualified Test.Parser as Parser
import qualified Test.Pretty as Pretty
import Test.Tasty (TestTree, defaultMain, testGroup)
import qualified Test.WP as WP
import qualified Test.WP2 as WP2

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [Lexer.tests, Parser.tests, Pretty.tests, WP2.tests, WP.tests]
