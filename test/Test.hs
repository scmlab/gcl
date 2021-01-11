import qualified Test.Type as Type
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [Type.tests]
-- tests = testGroup "Tests" [Lexer.tests, Parser.tests, Pretty.tests, WP.tests]
