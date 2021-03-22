-- import qualified Test.Lexer as Lexer
import qualified Test.WP as WP
import qualified Test.Type as Type
import qualified Test.Parser as Parser
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [Parser.tests, WP.tests, Type.tests]
