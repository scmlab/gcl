import qualified Test.Lexer as Lexer
import qualified Test.Parser as Parser
import qualified Test.Concrete2 as Concrete2
import qualified Test.ProofObligation as PO
import Test.Tasty (TestTree, defaultMain, testGroup)
import qualified Test.WP as WP

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [Lexer.tests, Parser.tests, Concrete2.tests, WP.tests]
