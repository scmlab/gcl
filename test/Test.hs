import qualified Test.Lexer as Lexer
import qualified Test.Parser as Parser
import qualified Test.Parser2 as Parser2
import qualified Test.Pretty as Pretty
import qualified Test.ProofObligation as PO
import Test.Tasty (TestTree, defaultMain, testGroup)
import qualified Test.WP as WP

main :: IO ()
main = defaultMain tests

tests :: TestTree
-- tests = testGroup "Tests" [Parser.tests, Parser2.tests]
-- tests = testGroup "Tests" [Parser.tests]
tests = testGroup "Tests" [Lexer.tests, Parser.tests, Pretty.tests, WP.tests]
-- tests = testGroup "Tests" [Lexer.tests, Parser.tests, Pretty.tests, PO.tests, WP.tests]
