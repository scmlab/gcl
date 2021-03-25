-- import qualified Test.Lexer as Lexer
-- import qualified Test.Parser as Parser
-- import qualified Test.ProofObligation as PO
import qualified Test.Type as Type
import qualified Test.Parser as Parser
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
-- tests = testGroup "Tests" [Parser.tests]
tests = testGroup "Tests" [Parser.tests, Type.tests]
    -- [Lexer.tests, Parser.tests, Concrete.tests, WP.tests]
