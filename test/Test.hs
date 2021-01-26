import qualified Test.Lexer as Lexer
import qualified Test.Parser as Parser
import qualified Test.Concrete2 as Concrete2
import qualified Test.ProofObligation as PO
import qualified Test.Type as Type
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [Type.tests, Lexer.tests, Parser.tests, Concrete2.tests]
    -- [Lexer.tests, Parser.tests, Concrete2.tests, WP.tests]
