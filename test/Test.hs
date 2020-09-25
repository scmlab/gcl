import Test.Tasty ( defaultMain, testGroup, TestTree )

import qualified Test.Parser                   as Parser
import qualified Test.Lexer                    as Lexer
import qualified Test.Pretty                   as Pretty
import qualified Test.WP                       as WP
import qualified Test.WP2                      as WP2

main :: IO ()
main = defaultMain tests

tests :: TestTree
-- tests = testGroup "Tests" [Lexer.tests]
tests = testGroup "Tests" [Lexer.tests, Parser.tests]
-- tests = testGroup "Tests" [Lexer.tests, Parser.tests, Pretty.tests, WP2.tests]
-- tests = testGroup "Tests" [Parser.tests, Pretty.tests, WP2.tests, WP.tests]
-- tests = testGroup "Tests" [Parser.tests]
