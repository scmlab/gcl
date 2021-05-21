-- import qualified Test.Lexer as Lexer
import qualified Test.WP as WP
import qualified Test.Type as Type
import qualified Test.Parser as Parser
import qualified Test.Server as Server
import qualified Test.SrcLoc as SrcLoc
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
-- tests = testGroup "Tests" [Server.tests]
tests = testGroup "Tests" [Parser.tests, WP.tests, Type.tests, SrcLoc.tests, Server.tests]