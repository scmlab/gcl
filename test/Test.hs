-- import qualified Test.Lexer as Lexer
import qualified Test.Parser                   as Parser
import qualified Test.Server                   as Server
import qualified Test.SrcLoc                   as SrcLoc
import qualified Test.Substitution             as Substitution
import           Test.Tasty                     ( TestTree
                                                , defaultMain
                                                , testGroup
                                                )
import qualified Test.Type                     as Type
import qualified Test.WP                       as WP

main :: IO ()
main = defaultMain tests

tests :: TestTree
-- tests = testGroup "Tests" [Substitution.tests]
tests = testGroup "Tests" [Parser.tests, Substitution.tests, WP.tests, Type.tests, SrcLoc.tests, Server.tests]
