-- import qualified Test.Lexer as Lexer
import qualified Test.Parser                   as Parser
import qualified Test.Server                   as Server
import qualified Test.SrcLoc                   as SrcLoc
import qualified Test.Expand             as Expand
import           Test.Tasty                     ( TestTree
                                                , defaultMain
                                                , testGroup
                                                )
import qualified Test.TypeChecking             as TypeChecking
import qualified Test.WP                       as WP

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup
    "Tests"
    [ Parser.tests
    , Expand.tests
    , WP.tests
    , TypeChecking.tests
    , SrcLoc.tests
    , Server.tests
    ]
