import qualified Test.Parser                   as Parser
import qualified Test.Render                   as Render
import qualified Test.Server                   as Server
import qualified Test.SrcLoc                   as SrcLoc
import qualified Test.Substitution             as Substitution
import           Test.Tasty                     ( TestTree
                                                , defaultMain
                                                , testGroup
                                                )
import qualified Test.TypeChecking             as TypeChecking
import qualified Test.WP                       as WP

main :: IO ()
main = defaultMain tests

-- TODO: un-un-comment other tests, after fixing parse errors
tests :: TestTree
tests = testGroup
    "Tests"
    [ Parser.tests
    , Render.tests
    , Substitution.tests
    , WP.tests
    , TypeChecking.tests
    , SrcLoc.tests
    , Server.tests
    ]
