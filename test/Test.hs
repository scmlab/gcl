import Test.Tasty

-- import qualified Test.Parser as Parser
-- import qualified Test.Pretty as Pretty
-- import qualified Test.WP as WP
import qualified Test.PO as PO
import qualified Test.WP2 as WP2

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [
    --   Parser.tests
    -- , Pretty.tests
    -- , WP.tests
    -- ,
    --   PO.tests
    -- ,
    WP2.tests
    ]
  -- [ testCase "2+2=4" $
  --     2+2 @?= 4
  -- , testCase "7 is even" $
  --     asserTBase TBool "Oops, 7 is odd" (even 7)
  -- ]
