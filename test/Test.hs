import Test.Tasty

-- import qualified Test.Parser as Parser
-- import qualified Test.Pretty as Pretty
-- import qualified Test.WP as WP
import qualified Test.Obligation as Obligation
import qualified Test.WeakestPrecond as WeakestPrecond

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [
    --   Parser.tests
    -- , Pretty.tests
    -- , WP.tests
    -- , WP2.tests

      Obligation.tests
    -- , WeakestPrecond.tests
    ]
  -- [ testCase "2+2=4" $
  --     2+2 @?= 4
  -- , testCase "7 is even" $
  --     asserTBase TBool "Oops, 7 is odd" (even 7)
  -- ]
