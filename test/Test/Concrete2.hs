{-# LANGUAGE OverloadedStrings #-}

module Test.Concrete2 where

import Data.Text (Text, pack)
import Data.Text.Lazy (fromStrict, toStrict)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
  ( renderLazy,
  )
import Error
import qualified LSP
import Pretty (renderStrict)
import Pretty.Util (PrettyWithLoc (prettyWithLoc))
import qualified Syntax.Concrete2 as Concrete2
import Syntax.Location
import Syntax.Parser (Parser)
import qualified Syntax.Parser as Parser
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (Ordering (..))

tests :: TestTree
tests = testGroup "Prettifier" [expression]

--------------------------------------------------------------------------------

render :: Pretty a => Parser a -> Text -> Text
render parser raw =
  let expr = LSP.runM $ LSP.scan "<test>" raw >>= LSP.parse parser "<test>"
  in renderStrict $ pretty expr

-- | Expression
expression :: TestTree
expression =
  testGroup
    "Expressions"
    [ testCase "1" $ run "X > (Y) && (X) > Y" @?= "X > (Y) && (X) > Y",
      testCase "2" $ run "1 + 2 * (3) - 4" @?= "1 + 2 * (3) - 4",
      testCase "3" $ run "1 + 2 * 3 = 4" @?= "1 + 2 * 3 = 4",
      testCase "4" $ run "1 > 2 = True" @?= "1 > 2 = True",
      testCase "5" $ run "(1 + 2) * (3) = (4)" @?= "(1 + 2) * (3) = (4)",
      testCase "6" $ run "3 / (2 + X)" @?= "3 / (2 + X)",
      testCase "7" $ run "3 / 2 + X" @?= "3 / 2 + X",
      testCase "8" $ run "X > (Y) ∧ (X) > Y" @?= "X > (Y) ∧ (X) > Y"
    ]
  where
    run :: Text -> Text
    run = render Parser.expression
