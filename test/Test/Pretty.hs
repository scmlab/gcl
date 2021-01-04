{-# LANGUAGE OverloadedStrings #-}

module Test.Pretty where


import Data.Text (Text)
import Data.Text.Lazy (toStrict, fromStrict)

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
  ( renderLazy,
  )
import Error
import qualified LSP
import Pretty (renderStrict)
import Syntax.Location
import qualified Syntax.Parser as Parser
import qualified Syntax.Concrete as Concrete
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (Ordering (..))

tests :: TestTree
tests = testGroup "Prettifier" [expression]

--------------------------------------------------------------------------------

-- | Expression
expression :: TestTree
expression =
  testGroup
    "Expressions"
    [ testCase "1" $ run "X > Y && X > Y" @?= "X > Y ∧ X > Y",
      testCase "2" $ run "1 + 2 * 3 - 4" @?= "1 + 2 * 3 - 4",
      testCase "3" $ run "1 + 2 * 3 = 4" @?= "1 + 2 * 3 = 4",
      testCase "4" $ run "1 > 2 = True" @?= "1 > 2 = True",
      testCase "5" $ run "(1 + 2) * 3 = (4)" @?= "(1 + 2) * 3 = 4",
      testCase "6" $ run "3 / (2 + X)" @?= "3 / (2 + X)",
      testCase "7" $ run "3 / 2 + X" @?= "3 / 2 + X"
    ]
  where
    run :: Text -> Text
    run text = 
      let expr = LSP.runM $ LSP.scan "<test>" text >>= LSP.parse Parser.expression "<test>" :: Either Error Concrete.Expr 
      in renderStrict $ pretty $ fmap depart expr
