{-# LANGUAGE OverloadedStrings #-}

module Test.Pretty where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text (renderLazy)
import Data.Loc
import Prelude hiding (Ordering(..))

import qualified Syntax.Parser as Parser
import qualified REPL as REPL
import Syntax.Parser (Parser)
import Syntax.Abstract
import Error
import Pretty ()

tests :: TestTree
tests = testGroup "Prettifier"
  [ expression
  ]

--------------------------------------------------------------------------------
-- | Expression

expression :: TestTree
expression = testGroup "Expressions"
  [ testCase "1" $ run "X > Y && X > Y" @?= Right "X > Y ⋀ X > Y"
  , testCase "2" $ run "1 + 2 * 3 - 4" @?= Right "1 + 2 * 3 - 4"
  , testCase "3" $ run "1 + 2 * 3 = 4" @?= Right "1 + 2 * 3 = 4"
  , testCase "4" $ run "1 > 2 = True" @?= Right "1 > 2 = True"
  , testCase "5" $ run "(1 + 2) * 3 = (4)" @?= Right "(1 + 2) * 3 = 4"
  ]
  where
    run :: Text -> Either [Error] Text
    run text = do
      expr <- REPL.scan "<test>" text
                >>= REPL.parse Parser.expression "<text>"
                >>= REPL.abstract
      return $ renderLazy $ layoutCompact $ pretty expr
