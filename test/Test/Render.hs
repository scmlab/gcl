{-# LANGUAGE OverloadedStrings #-}

module Test.Render where
import           Data.Text                      ( Text )
import           Pretty                         ( toByteString
                                                , toText
                                                , docToText
                                                )
import           Prettyprinter                  ( Pretty,pretty )
import           Syntax.Parser                 ( Parser )
import qualified Syntax.Parser                as Parser
import qualified Syntax.Concrete              as C
import qualified Syntax.Abstract              as A
import           Test.Tasty                     ( TestTree
                                                , testGroup
                                                )
import           Test.Tasty.HUnit               ( (@?=)
                                                , Assertion
                                                , testCase
                                                )
import Control.Monad.Except                     ( runExcept )
import           Test.Util                      ( removeTrailingWhitespace
                                                , runGoldenTest
                                                )

tests :: TestTree
tests = testGroup
  "Render"
  [ 
    expression
  ]

--------------------------------------------------------------------------------

-- | Expression
expression :: TestTree
expression = testGroup
  "Expressions"
  [ testCase "1" $ run "a + b + c + d" 
  , testCase "2" $ run "a - (b - c) - d" 
  , testCase "3" $ run "a * (1 + 2) + 3" 
  , testCase "4" $ prettyae "a * ((1 + 2) + 3)" @?= "a * (1 + 2 + 3)"
  , testCase "5" $ prettyae "a * (1 + (2 + 3))" @?= "a * (1 + 2 + 3)"
  , testCase "6" $ prettyae "a * (b * c)" @?= "a * b * c"
  , testCase "7" $ prettyae "a && b || c && d" @?= "((a && b) || c) && d"
  , testCase "8" $ run "a || b || c"
  , testCase "9" $ run "a => b => c"
  , testCase "10" $ run "(a => b) => c"
  , testCase "11" $ run "- a"
  , testCase "12" $ run "f (- a)"
  , testCase "13" $ prettyae  "(- a) + b" @?= "- a + b"
  , testCase "14" $ prettyae "- a + b - c" @?= "(- a + b) - c"
  , testCase "15" $ run "a * (- a)"
  , testCase "16" $ run "a => ~ b"
  , testCase "17" $ run "a > b >= c"
  , testCase "18" $ run "~ (a > b >= c)"
  ]
  where 
    run :: Text -> Assertion
    run e =  prettyae e @?= e
    prettyae = docToText . pretty . ae . ce
 

--------------------------------------------------------------------------------

parserCompare :: Pretty a => Parser a -> Text -> Text -> Assertion
parserCompare parser actual expected =
  (removeTrailingWhitespace . toText . Parser.scanAndParse parser "<test>")
      actual
    @?= removeTrailingWhitespace expected

parserIso :: Pretty a => Parser a -> Text -> Assertion
parserIso parser raw = parserCompare parser raw raw



ce :: Text -> C.Expr
ce = unsafeFromRight . Parser.scanAndParse Parser.expression ""
ae :: C.Expr -> A.Expr
ae = unsafeFromRight . runExcept . C.toAbstract

unsafeFromRight :: Either a b -> b
unsafeFromRight (Right x) = x
unsafeFromRight (Left _) = error "Error raised from 'unsafeFromRight', probably caused by parsing failure."