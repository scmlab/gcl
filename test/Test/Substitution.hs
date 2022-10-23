{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Test.Substitution
  ( tests
  ) where

import           Control.Monad.State            ( MonadState
                                                , evalState
                                                , forM
                                                )
import           Data.Foldable                  ( toList )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           GCL.Common                     ( FreshState )
import           GCL.Substitution               ( Decls
                                                , buildRedexMap
                                                , step
                                                )
import           Pretty
import           Render.Class                   ( Render(render) )
import           Render.Element                 ( Inlines(..) )
import           Server.Pipeline
import           Syntax.Abstract.Types          ( Expr )
import           Syntax.Abstract.Util           ( programToScopeForSubstitution
                                                )
import           Test.Server.Interpreter        ( runTest
                                                , serializeTestResultValueOnly
                                                )
import           Test.Tasty              hiding ( after )
import           Test.Util

tests :: TestTree
tests = testGroup "Substitution" [letBindings]


letBindings :: TestTree
letBindings = testGroup
  "Expanding let-bindings"
  [ run "let binding"                   "let-1.gcl"
  , run "let binding with assignment 1" "let-2.gcl"
  , run "let binding with assignment 2" "let-3.gcl"
  , run "let binding with assignment and application" "let-4.gcl"
  , run "fastmul"                       "subst-fastmul.gcl"
  , run "Issue #41"                     "issue41.gcl"
  , run "shrinking the mapping 1"       "shrink.gcl"
  , run "shrinking the mapping 2"       "issue51.gcl"
  , run "consecutive vs parallel"       "issue54.gcl"
  , run "redex begets redex"            "redex-begets-redex.gcl"
  ]
 where
  run :: String -> FilePath -> TestTree
  run =
    runGoldenTest "./test/source/Substitution/" "./test/golden/Substitution/" ""
      $ \sourcePath source -> do
          return $ serializeTestResultValueOnly $ runTest sourcePath source $ do
            parsed      <- parse source
            converted   <- convert parsed
            typeChecked <- typeCheck converted
            result      <- sweep typeChecked
            let scope =
                  programToScopeForSubstitution (convertedProgram converted)
            let treesFromRedexes = evalState
                  (mapM (fromRedex scope) (sweptRedexes result))
                  (sweptCounter result)
            return (Right (VList $ toList treesFromRedexes))

--------------------------------------------------------------------------------

-- Tree-like structure for representing the transition from one Expn to the next
data Tree = Node Int -- index of the redex s
                     Inlines -- before pressing any "buttons"
                             (Map Inlines [Tree]) -- transitions

instance Pretty Tree where
  pretty (Node index expr transitions) =
    pretty index <> ":" <+> pretty expr <> line <> indent
      2
      (vcat (prettyTransitions transitions))
   where
    prettyTransitions :: Map Inlines [Tree] -> [Doc ann]
    prettyTransitions xs = Map.toList xs >>= prettyTransition

    prettyTransition :: (Inlines, [Tree]) -> [Doc ann]
    prettyTransition (transition, children) =
      [pretty transition, indent 2 $ vcat (map pretty children)]

fromRedex :: MonadState FreshState m => Decls -> (Int, Expr) -> m Tree
fromRedex scope (i, before) = do
  trees <- do
    after <- step scope before
    let redexesInAfter = buildRedexMap after
    inAfter <- forM (toList redexesInAfter) (fromRedex scope)
    return $ Map.singleton (render before <> " ===> " <> render after) inAfter
  return $ Node i (render before) trees
