{-# LANGUAGE OverloadedStrings #-}

module Test.Substitution
  ( tests
  ) where

import           Control.Monad.State            ( evalState
                                                , forM
                                                )
import           Data.Foldable                  ( toList )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           GCL.Common                     ( Fresh )
import           GCL.Substitution               ( CollectRedexes(collectRedexes)
                                                , Scope
                                                , step
                                                )
import           Pretty
import           Render.Class                   ( Render(render) )
import           Render.Element                 ( Inlines(..) )
import           Server.DSL                     ( Cache(cacheRedexes)
                                                , parseProgram
                                                , sweep
                                                )
import           Server.Interpreter.Test        ( runTest
                                                , serializeTestResultValueOnly
                                                )
import           Syntax.Abstract.Types          ( Redex(..) )
import           Syntax.Abstract.Util           ( programToScopeForSubstitution
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
  ]
 where
  run :: String -> FilePath -> TestTree
  run =
    runGoldenTest "./test/source/Substitution/" "./test/golden/Substitution/" ""
      $ \sourcePath source -> do
          return $ serializeTestResultValueOnly $ runTest sourcePath source $ do
            program <- parseProgram source
            cache   <- sweep program
            let scope = programToScopeForSubstitution program
            let treesFromRedexes = evalState
                  (mapM (fromRedex scope) (cacheRedexes cache))
                  (0 :: Int)
            return (Right (VList $ toList treesFromRedexes))

--------------------------------------------------------------------------------

-- Tree-like structure for representing the transition from one Expn to the next
data Tree = Node Int -- index of the redex s
                     Inlines -- BEFORE + MAPPING (before pressing any "buttons")
                             (Map Inlines [Tree]) -- transitions

instance Pretty Tree where
  pretty (Node index before transitions) =
    pretty index <> ":" <+> pretty before <> line <> indent
      2
      (vcat (prettyTransitions transitions))
   where
    prettyTransitions :: Map Inlines [Tree] -> [Doc ann]
    prettyTransitions xs = Map.toList xs >>= prettyTransition

    prettyTransition :: (Inlines, [Tree]) -> [Doc ann]
    prettyTransition (transition, children) =
      [pretty transition, indent 2 $ vcat (map pretty children)]

fromRedex :: Fresh m => Scope -> Redex -> m Tree
fromRedex scope (Rdx i before _) = do
  trees <- do
    after <- step scope before
    let redexesInAfter = collectRedexes after
    inAfter <- forM redexesInAfter (fromRedex scope)
    return $ Map.singleton (render before <> " ===> " <> render after) inAfter
  return $ Node i (render before) trees
