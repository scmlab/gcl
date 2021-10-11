{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Test.Substitution
  ( tests
  ) where

import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           GCL.Predicate                  ( PO(PO)
                                                , Pred(..)
                                                )
import           GCL.Substitution               ( collectRedexes )
import           Pretty
import           Render                         ( Inlines(..)
                                                , Render(render)
                                                )
import           Server.DSL                     ( Cache(cachePOs)
                                                , parseProgram
                                                , sweep
                                                )
import           Server.Interpreter.Test        ( runTest
                                                , serializeTestResultValueOnly
                                                )
import           Syntax.Abstract                ( Case(CaseConstructor)
                                                , Expr(..)
                                                , Redex(..)
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
            let pos     = cachePOs cache
            let trees   = VList $ map toTree (pos >>= extractExpand)
            let redexes = VList (pos >>= collectRedexes)
            return (Right (trees, redexes))


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

toTree :: EXPN -> Tree
toTree (EXPN index before after inBefore inAfter) = Node
  index
  before
  (toBefore <> toAfter)
 where
  toBefore :: Map Inlines [Tree]
  toBefore = Map.fromList $ map
    (\expn ->
      ( renderedBefore expn <> " ===> " <> renderedAfter expn
      , map toTree (buttonsInBefore expn)
      )
    )
    inBefore

  toAfter :: Map Inlines [Tree]
  toAfter = Map.singleton (before <> " ===> " <> after) (map toTree inAfter)

--------------------------------------------------------------------------------
-- | Typeclass for extracting `Expand` from the syntax tree

-- like `Expand`, but augmented with "buttons" in "before" and "after"
data EXPN = EXPN
  { redexIndex      :: Int
  , renderedBefore  :: Inlines
  , renderedAfter   :: Inlines
  , buttonsInBefore :: [EXPN]
  , buttonsInAfter  :: [EXPN]
  }
  deriving Show

class ExtractExpand a where
  extractExpand :: a -> [EXPN]

instance ExtractExpand PO where
  extractExpand (PO pre post _ _ _) = extractExpand pre <> extractExpand post

instance ExtractExpand Pred where
  extractExpand = \case
    Constant x          -> extractExpand x
    GuardIf   x _       -> extractExpand x
    GuardLoop x _       -> extractExpand x
    Assertion x _       -> extractExpand x
    LoopInvariant x y _ -> extractExpand x <> extractExpand y
    Bound x _           -> extractExpand x
    Conjunct xs         -> xs >>= extractExpand
    Disjunct xs         -> xs >>= extractExpand
    Negate   x          -> extractExpand x

instance ExtractExpand Expr where
  extractExpand = \case
    App x y _       -> extractExpand x <> extractExpand y
    Lam _ x _       -> extractExpand x
    Quant _ _ _ z _ -> extractExpand z
    Redex (Rdx index before after) ->
      [ EXPN index
             (render before)
             (render after)
             (extractExpand before)
             (extractExpand after)
      ]
    DisplaySubst _ _ -> []
    ArrIdx x y _     -> extractExpand x <> extractExpand y
    ArrUpd x y z _   -> extractExpand x <> extractExpand y <> extractExpand z
    Case _ xs _      -> xs >>= extractExpand
    _                -> []

instance ExtractExpand Case where
  extractExpand (CaseConstructor _ _ x) = extractExpand x

--------------------------------------------------------------------------------
