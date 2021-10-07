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
import           Pretty
import           Render                         ( Inlines(..)
                                                , Render(render)
                                                )
import           Server.DSL                     ( parseProgram
                                                , sweep, Cache (cachePOs)
                                                )
import           Server.Interpreter.Test        ( runTest
                                                , serializeTestResultValueOnly
                                                )
import           Syntax.Abstract                ( Expr(..) )
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
            program        <- parseProgram source
            pos <- cachePOs <$> sweep program
            let substs = pos >>= extractExpand
            let trees  = map toTree substs
            return (Right (VList trees))


-- Tree-like structure for representing the transition from one Expn to the next
data Tree = Node Inlines -- BEFORE + MAPPING (before pressing any "buttons")
                         (Map Inlines [Tree]) -- transitions

instance Pretty Tree where
  pretty (Node before transitions) = pretty before <> line <> indent
    2
    (vcat (prettyTransitions transitions))
   where
    prettyTransitions :: Map Inlines [Tree] -> [Doc ann]
    prettyTransitions xs = Map.toList xs >>= prettyTransition

    prettyTransition :: (Inlines, [Tree]) -> [Doc ann]
    prettyTransition (transition, children) =
      [pretty transition, indent 2 $ vcat (map pretty children)]

toTree :: EXPN -> Tree
toTree (EXPN before after inBefore inAfter) = Node before (toBefore <> toAfter)
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
  { renderedBefore  :: Inlines
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
    --Chain x _ y _   -> extractExpand x <> extractExpand y
    App x y _       -> extractExpand x <> extractExpand y
    Lam _ x _       -> extractExpand x
    Quant x _ y z _ -> extractExpand x <> extractExpand y <> extractExpand z
    Expand before after ->
      [ EXPN (render before)
             (render after)
             (extractExpand before)
             (extractExpand after)
      ]
    Subst  x _ _   -> extractExpand x
    ArrIdx x y _   -> extractExpand x <> extractExpand y
    ArrUpd x y z _ -> extractExpand x <> extractExpand y <> extractExpand z
    _              -> []

--------------------------------------------------------------------------------
