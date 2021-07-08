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
                                                , sweep
                                                )
import           Server.Interpreter.Test        ( runTest
                                                , serializeTestResultValueOnly
                                                )
import           Syntax.Abstract                ( Expr(..) )
import           Test.Tasty              hiding ( after )
import Test.Util

tests :: TestTree
tests = testGroup "Substitution" [letBindings]


letBindings :: TestTree
letBindings = testGroup
  "Substitute let-bindings"
  [run "let binding" "let-1.gcl", run "let binding with assignment" "let-2.gcl"]
 where
  run :: String -> FilePath -> TestTree
  run = runGoldenTest "./test/source/Substitution/" "./test/golden/Substitution/" "" $ \sourcePath source -> do
    return $ serializeTestResultValueOnly $ runTest sourcePath source $ do
      program        <- parseProgram source
      (pos, _, _, _) <- sweep program
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
toTree (EXPN before after inAfter) = Node
  before
  toAfter
 where
  -- toBefore :: Map Inlines [Tree] 
  -- toBefore = Map.fromList $ map (\subst -> ("* ===> " <> renderedAfter subst ,map toTree (substsInAfter subst))) inBefore

  toAfter :: Map Inlines [Tree]
  toAfter = Map.singleton ("* ===> " <> after) (map toTree inAfter)

--------------------------------------------------------------------------------
-- | Typeclass for extracting `Substs` from the syntax tree 

-- like Subst, but augmented with Substs in "before" and "after"
data EXPN = EXPN
  { renderedBefore  :: Inlines
  , renderedAfter   :: Inlines
  , substsInAfter   :: [EXPN]
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
    Paren x _       -> extractExpand x
    Chain x _ y _   -> extractExpand x <> extractExpand y
    App x y _       -> extractExpand x <> extractExpand y
    Lam _ x _       -> extractExpand x
    Quant x _ y z _ -> extractExpand x <> extractExpand y <> extractExpand z
    Expand _ before after -> 
      [ EXPN (render before)
              (render after)
              (extractExpand after)
      ] 
    Subst before _mapping after ->
      [ EXPN (render before)
              (render after)
              (extractExpand after)
      ]
    ArrIdx x y _   -> extractExpand x <> extractExpand y
    ArrUpd x y z _ -> extractExpand x <> extractExpand y <> extractExpand z
    _              -> []

--------------------------------------------------------------------------------
