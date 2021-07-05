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
                                                , isEmpty
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
      let substs = pos >>= extractSubst
      let trees  = map toTree substs
      return (Right (VList trees))


-- Tree-like structure for representing the transition from one Subst to the next 
data Tree = Node Inlines -- BEFORE + MAPPING (before pressing any "buttons")
                         (Map Inlines [Tree]) -- transitions

                        --  MAPPING ==> AFTER 
                        --  (Map Inlines Tree) --  "buttons" with the result after pressing them 

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

toTree :: SUBST -> Tree
toTree (SUBST before mapping after inBefore inAfter) = Node
  (before <> " " <> mapping)
  (toBefore <> toAfter)
 where
  toBefore :: Map Inlines [Tree] 
  toBefore = Map.fromList $ map (\subst -> ("* " <> renderedMapping subst <> " ===>" <> renderedAfter subst ,map toTree (substsInAfter subst))) inBefore

  toAfter :: Map Inlines [Tree]
  toAfter = Map.singleton ("* " <> mapping <> " ===>" <> after) (map toTree inAfter)

--------------------------------------------------------------------------------
-- | Typeclass for extracting `Substs` from the syntax tree 

-- like Subst, but augmented with Substs in "before" and "after"
data SUBST = SUBST
  { renderedBefore  :: Inlines
  , renderedMapping :: Inlines
  , renderedAfter   :: Inlines
  , substsInBefore  :: [SUBST]
  , substsInAfter   :: [SUBST]
  }
  deriving Show

class ExtractSubst a where
  extractSubst :: a -> [SUBST]

instance ExtractSubst PO where
  extractSubst (PO pre post _ _ _) = extractSubst pre <> extractSubst post

instance ExtractSubst Pred where
  extractSubst = \case
    Constant x          -> extractSubst x
    GuardIf   x _       -> extractSubst x
    GuardLoop x _       -> extractSubst x
    Assertion x _       -> extractSubst x
    LoopInvariant x y _ -> extractSubst x <> extractSubst y
    Bound x _           -> extractSubst x
    Conjunct xs         -> xs >>= extractSubst
    Disjunct xs         -> xs >>= extractSubst
    Negate   x          -> extractSubst x

instance ExtractSubst Expr where
  extractSubst = \case
    Paren x _       -> extractSubst x
    Chain x _ y _   -> extractSubst x <> extractSubst y
    App x y _       -> extractSubst x <> extractSubst y
    Lam _ x _       -> extractSubst x
    Quant x _ y z _ -> extractSubst x <> extractSubst y <> extractSubst z
    Subst before mapping after ->
      [ SUBST (render before)
              renderedMapping'
              (render after)
              (extractSubst before)
              (extractSubst after)
      ]
     where
      -- render empty mapping as "[]"
      renderedMapping' =
        let rendered = render mapping
        in  if isEmpty rendered then "[]" else rendered
    ArrIdx x y _   -> extractSubst x <> extractSubst y
    ArrUpd x y z _ -> extractSubst x <> extractSubst y <> extractSubst z
    _              -> []

--------------------------------------------------------------------------------
