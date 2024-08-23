{-# LANGUAGE OverloadedStrings #-}

module Render.Predicate where

import           Data.Loc                       ( locOf, Loc (NoLoc) )
import           Data.Loc.Range                 ( fromLoc )
import qualified Data.Text                     as Text
import           GCL.Predicate
import           GCL.WP.Types
import           Render.Class
import           Render.Element
import           Render.Syntax.Abstract         ( )
import           Syntax.Abstract.Types          ( Expr(..) )
import           Syntax.Common.Types            ( ArithOp (..) )
import           Render.Syntax.Typed

instance Render StructWarning where
  render (MissingBound _)
    = "Bound missing at the end of the assertion before the DO construct \" , bnd : ... }\""

instance RenderSection StructWarning where
  renderSection x = case x of
    MissingBound range ->
      Section Yellow [Header "Bound Missing" (Just range), Paragraph (render x)]

instance RenderSection Spec where
  renderSection (Specification _ pre post range _) = Section
    Blue
    [ Header "Precondition" (Just range)
    , Code (render pre)
    , Header "Postcondition" Nothing
    , Code (render post)
    ]

-- instance Render Pred where
--   renderPrec n x = renderPrec n (exprOfPred x)
  -- renderPrec n x = case x of
  --   Constant p          -> renderPrec n p
  --   GuardIf   p _       -> renderPrec n p
  --   GuardLoop p _       -> renderPrec n p
  --   Assertion p _       -> renderPrec n p
  --   LoopInvariant p _ _ -> renderPrec n p
  --   Bound p _           -> renderPrec n p
  --   Conjunct ps         -> punctuateE " ∧" (map render ps)
  --   Disjunct ps         -> punctuateE " ∨" (map render ps)
  --   Negate   p          -> "¬" <+> renderPrec n p

{-
exprOfPred :: Pred -> Expr
exprOfPred p = case p of
  Constant ex -> ex
  GuardIf ex _ -> ex
  GuardLoop ex _ -> ex
  Assertion ex _ -> ex
  LoopInvariant ex _ _ -> ex
  Bound ex _ -> ex
  Conjunct prs -> case map exprOfPred prs of
    []    -> error "Impossible case: incomplete Pred (in Render.Predicate.exprOfPred, Conjunct case)"
    [ex]  -> ex
    x : y : exprs -> foldl (makeOpExpr conjOp)
                           (makeOpExpr conjOp x y)
                           exprs
  Disjunct prs -> case map exprOfPred prs of
    []    -> error "Impossible case: incomplete Pred (in Render.Predicate.exprOfPred, Disjunct case)"
    [ex]  -> ex
    x : y : exprs -> foldl (makeOpExpr disjOp)
                           (makeOpExpr disjOp x y)
                           exprs
  Negate pr ->
    let ex = exprOfPred pr
    in App (Op $ NegU NoLoc) ex (locOf ex)
  where -- TODO: Maybe we don't need `Op` here. This requires further investigation.
    conjOp = Op $ ConjU NoLoc
    disjOp = Op $ DisjU NoLoc
    makeOpExpr :: Expr -> Expr -> Expr -> Expr
    makeOpExpr op x y = App (App op x (locOf x)) y (locOf y)
-}

instance RenderSection PO where
  renderSection (PO pre post anchorHash anchorLoc origin) =
    Section Plain
      $  [ HeaderWithButtons (Text.pack $ show $ render origin)
                             (fromLoc (locOf origin))
                             anchorHash
                             anchorLoc
         ]
      <> [Code (vertE [render pre, "⇒", render post])]
      <> explanation
   where
    explanation = case origin of
      Explain _ x _ _ _ -> [Paragraph x]
      _                 -> [Paragraph "explanation not available"]

-- as header
instance Render Origin where
  render AtAbort{}                = "Abort"
  render AtSkip{}                 = "Skip"
  render AtSpec{}                 = "Spec"
  render AtAssignment{}           = "Assignment"
  render AtAssertion{}            = "Assertion"
  render AtIf{}                   = "Conditional"
  render AtLoop{}                 = "Loop Invariant"
  render AtTermination{}          = "Loop Termination"
  render (Explain header _ _ _ _) = render header
