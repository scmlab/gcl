{-# LANGUAGE OverloadedStrings #-}

module Render.WP where

-- import Data.Loc (locOf)
import qualified Data.Text as Text
import GCL.WP
import Pretty (pretty)
import Pretty.Util (renderStrict)
import Render.Class
import Render.Element
import Render.Syntax.Abstract ()
import Syntax.Predicate

instance Render StructWarning where
  render (MissingBound _) = "Bound missing at the end of the assertion before the DO construct \" , bnd : ... }\""
  render (ExcessBound _) = "The bound annotation at this assertion is unnecessary"

instance RenderBlock StructWarning where
  renderBlock x = case x of
    MissingBound range -> blockE (Just "Bound Missing") (Just range) (render x)
    ExcessBound range -> blockE (Just "Excess Bound") (Just range) (render x)

instance RenderBlock Spec where
  renderBlock (Specification _ pre post _loc) =
    proofObligationE
      Nothing
      (render pre)
      (render post)

instance Render Pred where
  renderPrec n x = case x of
    Constant p -> renderPrec n p
    GuardIf p _ -> renderPrec n p
    GuardLoop p _ -> renderPrec n p
    Assertion p _ -> renderPrec n p
    LoopInvariant p _ _ -> renderPrec n p
    Bound p _ -> renderPrec n p
    Conjunct ps -> horzE $ punctuateE " ∧ " (map render ps)
    Disjunct ps -> mconcat $ punctuateE " ∨ " (map render ps)
    Negate p -> "¬" <+> renderPrec n p

instance RenderBlock PO where
  renderBlock (PO _ pre post origin) =
    proofObligationE
      (Just $ Text.unpack $ renderStrict $ pretty origin)
      (render pre)
      (render post)
