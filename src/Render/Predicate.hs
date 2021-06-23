{-# LANGUAGE OverloadedStrings #-}

module Render.Predicate where

import           Data.Loc                       ( locOf )
import           Data.Loc.Range                 ( fromLoc )
import           GCL.Predicate
import           GCL.WP
import           Render.Class
import           Render.Element
import           Render.Syntax.Abstract         ( )
import qualified Data.Text as Text

instance Render StructWarning where
  render (MissingBound _)
    = "Bound missing at the end of the assertion before the DO construct \" , bnd : ... }\""
  render (ExcessBound _) =
    "The bound annotation at this assertion is unnecessary"

instance RenderSection StructWarning where
  renderSection x = case x of
    MissingBound range ->
      Section Yellow [Header "Bound Missing" (Just range), Paragraph (render x)]
    ExcessBound range ->
      Section Yellow [Header "Excess Bound" (Just range), Paragraph (render x)]

instance RenderSection Spec where
  renderSection (Specification _ pre post range) = Section
    Blue
    [ Header "Precondition" (Just range)
    , Code (render pre)
    , Header "Precondition" Nothing
    , Code (render post)
    ]

instance Render Pred where
  renderPrec n x = case x of
    Constant p          -> renderPrec n p
    GuardIf   p _       -> renderPrec n p
    GuardLoop p _       -> renderPrec n p
    Assertion p _       -> renderPrec n p
    LoopInvariant p _ _ -> renderPrec n p
    Bound p _           -> renderPrec n p
    Conjunct ps         -> punctuateE " ∧" (map render ps)
    Disjunct ps         -> punctuateE " ∨" (map render ps)
    Negate   p          -> "¬" <+> renderPrec n p

instance RenderSection PO where
  renderSection (PO _ pre post origin) = Section
    Plain
    [ Header (Text.pack $ show $ render origin) (fromLoc (locOf origin))
    , Code (vertE [render pre, "⇒", render post])
    ]

instance Render Origin where
  render AtAbort{}       = "Abort"
  render AtSkip{}        = "Skip"
  render AtSpec{}        = "Spec"
  render AtAssignment{}  = "Assigment"
  render AtAssertion{}   = "Assertion"
  render AtIf{}          = "Conditional"
  render AtLoop{}        = "Loop Invariant"
  render AtTermination{} = "Loop Termination"
