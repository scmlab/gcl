{-# LANGUAGE OverloadedStrings #-}

module Render.Predicate where

import           Data.Loc                       ( locOf )
import           Data.Loc.Range                 ( fromLoc )
import qualified Data.Text                     as Text
import           GCL.Predicate
import           GCL.WP.Type
import           Render.Class
import           Render.Element
import           Render.Syntax.Abstract         ( )

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
    , Header "Postcondition" Nothing
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
      Explain _ x _ _ -> [Paragraph x]
      _               -> [Paragraph "explanation not available"]

-- as header 
instance Render Origin where
  render AtAbort{}              = "Abort"
  render AtSkip{}               = "Skip"
  render AtSpec{}               = "Spec"
  render AtAssignment{}         = "Assigment"
  render AtAssertion{}          = "Assertion"
  render AtIf{}                 = "Conditional"
  render AtLoop{}               = "Loop Invariant"
  render AtTermination{}        = "Loop Termination"
  render (Explain header _ _ _) = render header
