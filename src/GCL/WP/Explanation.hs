{-# LANGUAGE OverloadedStrings #-}

module GCL.WP.Explanation where

import Data.Loc
import Syntax.Abstract
import Syntax.Common                  ( Name(..) )
import GCL.Predicate
import Render

emptyExplain title l = Explain
         { originHeader           = title
         , originExplanation      = mempty
         , originInfMode          = Secondary
         , originHighlightPartial = False
         , originLoc              = l
         }


explainAssignment :: Pred -> Pred -> [Name] -> [Expr] -> Loc -> Origin
explainAssignment pre post vars exprs l = Explain
  { originHeader           = "Assigment"
  , originExplanation      = "After assignment, the postcondition"
                             <> (codeE . render) post
                             <> "should be implied by the precondition"
                             <> (codeE . render) pre
                             <> "after free variables"
                             <> sepByCommaE (map (codeE . render) vars)
                             <> "have been substituted with"
                             <> sepByCommaE (map (codeE . render) exprs)
  , originInfMode          = Primary
  , originHighlightPartial = False
  , originLoc              = l
  }

explainAfterLoop :: Pred -> [Expr] -> Loc -> Origin
explainAfterLoop inv guards l = Explain
      { originHeader           = "After Loop"
      , originExplanation      = "The loop invariant"
                                 <> (codeE . render) inv
                                 <> "should remain true while all the guards"
                                 <> sepByCommaE (map (codeE . render) guards)
                                 <> "become false after executing the loop"
      , originInfMode          = Primary
      , originHighlightPartial = True
      , originLoc              = l
      }

explainTermination :: Pred -> [Expr] -> Expr -> Loc -> Origin
explainTermination inv guards bnd l = Explain
      { originHeader           = "Loop Termination"
      , originExplanation      =
        "When the loop invariant"
        <> (codeE . render) inv
        <> "and one of the guards"
        <> sepByCommaE (map (codeE . render) guards)
        <> "remain true (that is, whilst looping), the bound"
        <> (codeE . render) bnd
        <> "should be greater then"
        <> (codeE . render) (Lit (Num 0) NoLoc)
      , originInfMode          = Primary
      , originHighlightPartial = True
      , originLoc              = l
      }
