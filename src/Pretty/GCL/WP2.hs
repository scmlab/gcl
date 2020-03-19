{-# LANGUAGE OverloadedStrings #-}

module Pretty.GCL.WP2 where

import           Data.Text.Prettyprint.Doc
import           Prelude                 hiding ( Ordering(..) )
import           Data.Loc                       ( )

import           GCL.WP2

import           Pretty.Abstract                ( )
import           Pretty.Concrete                ( )
import           Pretty.Predicate               ( )

--------------------------------------------------------------------------------
-- | StructError

instance Pretty StructError2 where
  pretty (MissingLoopInvariant loc) = "Missing Loop Invariant" <+> pretty loc
  pretty (MissingBound         loc) = "Missing Bound" <+> pretty loc
  pretty (MissingPrecondition  loc) = "Missing Precondition" <+> pretty loc
  pretty (MissingPostcondition loc) = "Missing Postcondition" <+> pretty loc
  pretty (PreconditionUnknown  loc) = "Precondition Unknown" <+> pretty loc
  pretty (DigHole              loc) = "Dig Hole" <+> pretty loc
