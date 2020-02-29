{-# LANGUAGE OverloadedStrings #-}

module Pretty.Predicate where

import Data.Text.Prettyprint.Doc
import Data.Loc (unLoc)
import Prelude hiding (Ordering(..))

import Syntax.Predicate
import Pretty.Concrete ()
import Pretty.Util

--------------------------------------------------------------------------------
-- | Pred

instance PrettyPrec Pred where
  prettyPrec n predicate = case predicate of
    Constant p -> "Constant" <+> prettyPrec n p
    GuardIf p _ -> "Guard IF" <+> prettyPrec n p
    GuardLoop p _ -> "Guard LOOP" <+> prettyPrec n p
    Assertion p _ -> "Assertion" <+> prettyPrec n p
    LoopInvariant p b _ -> "LoopInvariant" <+> prettyPrec n p <+> "bnd:" <+> prettyPrec n b
    Bound p _ -> "Bound" <+> prettyPrec n p
    Conjunct ps -> "Conjunct" <+> prettyList ps
    Disjunct ps -> "Disjunct" <+> prettyList ps
    Negate p -> "Negate" <+> prettyPrec n p

instance Pretty Pred where
  pretty = prettyPrec 0

--------------------------------------------------------------------------------
-- | Struct & Stmt

instance Show Struct where
  show = show . pretty
instance Pretty Struct where
  pretty (Struct pre xs next) =
    "----------------------------------------------------------------" <> line
    <> braces (pretty pre) <> line
    <> vsep (map (\x -> indent 2 (pretty x)) xs) <> line
    <> pretty next
  pretty (Postcond post) =
    "----------------------------------------------------------------" <> line
    <> braces (pretty post)

instance Show Stmt where
  show = show . pretty
instance Pretty Stmt where
  pretty (Skip l) = braces (pretty (unLoc l)) <> line <> "Skip"
  pretty (Abort l) = braces (pretty (unLoc l)) <> line <> "Abort"
  pretty (Assign l _ _) = braces (pretty (unLoc l)) <> line <> "Assign"
  pretty (Do l _ xs) = braces (pretty (unLoc l)) <> line
    <> "Loop" <> line
    <> vsep (map pretty xs)
  pretty (If l xs) = braces (pretty (unLoc l)) <> line
    <> "If" <> line
    <> vsep (map pretty xs)
  pretty (Spec l _) = braces (pretty (unLoc l)) <> line <> "Spec" 


instance Show GdCmd where
  show = show . pretty
instance Pretty GdCmd where
  pretty (GdCmd guard struct) = "  |" <+> pretty guard <+> "=>" <+> line
    <> "    " <> align (pretty struct)
