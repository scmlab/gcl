module Pretty.Common where

import Data.Text.Prettyprint.Doc (Pretty (pretty))
import Pretty.Util
import Syntax.Common
import Prelude hiding (Ordering (..))
import Syntax.Parser.Token


-- | Name
-- instance Pretty Name where
--   pretty = toDoc . prettyWithLoc
instance PrettyWithLoc Name where
  prettyWithLoc (Name n l) = fromDoc l (pretty n)

-- | Operators
instance Pretty ChainOp where
  pretty = toDoc . prettyWithLoc

instance PrettyWithLoc ChainOp where
  prettyWithLoc (EQProp l) = fromDoc l . pretty $ tokEQProp
  prettyWithLoc (EQPropU l) = fromDoc l . pretty $ tokEQPropU
  prettyWithLoc (EQ l) = fromDoc l . pretty $ tokEQ
  prettyWithLoc (NEQ l) = fromDoc l . pretty $ tokNEQ
  prettyWithLoc (NEQU l) = fromDoc l . pretty $ tokNEQU
  prettyWithLoc (LTE l) = fromDoc l . pretty $ tokLTE
  prettyWithLoc (LTEU l) = fromDoc l . pretty $ tokLTEU
  prettyWithLoc (GTE l) = fromDoc l . pretty $ tokGTE
  prettyWithLoc (GTEU l) = fromDoc l . pretty $ tokGTEU
  prettyWithLoc (LT l) = fromDoc l . pretty $ tokLT
  prettyWithLoc (GT l) = fromDoc l . pretty $ tokGT

instance Pretty ArithOp where
  pretty = toDoc . prettyWithLoc

instance PrettyWithLoc ArithOp where
  prettyWithLoc (Implies l) = fromDoc l . pretty $ tokImpl
  prettyWithLoc (ImpliesU l) = fromDoc l . pretty $ tokImplU
  prettyWithLoc (Conj l) = fromDoc l . pretty $ tokConj
  prettyWithLoc (ConjU l) = fromDoc l . pretty $ tokConjU
  prettyWithLoc (Disj l) = fromDoc l . pretty $ tokDisj
  prettyWithLoc (DisjU l) = fromDoc l . pretty $ tokDisjU
  prettyWithLoc (Neg l) = fromDoc l . pretty $ tokNeg
  prettyWithLoc (NegU l) = fromDoc l . pretty $ tokNegU
  prettyWithLoc (Add l) = fromDoc l . pretty $ tokAdd
  prettyWithLoc (Sub l) = fromDoc l . pretty $ tokSub
  prettyWithLoc (Mul l) = fromDoc l . pretty $ tokMul
  prettyWithLoc (Div l) = fromDoc l . pretty $ tokDiv
  prettyWithLoc (Mod l) = fromDoc l . pretty $ tokMod

instance Pretty QuantOp where
  pretty = toDoc . prettyWithLoc

instance PrettyWithLoc QuantOp where
  prettyWithLoc (Sum l) = fromDoc l . pretty $ tokSum
  prettyWithLoc (Forall l) = fromDoc l . pretty $ tokForall
  prettyWithLoc (Exists l) = fromDoc l . pretty $ tokExists
  prettyWithLoc (Max l) = fromDoc l . pretty $ tokMax
  prettyWithLoc (Min l) = fromDoc l . pretty $ tokMin
  prettyWithLoc (Hash l) = fromDoc l . pretty $ tokHash

instance Pretty Op where
  pretty = toDoc . prettyWithLoc

instance PrettyWithLoc Op where
  prettyWithLoc (ChainOp op) = prettyWithLoc op
  prettyWithLoc (ArithOp op) = prettyWithLoc op
  prettyWithLoc (QuantOp op) = prettyWithLoc op
