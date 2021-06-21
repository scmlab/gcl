module Render.Syntax.Common where

import Render.Class
import Syntax.Common
import Syntax.Parser.Token
import Prelude hiding (Ordering (..))

instance Render Name where
  render (Name n l) = tempHandleLoc l (render n)

instance Render ChainOp where
  render (EQProp l) = tempHandleLoc l $ render tokEQProp
  render (EQPropU l) = tempHandleLoc l $ render tokEQPropU
  render (EQ l) = tempHandleLoc l $ render tokEQ
  render (NEQ l) = tempHandleLoc l $ render tokNEQ
  render (NEQU l) = tempHandleLoc l $ render tokNEQU
  render (LTE l) = tempHandleLoc l $ render tokLTE
  render (LTEU l) = tempHandleLoc l $ render tokLTEU
  render (GTE l) = tempHandleLoc l $ render tokGTE
  render (GTEU l) = tempHandleLoc l $ render tokGTEU
  render (LT l) = tempHandleLoc l $ render tokLT
  render (GT l) = tempHandleLoc l $ render tokGT

instance Render ArithOp where
  render (Implies l) = tempHandleLoc l $ render tokImpl
  render (ImpliesU l) = tempHandleLoc l $ render tokImplU
  render (Conj l) = tempHandleLoc l $ render tokConj
  render (ConjU l) = tempHandleLoc l $ render tokConjU
  render (Disj l) = tempHandleLoc l $ render tokDisj
  render (DisjU l) = tempHandleLoc l $ render tokDisjU
  render (Neg l) = tempHandleLoc l $ render tokNeg
  render (NegU l) = tempHandleLoc l $ render tokNegU
  render (Add l) = tempHandleLoc l $ render tokAdd
  render (Sub l) = tempHandleLoc l $ render tokSub
  render (Mul l) = tempHandleLoc l $ render tokMul
  render (Div l) = tempHandleLoc l $ render tokDiv
  render (Mod l) = tempHandleLoc l $ render tokMod
  render (Max l) = tempHandleLoc l $ render tokMax
  render (Min l) = tempHandleLoc l $ render tokMin
  render (Exp l) = tempHandleLoc l $ render tokExp

instance Render QuantOp where
  render (Sum l) = tempHandleLoc l $ render tokSum
  render (Pi l) = tempHandleLoc l $ render tokPi
  render (Forall l) = tempHandleLoc l $ render tokForall
  render (Exists l) = tempHandleLoc l $ render tokExists
  render (Hash l) = tempHandleLoc l $ render tokHash

instance Render Op where
  render (ChainOp op) = render op
  render (ArithOp op) = render op
  render (QuantOp op) = render op
