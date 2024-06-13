module Render.Syntax.Common where

import           Render.Class
import           Syntax.Common
import           Syntax.Parser.Lexer
import           Prelude                 hiding ( Ordering(..) )

instance Render Name where
  render (Name n l) = tempHandleLoc l (render n)

instance Render ChainOp where
  render (EQProp  l) = tempHandleLoc l $ render (show TokEQProp)
  render (EQPropU l) = tempHandleLoc l $ render (show TokEQPropU)
  render (EQ      l) = tempHandleLoc l $ render (show TokEQ)
  render (NEQ     l) = tempHandleLoc l $ render (show TokNEQ)
  render (NEQU    l) = tempHandleLoc l $ render (show TokNEQU)
  render (LTE     l) = tempHandleLoc l $ render (show TokLTE)
  render (LTEU    l) = tempHandleLoc l $ render (show TokLTEU)
  render (GTE     l) = tempHandleLoc l $ render (show TokGTE)
  render (GTEU    l) = tempHandleLoc l $ render (show TokGTEU)
  render (LT      l) = tempHandleLoc l $ render (show TokLT)
  render (GT      l) = tempHandleLoc l $ render (show TokGT)

instance Render ArithOp where
  render (Implies  l) = tempHandleLoc l $ render (show TokImpl)
  render (ImpliesU l) = tempHandleLoc l $ render (show TokImplU)
  render (Conj     l) = tempHandleLoc l $ render (show TokConj)
  render (ConjU    l) = tempHandleLoc l $ render (show TokConjU)
  render (Disj     l) = tempHandleLoc l $ render (show TokDisj)
  render (DisjU    l) = tempHandleLoc l $ render (show TokDisjU)
  render (Neg      l) = tempHandleLoc l $ render (show TokNeg)
  render (NegU     l) = tempHandleLoc l $ render (show TokNegU)
  render (NegNum   l) = tempHandleLoc l $ render (show TokSub)
  render (Add      l) = tempHandleLoc l $ render (show TokAdd)
  render (Sub      l) = tempHandleLoc l $ render (show TokSub)
  render (Mul      l) = tempHandleLoc l $ render (show TokMul)
  render (Div      l) = tempHandleLoc l $ render (show TokDiv)
  render (Mod      l) = tempHandleLoc l $ render (show TokMod)
  render (Max      l) = tempHandleLoc l $ render (show TokMax)
  render (Min      l) = tempHandleLoc l $ render (show TokMin)
  render (Exp      l) = tempHandleLoc l $ render (show TokExp)
  render (Hash     l) = tempHandleLoc l $ render (show TokHash)
  render (PointsTo l) = tempHandleLoc l $ render (show TokPointsTo)
  render (SConj    l) = tempHandleLoc l $ render (show TokSConj)
  render (SImp     l) = tempHandleLoc l $ render (show TokLolipop)


instance Render TypeOp where
  render (Arrow l) = tempHandleLoc l $ render (show TokArrowU)

instance Render Op where
  render (ChainOp op) = render op
  render (ArithOp op) = render op
  render (TypeOp op) = render op