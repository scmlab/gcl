{-# LANGUAGE FlexibleContexts #-}

module GCL.Pred where

import GCL.Expr
import GCL.EnumHole

import Control.Monad (liftM2)
import Data.Map (Map)

data BinRel = Eq | LEq | GEq | LTh | GTh
  deriving Eq

data Pred = Term BinRel Expr Expr
          | Implies Pred Pred
          | Conj Pred Pred
          | Disj Pred Pred
          | Neg Pred
          | HoleP (Maybe EIdx)
  deriving Eq

predEq :: Pred -> Pred -> Bool
predEq = (==)

instance EnumHole Pred where
  enumHole (Term rel e1 e2) =
    liftM2 (Term rel) (enumHole e1) (enumHole e2)
  enumHole (Implies p q) =
    liftM2 Implies (enumHole p) (enumHole q)
  enumHole (Conj p q) =
    liftM2 Conj (enumHole p) (enumHole q)
  enumHole (Disj p q) =
    liftM2 Disj (enumHole p) (enumHole q)
  enumHole (Neg p) = Neg <$> enumHole p
  enumHole (HoleP _)  = HoleP . Just <$> freshHole

substP :: Map VName Expr -> Pred -> Pred
substP env (Term rel e1 e2) =
  Term rel (substE env e1) (substE env e2)
substP env (Implies p q) = Implies (substP env p) (substP env q)
substP env (Conj p q) = Conj (substP env p) (substP env q)
substP env (Disj p q) = Disj (substP env p) (substP env q)
substP env (Neg p) = Neg (substP env p)

showBinRel :: BinRel -> ShowS
showBinRel Eq  = ('=':)
showBinRel LEq = ("<="++)
showBinRel GEq = (">="++)
showBinRel LTh = ('<':)
showBinRel GTh = ('>':)

showPredS :: Pred -> ShowS
showPredS (Term rel e1 e2) =
  ('(':) . showExprS e1 . (' ':) . showBinRel rel .
  (' ':) . showExprS e2 . (')':)
showPredS (Implies p q) =
  ('(':) . showPredS p . (" => "++) . showPredS q . (')':)
showPredS (Conj p q) =
  ('(':) . showPredS p . (" && "++) . showPredS q . (')':)
showPredS (Disj p q) =
  ('(':) . showPredS p . (" || "++) . showPredS q . (')':)
showPredS (Neg p) =
  ("(not "++) . showPredS p . (')':)
showPredS (HoleP Nothing) = ("[_]" ++)
showPredS (HoleP (Just i)) =
    ('[':) . showsPrec 0 i . (']':)

instance Show Pred where
  showsPrec _ = showPredS
