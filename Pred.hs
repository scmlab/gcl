{-# LANGUAGE FlexibleContexts #-}

module Pred where

import Control.Monad
import Control.Monad.Gensym
import Expr

data BinRel = Eq | LEq | GEq | LTh | GTh
  deriving Show

data Pred = Term BinRel Expr Expr
          | Implies Pred Pred
          | Conj Pred Pred
          | Disj Pred Pred
          | Neg Pred
          | HoleP (Maybe EIdx)
  deriving Show

substP :: [(VName, Expr)] -> Pred -> Pred
substP env (Term rel e1 e2) =
  Term rel (substE env e1) (substE env e2)
substP env (Implies p q) = Implies (substP env p) (substP env q)
substP env (Conj p q) = Conj (substP env p) (substP env q)
substP env (Disj p q) = Disj (substP env p) (substP env q)
substP env (Neg p) = Neg (substP env p)

enumHolesP :: MonadSymGen EIdx m => Pred -> m Pred
enumHolesP (Term rel e1 e2) =
  liftM2 (Term rel) (enumHoles e1) (enumHoles e2)
enumHolesP (Implies p q) =
  liftM2 Implies (enumHolesP p) (enumHolesP q)
enumHolesP (Conj p q) =
  liftM2 Conj (enumHolesP p) (enumHolesP q)
enumHolesP (Disj p q) =
    liftM2 Disj (enumHolesP p) (enumHolesP q)
enumHolesP (Neg p) = Neg <$> enumHolesP p
enumHolesP (HoleP _)  = HoleP . Just <$> gensym

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
