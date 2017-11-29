{-# LANGUAGE FlexibleContexts #-}

module GCL.Expr where

import Data.Char(isAlpha)

import Control.Monad.Gensym

type Idx = Int
type EIdx = Int

-- newtype EIdx = EIdx {unEIdx :: Int}
--    deriving Show

type VName = String

type OpName = String

data Lit = Num Int | Bol Bool
  deriving Show

data Expr = Var VName
          | Lit Lit
          | Op OpName [Expr]
          | HoleE (Maybe EIdx) [Subst]
  deriving Show

type Subst = [(VName, Expr)]

substE :: Subst -> Expr -> Expr
substE env (Var x) =
  case lookup x env of
    Just e -> e
    Nothing -> Var x
substE env (Lit n)     = Lit n
substE env (Op op es)  = Op op (map (substE env) es)
substE env (HoleE idx subs) = HoleE idx (env:subs)

enumHoles :: MonadSymGen EIdx m => Expr -> m Expr
enumHoles (Var x)    = return $ Var x
enumHoles (Lit n)    = return $ Lit n
enumHoles (Op op es) = Op op <$> mapM enumHoles es
enumHoles (HoleE Nothing subs)  =
  do subs' <- mapM enumHolesSub subs
     i <- gensym
     return (HoleE (Just i) subs')
enumHoles (HoleE (Just i) subs)  =
  HoleE (Just i) <$> mapM enumHolesSub subs

enumHolesSub :: MonadSymGen EIdx m => Subst -> m Subst
enumHolesSub [] = return []
enumHolesSub ((v,e):env) =
   do e' <- enumHoles e
      env' <- enumHolesSub env
      return ((v,e'):env')

instance Gensym Int where
  genzero = 0
  nextsym = succ

-- instance Gensym EIdx where
--   genzero = EIdx 0
--   nextsym (EIdx n) = (EIdx (succ n))

showLitS :: Lit -> ShowS
showLitS (Num n) = showsPrec 0 n
showLitS (Bol b) = showsPrec 0 b

showExprS :: Expr -> ShowS
showExprS (Var x) = (x ++)
showExprS (Lit n) = showLitS n
showExprS (Op op [x,y]) | not (isAlpha (head op)) =
    ('(':) . showExprS x . (' ':) . (op ++) . (' ':) . showExprS y . (')':)
showExprS (Op op es) =
  ('(':) . (op ++) . (' ':) . showArgs es . (')':)
showExprS (HoleE Nothing subs) = ("[_]" ++)
showExprS (HoleE (Just i) subs) =
  ('[':) . showsPrec 0 i . (']':)

showArgs :: [Expr] -> ShowS
showArgs [] = id
showArgs [x] = showExprS x
showArgs (x:xs) = showExprS x . (' ':) . showArgs xs
