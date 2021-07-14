{-# LANGUAGE FlexibleInstances #-}
module GCL.Expr where

import GCL.Common ( Env, FreshState, Substitutable (subst) )
import Syntax.Abstract ( Expr (..), Bindings(BetaBinding) )
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.State (State)
import qualified Data.Map as Map

type Expand = ReaderT (Env Expr) (State FreshState)

-- replace "abstract names" with concrete definitions
expand :: Expr -> Expand Expr
expand (Paren expr l) = Paren <$> expand expr <*> pure l
expand lit@(Lit _ _) = return lit
expand c@(Const n _) = do
  env <- ask
  case Map.lookup n env of
    Just expr -> expand expr
    Nothing -> return c
expand v@(Var n _) = do
  env <- ask
  case Map.lookup n env of
    Just expr -> expand expr
    Nothing -> return v
expand op@(Op _) = return op
expand (Chain a op b l) = do
  a' <- expand a
  b' <- expand b
  return (Chain a' op b' l)
expand (App a b l) = do
  a' <- expand a
  b' <- expand b
  case a' of
    Lam x body _ -> return $ subst (Map.singleton x (BetaBinding b')) body
    _ -> return $ App a' b' l
expand (Lam x e l) = return (Lam x e l)
expand (Quant op xs rng t l) = do
  rng' <- expand rng
  t' <- expand t
  return $ Quant op xs rng' t' l
expand (Subst e s _) = do
  e' <- expand e
  return $ subst s e'
expand (Subst2 a b) = return $ Subst2 a b
expand (Expand a b) = return $ Expand a b 
expand (ArrIdx e1 e2 l) = do
  e1' <- expand e1
  e2' <- expand e2
  return (ArrIdx e1' e2' l)
expand (ArrUpd e1 e2 e3 l) = do
  e1' <- expand e1
  e2' <- expand e2
  e3' <- expand e3
  return (ArrUpd e1' e2' e3' l)
