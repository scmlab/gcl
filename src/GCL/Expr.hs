{-# LANGUAGE FlexibleInstances #-}
module GCL.Expr where

import GCL.Common ( Env, FreshState, Substitutable (subst), Subs )
import Syntax.Abstract ( Expr (..) )
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.State (State)
import qualified Data.Map as Map

type Expand = ReaderT (Env Expr) (State FreshState)

expand :: Expr -> Expand Expr
expand (Paren expr) = Paren <$> expand expr
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
    Lam x body _ -> return $ subst (Map.singleton x (Left b' :: Either Expr Expr)) body
    _ -> return $ App a' b' l
expand (Lam x e l) = return (Lam x e l)
expand h@(Hole _) = return h
expand (Quant op xs rng t l) = do
  rng' <- expand rng
  t' <- expand t 
  return $ Quant op xs rng' t' l
expand (Subst e s _) = do
  e' <- expand e
  return $ subst s e'
