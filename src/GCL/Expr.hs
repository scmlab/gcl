{-# LANGUAGE OverloadedStrings #-}

module GCL.Expr where

import           Data.Loc
import qualified Data.Map                      as Map
import           Data.Set                       ( Set
                                                  , (\\)
                                                  )
import qualified Data.Set                      as Set
import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy                as Text

import Syntax.Concrete

--------------------------------------------------------------------------------
-- | Names and Fresh Names

type Name = Text

class Monad m => Fresh m where
  fresh :: m Int

  -- generate a fresh var with a given prefix
  freshVar :: Text -> m Name
  freshVar prefix = do
    i <- fresh
    return ("_" <> prefix <> Text.pack (show i))

  -- generate a bunch of fresh vars with a given prefix
  freshVars :: Text -> Int -> m [Name]
  freshVars _  0 = return []
  freshVars pf n = (:) <$> freshVar pf <*> freshVars pf (n-1)

--------------------------------------------------------------------------------
-- | Free Names

free :: Expr -> Set Name
free (Var   x _  ) = Set.singleton (lowerToText x)
free (Const x _  ) = Set.singleton (upperToText x)
free (Op    _ _  ) = mempty
free (Lit   _ _  ) = mempty
free (App e1 e2 _) = free e1 <> free e2
free (Lam x e   _) = free e \\ Set.singleton x
free (Quant op xs range term _) =
  (free op <> free range <> free term) \\ Set.fromList (map lowerToText xs)
free (Hole _     ) = mempty -- banacorn: `subs` has been always empty anyway
    -- concat (map freeSubst subs) -- correct?
-- free (Subst e s  ) = (free e \\ Set.fromList (Map.keys s)) <> freeSubst s

-- free variables in the Subst table
freeSubst :: Subst -> Set Name
freeSubst = Set.unions . map free . Map.elems

--------------------------------------------------------------------------------
-- | Substitution

subst :: Fresh m => Subst -> Expr -> m Expr
subst env v@(Var x _) =
  return $ maybe v id (Map.lookup (lowerToText x) env)
subst env c@(Const x _) =
  return $ maybe c id (Map.lookup (upperToText x) env)
subst _   (Op  op l                ) = return $ Op op l
subst _   (Lit n  l                ) = return $ Lit n l
subst env (App e1 e2 l) = App <$> subst env e1 <*> subst env e2 <*> pure l
subst _   (Hole l                  ) = return $ Hole l
-- subst env (Hole      l) = return $ Hole 0 [env]
-- subst env (Hole idx subs l) = return $ Hole idx (env:subs)
subst env (Lam x e l)
  | x `elem` freeSubst env = do
      x' <- freshVar "dummy"
      e' <- subst (Map.singleton x (Var (Lower x' NoLoc) NoLoc)) e
      Lam x <$> subst env e' <*> pure l
  | otherwise = do
    let env' = Map.filterWithKey (const . (x /=)) env
    Lam x <$> subst env' e <*> pure l
subst env (Quant op xs range term l) = do
  op'                  <- subst env op
  (xs', range', term') <- subLocal xs range term

  let env' = Map.filterWithKey (const . not . (`elem` map lowerToText xs')) env

  Quant op' xs' <$> subst env' range' <*> subst env' term' <*> pure l

 where
  subLocal :: Fresh m => [Lower] -> Expr -> Expr -> m ([Lower], Expr, Expr)
  subLocal [] r t = return ([], r, t)
  subLocal (Lower i ll : is) r t
    | i `elem` freeInEnv = do
        -- if `i` is a free variable of `env`
        -- instantiate a dummy varialbe `j`
      j  <- freshVar "dummy"
      -- substitute the variable `i` in `r` with the dummy variable
      r' <- subst (Map.singleton i (Var (Lower j NoLoc) NoLoc)) r
      -- substitute the variable `i` in `t` with the dummy variable
      t' <- subst (Map.singleton i (Var (Lower j NoLoc) NoLoc)) t
      first3 ((Lower j ll) :) <$> subLocal is r' t'
    | otherwise = first3 (Lower i l :) <$> subLocal is r t

  freeInEnv = freeSubst env
  first3 f (x, y, z) = (f x, y, z)

-- subst env (Subst e theta) = return $ Subst (Subst e theta) env

--------------------------------------------------------------------------------
-- | Expansion


expand :: Fresh m => [(Text, ([Text], Expr))] -> Int -> Expr -> m Expr
expand _    0 e = return e
expand _    _ (Lit v l)             = return $ Lit v l
expand defs n c@(Const (Upper x _) _) =
  case lookup x defs of
    Just (args,body) -> wrapLam args <$> expand defs (n-1) body
    Nothing -> return $ c
expand defs n v@(Var   (Lower x _) _) =
  case lookup x defs of
    Just (args,body) -> wrapLam args <$> expand defs (n-1) body
    Nothing -> return $ v
expand _    _ op@(Op _ _) = return op
expand defs n (App e1 e2 l) = do
  e1' <- expand defs n e1
  e2' <- expand defs n e2
  case e1' of
       Lam x body _ -> subst (Map.singleton x e2') body
       _ -> return $ App e1' e2' l
expand _    _ (Lam x e l) = return $ Lam x e l --- SCM: deal with this later
expand _    _ h@(Hole _) = return h
expand _    _ (Quant op ys r t l) =  --- SCM: deal with this later
  return $ Quant op ys r t l
-- expand _    _ (Subst _ _) = undefined -- SCM: deal with this later

 -- SCM: expand is not correct yet -- it does not handle
 --      variable capture correctly. Deal with this later.

wrapLam :: [Text] -> Expr -> Expr
wrapLam []     body = body
wrapLam (x:xs) body = Lam x (wrapLam xs body) NoLoc
