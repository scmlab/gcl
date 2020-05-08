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
import           Data.Maybe                     ( fromMaybe )
import           Syntax.Concrete

--------------------------------------------------------------------------------
-- | Names and Fresh Names

class Monad m => Fresh m where
  fresh :: m Int

  -- generate a fresh var with a given prefix
  freshVar :: Text -> m Text
  freshVar prefix = do
    i <- fresh
    return ("_" <> prefix <> Text.pack (show i))

  -- generate a bunch of fresh vars with a given prefix
  freshVars :: Text -> Int -> m [Text]
  freshVars _  0 = return []
  freshVars pf n = (:) <$> freshVar pf <*> freshVars pf (n-1)

--------------------------------------------------------------------------------
-- | Free Names

free :: Expr -> Set Text
free (Var   x _  ) = Set.singleton (nameToText x)
free (Const x _  ) = Set.singleton (nameToText x)
free (Op    _ _  ) = mempty
free (Lit   _ _  ) = mempty
free (App e1 e2 _) = free e1 <> free e2
free (Lam x  e  _) = free e \\ Set.singleton x
free (Quant op xs range term _) =
  (free op <> free range <> free term) \\ Set.fromList (map nameToText xs)
free (Hole _   ) = mempty -- banacorn: `subs` has been always empty anyway
    -- concat (map freeSubst subs) -- correct?
free (Subst e s) = (free e \\ Set.fromList (Map.keys s)) <> freeSubst s

-- free variables in substitutions and list of definitions

freeSubst :: Subst -> Set Text
freeSubst = Set.unions . Map.map free

freeDefns :: Defns -> Set Text
freeDefns = Set.unions . Map.map free

--------------------------------------------------------------------------------
-- | Substitution

  -- alpha-convert a lambda expression if the bound
  --   name would be caught in a list of free names

convLam :: Fresh m => Set Text -> Text -> Expr -> m (Text, Expr, Bool)
convLam xs x e
  | x `elem` xs = do
    x' <- freshVar "dummy"
    e' <- subst (Map.singleton x (Var (Name x' NoLoc) NoLoc)) e
    return (x', e', True)
  | otherwise = return (x, e, False)

subst :: Fresh m => Subst -> Expr -> m Expr
subst env v@(Var   x  _ ) = return $ fromMaybe v (Map.lookup (nameToText x) env)
subst env c@(Const x  _ ) = return $ fromMaybe c (Map.lookup (nameToText x) env)
subst _   (  Op    op l ) = return $ Op op l
subst _   (  Lit   n  l ) = return $ Lit n l
subst env (  App e1 e2 l) = App <$> subst env e1 <*> subst env e2 <*> pure l
subst _   (  Hole l     ) = return $ Hole l
subst env (  Lam x e l  ) = do
  (x', e', conv) <- convLam (freeSubst env) x e
  let env' = if not conv then env else Map.filterWithKey (const . (x /=)) env
  Lam x' <$> subst env' e' <*> pure l
subst env (Quant op xs range term l) = do
  op'                  <- subst env op
  (xs', range', term') <- subLocal xs range term
  let env' = Map.filterWithKey (const . not . (`elem` map nameToText xs')) env
  Quant op' xs' <$> subst env' range' <*> subst env' term' <*> pure l
 where
  subLocal :: Fresh m => [Name] -> Expr -> Expr -> m ([Name], Expr, Expr)
  subLocal [] r t = return ([], r, t)
  subLocal (Name i ll : is) r t
    | i `elem` freeInEnv = do
        -- if `i` is a free variable of `env`
        -- instantiate a dummy varialbe `j`
      j  <- freshVar "dummy"
      -- substitute the variable `i` in `r` with the dummy variable
      r' <- subst (Map.singleton i (Var (Name j NoLoc) NoLoc)) r
      -- substitute the variable `i` in `t` with the dummy variable
      t' <- subst (Map.singleton i (Var (Name j NoLoc) NoLoc)) t
      first3 ((Name j ll) :) <$> subLocal is r' t'
    | otherwise = first3 (Name i l :) <$> subLocal is r t

  freeInEnv = freeSubst env
  first3 f (x, y, z) = (f x, y, z)

subst env (Subst e theta) = return $ Subst (Subst e theta) env

--------------------------------------------------------------------------------
-- | Expansion

expand :: Fresh m => Defns -> Int -> Expr -> m Expr
expand _    0 e                      = return e
expand _    _ (  Lit   v          l) = return $ Lit v l
expand defs n c@(Const (Name x _) _) = case Map.lookup x defs of
  Just e  -> expand defs (n - 1) e
  Nothing -> return $ c
expand defs n v@(Var (Name x _) _) = case Map.lookup x defs of
  Just e  -> expand defs (n - 1) e
  Nothing -> return $ v
expand _    _ op@(Op _ _     ) = return op
expand defs n (   App e1 e2 l) = do
  e1' <- expand defs n e1
  e2' <- expand defs n e2
  case e1' of
    Lam x body _ ->
      subst (extendSubstWithDefns (Map.singleton x e2') defs) body
    _ -> return $ App e1' e2' l
expand defs n (Lam x e l) = do
  (x', e', c) <- convLam (freeDefns defs) x e
  let defs' = if not c then defs else Map.filterWithKey (\k _ -> x /= k) defs
  e'' <- expand defs' n e'
  return $ Lam x' e'' l
expand _ _ h@(Hole _) = return h
expand _ _ (Quant op ys r t l) =  --- SCM: deal with this later
  return $ Quant op ys r t l
expand defs _ (Subst e env) = return $ Subst (Subst e env) defs
     -- SCM: deal with this later

-- Extend a `Subst` with a `Defns` 
extendSubstWithDefns :: Subst -> Defns -> Subst
extendSubstWithDefns env defns = env <> Map.mapMaybeWithKey (substDefn) defns

 where
  substDefn :: Text -> Expr -> Maybe Expr
  substDefn name expr = if null env'
    then Nothing
    else Just (Subst (Var (Name name NoLoc) NoLoc) env')
    where
      -- entries of `env` that are free in `expr`
          env' = Map.filterWithKey (\x _ -> x `elem` free expr) env
