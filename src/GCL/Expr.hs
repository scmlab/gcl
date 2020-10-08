{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module GCL.Expr where

import           Data.Loc
import qualified Data.Map                      as Map
import           Data.Set                       ( Set
                                                , (\\)
                                                )
import qualified Data.Set                      as Set
import           Data.Text.Lazy                 ( Text )
import           Data.Maybe                     ( fromMaybe )
import           Syntax.Abstract                ( Fresh(..) )
import           Syntax.Concrete
import           Control.Monad.State
import           Control.Monad.Reader

--------------------------------------------------------------------------------
-- | Subst Monad

type SubstM = ReaderT Int (ReaderT Defns (State Int))

runSubstM :: SubstM a -> Defns -> Int -> a
runSubstM m defs n = evalState (runReaderT (runReaderT m n) defs) 0

instance Fresh SubstM where
  fresh = get

class Monad m => DefsM m where
  askDefns :: m Defns

class (Fresh m, DefsM m) => ExpandM m where
  ifExpand :: m a -> m a -> m a
  doExLevel :: Int -> m a -> m a

instance DefsM SubstM where
  askDefns = lift ask

instance ExpandM SubstM where
  ifExpand me mb = do i <- ask
                      if i == 0 then mb
                         else local (\n -> n - 1) me
  doExLevel n = local (const n)

ifInDefns :: DefsM m => Text -> (Expr -> m a) -> m a -> m a
ifInDefns x f y = do
  defs <- askDefns
  case Map.lookup x defs of
    Just e -> f e
    Nothing -> y


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

shrinkSubst :: Set Text -> Subst -> Subst
shrinkSubst fs env =
   Map.filterWithKey (const . (`elem` fs)) env

applySubst :: Expr -> Subst -> Expr
applySubst e env | null env = e
                 | otherwise = Subst e env

--------------------------------------------------------------------------------
-- | Substitution

  -- alpha-convert a lambda expression if the bound
  --   name would be caught in a list of free names

convLam :: ExpandM m => Set Text -> Text -> Expr -> m (Text, Expr, Bool)
convLam xs x e
  | x `elem` xs = do
    x' <- freshVar "dummy"
    e' <- subst (Map.singleton x (Var (Name x' NoLoc) NoLoc)) e
    return (x', e', True)
  | otherwise = return (x, e, False)

subst :: ExpandM m => Subst -> Expr -> m Expr
subst env v@(Var (Name x _) _) = do
  -- e' <- expand v
  -- case e' of
  --   v'@(Var (Name x _) _) ->
  --     ifInDefns x (\e -> return $ applySubst v' (shrinkSubst (free e) env))
  --                 (return $ fromMaybe v' (Map.lookup x env))
  --   _ -> subst env e'
  defs <- askDefns
  case Map.lookup x defs of
    Just e  -> ifExpand (subst env e)
                (return (applySubst v env))
    Nothing -> return $ fromMaybe v (Map.lookup x env)
subst env c@(Const (Name x _)  _) = do
  -- e' <- expand c
  -- case e' of
  --   c'@(Var (Name x _) _) ->
  --     ifInDefns x (\e -> return $ applySubst c' (shrinkSubst (free e) env))
  --                 (return $ fromMaybe c' (Map.lookup x env))
  --   _ -> subst env e'
  defs <- askDefns
  case Map.lookup x defs of
    Just e  -> ifExpand (subst env e)
                (return (applySubst c env))
    Nothing -> return $ fromMaybe c (Map.lookup x env)
subst _   (Op    op l ) = return $ Op op l
subst _   (Lit   n  l ) = return $ Lit n l
subst env (App e1 e2 l) = do
    e1' <- subst env e1
    e2' <- subst env e2
    case e1' of
      Lam x body _ -> subst (Map.singleton x e2') body
      _ -> return $ App e1' e2' l
  -- App <$> subst env e1 <*> subst env e2 <*> pure l
subst _   (Hole l     ) = return $ Hole l
subst env (Lam x e l  ) = do
  (x', e', conv) <- convLam (freeSubst env) x e
  let env' = if not conv then env else Map.filterWithKey (const . (x /=)) env
  Lam x' <$> subst env' e' <*> pure l
subst env (Quant op xs range term l) = do
  -- let env' = shrinkSubst (free (Quant op xs range term l)) env
  op'                  <- subst env op
  (xs', range', term') <- subLocal (freeSubst env) xs range term
  Quant op' xs' <$> subst env range' <*> subst env term' <*> pure l
 where
  subLocal :: ExpandM m => Set Text ->
      [Name] -> Expr -> Expr -> m ([Name], Expr, Expr)
  subLocal _ [] r t = return ([], r, t)
  subLocal freeInEnv' (Name i ll : is) r t
    | i `elem` freeInEnv' = do
        -- if `i` is a free variable of `env'`
        -- instantiate a dummy varialbe `j`
      j  <- freshVar "dummy"
      -- substitute the variable `i` in `r` with the dummy variable
      r' <- subst (Map.singleton i (Var (Name j NoLoc) NoLoc)) r
      -- substitute the variable `i` in `t` with the dummy variable
      t' <- subst (Map.singleton i (Var (Name j NoLoc) NoLoc)) t
      first3 ((Name j ll) :) <$> subLocal freeInEnv' is r' t'
    | otherwise = first3 (Name i ll :) <$> subLocal freeInEnv' is r t

  first3 f (x, y, z) = (f x, y, z)

subst env s@(Subst _ _) =
  return $ applySubst s env -- (shrinkSubst (free s) env)

--------------------------------------------------------------------------------
-- | Expansion

expand :: ExpandM m => Expr -> m Expr
expand (Lit v l) = return $ Lit v l
expand c@(Const (Name x _) _) = do
  ifExpand (ifInDefns x expand (return c))
           (return c)
expand v@(Var (Name x _) _) = do
  ifExpand (ifInDefns x expand (return v))
           (return v)
expand op@(Op _ _) = return op
expand (App e1 e2 l) = do
  e1' <- expand e1
  e2' <- expand e2
  case e1' of
    Lam x body _ -> subst (Map.singleton x e2') body
    _ -> return $ App e1' e2' l
expand (Lam x e l) = return (Lam x e l)
  -- SCM: let's not expand under lambda for now?
  --   was:
  -- (x', e', c) <- convLam (freeDefns defs) x e
  -- let defs' = if not c then defs else Map.filterWithKey (\k _ -> x /= k) defs
  -- e'' <- expand defs' n e'
  -- return $ Lam x' e'' l
expand h@(Hole _) = return h
expand (Quant op xs range term l) =  do
  range' <- expand range
  term' <- expand term
  return $ Quant op xs range' term' l
expand (Subst e env) = do
    e' <- expand e
    subst env e'

{-
-- Extend a `Subst` with a `Defns`
extendSubstWithDefns :: Subst -> Defns -> Subst
extendSubstWithDefns env defns = env <> Map.mapMaybeWithKey substDefn defns

 where
  substDefn :: Text -> Expr -> Maybe Expr
  substDefn name expr = if null env'
    then Nothing
    else Just (Subst (Var (Name name NoLoc) NoLoc) env')
    where
      -- entries of `env` that are free in `expr`
          env' = Map.filterWithKey (\x _ -> x `elem` free expr) env
-}
