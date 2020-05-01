{-# LANGUAGE OverloadedStrings #-}

module GCL.Expr where

import           Data.Loc
import           Data.Map                       ( Map )
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

-- Names are "read-only", they can be variables or constants
type Name  = Either Lower Upper
type Subst = Map Name Expr

-- Vars can be substituted. They can only be variables
type Var = Lower

class Monad m => Fresh m where
  fresh :: m Int

  -- generate a fresh var with a given prefix
  freshVar :: Text -> m Var
  freshVar prefix = do
    i <- fresh
    let name = "_" <> prefix <> Text.pack (show i)
    return $ Lower name NoLoc

  -- generate a bunch of fresh vars with a given prefix
  freshVars :: Text -> Int -> m [Var]
  freshVars _  0 = return []
  freshVars pf n = (:) <$> freshVar pf <*> freshVars pf (n-1)

--------------------------------------------------------------------------------
-- | Free Names

free :: Expr -> Set Name
free (Var   x _  ) = Set.singleton (Left x)
free (Const x _  ) = Set.singleton (Right x)
free (Op    _ _  ) = mempty
free (Lit   _ _  ) = mempty
free (App e1 e2 _) = free e1 <> free e2
free (Lam x e   _) = free e \\ Set.singleton (Left (Lower x NoLoc))
free (Quant op xs range term _) =
  (free op <> free range <> free term) \\ Set.fromList (map Left xs)
free (Hole _) = mempty -- banacorn: `subs` has been always empty anyway
    -- concat (map freeSubst subs) -- correct?
    -- SCM: is the equality check still correct with locations around?

-- free variables in the Subst table
freeSubst :: Subst -> Set Name
freeSubst = Set.unions . map free . Map.elems

--------------------------------------------------------------------------------
-- | Substitution

subst :: Fresh m => Subst -> Expr -> m Expr
subst env (Var x l) = return $ maybe (Var x l) id (Map.lookup (Left x) env)
subst env (Const x l) =
  return $ maybe (Const x l) id (Map.lookup (Right x) env)
subst _   (Op  op l                ) = return $ Op op l
subst _   (Lit n  l                ) = return $ Lit n l
subst env (App e1 e2 l) = App <$> subst env e1 <*> subst env e2 <*> pure l
subst _   (Hole l                  ) = return $ Hole l
-- subst env (Hole      l) = return $ Hole 0 [env]
-- subst env (Hole idx subs l) = return $ Hole idx (env:subs)
subst env (Lam x e l)
  | Left (Lower x NoLoc) `elem` freeSubst env = do -- SCM: looks very wrong to me!
      x' <- freshVar "dummy"
      e' <- subst (Map.singleton (Left (Lower x NoLoc)) (Var x' NoLoc)) e
      Lam x <$> subst env e' <*> pure l
  | otherwise = do
    let env' = Map.filterWithKey
          (\k _ -> case k of
            Left  (Lower k' _) -> not (k' == x)
            Right _  -> False )
          env
    Lam x <$> subst env' e <*> pure l
subst env (Quant op xs range term l) = do
  op'                  <- subst env op
  (xs', range', term') <- subLocal xs range term

  let env' = Map.filterWithKey
        (\k _ -> case k of
          Left  k' -> not (k' `elem` xs')
          Right _  -> False
        )
        env

  Quant op' xs' <$> subst env' range' <*> subst env' term' <*> pure l

 where
  subLocal :: Fresh m => [Var] -> Expr -> Expr -> m ([Var], Expr, Expr)
  subLocal [] r t = return ([], r, t)
  subLocal (i : is) r t
    | Left i `elem` freeInEnv = do
        -- if `i` is a free variable of `env`
        -- instantiate a dummy varialbe `j`
      j  <- freshVar "dummy"
      -- substitute the variable `i` in `r` with the dummy variable
      r' <- subst (Map.singleton (Left i) (Var j NoLoc)) r
      -- substitute the variable `i` in `t` with the dummy variable
      t' <- subst (Map.singleton (Left i) (Var j NoLoc)) t
      first3 (j :) <$> subLocal is r' t'
    | otherwise = first3 (i :) <$> subLocal is r t

  freeInEnv = freeSubst env
  first3 f (x, y, z) = (f x, y, z)

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
       Lam x body _ -> subst (Map.singleton (Left (Lower x NoLoc)) e2') body
       _ -> return $ App e1' e2' l
expand _    _ (Lam x e l) = return $ Lam x e l --- SCM: deal with this later
expand _    _ h@(Hole _) = return h
expand _    _ (Quant op ys r t l) =  --- SCM: deal with this later
  return $ Quant op ys r t l

 -- SCM: expand is not correct yet -- it does not handle
 --      variable capture correctly. Deal with this later.

wrapLam :: [Text] -> Expr -> Expr
wrapLam []     body = body
wrapLam (x:xs) body = Lam x (wrapLam xs body) NoLoc

{-
data Expr = Lit   Lit       Loc
          | Var   Lower     Loc
          | Const Upper     Loc
          | Op    Op        Loc
          | App   Expr Expr Loc
          | Hole            Loc
          | Quant Expr [Lower] Expr Expr Loc
          deriving (Eq, Show, Generic)

-}
