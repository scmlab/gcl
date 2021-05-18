{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module GCL.Common where

import Data.Text(Text)
import qualified Data.Text as Text
import Control.Monad (liftM2)
import Data.Map (Map)
import Syntax.Common (Name)
import Data.Set (Set, (\\))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Syntax.Abstract (Type(..), Expr(..))
import Control.Monad.RWS (RWST, MonadState (get), MonadReader (ask, local), evalRWST, MonadWriter (tell))
import GCL.Predicate (Pred (..))
import GCL.Predicate.Util (toExpr)

-- Monad for generating fresh variable
class Monad m => Fresh m where
  fresh :: m Int
  freshText :: m Text
  freshTexts :: Int -> m [Text]

  freshText =
    (\i -> Text.pack ("?m_" ++ show i)) <$> fresh

  freshTexts 0 = return []
  freshTexts n = liftM2 (:) freshText (freshTexts (n - 1))

type FreshState = Int

initFreshState :: FreshState
initFreshState = 0

type Subs a = Map Name a
type Env a = Map Name a

emptySubs :: Subs a
emptySubs = mempty

emptyEnv :: Env a
emptyEnv = mempty

-- Not sure if neccessary
-- class (Monad m) => HasEnv m a where
--   askEnv :: m (Env a)

extend :: Env a -> (Name, a) -> Env a
extend env (n, x) = Map.insert n x env


-- Monad for free variable
class Free a where
  fv :: a -> Set Name

occurs :: Free a => Name -> a -> Bool
occurs n x = n `Set.member` fv x

instance Free a => Free (Map Name a) where
  fv = Set.unions . Map.map fv

instance Free Type where
  fv (TBase _ _) = mempty
  fv (TArray _ t _) = fv t
  fv (TFunc t1 t2 _) = fv t1 <> fv t2
  fv (TVar x _) = Set.singleton x

instance Free Expr where
  fv (Paren expr) = fv expr
  fv (Var x _) = Set.singleton x
  fv (Const x _) = Set.singleton x
  fv (Op _) = mempty
  fv (Lit _ _) = mempty
  fv (Chain a _ b _) = fv a <> fv b
  fv (App e1 e2 _) = fv e1 <> fv e2
  fv (Lam x e _) = fv e \\ Set.singleton x
  fv (Quant op xs range term _) =
    (either (const mempty) fv op <> fv range <> fv term) \\ Set.fromList xs
  fv (Hole _) = mempty -- banacorn: `subs` has been always empty anyway
  -- concat (map freeSubst subs) -- correct?
  fv (Subst e s) = (fv e \\ (Set.fromList . Map.keys) s) <> fv s

-- class for data that is substitutable
class Substitutable a where
  apply :: Subs a -> a -> a

compose :: Substitutable a => Subs a -> Subs a -> Subs a
s1 `compose` s2 = s1 <> Map.map (apply s1) s2

instance Substitutable Type where
  apply _ t@(TBase _ _) = t
  apply s (TArray i t l) = TArray i (apply s t) l
  apply s (TFunc t1 t2 l) = TFunc (apply s t1) (apply s t2) l
  apply s t@(TVar x _) = Map.findWithDefault t x s

instance Substitutable Expr where
  apply s (Paren expr) = Paren (apply s expr)
  apply _ lit@(Lit _ _) = lit
  apply s v@(Var n _) = Map.findWithDefault v n s
  apply s c@(Const n _) = Map.findWithDefault c n s
  apply _ op@(Op _) = op
  apply s (Chain a op b l) = Chain (apply s a) op (apply s b) l
  apply s (App a b l) = 
    let a' = apply s a in
    let b' = apply s b in
    case a' of
      Lam x body _ -> apply (Map.singleton x b') body
      _ -> App a' b' l 
  apply s (Lam x e l) = let s' = Map.filterWithKey (\n _ -> n /= x) s in
      Lam x (apply s' e) l
  apply _ (Hole l) = Hole l
  apply s (Quant qop xs rng t l) = 
    let op' = case qop of
                Left op -> Left op
                Right op -> Right (apply s op) in
    let s' = Map.withoutKeys s (Set.fromList xs) in
    Quant op' xs (apply s' rng) (apply s' t) l
  apply s (Subst expr s') = Subst expr (s `compose` s')

instance Substitutable Pred where
  apply s (Constant e) = Constant (apply (Map.map toExpr s) e)
  apply s (Bound e l) = Bound (apply (Map.map toExpr s) e) l
  apply s (Assertion e l) = Assertion (apply (Map.map toExpr s) e) l
  apply s (LoopInvariant e b l) = LoopInvariant (apply (Map.map toExpr s) e) b l
  apply s (GuardIf e l) = GuardLoop (apply (Map.map toExpr s) e) l
  apply s (GuardLoop e l) = GuardLoop (apply (Map.map toExpr s) e) l
  apply s (Conjunct xs) = Conjunct (map (apply s) xs)
  apply s (Disjunct es) = Disjunct (map (apply s) es)
  apply s (Negate x) = Negate (apply s x)

--   apply env e
--       | null env = e
--       | otherwise = Subst e env

type Solver m a = RWST (Env a) [(a, a)] FreshState m

instance Monad m => Fresh (Solver m a) where
  fresh = get

-- instance Monad m => HasEnv (Solver m a) a where
--   askEnv = ask

runSolver :: Monad m => Env a -> Solver m a a -> m (a, [(a, a)])
runSolver env s = evalRWST s env initFreshState

uni :: Monad m => a -> a -> Solver m a ()
uni x y = tell [(x, y)]

lookupEnv :: Monad m =>
  Name ->
  Solver m a a ->
  (a -> Solver m a a) ->
  Solver m a a
lookupEnv n f g = do
  env <- ask
  maybe f g (Map.lookup n env)

inEnv :: Monad m => [(Name, a)] -> Solver m a a -> Solver m a a
inEnv l m = do
  let scope e = foldl (\e' (x, sc) -> Map.insert x sc e') e l
  local scope m