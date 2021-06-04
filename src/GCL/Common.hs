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
import GCL.Predicate (Pred (..))

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
  fv (Subst e s _) = (fv e \\ (Set.fromList . Map.keys) s) <> fv s

-- class for data that is substitutable
class Substitutable a b where
  apply :: Subs a -> b -> b

compose :: Substitutable a a => Subs a -> Subs a -> Subs a
s1 `compose` s2 = s1 <> Map.map (apply s1) s2

instance Substitutable Type Type where
  apply _ t@(TBase _ _) = t
  apply s (TArray i t l) = TArray i (apply s t) l
  apply s (TFunc t1 t2 l) = TFunc (apply s t1) (apply s t2) l
  apply s t@(TVar x _) = Map.findWithDefault t x s

-- `Subs` is either local bindings or let bindings
-- local bindings need not to be recorded by constructor `Subst`
instance Substitutable (Either Expr Expr) Expr where
  apply s (Paren expr) = Paren (apply s expr)
  apply _ lit@(Lit _ _) = lit
  apply s v@(Var n _) = -- Map.findWithDefault v n s
    case Map.lookup n s of
      Just (Left v') -> apply (Map.delete n s) v'
      Just (Right v') -> do
        let s' = snd $ Map.mapEither id s
        Subst v s' (apply (Map.delete n s) v') 
      Nothing -> v
  apply s c@(Const n _) = -- Map.findWithDefault c n s
    case Map.lookup n s of
      Just (Left c') -> apply (Map.delete n s) c'
      Just (Right c') -> do
        let s' = snd $ Map.mapEither id s
        Subst c s' (apply (Map.delete n s) c')
      Nothing -> c
  apply _ op@(Op _) = op
  apply s (Chain a op b l) = Chain (apply s a) op (apply s b) l
  apply s (App a b l) = 
    let a' = apply s a in
    let b' = apply s b in
    case a' of
      Lam x body _ -> apply (Map.singleton x (Left b' :: Either Expr Expr)) body
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
  apply s1 (Subst before s2 after) = do
    let s1' = snd $ Map.mapEither id s1
    Subst before (s1' `Map.union` s2) (apply s1 after)
    -- Subst before (s `compose` s') after

instance Substitutable (Either Expr Expr) Pred where
  apply s (Constant e) = Constant (apply s e)
  apply s (Bound e l) = Bound (apply s e) l
  apply s (Assertion e l) = Assertion (apply s e) l
  apply s (LoopInvariant e b l) = LoopInvariant (apply s e) b l
  apply s (GuardIf e l) = GuardLoop (apply s e) l
  apply s (GuardLoop e l) = GuardLoop (apply s e) l
  apply s (Conjunct xs) = Conjunct (map (apply s) xs)
  apply s (Disjunct es) = Disjunct (map (apply s) es)
  apply s (Negate x) = Negate (apply s x)

--   apply env e
--       | null env = e
--       | otherwise = Subst e env

-- instance Monad m => HasEnv (Solver m a) a where
--   askEnv = ask