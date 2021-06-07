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
import qualified Syntax.Abstract as A
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

instance Free A.Type where
  fv (A.TBase _ _) = mempty
  fv (A.TArray _ t _) = fv t
  fv (A.TFunc t1 t2 _) = fv t1 <> fv t2
  fv (A.TVar x _) = Set.singleton x

instance Free A.Expr where
  fv (A.Paren expr) = fv expr
  fv (A.Var x _) = Set.singleton x
  fv (A.Const x _) = Set.singleton x
  fv (A.Op _) = mempty
  fv (A.Lit _ _) = mempty
  fv (A.Chain a _ b _) = fv a <> fv b
  fv (A.App e1 e2 _) = fv e1 <> fv e2
  fv (A.Lam x e _) = fv e \\ Set.singleton x
  fv (A.Quant op xs range term _) =
    (either (const mempty) fv op <> fv range <> fv term) \\ Set.fromList xs
  fv (A.Hole _) = mempty -- banacorn: `subs` has been always empty anyway
  -- concat (map freeSubst subs) -- correct?
  fv (A.Subst e s _) = (fv e \\ (Set.fromList . Map.keys) s) <> fv s

-- class for data that is substitutable
class Substitutable a b where
  apply :: Subs a -> b -> b

compose :: Substitutable a a => Subs a -> Subs a -> Subs a
s1 `compose` s2 = s1 <> Map.map (apply s1) s2

instance Substitutable a b => Substitutable a [b] where
  apply = map . apply

instance Substitutable A.Type A.Type where
  apply _ t@(A.TBase _ _) = t
  apply s (A.TArray i t l) = A.TArray i (apply s t) l
  apply s (A.TFunc t1 t2 l) = A.TFunc (apply s t1) (apply s t2) l
  apply s t@(A.TVar x _) = Map.findWithDefault t x s

-- instance Substitutable Expr Expr where
--   apply s (Paren expr) = Paren (apply s expr)
--   apply _ lit@(Lit _ _) = lit
--   apply s v@(Var n _) = Map.findWithDefault v n s
--   apply s c@(Const n _) = Map.findWithDefault c n s
--   apply _ o@(Op _) = o
--   apply s (Chain a op b l) = Chain (apply s a) op (apply s b) l
--   apply s (App a b l) =
--     let a' = apply s a in
--     let b' = apply s b in 
--       case a' of
--         Lam x body _ -> apply (Map.singleton x b') body
--         _ -> App a' b' l
--   apply s (Lam x e l) =
--     let s' = Map.withoutKeys s (Set.singleton x) in
--     Lam x (apply s' e) l
--   apply _ h@(Hole _) = h
--   apply s (Quant qop xs rng t l) = 
--     let op' = case qop of
--                 Left op -> Left op
--                 Right op -> Right (apply s op) in
--     let s' = Map.withoutKeys s (Set.fromList xs) in
--     Quant op' xs (apply s' rng) (apply s' t) l
--   -- after should already be applied to subs 
--   apply s1 (Subst before s2 after) = 
--     Subst before (s1 `compose` s2) (apply s1 after)

-- Left of Bindings will be rendered,   
--    including assignment
-- Right of Bindings will not be rendered, 
--    including global let bindings, and lambda bindings
type Bindings = Either A.Expr A.Expr

bindToSubs :: Subs Bindings -> Subs A.Expr
bindToSubs = fst . Map.mapEither id

singleBinding :: Name -> Bindings -> Subs Bindings
singleBinding = Map.singleton

-- should make sure `fv Bindings` and `fv Expr` are disjoint before substitution
instance Substitutable Bindings A.Expr where
  apply s (A.Paren expr) = A.Paren (apply s expr)
  apply _ lit@(A.Lit _ _) = lit
  apply s v@(A.Var n _) = 
    case Map.lookup n s of
      Just (Left v') -> do
        A.Subst v (Map.singleton n v') (apply (Map.delete n s) v')
      Just (Right v') -> do
        A.Subst v emptySubs (apply (Map.delete n s) v')
      Nothing -> v
  apply s c@(A.Const n _) = 
    case Map.lookup n s of
      Just (Left c') -> do
        A.Subst c (Map.singleton n c') (apply (Map.delete n s) c')
      Just (Right c') -> do
        A.Subst c emptySubs (apply (Map.delete n s) c')
      Nothing -> c
  apply _ op@(A.Op _) = op
  apply s (A.Chain a op b l) = A.Chain (apply s a) op (apply s b) l
  apply s (A.App a b l) = 
    let a' = apply s a in
    let b' = apply s b in
    case a' of
      A.Lam x body _ -> 
        apply (singleBinding x (Right b')) body
      A.Subst _ s1 (A.Lam x body _) -> do
        let body' = apply (singleBinding x (Right b')) body
        let (s', after) = case body' of
                  A.Subst _ s2 bodyAfter -> (s1 `Map.union` s2, bodyAfter)
                  _ -> (s1, body')
        A.Subst (A.App a b l) s' after
      _ -> A.App a' b' l 
  apply s (A.Lam x e l) = 
    let s' = Map.withoutKeys s (Set.singleton x) in
    A.Lam x (apply s' e) l
  apply _ (A.Hole l) = A.Hole l
  apply s (A.Quant qop xs rng t l) = 
    let op' = case qop of
                Left op -> Left op
                Right op -> Right (apply s op) in
    let s' = Map.withoutKeys s (Set.fromList xs) in
    A.Quant op' xs (apply s' rng) (apply s' t) l
  -- after should already be applied to subs 
  apply s1 (A.Subst before s2 after) = do
    let s1' = fst $ Map.mapEither id s1
    A.Subst before (s1' `Map.union` s2) (apply s1 after)


instance Substitutable Bindings Pred where
  apply s (Constant e) = Constant (apply s e)
  apply s (Bound e l) = Bound (apply s e) l
  apply s (Assertion e l) = Assertion (apply s e) l
  apply s (LoopInvariant e b l) = LoopInvariant (apply s e) b l
  apply s (GuardIf e l) = GuardLoop (apply s e) l
  apply s (GuardLoop e l) = GuardLoop (apply s e) l
  apply s (Conjunct xs) = Conjunct (apply s xs)
  apply s (Disjunct es) = Disjunct (apply s es)
  apply s (Negate x) = Negate (apply s x)

instance Substitutable Bindings A.Stmt where
  apply _ st@(A.Skip _) = st
  apply _ st@(A.Abort _) = st
  apply s (A.Assign ns es l) = A.Assign ns (apply s es) l
  apply s (A.Assert e l) = A.Assert (apply s e) l
  apply s (A.LoopInvariant p bnd l) = A.LoopInvariant (apply s p) (apply s bnd) l
  apply s (A.Do gds l) = A.Do (apply s gds) l
  apply s (A.If gds l) = A.If (apply s gds) l
  apply _ st@(A.Spec _ _) = st
  apply _ st@(A.Proof _) = st

instance Substitutable Bindings A.GdCmd where
  apply s (A.GdCmd gd stmts l) = A.GdCmd (apply s gd) (apply s stmts) l

-- rebindLam :: Fresh m => Expr -> m Expr
-- rebindLam (Lam x e l) = do
--   env <- ask
