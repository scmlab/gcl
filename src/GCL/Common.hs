{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module GCL.Common where

import Data.Text(Text)
import qualified Data.Text as Text
import Data.Loc (Loc (..))
import Control.Monad (liftM2)
import Data.Map (Map)
import Syntax.Common (Name(..))
import Data.Set (Set, (\\))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Syntax.Abstract as A
import GCL.Predicate (Pred (..))
import Control.Monad.RWS (RWST(..))
import Control.Monad.State (StateT(..))

-- Monad for generating fresh variable
class Monad m => Fresh m where
  fresh :: m Int
  freshText :: m Text
  freshTexts :: Int -> m [Text]
  freshName :: m Name

  freshText =
    (\i -> Text.pack ("?m_" ++ show i)) <$> fresh

  freshTexts 0 = return []
  freshTexts n = liftM2 (:) freshText (freshTexts (n - 1))

  freshName = (\v -> Name v NoLoc) <$> freshText

type FreshState = Int

initFreshState :: FreshState
initFreshState = 0

type Subs a = Map Name a
type Env a = Map Name a

emptySubs :: Subs a
emptySubs = mempty

emptyEnv :: Env a
emptyEnv = mempty

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
    (fv op <> fv range <> fv term) \\ Set.fromList xs
  fv (A.Hole _) = mempty -- banacorn: `subs` has been always empty anyway
  -- concat (map freeSubst subs) -- correct?
  fv (A.Subst e s _) = (fv e \\ (Set.fromList . Map.keys) s) <> fv s
  fv (A.ArrIdx e1 e2 _) = fv e1 <> fv e2
  fv (A.ArrUpd e1 e2 e3 _) = fv e1 <> fv e2 <> fv e3

instance Free Bindings where
  fv (Left expr) = fv expr
  fv (Right expr) = fv expr

-- class for data that is substitutable
class Substitutable a b where
  subst :: Subs a -> b -> b

compose :: Substitutable a a => Subs a -> Subs a -> Subs a
s1 `compose` s2 = s1 <> Map.map (subst s1) s2

instance Substitutable a b => Substitutable a [b] where
  subst = map . subst

instance Substitutable A.Type A.Type where
  subst _ t@(A.TBase _ _) = t
  subst s (A.TArray i t l) = A.TArray i (subst s t) l
  subst s (A.TFunc t1 t2 l) = A.TFunc (subst s t1) (subst s t2) l
  subst s t@(A.TVar x _) = Map.findWithDefault t x s

instance Substitutable A.Expr A.Expr where
  subst s (A.Paren expr) = A.Paren (subst s expr)
  subst _ lit@(A.Lit _ _) = lit
  subst s v@(A.Var n _) = Map.findWithDefault v n s
  subst s c@(A.Const n _) = Map.findWithDefault c n s
  subst _ o@(A.Op _) = o
  subst s (A.Chain a op b l) = A.Chain (subst s a) op (subst s b) l
  subst s (A.App a b l) =
    let a' = subst s a in
    let b' = subst s b in
      case a' of
        A.Lam x body _ -> subst (Map.singleton x b') body
        _ -> A.App a' b' l
  subst s (A.Lam x e l) =
    let s' = Map.withoutKeys s (Set.singleton x) in
    A.Lam x (subst s' e) l
  subst _ h@(A.Hole _) = h
  subst s (A.Quant qop xs rng t l) =
    let s' = Map.withoutKeys s (Set.fromList xs) in
    A.Quant (subst s' qop) xs (subst s' rng) (subst s' t) l
  -- after should already be applied to subs
  subst s1 (A.Subst before s2 after) =
    -- NOTE: use `Map.union` or `compose` ?
    A.Subst before (s1 `Map.union` s2) (subst s1 after)
  subst s (A.ArrIdx e1 e2 l) =
    A.ArrIdx (subst s e1) (subst s e2) l
  subst s (A.ArrUpd e1 e2 e3 l) =
    A.ArrUpd (subst s e1) (subst s e2) (subst s e3) l

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
  subst s (A.Paren expr) = A.Paren (subst s expr)
  subst _ lit@(A.Lit _ _) = lit
  subst s v@(A.Var n _) =
    case Map.lookup n s of
      Just (Left v') -> do
        subst (Map.delete n s) v'
      Just (Right v') -> do
        A.Subst v emptySubs (subst (Map.delete n s) v')
      Nothing -> v
  subst s c@(A.Const n _) =
    case Map.lookup n s of
      Just (Left c') -> do
        subst (Map.delete n s) c'
      Just (Right c') ->
        A.Subst c emptySubs (subst (Map.delete n s) c')
      Nothing -> c
  subst _ op@(A.Op _) = op
  subst s (A.Chain a op b l) = A.Chain (subst s a) op (subst s b) l
  subst s (A.App a b l) =
    let a' = subst s a in
    let b' = subst s b in
    case a' of
      A.Lam x body _ ->
        subst (Map.singleton x b') body
      A.Subst _ s1 (A.Lam x body _) -> do
        let body' = subst (Map.singleton x b') body
        A.Subst (A.App a b l) s1 body'
      _ -> A.App a' b' l
  subst s (A.Lam x e l) =
    let s' = Map.withoutKeys s (Set.singleton x) in
    A.Lam x (subst s' e) l
  subst _ (A.Hole l) = A.Hole l
  subst s (A.Quant qop xs rng t l) =
    let s' = Map.withoutKeys s (Set.fromList xs) in
    A.Quant (subst s' qop) xs (subst s' rng) (subst s' t) l
  -- after should already be applied to subs
  subst s1 (A.Subst before s2 after) = do
    let s1' = fst $ Map.mapEither id s1
    A.Subst before (s1' `Map.union` s2) (subst s1 after)
  subst s (A.ArrIdx e1 e2 l) =
      A.ArrIdx (subst s e1) (subst s e2) l
  subst s (A.ArrUpd e1 e2 e3 l) =
    A.ArrUpd (subst s e1) (subst s e2) (subst s e3) l

afterMost :: A.Expr -> A.Expr
afterMost (A.Subst _ _ after) = afterMost after
afterMost expr = expr

chainAfter :: A.Expr -> Subs A.Expr -> A.Expr -> A.Expr
chainAfter subE@(A.Subst _ _ after) s2 after' =
  A.Subst subE s2 (chainAfter after s2 after')
chainAfter before s after = A.Subst before s after

instance Substitutable Bindings Pred where
  subst s (Constant e) = Constant (subst s e)
  subst s (Bound e l) = Bound (subst s e) l
  subst s (Assertion e l) = Assertion (subst s e) l
  subst s (LoopInvariant e b l) = LoopInvariant (subst s e) b l
  subst s (GuardIf e l) = GuardLoop (subst s e) l
  subst s (GuardLoop e l) = GuardLoop (subst s e) l
  subst s (Conjunct xs) = Conjunct (subst s xs)
  subst s (Disjunct es) = Disjunct (subst s es)
  subst s (Negate x) = Negate (subst s x)

-- SCM: I don't think this is well-defined and I wonder whether
--      we ever need this. Applying a substitution to a statement
--      is not even syntatically correct in general.
instance Substitutable Bindings A.Stmt where
  subst _ st@(A.Skip _) = st
  subst _ st@(A.Abort _) = st
  subst s (A.Assign ns es l) = A.Assign ns (subst s es) l
  subst s (A.AAssign a i e l) = A.AAssign a (subst s i) (subst s e) l
  subst s (A.Assert e l) = A.Assert (subst s e) l
  subst s (A.LoopInvariant p bnd l) = A.LoopInvariant (subst s p) (subst s bnd) l
  subst s (A.Do gds l) = A.Do (subst s gds) l
  subst s (A.If gds l) = A.If (subst s gds) l
  subst _ st@(A.Spec _ _) = st
  subst _ st@(A.Proof _) = st
  subst s (A.Alloc x es l) = A.Alloc x (map (subst s) es) l
  subst s (A.HLookup x e l) = A.HLookup x (subst s e) l
  subst s (A.HMutate e1 e2 l) = A.HMutate (subst s e1) (subst s e2) l
  subst s (A.Dispose e l) = A.Dispose (subst s e) l

instance Substitutable Bindings A.GdCmd where
  subst s (A.GdCmd gd stmts l) = A.GdCmd (subst s gd) (subst s stmts) l


toStateT :: Monad m => r -> RWST r w s m a -> StateT s m a
toStateT r m = StateT (\s -> do
    (a, s', _) <- runRWST m r s
    return (a, s')
  )

toEvalStateT :: Monad m => r -> RWST r w s m a -> StateT s m (a, w)
toEvalStateT r m = StateT (\s -> do
    (a, s', w) <- runRWST m r s
    return ((a, w), s')
  )
