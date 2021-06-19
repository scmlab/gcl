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
import Control.Monad.RWS (RWST(..))
import Control.Monad.State (StateT(..))
import Data.Loc (Loc)

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
  fv (A.Subst _ _ after) = fv after

instance Free A.Bindings where
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
    let op' = case qop of
                Left op -> Left op
                Right op -> Right (subst s op) in
    let s' = Map.withoutKeys s (Set.fromList xs) in
    A.Quant op' xs (subst s' rng) (subst s' t) l
  -- after should already be applied to subs 
  subst s1 (A.Subst before s2 after) = A.Subst (subst s1 before) s2 (subst s1 after)

-- Left of Bindings will be rendered,   
--    including assignment
-- Right of Bindings will not be rendered, 
--    including global let bindings, and lambda bindings

bindToSubs :: Subs A.Bindings -> Subs A.Expr
bindToSubs = fst . Map.mapEither id

singleBinding :: Name -> A.Bindings -> Subs A.Bindings
singleBinding = Map.singleton

shrinkSubs :: A.Expr -> Subs a -> Subs a
shrinkSubs expr = Map.filterWithKey (\n _ -> n `Set.member` fv expr)

-- should make sure `fv Bindings` and `fv Expr` are disjoint before substitution
instance Substitutable A.Bindings A.Expr where
  subst sub expr =
    let s = shrinkSubs expr sub in
    if null s
    then expr
    else
    case expr of
      (A.Paren e) -> A.Paren (subst s e)
      A.Lit {} -> expr
      (A.Var n _) ->
        case Map.lookup n s of
          Just v ->
            either (simpleSubs expr s n) (simpleSubs expr s n) v
          Nothing -> expr
      (A.Const n _) ->
        case Map.lookup n s of
          Just c ->
            either (simpleSubs expr s n) (simpleSubs expr s n) c
          Nothing -> expr
      A.Op {} -> expr
      (A.Chain a op b l) -> A.Chain (subst s a) op (subst s b) l
      (A.App a b l) ->
        let (a', b'a, expr') = substApp s a b l in
        case a' of
          A.Lam x body _ ->
            A.Subst expr' emptySubs (subst (Map.singleton x b'a) body)
          A.Subst _ _ (A.Lam x body _) ->
            A.Subst expr' emptySubs (subst (Map.singleton x b'a) body)
          _ -> expr'
      (A.Lam x e l) ->
        let s' = Map.withoutKeys s (Set.singleton x) in
        let e' = subst s' e in
        quotSubs expr s (A.Lam x e' l)
      A.Hole {} -> expr
      (A.Quant qop xs rng t l) ->
        let op' = case qop of
                    Left op -> Left op
                    Right op -> Right (subst s op) in
        let s' = Map.withoutKeys s (Set.fromList xs) in
        A.Quant op' xs (subst s' rng) (subst s' t) l
  -- after should already be applied to subs 
      A.Subst {} -> substSubst s expr


-- e1 [s] -> e2
-- e1 [s] -> e2 [s//(n, e2)]
simpleSubs :: A.Expr -> Subs A.Bindings -> Name -> A.Expr -> A.Expr
simpleSubs e1 s n e2 =
  A.Subst e1 s
    (quotSubs e2 (Map.delete n s) (subst (Map.delete n s) e2))

quotSubs :: A.Expr -> Subs A.Bindings -> A.Expr -> A.Expr
quotSubs before s after
  | before == after = before
  | otherwise = A.Subst before s after

getSubstAfter :: A.Expr -> A.Expr
getSubstAfter (A.Subst _ _ after) = after
getSubstAfter expr = expr

substApp :: Subs A.Bindings -> A.Expr -> A.Expr -> Loc -> (A.Expr, A.Expr, A.Expr)
substApp s a b l =
  let a' = subst s a in
  let b' = subst s b in
  let b'a = getSubstAfter b' in
  if a == a' && b == b'
  then (a', b'a, A.App a b l)
  else (a', b'a, A.Subst (A.App a b l) s (A.App a' b'a l))

isAllApp :: A.Expr -> Bool
isAllApp (A.Subst A.App {} _ A.App {}) = True
isAllApp (A.Subst b@A.Subst {} _ A.App {}) = isAllApp b
isAllApp _ = False

substSubst :: Subs A.Bindings -> A.Expr -> A.Expr
substSubst s (A.Subst b@(A.Subst _ _ b2@(A.App b2a b2b l)) s2 a)
  | isAllApp b && s2 == emptySubs =
    let (_, _, b2') = substApp s b2a b2b l in
    let b3 = getSubstAfter b2' in
    let a' = getSubstAfter (subst s a) in
    if b2 == b3
    then A.Subst b emptySubs a'
    else A.Subst (A.Subst b s b3) emptySubs a'
substSubst s (A.Subst b s1 a) =
  let a' = getSubstAfter (subst s a) in
  A.Subst (A.Subst b s1 a) s a'
substSubst s expr = subst s expr

instance Substitutable A.Bindings Pred where
  subst s (Constant e) = Constant (subst s e)
  subst s (Bound e l) = Bound (subst s e) l
  subst s (Assertion e l) = Assertion (subst s e) l
  subst s (LoopInvariant e b l) = LoopInvariant (subst s e) b l
  subst s (GuardIf e l) = GuardLoop (subst s e) l
  subst s (GuardLoop e l) = GuardLoop (subst s e) l
  subst s (Conjunct xs) = Conjunct (subst s xs)
  subst s (Disjunct es) = Disjunct (subst s es)
  subst s (Negate x) = Negate (subst s x)

instance Substitutable A.Bindings A.Stmt where
  subst _ st@(A.Skip _) = st
  subst _ st@(A.Abort _) = st
  subst s (A.Assign ns es l) = A.Assign ns (subst s es) l
  subst s (A.Assert e l) = A.Assert (subst s e) l
  subst s (A.LoopInvariant p bnd l) = A.LoopInvariant (subst s p) (subst s bnd) l
  subst s (A.Do gds l) = A.Do (subst s gds) l
  subst s (A.If gds l) = A.If (subst s gds) l
  subst _ st@(A.Spec _ _) = st
  subst _ st@(A.Proof _) = st

instance Substitutable A.Bindings A.GdCmd where
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