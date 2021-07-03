{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module GCL.Common where

import Data.Text(Text)
import qualified Data.Text as Text
import Data.Loc ( Loc(..), Loc, Located(..) )
import Control.Monad (liftM2)
import Data.Map (Map)
import Syntax.Common (Name(..), nameToText)
import Data.Set (Set, (\\))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Syntax.Abstract as A
import qualified Syntax.Abstract.Util as A
import GCL.Predicate (Pred (..))
import Control.Monad.RWS (RWST(..))
import Control.Monad.State (StateT(..))

-- Monad for generating fresh variable
class Monad m => Fresh m where
  fresh :: m Int
  freshText :: m Text
  freshWithLabel :: Text -> m Text
  freshTexts :: Int -> m [Text]
  -- freshName :: m Name

  freshText =
    (\i -> Text.pack ("?m_" ++ show i)) <$> fresh

  freshWithLabel l =
    (\i -> Text.pack ("?" ++ Text.unpack l ++ "_" ++ show i)) <$> fresh

  freshTexts 0 = return []
  freshTexts n = liftM2 (:) freshText (freshTexts (n - 1))


freshName :: Fresh m => Loc -> m Name
freshName l = Name <$> freshText <*> pure l

freshName' :: Fresh m => m Name
freshName' = freshName NoLoc

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

instance Free a => Free (Subs a) where
  fv = Set.unions . Map.map fv

instance Free A.Type where
  fv (A.TBase _ _) = mempty
  fv (A.TArray _ t _) = fv t
  fv (A.TFunc t1 t2 _) = fv t1 <> fv t2
  fv (A.TVar x _) = Set.singleton x

instance Free A.Expr where
  fv (A.Paren expr _) = fv expr
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
  fv (A.Subst _ _ after) = fv after
  fv (A.ArrIdx e1 e2 _) = fv e1 <> fv e2
  fv (A.ArrUpd e1 e2 e3 _) = fv e1 <> fv e2 <> fv e3

instance Free A.Bindings where
  fv = fv . A.bindingsToExpr

-- class for data that is substitutable
class Substitutable a b where
  subst :: Subs a -> b -> b

compose :: Substitutable a a => Subs a -> Subs a -> Subs a
s1 `compose` s2 = s1 <> Map.map (subst s1) s2

instance {-# INCOHERENT #-} Substitutable a b => Substitutable a [b] where
  subst = map . subst

instance Substitutable A.Type A.Type where
  subst _ t@(A.TBase _ _) = t
  subst s (A.TArray i t l) = A.TArray i (subst s t) l
  subst s (A.TFunc t1 t2 l) = A.TFunc (subst s t1) (subst s t2) l
  subst s t@(A.TVar x _) = Map.findWithDefault t x s

instance Substitutable A.Expr A.Expr where
  subst s (A.Paren expr l) = A.Paren (subst s expr) l
  subst _ lit@A.Lit {} = lit
  subst s v@(A.Var n _) =
    case Map.lookup n s of
      Just v' -> subst (Map.delete n s) v'
      Nothing -> v
  subst s c@(A.Const n _) =
    case Map.lookup n s of
      Just c' -> subst (Map.delete n s) c'
      Nothing -> c
  subst _ o@A.Op {} = o
  subst s (A.Chain a op b l) = A.Chain (subst s a) op (subst s b) l
  subst s (A.App a b l) =
    let a' = subst s a in
    let b' = subst s b in
      case a' of
        A.Lam x body _ -> subst (Map.singleton x b') body
        A.Subst _ _ (A.Lam x body _) -> subst (Map.singleton x b') body
        _ -> A.App a' b' l
  subst s (A.Lam x e l) =
    let s' = Map.withoutKeys s (Set.singleton x) in
    A.Lam x (subst s' e) l
  subst _ h@A.Hole {} = h
  subst s (A.Quant qop xs rng t l) =
    let s' = Map.withoutKeys s (Set.fromList xs) in
    A.Quant (subst s' qop) xs (subst s' rng) (subst s' t) l
  subst s1 (A.Subst before s2 after) = A.Subst (subst s1 before) s2 (subst s1 after)
  subst s (A.ArrIdx e1 e2 l) =
    A.ArrIdx (subst s e1) (subst s e2) l
  subst s (A.ArrUpd e1 e2 e3 l) =
    A.ArrUpd (subst s e1) (subst s e2) (subst s e3) l

instance {-# OVERLAPPABLE #-} Substitutable a b => Substitutable (Maybe a) b where
  subst = subst . Map.mapMaybe id

-- should make sure `fv Bindings` and `fv Expr` are disjoint before substitution
instance Substitutable A.Bindings A.Expr where
  subst s expr = 
    -- let s = shrinkSubs expr sub in
    if null s
    then expr
    else
    case expr of
      (A.Paren e l) -> A.Paren (subst s e) l
      A.Lit {} -> expr
      (A.Var n _) ->
        case Map.lookup n s of
          Just (A.LetBinding v) -> A.Subst expr s v
          Just v -> subst (Map.delete n s) (A.bindingsToExpr v)
          Nothing -> expr
      (A.Const n _) ->
        case Map.lookup n s of
          Just (A.LetBinding c) -> A.Subst expr s c
          Just c -> subst (Map.delete n s) (A.bindingsToExpr c)
          Nothing -> expr
      A.Op {} -> expr
      (A.Chain a op b l) -> A.Chain (subst s a) op (subst s b) l
      A.App (A.Op op) a l -> A.App (A.Op op) (subst s a) l
      A.App (A.App (A.Op op) a l1) b l2 ->
        A.App (A.App (A.Op op) (subst s a) l1) (subst s b) l2
      A.App a b l ->
        let a' = subst s a in
        let b' = subst s b in
        if
        | a == a' && b == b' -> expr
        | isAllLetBindings s -> A.App a' b' l
        | isAllBetaBindings s -> A.App a' b' l
        | otherwise -> A.Subst expr s (A.App a' b' l)
      (A.Lam x e l) ->
        let s' = Map.withoutKeys s (Set.singleton x) in
        let e' = subst s' e in
        A.Lam x e' l
      A.Hole {} -> expr
      (A.Quant qop xs rng t l) ->
        let s' = Map.withoutKeys s (Set.fromList xs) in
        A.Quant (subst s' qop) xs (subst s' rng) (subst s' t) l
      A.Subst a s1 b@A.Lam {} ->
        if isAllLetBindings s1
        then A.Subst a s (subst s b)
        else A.Subst expr s (subst s b)
      A.Subst a s1 (A.App b1 b2 l) ->
        let b1' = subst s b1 in
        let b2' = subst s b2 in
        if
         | b1 == b1' && b2 == b2' -> expr
         | isAllLetBindings s -> A.Subst a s1 (A.App b1' b2' l)
         | isAllBetaBindings s -> A.Subst a s1 (A.App b1' b2' l)
         | otherwise -> A.Subst expr s (A.App b1' b2' l)
      A.Subst a s1 b ->
        let c = subst s b in
        if
        | b == c -> expr
        | isAllBetaBindings s -> A.Subst a s1 c
        | isAllLetBindings s -> A.Subst a s1 c
        | otherwise -> A.Subst expr s c
      A.ArrIdx e1 e2 l ->
        A.ArrIdx (subst s e1) (subst s e2) l
      A.ArrUpd e1 e2 e3 l ->
        A.ArrUpd (subst s e1) (subst s e2) (subst s e3) l


shrinkSubs :: A.Expr -> Subs a -> Subs a
shrinkSubs expr = Map.filterWithKey (\n _ -> n `Set.member` fv expr)

isAllBetaBindings :: Subs A.Bindings -> Bool
isAllBetaBindings = Map.foldl f True
  where
    f t A.BetaBinding {} = t
    f _ _ = False

isAllLetBindings :: Subs A.Bindings -> Bool
isAllLetBindings = Map.foldl f True
  where
    f t A.LetBinding {} = t
    f _ _ = False

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

class Fresh m => AlphaRename m a where
  alphaRename :: [Name] -> a -> m a

instance (Fresh m, AlphaRename m a) => AlphaRename m (Subs a) where
  alphaRename = mapM . alphaRename

instance (Fresh m, AlphaRename m a) => AlphaRename m (Maybe a) where
  alphaRename = mapM . alphaRename

instance Fresh m => AlphaRename m A.Expr where
  alphaRename s expr =
    case expr of
      A.Paren e l -> A.Paren <$> alphaRename s e <*> pure l
      A.Chain a op b l -> A.Chain <$> alphaRename s a <*> pure op <*> alphaRename s b <*> pure l
      A.App a b l -> A.App <$> alphaRename s a <*> alphaRename s b <*> pure l
      A.Lam x body l -> do
        x' <- capture x
        let vx' = A.Var x' (locOf x)
        A.Lam x' <$> alphaRename s (subst (Map.singleton x vx') body) <*> pure l
      A.Quant op ns rng t l -> do
        ns' <- mapM capture ns
        let mns' = Map.fromList . zip ns . map (\x -> A.Var x (locOf x)) $ ns'
        return $ A.Quant op ns' (subst mns' rng) (subst mns' t) l
      _ -> return expr
    where
      capture x = do
        if x `elem` s
        then do
          tx <- freshWithLabel (Text.pack "m" <> nameToText x)
          return $ Name tx (locOf x)
        else return x

instance Fresh m => AlphaRename m A.Bindings where
  alphaRename s (A.AssignBinding e) = A.AssignBinding <$> alphaRename s e
  alphaRename s (A.LetBinding e) = A.LetBinding <$> alphaRename s e
  alphaRename s (A.BetaBinding e) = A.BetaBinding <$> alphaRename s e
  alphaRename s (A.AlphaBinding e) = A.AlphaBinding <$> alphaRename s e

instance Fresh m => AlphaRename m Pred where
  alphaRename s (Constant e) = Constant <$> alphaRename s e
  alphaRename s (Bound e l) = Bound <$> alphaRename s e <*> pure l
  alphaRename s (Assertion e l) = Assertion <$> alphaRename s e <*> pure l
  alphaRename s (LoopInvariant e b l) = LoopInvariant <$> alphaRename s e <*> alphaRename s b <*> pure l
  alphaRename s (GuardIf e l) = GuardIf <$> alphaRename s e <*> pure l
  alphaRename s (GuardLoop e l) = GuardLoop <$> alphaRename s e <*> pure l
  alphaRename s (Conjunct xs) = Conjunct <$> mapM (alphaRename s) xs
  alphaRename s (Disjunct es) = Disjunct <$> mapM (alphaRename s) es
  alphaRename s (Negate x) = Negate <$> alphaRename s x

class BetaReduction a where
  betaReduction :: a -> a

instance BetaReduction A.Expr where
  betaReduction (A.Paren e l) = A.Paren (betaReduction e) l
  betaReduction e@A.Lit {} = e
  betaReduction e@A.Var {} = e
  betaReduction e@A.Const {} = e
  betaReduction e@A.Op {} = e
  betaReduction (A.Chain a op b l) = A.Chain (betaReduction a) op (betaReduction b) l
  betaReduction (A.App (A.Lam x e _) a _) =
    let a' = betaReduction a in
    let s = Map.singleton x (A.BetaBinding a') in
    betaReduction (subst s e)
  betaReduction (A.App (A.Subst a _ (A.Lam x e _)) b l) =
    let b' = betaReduction b in
    let s = Map.singleton x (A.BetaBinding b') in
    A.Subst (A.App a b l) s (betaReduction (subst s e))
  betaReduction (A.App a b l) = 
    let a' = betaReduction a in
    let b' = betaReduction b in
    case a' of
      A.Lam {} -> betaReduction (A.App a' b' l)
      A.Subst _ _ A.Lam {} -> betaReduction (A.App a' b' l)
      _ -> A.App a' b' l
  betaReduction (A.Lam x e l) = A.Lam x (betaReduction e) l
  betaReduction e@A.Hole {} = e
  betaReduction (A.Quant op xs rng t l)= A.Quant (betaReduction op) xs (betaReduction rng) (betaReduction t) l
  betaReduction (A.Subst a s1 (A.App (A.Subst b1 _ b2@(A.Lam x e _)) c l2)) =
    let c' = betaReduction c in
    let s = Map.singleton x (A.BetaBinding c') in
    let d = betaReduction (subst s e) in
    A.Subst 
      (A.Subst a s1 (A.App (A.Subst b1 s1 (subst s1 b2)) c l2)) s d
  betaReduction (A.Subst a s b) = A.Subst a s (betaReduction b)
  betaReduction (A.ArrIdx e1 e2 l) = A.ArrIdx (betaReduction e1) (betaReduction e2) l
  betaReduction (A.ArrUpd e1 e2 e3 l) = A.ArrUpd (betaReduction e1) (betaReduction e2) (betaReduction e3) l

instance BetaReduction Pred where
  betaReduction (Constant e) = Constant (betaReduction e)
  betaReduction (Bound e l) = Bound (betaReduction e) l
  betaReduction (Assertion e l) = Assertion (betaReduction e) l
  betaReduction (LoopInvariant e b l) = LoopInvariant (betaReduction e) b l
  betaReduction (GuardIf e l) = GuardIf (betaReduction e) l
  betaReduction (GuardLoop e l) = GuardLoop (betaReduction e) l
  betaReduction (Conjunct xs) = Conjunct (map betaReduction xs)
  betaReduction (Disjunct xs) = Disjunct (map betaReduction xs)
  betaReduction (Negate x) = Negate (betaReduction x)

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
