{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module GCL.Common where

import           Control.Monad.RWS              ( RWST(..) )
import           Control.Monad.State            ( StateT(..) )
import           Data.Loc                       ( Loc(..) )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set
                                                , (\\)
                                                )
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Syntax.Abstract
import           Syntax.Common.Types

-- get a fresh variable (and bump the counter)
class Monad m => Fresh m where
    getCounter :: m Int
    setCounter :: Int -> m ()


    fresh :: m Int
    fresh = do
        i <- getCounter
        setCounter (succ i)
        return i

  -- get a fresh variable in the form of Text
freshText :: Fresh m => m Text
freshText = (\i -> Text.pack ("?m_" ++ show i)) <$> fresh

  -- a more fancy `freshText`
freshWithLabel :: Fresh m => Text -> m Text
freshWithLabel l =
  (\i -> Text.pack ("?" ++ Text.unpack l ++ "_" ++ show i)) <$> fresh

freshName :: Fresh m => Text -> Loc -> m Name
freshName prefix l = Name <$> freshWithLabel prefix <*> pure l

freshName' :: Fresh m => Text -> m Name
freshName' prefix = freshName prefix NoLoc

type FreshState = Int

initFreshState :: FreshState
initFreshState = 0

type Subs a = Map Name a
type Env a = Map Name a

emptySubs :: Subs a
emptySubs = mempty

emptyEnv :: Env a
emptyEnv = mempty

-- A class of types for which we may compute their free variables.
class Free a where
  fv :: a -> Set Name

occurs :: Free a => Name -> a -> Bool
occurs n x = n `Set.member` fv x

instance Free a => Free (Subs a) where
  fv = Set.unions . Map.map fv


instance Free a => Free [a] where
  fv l = foldMap fv l

instance Free Type where
  fv (TBase _ _    ) = mempty
  fv (TArray _ t _ ) = fv t
  fv (TTuple ts    ) = Set.unions (map fv ts)
  fv (TFunc t1 t2 _) = fv t1 <> fv t2
  fv (TCon  _  ns _) = Set.fromList ns
  fv (TVar x _     ) = Set.singleton x
  fv (TMetaVar n   ) = Set.singleton n

instance Free Expr where
  fv (Var   x _        ) = Set.singleton x
  fv (Const x _        ) = Set.singleton x
  fv (Op _             ) = mempty
  fv (Lit _ _          ) = mempty
  fv (App  e1 e2      _) = fv e1 <> fv e2
  fv (Func _  clauses _) = Set.unions (fmap fv clauses)
  fv (Lam  x  e       _) = fv e \\ Set.singleton x
  fv (Tuple xs         ) = Set.unions (map fv xs)
  fv (Quant op xs range term _) =
    (fv op <> fv range <> fv term) \\ Set.fromList xs
  fv (RedexKernel _ _ freeVars _) = freeVars
  fv (RedexShell _ e            ) = fv e
  fv (ArrIdx e1 e2 _            ) = fv e1 <> fv e2
  fv (ArrUpd e1 e2 e3 _         ) = fv e1 <> fv e2 <> fv e3
  fv (Case e clauses _          ) = fv e <> Set.unions (map fv clauses)
  

instance Free FuncClause where
  fv (FuncClause patterns expr) = fv expr \\ Set.unions (map fv patterns)

instance Free CaseClause where
  fv (CaseClause patt expr) = fv expr \\ fv patt
instance Free Pattern where
  fv (PattLit      _      ) = mempty
  fv (PattBinder   n      ) = Set.singleton n
  fv (PattWildcard _      ) = mempty
  fv (PattConstructor _ ps) = fv ps

-- class for data that is substitutable
class Substitutable a b where
  subst :: Subs a -> b -> b

compose :: Substitutable a a => Subs a -> Subs a -> Subs a
s1 `compose` s2 = s1 <> Map.map (subst s1) s2

instance (Substitutable a b, Functor f) => Substitutable a (f b) where
  subst = fmap . subst

instance Substitutable Type Type where
  subst _ t@TBase{}       = t
  subst s (TArray i t l ) = TArray i (subst s t) l
  subst s (TTuple ts    ) = TTuple (map (subst s) ts)
  subst s (TFunc t1 t2 l) = TFunc (subst s t1) (subst s t2) l
  subst _ t@TCon{}        = t
  subst _ t@TVar{}        = t
  subst s t@(TMetaVar n)  = Map.findWithDefault t n s

toStateT :: Monad m => r -> RWST r w s m a -> StateT s m a
toStateT r m = StateT
  (\s -> do
    (a, s', _) <- runRWST m r s
    return (a, s')
  )

toEvalStateT :: Monad m => r -> RWST r w s m a -> StateT s m (a, w)
toEvalStateT r m = StateT
  (\s -> do
    (a, s', w) <- runRWST m r s
    return ((a, w), s')
  )
