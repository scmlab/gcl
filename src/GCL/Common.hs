{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module GCL.Common where

import           Control.Monad.RWS              ( RWST(..) )
import           Control.Monad.State            ( StateT(..), State, MonadState (get, put) )
import           Data.Loc                       ( Loc(..) )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set
                                                , (\\)
                                                )
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Syntax.Abstract               as A
import           Syntax.Common                  ( Name(..) )
import qualified Data.List.NonEmpty as NonEmpty

-- Monad for generating fresh variables 
class Monad m => Fresh m where
  -- minimum requirement:
  getCounter :: m Int 
  setCounter :: Int -> m () 
  -- get a fresh variable (and bump the counter)
  fresh :: m Int
  fresh = do 
    i <- getCounter
    setCounter (succ i)
    return i 

  -- get a fresh variable in the form of Text 
  freshText :: m Text
  freshText =
    (\i -> Text.pack ("?m_" ++ show i)) <$> fresh

  -- a more fancy `freshText`
  freshWithLabel :: Text -> m Text
  freshWithLabel l =
    (\i -> Text.pack ("?" ++ Text.unpack l ++ "_" ++ show i)) <$> fresh

freshName :: Fresh m => Text -> Loc -> m Name
freshName prefix l = Name <$> freshWithLabel prefix <*> pure l

freshName' :: Fresh m => Text -> m Name
freshName' prefix = freshName prefix NoLoc

instance Fresh (State Int) where 
  getCounter = get 
  setCounter = put 

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


instance Free a => Free [a] where
  fv l = foldMap fv l

instance Free A.Type where
  fv (A.TBase _ _     ) = mempty
  fv (A.TArray _  t  _) = fv t
  fv (A.TFunc  t1 t2 _) = fv t1 <> fv t2
  fv (A.TCon   _  ns _) = Set.fromList ns
  fv (A.TVar x _      ) = Set.singleton x

instance Free A.Expr where
  fv (A.Var   x _  ) = Set.singleton x
  fv (A.Const x _  ) = Set.singleton x
  fv (A.Op _       ) = mempty
  fv (A.Lit _ _    ) = mempty
  fv (A.App e1 e2 _) = fv e1 <> fv e2
  fv (A.Lam x  e  _) = fv e \\ Set.singleton x
  fv (A.Quant op xs range term _) =
    (fv op <> fv range <> fv term) \\ Set.fromList xs
  fv (A.RedexStem _ _ pairs) = fst (NonEmpty.head pairs)
  fv (A.Redex x             ) = fv (A.redexAfter x)
  fv (A.ArrIdx e1 e2 _      ) = fv e1 <> fv e2
  fv (A.ArrUpd e1 e2 e3 _   ) = fv e1 <> fv e2 <> fv e3
  fv (A.Case e cases _      ) = fv e <> Set.unions (map fv cases)

instance Free A.Case where
  fv (A.CaseConstructor _ xs expr) = fv expr \\ Set.fromList xs

instance Free A.Mapping where
  fv mapping = Set.unions (map fv (Map.elems mapping))

-- class for data that is substitutable
class Substitutable a b where
  subst :: Subs a -> b -> b

compose :: Substitutable a a => Subs a -> Subs a -> Subs a
s1 `compose` s2 = s1 <> Map.map (subst s1) s2

instance {-# INCOHERENT #-} Substitutable a b => Substitutable a [b] where
  subst = map . subst

instance Substitutable A.Type A.Type where
  subst _ t@A.TBase{}        = t
  subst s (A.TArray i  t  l) = A.TArray i (subst s t) l
  subst s (A.TFunc  t1 t2 l) = A.TFunc (subst s t1) (subst s t2) l
  subst _ t@A.TCon{}         = t
  subst s t@(A.TVar x _)     = Map.findWithDefault t x s

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
