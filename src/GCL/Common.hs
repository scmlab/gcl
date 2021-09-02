{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module GCL.Common where

import Data.Text(Text)
import qualified Data.Text as Text
import Data.Loc ( Loc(..), Loc )
import Control.Monad (liftM2)
import Data.Map (Map)
import Syntax.Common (Name(..))
import Data.Set (Set, (\\))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Syntax.Abstract as A
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
  fv (A.TBase _ _) = mempty
  fv (A.TArray _ t _) = fv t
  fv (A.TFunc t1 t2 _) = fv t1 <> fv t2
  fv (A.TCon (A.QTyCon _ ns)) = Set.fromList ns
  fv (A.TVar x _) = Set.singleton x

instance Free A.Expr where
  fv (A.Var   x _    ) = Set.singleton x
  fv (A.Const x _    ) = Set.singleton x
  fv (A.Op _         ) = mempty
  fv (A.Lit _ _      ) = mempty
  fv (A.Chain a _ b _) = fv a <> fv b
  fv (A.App e1 e2 _  ) = fv e1 <> fv e2
  fv (A.Lam x  e  _  ) = fv e \\ Set.singleton x
  fv (A.Quant op xs range term _) =
    (fv op <> fv range <> fv term) \\ Set.fromList xs
  fv (A.Subst _ set _    ) = set
  fv (A.Expand _ after    ) = fv after
  fv (A.ArrIdx e1 e2 _    ) = fv e1 <> fv e2
  fv (A.ArrUpd e1 e2 e3 _ ) = fv e1 <> fv e2 <> fv e3

-- instance Free A.Bindings where
--   fv = fv . A.bindingsToExpr

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
  subst _ t@A.TBase {} = t
  subst s (A.TArray i t l) = A.TArray i (subst s t) l
  subst s (A.TFunc t1 t2 l) = A.TFunc (subst s t1) (subst s t2) l
  subst _ t@A.TCon {} = t
  subst s t@(A.TVar x _) = Map.findWithDefault t x s

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
