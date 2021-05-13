{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module GCL.Common where

import Data.Text(Text)
import qualified Data.Text as Text
import Control.Monad (liftM2)
import Data.Map (Map)
import Syntax.Common (Name)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

class Monad m => Fresh m where
  fresh :: m Int
  freshVar :: m Text
  freshVars :: Int -> m [Text]

  freshVar =
    (\i -> Text.pack ("?m_" ++ show i)) <$> fresh

  freshVars 0 = return []
  freshVars n = liftM2 (:) freshVar (freshVars (n - 1))

type Subst a = Map Name a
type Env a = Map Name a

class (Monad m) => HasEnv m a where
  askEnv :: m (Env a)

extend :: Env a -> (Name, a) -> Env a
extend env (n, x) = Map.insert n x env

class Free a where
  fv :: a -> Set Name

occurs :: Free a => Name -> a -> Bool
occurs n x = n `Set.member` fv x

class Substitutable a where
  apply :: Subst a -> a -> a

compose :: Substitutable a => Subst a -> Subst a -> Subst a
s1 `compose` s2 = s1 `Map.union` Map.map (apply s1) s2
