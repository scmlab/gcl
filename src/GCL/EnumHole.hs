{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module GCL.EnumHole where

import GCL.Expr
import GCL.Pred

import Control.Monad.State
import qualified Data.Map as Map

--------------------------------------------------------------------------------
-- | Enumerate and index Holes

type HoleIndex = Int
type EnumHoleM = State HoleIndex

class EnumHole p where
  enumHole :: p -> EnumHoleM p

runEnumHole :: EnumHole p => p -> p
runEnumHole x = evalState (enumHole x) 0

fresh :: EnumHoleM HoleIndex
fresh = do
  i <- get
  put (succ i)
  return i

instance EnumHole Subst where
  -- `const` for throwing away the key, we don't need it anyway
  enumHole = Map.traverseWithKey (const enumHole)

instance EnumHole Expr where
  enumHole (Var x)    = return $ Var x
  enumHole (Lit n)    = return $ Lit n
  enumHole (Op op es) = Op op <$> mapM enumHole es
  enumHole (HoleE Nothing subs) = do
    subs' <- mapM enumHole subs
    i <- fresh
    return (HoleE (Just i) subs')
  enumHole (HoleE (Just i) subs) = HoleE (Just i) <$> mapM enumHole subs

instance EnumHole Pred where
  enumHole (Term rel e1 e2) =
    liftM2 (Term rel) (enumHole e1) (enumHole e2)
  enumHole (Implies p q) =
    liftM2 Implies (enumHole p) (enumHole q)
  enumHole (Conj p q) =
    liftM2 Conj (enumHole p) (enumHole q)
  enumHole (Disj p q) =
    liftM2 Disj (enumHole p) (enumHole q)
  enumHole (Neg p) = Neg <$> enumHole p
  enumHole (HoleP _)  = HoleP . Just <$> fresh
