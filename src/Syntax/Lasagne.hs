{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveFunctor #-}
--
module Syntax.Lasagne where

import Data.Loc
import Control.Monad.Free
import qualified Data.Map as Map

import Syntax.Abstract hiding (Stmt(..), GdCmd(..), Program(..), getGuards)
import qualified Syntax.Abstract as A

--------------------------------------------------------------------------------
-- | The Stmt functor

data Stmt f
  = Skip                          f
  | Abort                         f
  | Assign [Var] [Expr]           f
  | Assert Pred                   f
  | Do     Pred Expr    [GdCmd f] f
  | If     (Maybe Pred) [GdCmd f] f
  | Spec                          f
  deriving (Show, Functor)

-- applying Pred & Loc on top of `Stmt`
data Lasagne f = Sauce Pred Loc (Stmt f)
  deriving (Show, Functor)


-- a program is like a Lasagne
-- data Free f a = Pure a	| Free (f (Free f a))
type Program = Free
                  Lasagne -- Pred & Loc & Stmt
                  Pred    -- the post condition of a program

data GdCmd f = GdCmd Pred f
  deriving (Show, Functor)

--------------------------------------------------------------------------------

getGuards :: [GdCmd f] -> [Pred]
getGuards = map (\(GdCmd p _) -> p)

-- extracting the calculated precondition from a program
precond :: Program -> Pred
precond (Pure p) = p
precond (Free (Sauce p _ _)) = p

skipToNext :: Program -> Maybe Program
skipToNext (Pure _) = Nothing
skipToNext (Free (Sauce _ _ (Skip next))) = Just next
skipToNext (Free (Sauce _ _ (Abort next))) = Just next
skipToNext (Free (Sauce _ _ (Assign _ _ next))) = Just next
skipToNext (Free (Sauce _ _ (Assert _ next))) = Just next
skipToNext (Free (Sauce _ _ (Do _ _ _ next))) = Just next
skipToNext (Free (Sauce _ _ (If _ _ next))) = Just next
skipToNext (Free (Sauce _ _ (Spec next))) = Just next

-- calculate the precondition with a different postcondition
recalculatePrecond :: Program -> Pred -> Pred
recalculatePrecond (Pure _) post = post
recalculatePrecond (Free (Sauce _ _ stmt)) post = case stmt of
  Skip xs -> recalculatePrecond xs post
  Abort xs -> recalculatePrecond xs post
  {-  SCM: temporarily disabled due to use of subst
  Assign vs es xs -> subst (zip vs es) $ recalculatePrecond xs post
  -}
  Assert pre _ -> pre
  Do inv _ _ _ -> inv
  If (Just pre) _ _ -> pre
  If Nothing branches xs -> (conjunct brConds `conj` disjunct guards)
    where
      post' = recalculatePrecond xs post
      brConds = map precondGuard branches
      guards = getGuards branches

      precondGuard :: GdCmd Program -> Pred
      precondGuard (GdCmd guard body)
        = implies guard $ recalculatePrecond body post'
  Spec xs -> recalculatePrecond xs post

-- converting from `[A.Stmt]` to `Program`
makeLasagne :: [A.Stmt] -> Pred -> Program
makeLasagne []     post = Pure post
makeLasagne (x:xs) post =
  let rest = makeLasagne xs post
      post' = precond rest
  in case x of

    A.Skip loc -> Free
      $ Sauce post' loc
      $ Skip
      $ rest

    A.Abort loc -> Free
      $ Sauce post' loc
      $ Abort
      $ rest

  {-  SCM: temporarily disabled due to use of subst
    A.Assign vs es loc -> Free
      $ Sauce (subst (zip vs es) post') loc
      $ Assign vs es
      $ rest
  -}

    A.Assert pre loc -> Free
      $ Sauce pre loc
      $ Assert pre
      $ rest

    A.Do inv bnd branches loc -> Free
      $ Sauce inv loc
      $ Do inv bnd (map (makeLasagneGdCmd post') branches)
      $ rest

    A.If (Just pre) branches loc -> Free
      $ Sauce pre loc
      $ If (Just pre) (map (makeLasagneGdCmd post') branches)
      $ rest

    A.If Nothing branches loc -> Free
      $ Sauce (conjunct brConds `conj` disjunct guards) loc
      $ If Nothing branches'
      $ rest

      where
        branches' = map (makeLasagneGdCmd post') branches
        brConds = map precondGuard branches'
        guards = getGuards branches'

        precondGuard :: GdCmd Program -> Pred
        precondGuard (GdCmd guard body)
          = implies guard $ precond body

    A.Spec loc -> Free
      $ Sauce post' loc
      $ Spec
      $ rest

makeLasagneGdCmd :: Pred -> A.GdCmd -> GdCmd Program
makeLasagneGdCmd post (A.GdCmd guard body)
  = GdCmd guard (makeLasagne body post)
