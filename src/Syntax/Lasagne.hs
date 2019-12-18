{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveFunctor #-}

module Syntax.Lasagne where

-- import Control.Monad.State
-- import Control.Monad.Except
--
-- import Data.Aeson
-- import Data.Text.Lazy (Text)
-- import Data.Map (Map)
import Data.Loc
import Control.Monad.Free
import qualified Data.Map as Map
-- import GHC.Generics (Generic)
-- import Prelude hiding (Ordering(..))

import Syntax.Abstract hiding (Stmt(..), GdCmd(..), Program(..), getGuards)
import qualified Syntax.Abstract as A


data Stmt f
  = Skip                          f
  | Abort                         f
  | Assign [Var] [Expr]           f
  | Assert Pred                   f
  | Do     Pred Expr    [GdCmd f] f
  | If     (Maybe Pred) [GdCmd f] f
  | Spec                          f
  deriving (Show, Functor)

data GdCmd f = GdCmd Pred f deriving (Show, Functor)

getGuards :: [GdCmd f] -> [Pred]
getGuards = map (\(GdCmd p _) -> p)

-- applying Pred & Loc
data Lasagne f = Sauce Pred Loc (Stmt f)
type Program = Free Lasagne Pred


precond :: Program -> Pred
precond (Pure p) = p
precond (Free (Sauce p _ _)) = p

makeLasagne :: [A.Stmt] -> Pred -> Program
makeLasagne []     post = Pure post
makeLasagne (x:xs) post = case x of

  A.Skip loc -> Free
    $ Sauce post loc
    $ Skip
    $ makeLasagne xs post

  A.Abort loc -> Free
    $ Sauce post loc
    $ Abort
    $ makeLasagne xs post

  A.Assign vs es loc -> Free
    $ Sauce post loc
    $ Assign vs es
    $ makeLasagne xs $ subst (Map.fromList (zip vs es)) post

  A.Assert pre loc -> Free
    $ Sauce post loc
    $ Assert pre
    $ makeLasagne xs pre

  A.Do inv bnd branches loc -> Free
    $ Sauce post loc
    $ Do inv bnd (map (makeLasagneGdCmd post) branches)
    $ makeLasagne xs inv

  A.If (Just pre) branches loc -> Free
    $ Sauce post loc
    $ If (Just pre) (map (makeLasagneGdCmd post) branches)
    $ makeLasagne xs pre

  A.If Nothing branches loc -> Free
    $ Sauce post loc
    $ If Nothing branches'
    $ makeLasagne xs (conjunct brConds `conj` disjunct guards)

    where
      branches' = map (makeLasagneGdCmd post) branches
      brConds = map precondGuard branches'
      guards = getGuards branches'

      precondGuard :: GdCmd Program -> Pred
      precondGuard (GdCmd guard body)
        = implies guard $ precond body

  A.Spec loc -> Free
    $ Sauce post loc
    $ Spec
    $ makeLasagne xs post

makeLasagneGdCmd :: Pred -> A.GdCmd -> GdCmd Program
makeLasagneGdCmd post (A.GdCmd guard body)
  = GdCmd guard (makeLasagne body post)



-- a :: Program ()
-- a = Free $ Saused NoLoc $ Skip _

-- data Stmt
--   = Skip                          Loc
--   | Abort                         Loc
--   | Assign  [Var] [Expr]          Loc
--   | Assert  Pred                  Loc
--   | Do      Pred Expr [GdCmd]     Loc
--   | If      (Maybe Pred) [GdCmd]  Loc
--   | Spec                          Loc
--   deriving (Show)
