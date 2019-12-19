{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveFunctor #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
--
module Syntax.Lasagne where

import Data.Loc
import Control.Monad.Free
import qualified Data.Map as Map
import Data.Text.Prettyprint.Doc

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


instance Pretty (GdCmd Program) where
  pretty (GdCmd p x) = "|" <+> (pretty . show) p <+> "->" <> line <> indent 2 (pretty x) <> line


instance Pretty (Stmt Program) where
  pretty (Skip x) = "skip" <> line <> pretty x
  pretty (Abort x) = "abort" <> line <> pretty x
  pretty (Assign vars exprs x)
    = hcat (punctuate comma (map pretty vars))
    <+> ":="
    <+> hcat (punctuate comma (map (pretty . show) exprs))
    <> line <> pretty x
  pretty (Assert p x)
    = lbrace <+> (pretty . show) p <+> rbrace
    <> line <> pretty x
  pretty (Do inv bnd branches x)
    = lbrace <+> ((pretty . show) inv <+> comma <+> "bnd: " <> (pretty . show) bnd) <+> rbrace <> line
    <> "  do" <> line
    <> indent 2 (vsep (map pretty branches)) <> line
    <> "  od" <> line
    <> pretty x
  pretty (If Nothing branches x)
    = "  if" <> line
    <> indent 2 (vsep (map pretty branches)) <> line
    <> "  fi" <> line
    <> pretty x
  pretty (If (Just p) branches x)
    = lbrace <+> (pretty . show) p <+> rbrace <> line
    <> "  if" <> line
    <> indent 2 (vsep (map pretty branches)) <> line
    <> "  fi" <> line
    <> pretty x
  pretty (Spec x)
    = "  {!" <> line
    <> " !}" <> line
    <> pretty x


instance Pretty Program where
  pretty (Pure p) = line <> "# " <> pretty (show p)
  pretty (Free (Sauce p _ x)) = line <> "# " <> pretty (show p) <> line <> line <> "  " <> (pretty x)
-- instance Show Program where

-- instance Show1 Stmt where
--   -- liftShowsPrec :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> f a -> ShowS
--   liftShowsPrec f g n (Skip x) = showString "skip\n" . f n x
--   liftShowsPrec f g n (Abort x) = showString "abort\n" . f n x
--   liftShowsPrec f g n (Assign v e x) = showString (show v <> " := " <> show e <> "\n") . f n x
--   liftShowsPrec f g n (Assert p x) = showString ("{ " <> show p <> "}\n") . f n x
--   liftShowsPrec f g n (Do i b s x)
--     = showString ("do " <> show i <> show b)
--       . liftShowsPrec f g n (map test s)
--       . showString "\n"
--       . f n x
--   liftShowsPrec f g n (If _ _ x) = f n x . ((<>) "If")
--   liftShowsPrec f g n (Spec x) = f n x . ((<>) "Spec")
--
-- instance Show1 Lasagne where
--   liftShowsPrec f g n (Sauce _ _ x) = liftShowsPrec f g n x

--------------------------------------------------------------------------------

getGuards :: [GdCmd f] -> [Pred]
getGuards = map (\(GdCmd p _) -> p)

-- extracting the calculated precondition from a program
precond :: Program -> Pred
precond (Pure p) = p
precond (Free (Sauce p _ _)) = p

-- calculate the precondition with a different postcondition
recalculatePrecond :: Program -> Pred -> Pred
recalculatePrecond (Pure _) post = post
recalculatePrecond (Free (Sauce _ _ stmt)) post = case stmt of
  Skip xs -> recalculatePrecond xs post
  Abort xs -> recalculatePrecond xs post
  Assign vs es xs -> subst (Map.fromList (zip vs es)) $ recalculatePrecond xs post
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

    A.Assign vs es loc -> Free
      $ Sauce (subst (Map.fromList (zip vs es)) post') loc
      $ Assign vs es
      $ rest

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
