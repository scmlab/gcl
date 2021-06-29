{-# LANGUAGE DeriveGeneric #-}

module GCL.Predicate where

import           Data.Aeson                     ( ToJSON )
import           Data.Loc                       ( L
                                                , Loc(..)
                                                , Located(locOf)
                                                )
import           Data.Loc.Range                 ( Range
                                                , Ranged(rangeOf)
                                                , fromLoc
                                                , within
                                                )
import           GHC.Generics                   ( Generic )
import           Syntax.Abstract                ( Expr )
import           Syntax.Common                  ( Name )
import Data.Text (Text)

-- | Predicates
data Pred
  = Constant Expr
  | GuardIf Expr Loc
  | GuardLoop Expr Loc
  | Assertion Expr Loc
  | LoopInvariant Expr Expr Loc -- predicate & bound
  | Bound Expr Loc
  | Conjunct [Pred]
  | Disjunct [Pred]
  | Negate Pred
  deriving (Eq, Show, Generic)

instance ToJSON Pred

--------------------------------------------------------------------------------

-- | Data structure for storing Assertions in a program
--
--    { P }   ----- Struct { P } ----
--    stmt
--    stmt
--    stmt
--    { Q }    ----- Struct { Q } ----
--    stmt
--    stmt
--    stmt
--    { R }    ----- Postcond { R } ----
data Struct
  = Struct
      Pred --  assertion
      [Stmt] --  statements after the assertion
      Struct --  the next chunk of assertion with statements after it
  | Postcond Pred
  deriving (Eq)


--------------------------------------------------------------------------------

-- | Statement with its computed precondition
data Stmt
  = Skip (L Pred)
  | Abort (L Pred)
  | Assign (L Pred) [Name] [Expr]
  | Do (L Pred) Expr [GdCmd]
  | If (L Pred) [GdCmd]
  | Spec (L Pred) Pred -- pre and post

data GdCmd = GdCmd Pred Struct
  deriving Eq

instance Eq Stmt where
  Skip  l       == Skip  m       = l == m
  Abort l       == Abort m       = l == m
  Assign l _ _  == Assign m _ _  = l == m
  Do     l _ xs == Do     m _ ys = l == m && xs == ys
  If   l xs     == If   m ys     = l == m && xs == ys
  Spec l _      == Spec m _      = l == m
  _             == _             = False

--------------------------------------------------------------------------------

-- | Proof obligation
data PO = PO
  { poPre    :: Pred -- precondition 
  , poPost   :: Pred -- post-condition 
  , poAnchorHash :: Text -- anchor hash
  , poAnchorLoc :: Maybe Range -- anchor location, if it exists in the source 
  , poOrigin :: Origin -- whereabouts
  }
  deriving (Eq, Show, Generic)

instance Ord PO where
  compare (PO _ _ _ _ x) (PO _ _ _ _ y) = compare y x

instance Located PO where
  locOf (PO _ _ _ _ o) = locOf o

data Origin
  = AtAbort Loc
  | AtSkip Loc
  | AtSpec Loc
  | AtAssignment Loc
  | AtAssertion Loc -- AssertSufficient
  | AtIf Loc
  | AtLoop Loc
  | AtTermination Loc
  deriving (Eq, Show, Generic)

-- | This ordering would affect how they are presented to the user 
-- | A PO should be placed in front of another PO when: 
-- |  1. its range is within another PO 
-- |  2. its range is ahead of that of another PO 
instance Ord Origin where
  compare x y = case fromLoc (locOf x) of
    Nothing -> LT
    Just a  -> case fromLoc (locOf y) of
      Nothing -> GT
      Just b ->
        if a `within` b then LT else if b `within` a then GT else compare a b

instance Located Origin where
  locOf (AtAbort       l) = l
  locOf (AtSkip        l) = l
  locOf (AtSpec        l) = l
  locOf (AtAssignment  l) = l
  locOf (AtAssertion   l) = l
  locOf (AtIf          l) = l
  locOf (AtLoop        l) = l
  locOf (AtTermination l) = l

data Spec = Specification
  { specID       :: Int
  , specPreCond  :: Pred
  , specPostCond :: Pred
  , specRange    :: Range
  }
  deriving (Eq, Show, Generic)

instance Located Spec where
  locOf (Specification _ _ _ l) = locOf l

instance Ranged Spec where
  rangeOf (Specification _ _ _ r) = r
