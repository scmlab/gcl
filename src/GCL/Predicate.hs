{-# LANGUAGE DeriveGeneric #-}

module GCL.Predicate where

import           Data.Aeson                     ( ToJSON )
import qualified Data.Aeson                    as JSON
import           Data.Loc                       ( L
                                                , Loc(..)
                                                , Located(locOf)
                                                )
import           Data.Loc.Range                 ( Range
                                                , Ranged(rangeOf)
                                                , fromLoc
                                                , within
                                                )
import           Data.Text                      ( Text )
import qualified Data.Set                      as Set
import           GHC.Generics                   ( Generic )
import           GCL.Common
import           Render.Element
import           Syntax.Typed                   ( Expr )
import           Syntax.Common                  ( Name )
-- | A predicate is an expression, whose type happens to be Bool.
type Pred = Expr

{-
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

instance Free Pred where
  freeVars (Constant e) = freeVars e
  freeVars (GuardIf e _) = freeVars e
  freeVars (GuardLoop e _) = freeVars e
  freeVars (Assertion e _) = freeVars e
  freeVars (LoopInvariant e1 e2 _) = freeVars e1 <> freeVars e2 -- predicate & bound
  freeVars (Bound e _) = freeVars e
  freeVars (Conjunct es) = Set.unions $ map freeVars es
  freeVars (Disjunct es) = Set.unions $ map freeVars es
  freeVars (Negate e) = freeVars e

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
-}
--------------------------------------------------------------------------------

-- | Proof obligation
data PO = PO
  { poPre        :: Pred -- precondition
  , poPost       :: Pred -- post-condition
  , poAnchorHash :: Text -- anchor hash
  , poAnchorLoc  :: Maybe Range -- anchor location, if it exists in the source
  , poOrigin     :: Origin -- whereabouts
  }
  deriving (Eq, Show, Generic)

instance Ord PO where
  compare (PO _ _ _ _ x) (PO _ _ _ _ y) = compare y x

instance Located PO where
  locOf (PO _ _ _ _ o) = locOf o

-- instance ToJSON PO

data InfMode = Primary     -- the main inference mode
             | Secondary   -- non-functional postconditions. ignore assertions
             deriving (Eq, Show, Generic)

instance ToJSON InfMode

data Origin
  = AtAbort Loc
  | AtSkip Loc
  | AtSpec Loc
  | AtAssignment Loc
  | AtAssertion Loc -- AssertSufficient
  | AtIf Loc
  | AtLoop Loc
  | AtTermination Loc
  | Explain { originHeader :: Text -- the text you see on the top of a PO
            , originExplanation :: Inlines -- the text you see at the bottom of a PO (after clicking the header)
            , originInfMode :: InfMode
            , originHighlightPartial :: Bool -- for highlighting only "if" in conditionals and "do" in loops
            , originLoc :: Loc
            }
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
  locOf (AtAbort       l  ) = l
  locOf (AtSkip        l  ) = l
  locOf (AtSpec        l  ) = l
  locOf (AtAssignment  l  ) = l
  locOf (AtAssertion   l  ) = l
  locOf (AtIf          l  ) = l
  locOf (AtLoop        l  ) = l
  locOf (AtTermination l  ) = l
  locOf (Explain _ _ _ _ l) = l

data Spec = Specification
  { specID       :: Int
  , specPreCond  :: Pred
  , specPostCond :: Pred
  , specRange    :: Range
  , specTypeEnv  :: [(Index, TypeInfo)]
  }
  deriving (Eq, Show, Generic)

instance Located Spec where
  locOf (Specification _ _ _ l _) = locOf l

instance Ranged Spec where
  rangeOf (Specification _ _ _ r _) = r
