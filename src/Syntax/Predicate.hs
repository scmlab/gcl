{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Syntax.Predicate where

import Data.Aeson
import Data.Char (isUpper)
-- import           Syntax.Abstract                ( Fresh )

import Data.Function (on)
import Data.Loc
import Data.Text (Text)
import qualified Data.Text as Text
import qualified GCL.Expr as E
import GHC.Generics
import Syntax.Common (Name)
import Syntax.Abstract
  ( Expr,
    Subst,
  )
import qualified Syntax.Abstract as A

--------------------------------------------------------------------------------

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

instance Located Pred where
  locOf (Constant _) = NoLoc
  locOf (GuardIf _ l) = l
  locOf (GuardLoop _ l) = l
  locOf (Assertion _ l) = l
  locOf (LoopInvariant _ _ l) = l
  locOf (Bound _ l) = l
  locOf (Conjunct _) = NoLoc
  locOf (Disjunct _) = NoLoc
  locOf (Negate _) = NoLoc

instance ToJSON Pred

toExpr :: Pred -> Expr
toExpr (Constant e) = e
toExpr (Bound e _) = e
toExpr (Assertion e _) = e
toExpr (LoopInvariant e _ _) = e
toExpr (GuardIf e _) = e
toExpr (GuardLoop e _) = e
toExpr (Conjunct xs) = A.conjunct (map toExpr xs)
toExpr (Disjunct xs) = A.disjunct (map toExpr xs)
toExpr (Negate x) = A.neg (toExpr x)

subst :: E.ExpandM m => Subst -> Pred -> m Pred
subst env (Constant e) = Constant <$> E.subst env e
subst env (Bound e l) = Bound <$> E.subst env e <*> pure l
subst env (Assertion e l) = Assertion <$> E.subst env e <*> pure l
subst env (LoopInvariant e b l) =
  LoopInvariant <$> E.subst env e <*> pure b <*> pure l
subst env (GuardIf e l) = GuardLoop <$> E.subst env e <*> pure l
subst env (GuardLoop e l) = GuardLoop <$> E.subst env e <*> pure l
subst env (Conjunct xs) = Conjunct <$> mapM (subst env) xs
subst env (Disjunct es) = Disjunct <$> mapM (subst env) es
subst env (Negate x) = Negate <$> subst env x

--------------------------------------------------------------------------------

-- | Smart constructors for testing
pos :: Int -> Int -> Int -> Pos
pos = Pos "<test>"

assertion :: Expr -> Pred
assertion x = Assertion x NoLoc

loopInvariant :: Expr -> Text -> Pred
loopInvariant x b = LoopInvariant x (bnd b) NoLoc
  where
    bnd =
      if Text.null b
        then A.variable
        else if isUpper (Text.head b) then A.constant else A.variable

guardIf :: Expr -> Pred
guardIf x = GuardIf x NoLoc

guardLoop :: Expr -> Pred
guardLoop x = GuardLoop x NoLoc

boundEq :: Expr -> Expr -> Pred
boundEq x var = Bound (x `A.eqq` var) NoLoc

boundLT :: Expr -> Expr -> Pred
boundLT x var = Bound (x `A.lt` var) NoLoc

boundGTE :: Expr -> Expr -> Pred
boundGTE x var = Bound (x `A.gte` var) NoLoc

(===) :: Int -> Int -> Expr
x === y = A.number x `A.eqq` A.number y

conjunct :: [Pred] -> Pred
conjunct [] = Constant A.true
conjunct [x] = x
conjunct xs = Conjunct xs

disjunct :: [Pred] -> Pred
disjunct [] = Constant A.false
disjunct [x] = x
disjunct xs = Disjunct xs

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

-- extracting the assertion from a Struct
extractAssertion :: Struct -> Pred
extractAssertion (Struct p _ _) = p
extractAssertion (Postcond p) = p

-- given a line number, get the precondition of the statement after that line
precondAtLine :: Int -> Struct -> Maybe Pred
precondAtLine i = findNext i . toPredList
  where
    toPredList :: Struct -> [Pred]
    toPredList (Postcond p) = [p]
    toPredList (Struct p xs next) = p : map precond xs ++ toPredList next

    findNext :: Int -> [Pred] -> Maybe Pred
    findNext _ [] = Nothing
    findNext n (p : ps) = case locOf p of
      NoLoc -> Nothing
      Loc start _ -> if n <= posLine start then Just p else findNext n ps

--------------------------------------------------------------------------------

-- | Statement with its computed precondition
data Stmt
  = Skip (L Pred)
  | Abort (L Pred)
  | Assign (L Pred) [Name] [Expr]
  | Do (L Pred) Expr [GdCmd]
  | If (L Pred) [GdCmd]
  | Spec (L Pred) Pred -- pre and post

data GdCmd = GdCmd
  { gdCmdGuard :: Pred,
    gdCmdBody :: Struct
  }
  deriving (Eq)

instance Located Stmt where
  locOf (Skip l) = locOf l
  locOf (Abort l) = locOf l
  locOf (Assign l _ _) = locOf l
  locOf (Do l _ _) = locOf l
  locOf (If l _) = locOf l
  locOf (Spec l _) = locOf l

-- comparing only the constructor and the predicate
instance Eq Stmt where
  Skip l == Skip m = l == m
  Abort l == Abort m = l == m
  Assign l _ _ == Assign m _ _ = l == m
  Do l _ xs == Do m _ ys = l == m && xs == ys
  If l xs == If m ys = l == m && xs == ys
  Spec l _ == Spec m _ = l == m
  _ == _ = False

precond :: Stmt -> Pred
precond (Skip l) = unLoc l
precond (Abort l) = unLoc l
precond (Assign l _ _) = unLoc l
precond (Do l _ _) = unLoc l
precond (If l _) = unLoc l
precond (Spec l _) = unLoc l

--------------------------------------------------------------------------------

-- | Obligation
data PO
  = PO Int Pred Pred Origin
  deriving (Eq, Show, Generic)

instance Located PO where
  locOf (PO _ _ _ o) = locOf o

instance Ord PO where
  compare = compare `on` (\(PO _ _ _ o) -> o)

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

instance Located Origin where
  locOf (AtAbort l) = l
  locOf (AtSkip l) = l
  locOf (AtSpec l) = l
  locOf (AtAssignment l) = l
  locOf (AtAssertion l) = l
  locOf (AtIf l) = l
  locOf (AtLoop l) = l
  locOf (AtTermination l) = l

instance Ord Loc where
  compare NoLoc NoLoc = EQ
  compare NoLoc (Loc _ _) = LT
  compare (Loc _ _) NoLoc = GT
  compare (Loc x _) (Loc y _) = compare x y

instance Ord Origin where
  compare = compare `on` locOf

--------------------------------------------------------------------------------

-- | Specification
data Spec = Specification
  { specID :: Int,
    specPreCond :: Pred,
    specPostCond :: Pred,
    specLoc :: Loc
  }
  deriving (Eq, Show, Generic)
