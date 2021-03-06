{-# LANGUAGE DeriveGeneric #-}
module Syntax.Common.Types where

import Data.Loc
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Function (on)
import Prelude hiding (Ordering (..))
import Data.Loc.Range ()
--------------------------------------------------------------------------------

-- | Variables and stuff
data Name = Name Text Loc
  deriving (Show, Generic)

-- | Compare regardless of their locations
instance Eq Name where
  (==) = (==) `on` nameToText

instance Ord Name where
  compare (Name a _) (Name b _) = compare a b

nameToText :: Name -> Text
nameToText (Name x _) = x

--------------------------------------------------------------------------------
data ChainOp =
    EQProp Loc
  | EQPropU Loc
  | EQ Loc
  | NEQ Loc
  | NEQU Loc
  | LTE Loc
  | LTEU Loc
  | GTE Loc
  | GTEU Loc
  | LT Loc
  | GT Loc
  deriving (Eq, Show, Generic)

data ArithOp =
    -- logic
    Implies Loc
  | ImpliesU Loc
  | Conj Loc
  | ConjU Loc
  | Disj Loc
  | DisjU Loc
  | Neg Loc
  | NegU Loc
    -- arithmetics
  | Add Loc
  | Sub Loc
  | Mul Loc
  | Div Loc
  | Mod Loc
  | Max Loc
  | Min Loc
  | Exp Loc
  | Hash Loc
    -- pointers and sep. logic
  | PointsTo Loc  -- a |-> v
  | SConj Loc
  | SImp Loc
  deriving (Eq, Show, Generic)

-- | Operators
data Op = ChainOp ChainOp | ArithOp ArithOp
  deriving (Show, Eq, Generic)


classifyChainOp :: ChainOp -> Fixity
classifyChainOp (EQProp _) = Infix 0
classifyChainOp (EQPropU _) = Infix 0
classifyChainOp (EQ _) = Infix 5
classifyChainOp (NEQ _) = Infix 4
classifyChainOp (NEQU _) = Infix 4
classifyChainOp (LTE _) = Infix 4
classifyChainOp (LTEU _) = Infix 4
classifyChainOp (GTE _) = Infix 4
classifyChainOp (GTEU _) = Infix 4
classifyChainOp (LT _) = Infix 4
classifyChainOp (GT _) = Infix 4

classifyArithOp :: ArithOp -> Fixity
classifyArithOp (Implies _) = InfixR 1
classifyArithOp (ImpliesU _) = InfixR 1
classifyArithOp (Disj _) = InfixL 2
classifyArithOp (DisjU _) = InfixL 2
classifyArithOp (Conj _) = InfixL 3
classifyArithOp (ConjU _) = InfixL 3
classifyArithOp (Neg _) = Prefix 6
classifyArithOp (NegU _) = Prefix 6
classifyArithOp (Add _) = InfixL 7
classifyArithOp (Sub _) = InfixL 7
classifyArithOp (Mul _) = InfixL 8
classifyArithOp (Div _) = InfixL 8
classifyArithOp (Mod _) = InfixL 9
classifyArithOp (Max _) = Infix 10
classifyArithOp (Min _) = Infix 10
classifyArithOp (Exp _) = Infix 11
classifyArithOp (Hash _) = Infix (-1)
classifyArithOp (PointsTo _) = Infix 4
classifyArithOp (SConj _) = InfixL 3
classifyArithOp (SImp _) = Infix 1

classify :: Op -> Fixity
classify (ChainOp op) = classifyChainOp op
classify (ArithOp op) = classifyArithOp op

--------------------------------------------------------------------------------

-- | Make Loc/Pos instances of FromJSON and ToJSON
-- instance ToJSON Pos where
--   toJSON (Pos filepath line column offset) = toJSON (filepath, line, column, offset)

-- instance FromJSON Pos where
--   parseJSON v = do
--     (filepath, line, column, offset) <- parseJSON v
--     return $ Pos filepath line column offset


--------------------------------------------------------------------------------

-- | Fixity & Precedence
data Fixity = Infix Int | InfixR Int | InfixL Int | Prefix Int | Postfix Int
  deriving (Show, Eq)

-- --------------------------------------------------------------------------------

-- -- | For annotating the usage of unicode symbols in some constructs

-- type UseUnicodeSymbol = Bool

-- useUnicodeSymbol :: UseUnicodeSymbol
-- useUnicodeSymbol = True

-- usePlainSymbol :: UseUnicodeSymbol
-- usePlainSymbol = False
