{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
module Syntax.Common.Types where

import           Data.Loc
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Data.Function                  ( on )
import           Prelude                 hiding ( Ordering(..) )
import           Data.Loc.Range                 ( )
import           Data.Map                       (Map)
import qualified Data.Map as Map
import Control.Arrow ( Arrow(second) )
import Data.Maybe ( fromMaybe )
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
  deriving (Eq, Show, Generic, Ord)

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
  | NegNum Loc
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
  deriving (Eq, Show, Generic, Ord)

newtype TypeOp = Arrow Loc
  deriving (Eq, Show, Generic, Ord)

-- | Operators
data Op = ChainOp ChainOp | ArithOp ArithOp
        -- It's debatable whether we should put type operators here.
        -- This could be seen as a hack (as it is used as a workaround in order not to change too much code),
        -- However, this might also be justified in future versions of Guabao when we don't distinguish term-level and type-level operators.
        | TypeOp TypeOp
  deriving (Show, Eq, Generic, Ord)


-- | The order should be same as 'opTable' defined in Syntax.Parser
-- Except NegNum (minus), which is dealt exceptionally in parser, but the objective is to make NegNum has the same precedence as below.
precedenceOrder :: [[(Op, Fixity)]]
precedenceOrder =
  [ -- application is supposed to be here
    [ (ArithOp (Hash     NoLoc), Prefix)
    , (ArithOp (Neg      NoLoc), Prefix)
    , (ArithOp (NegU     NoLoc), Prefix)
    , (ArithOp (NegNum   NoLoc), Prefix)
    ]
  , [ (ArithOp (Exp      NoLoc), InfixL)
    ]
  , [ (ArithOp (Mul      NoLoc), InfixL)
    , (ArithOp (Div      NoLoc), InfixL)
    , (ArithOp (Mod      NoLoc), InfixL)
    ]
  , [ (ArithOp (Add      NoLoc), InfixL)
    , (ArithOp (Sub      NoLoc), InfixL)
    ]
  , [ (ArithOp (Max      NoLoc), InfixL)
    , (ArithOp (Min      NoLoc), InfixL)
    ]
  , [ (ArithOp (PointsTo NoLoc), Infix)
    ]
  , [ (ArithOp (SConj    NoLoc), InfixL)
    ]
  , [ (ArithOp (SImp     NoLoc), InfixR)
    ]
  , [ (ChainOp (EQ      NoLoc), InfixL)
    , (ChainOp (NEQ     NoLoc), InfixL)
    , (ChainOp (NEQU    NoLoc), InfixL)
    , (ChainOp (LTE     NoLoc), InfixL)
    , (ChainOp (LTEU    NoLoc), InfixL)
    , (ChainOp (GTE     NoLoc), InfixL)
    , (ChainOp (GTEU    NoLoc), InfixL)
    , (ChainOp (LT      NoLoc), InfixL)
    , (ChainOp (GT      NoLoc), InfixL)
    ]
  , [ (ArithOp (Disj     NoLoc), InfixL)
    , (ArithOp (DisjU    NoLoc), InfixL)
    , (ArithOp (Conj     NoLoc), InfixL)
    , (ArithOp (ConjU    NoLoc), InfixL)
    ]
  , [ (ArithOp (Implies  NoLoc), InfixR)
    , (ArithOp (ImpliesU NoLoc), InfixR)
    ]
  , [ (ChainOp (EQProp  NoLoc), InfixL)
    , (ChainOp (EQPropU NoLoc), InfixL)
    ]
  -- Below is a type operator and is naturally very different from other operators.
  -- It is put here because we need a way to know its fixity.
  , [ (TypeOp (Arrow NoLoc), InfixR) ]
  ]

initOrderIndex :: Int
initOrderIndex = 1

classificationMap :: Map Op (Fixity,Int)
classificationMap = Map.fromList $ concat $ zipWith f [initOrderIndex..] precedenceOrder
  where
    f :: Int -> [(Op,Fixity)] -> [(Op,(Fixity,Int))]
    f n = map (second (,n))

-- classifyChainOp :: ChainOp -> Fixity
-- classifyChainOp (EQ      _) = Infix 8
-- classifyChainOp (NEQ     _) = Infix 8
-- classifyChainOp (NEQU    _) = Infix 8
-- classifyChainOp (LTE     _) = Infix 8
-- classifyChainOp (LTEU    _) = Infix 8
-- classifyChainOp (GTE     _) = Infix 8
-- classifyChainOp (GTEU    _) = Infix 8
-- classifyChainOp (LT      _) = Infix 8
-- classifyChainOp (GT      _) = Infix 8
-- classifyChainOp (EQProp  _) = Infix 8
-- classifyChainOp (EQPropU _) = Infix 8

-- classifyArithOp :: ArithOp -> Fixity
-- classifyArithOp (Hash     _) = Infix (-1)

-- classifyArithOp (Neg      _) = Prefix 1
-- classifyArithOp (NegU     _) = Prefix 1
-- classifyArithOp (NegNum   _) = Prefix 1

-- classifyArithOp (Exp      _) = InfixL 2

-- classifyArithOp (Mul      _) = InfixL 2
-- classifyArithOp (Div      _) = InfixL 2
-- classifyArithOp (Mod      _) = InfixL 2

-- classifyArithOp (Add      _) = InfixL 3
-- classifyArithOp (Sub      _) = InfixL 3
-- classifyArithOp (Max      _) = InfixL 4
-- classifyArithOp (Min      _) = InfixL 4

-- classifyArithOp (PointsTo _) = Infix 5
-- classifyArithOp (SConj    _) = InfixL 6
-- classifyArithOp (SImp     _) = Infix 7

-- classifyArithOp (Disj     _) = InfixL 9
-- classifyArithOp (DisjU    _) = InfixL 9
-- classifyArithOp (Conj     _) = InfixL 9
-- classifyArithOp (ConjU    _) = InfixL 9

-- classifyArithOp (Implies  _) = InfixR 10
-- classifyArithOp (ImpliesU _) = InfixR 10


classify :: Op -> (Fixity,Int)
classify op = fromMaybe (error "Operator's precedenceOrder is not completely defined.")
              $ Map.lookup (wipeLoc op) classificationMap

precOf :: Op -> Int
precOf = snd . classify

sameOpSym :: Op -> Op -> Bool
sameOpSym x y = wipeLoc x == wipeLoc y

wipeLoc :: Op -> Op
wipeLoc op = case op of
  ChainOp co -> ChainOp (case co of
    EQProp _ -> EQProp NoLoc
    EQPropU _-> EQPropU NoLoc
    EQ _     -> EQ NoLoc
    NEQ _    -> NEQ NoLoc
    NEQU _   -> NEQU NoLoc
    LTE _    -> LTE NoLoc
    LTEU _   -> LTEU NoLoc
    GTE _    -> GTE NoLoc
    GTEU _   -> GTEU NoLoc
    LT _     -> LT NoLoc
    GT _     -> GT NoLoc
    )
  ArithOp ao -> ArithOp (case ao of
    Implies _ -> Implies NoLoc
    ImpliesU _-> ImpliesU NoLoc
    Conj _    -> Conj NoLoc
    ConjU _   -> ConjU NoLoc
    Disj _    -> Disj NoLoc
    DisjU _   -> DisjU NoLoc
    Neg _     -> Neg NoLoc
    NegU _    -> NegU NoLoc
    NegNum _  -> NegNum NoLoc
    Add _     -> Add NoLoc
    Sub _     -> Sub NoLoc
    Mul _     -> Mul NoLoc
    Div _     -> Div NoLoc
    Mod _     -> Mod NoLoc
    Max _     -> Max NoLoc
    Min _     -> Min NoLoc
    Exp _     -> Exp NoLoc
    Hash _    -> Hash NoLoc
    PointsTo _-> PointsTo NoLoc
    SConj _   -> SConj NoLoc
    SImp _    -> SImp NoLoc
    )
  TypeOp (Arrow _) -> TypeOp $ Arrow NoLoc



-- associative operators
-- notice that =>,-> are not associative, a->(b->c) can be shown as a->b->c, but not the case of (a->b)->c
isAssocOp :: Op -> Bool
isAssocOp (ArithOp (Mul _)) = True
isAssocOp (ArithOp (Add _)) = True
isAssocOp (ArithOp (Conj _))  = True
isAssocOp (ArithOp (ConjU _)) = True
isAssocOp (ArithOp (Disj _))  = True
isAssocOp (ArithOp (DisjU _)) = True
isAssocOp (ArithOp (Max _)) = True
isAssocOp (ArithOp (Min _)) = True
isAssocOp (ChainOp _) = True
isAssocOp _ = False



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
data Fixity = Infix | InfixR | InfixL | Prefix | Postfix
  deriving (Show, Eq)

-- --------------------------------------------------------------------------------

-- -- | For annotating the usage of unicode symbols in some constructs

-- type UseUnicodeSymbol = Bool

-- useUnicodeSymbol :: UseUnicodeSymbol
-- useUnicodeSymbol = True

-- usePlainSymbol :: UseUnicodeSymbol
-- usePlainSymbol = False
