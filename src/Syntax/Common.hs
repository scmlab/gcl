{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Syntax.Common where

import Data.Aeson
import Data.Aeson.Types (Parser)
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

instance Located Name where
  locOf (Name _ l) = l

instance ToJSON Name
instance ToJSONKey Name

instance FromJSON Name
instance FromJSONKey Name

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
   Implies Loc
  | ImpliesU Loc
  | Conj Loc
  | ConjU Loc
  | Disj Loc
  | DisjU Loc
  | Neg Loc
  | NegU Loc
  | Add Loc
  | Sub Loc
  | Mul Loc
  | Div Loc
  | Mod Loc 
  | Max Loc
  | Min Loc
  deriving (Eq, Show, Generic)

data QuantOp = 
    Sum Loc
  | Forall Loc
  | Exists Loc 
  | Hash Loc
  deriving (Eq, Show, Generic)

-- | Operators
data Op = ChainOp ChainOp | ArithOp ArithOp | QuantOp QuantOp
  deriving (Show, Eq, Generic)


-- classifyChainOp :: ChainOp -> Fixity
-- classifyChainOp (EQProp _) = Infix 0
-- classifyChainOp (EQPropU _) = Infix 0
-- classifyChainOp (EQ _) = Infix 5
-- classifyChainOp (NEQ _) = Infix 6
-- classifyChainOp (NEQU _) = Infix 6
-- classifyChainOp (LTE _) = Infix 6
-- classifyChainOp (LTEU _) = Infix 6
-- classifyChainOp (GTE _) = Infix 6
-- classifyChainOp (GTEU _) = Infix 6
-- classifyChainOp (LT _) = Infix 6
-- classifyChainOp (GT _) = Infix 6

classifyArithOp :: ArithOp -> Fixity
classifyArithOp (Implies _) = InfixR 1
classifyArithOp (ImpliesU _) = InfixR 1
classifyArithOp (Disj _) = InfixL 2
classifyArithOp (DisjU _) = InfixL 2
classifyArithOp (Conj _) = InfixL 3
classifyArithOp (ConjU _) = InfixL 3
classifyArithOp (Neg _) = Prefix 4
classifyArithOp (NegU _) = Prefix 4
classifyArithOp (Add _) = InfixL 7
classifyArithOp (Sub _) = InfixL 7
classifyArithOp (Mul _) = InfixL 8
classifyArithOp (Div _) = InfixL 8
classifyArithOp (Mod _) = InfixL 9
classifyArithOp (Max _) = Infix 10
classifyArithOp (Min _) = Infix 10

-- classifyQuantOp :: QuantOp -> Fixity
-- classifyQuantOp (Sum _) = Prefix 5
-- classifyQuantOp (Exists _) = Prefix 6
-- classifyQuantOp (Forall _) = Prefix 7
-- classifyQuantOp (Hash _) = Prefix 8

-- classify :: Op -> Fixity
-- classify (ChainOp op) = classifyChainOp op
-- classify (ArithOp op) = classifyArithOp op
-- classify (QuantOp op) = classifyQuantOp op

instance ToJSON ChainOp
instance FromJSON ChainOp

instance ToJSON ArithOp
instance FromJSON ArithOp

instance ToJSON QuantOp
instance FromJSON QuantOp

instance ToJSON Op
instance FromJSON Op

--------------------------------------------------------------------------------

-- | Make Loc/Pos instances of FromJSON and ToJSON
-- instance ToJSON Pos where
--   toJSON (Pos filepath line column offset) = toJSON (filepath, line, column, offset)

-- instance FromJSON Pos where
--   parseJSON v = do
--     (filepath, line, column, offset) <- parseJSON v
--     return $ Pos filepath line column offset

instance ToJSON Loc where
  toJSON NoLoc =
    object
      [ "tag" .= String "NoLoc"
      ]
  toJSON (Loc start end) =
    object
      [ "tag" .= String "Loc",
        "contents" .= (start, end)
      ]

instance FromJSON Loc where
  parseJSON = withObject "Loc" $ \v -> do
    result <- v .:? "tag" :: Parser (Maybe String)
    case result of
      Just "Loc" -> do 
        positions <- (v .:? "contents") :: Parser (Maybe (Pos, Pos))
        case positions of 
          Just (start, end) -> return $ Loc start end 
          Nothing -> return NoLoc
      _ -> return NoLoc

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
