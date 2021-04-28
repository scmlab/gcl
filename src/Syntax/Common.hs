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

-- | Operators
data Op
  = -- binary relations
    EQ Loc
  | NEQ Loc
  | NEQU Loc
  | LTE Loc
  | LTEU Loc
  | GTE Loc
  | GTEU Loc
  | LT Loc
  | GT Loc
  | -- logic operators
    Implies Loc
  | ImpliesU Loc
  | Conj Loc
  | ConjU Loc
  | Disj Loc
  | DisjU Loc
  | -- arithmetics
    Neg Loc
  | NegU Loc
  | Add Loc
  | Sub Loc
  | Mul Loc
  | Div Loc
  | Mod Loc
  | -- For Quant
    Sum Loc
  | Forall Loc
  | Exists Loc
  deriving (Show, Eq, Generic)

instance Located Op where
  locOf (Implies l) = l
  locOf (ImpliesU l) = l
  locOf (Disj l) = l
  locOf (DisjU l) = l
  locOf (Conj l) = l
  locOf (ConjU l) = l
  locOf (Neg l) = l
  locOf (NegU l) = l
  locOf (EQ l) = l
  locOf (NEQ l) = l
  locOf (NEQU l) = l
  locOf (LTE l) = l
  locOf (LTEU l) = l
  locOf (GTE l) = l
  locOf (GTEU l) = l
  locOf (LT l) = l
  locOf (GT l) = l
  locOf (Add l) = l
  locOf (Sub l) = l
  locOf (Mul l) = l
  locOf (Div l) = l
  locOf (Mod l) = l
  locOf (Sum l) = l
  locOf (Exists l) = l
  locOf (Forall l) = l

classify :: Op -> Fixity
classify (Implies _) = InfixR 1
classify (ImpliesU _) = InfixR 1
classify (Disj _) = InfixL 2
classify (DisjU _) = InfixL 2
classify (Conj _) = InfixL 3
classify (ConjU _) = InfixL 3
classify (Neg _) = Prefix 4
classify (NegU _) = Prefix 4
classify (EQ _) = Infix 5
classify (NEQ _) = Infix 6
classify (NEQU _) = Infix 6
classify (LTE _) = Infix 6
classify (LTEU _) = Infix 6
classify (GTE _) = Infix 6
classify (GTEU _) = Infix 6
classify (LT _) = Infix 6
classify (GT _) = Infix 6
classify (Add _) = InfixL 7
classify (Sub _) = InfixL 7
classify (Mul _) = InfixL 8
classify (Div _) = InfixL 8
classify (Mod _) = InfixL 9
classify (Sum _) = Prefix 5
classify (Exists _) = Prefix 6
classify (Forall _) = Prefix 7

instance ToJSON Op

instance FromJSON Op

--------------------------------------------------------------------------------

-- | Make Loc/Pos instances of FromJSON and ToJSON
instance ToJSON Pos where
  toJSON (Pos filepath line column offset) = toJSON (filepath, line, column, offset)

instance FromJSON Pos where
  parseJSON v = do
    (filepath, line, column, offset) <- parseJSON v
    return $ Pos filepath line column offset

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
