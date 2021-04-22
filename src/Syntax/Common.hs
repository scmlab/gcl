{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE DeriveGeneric #-}
module Syntax.Common where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Loc
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Function (on)

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

instance FromJSON Name

instance Ord Name where
  compare (Name a _) (Name b _) = compare a b

nameToText :: Name -> Text
nameToText (Name x _) = x

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
