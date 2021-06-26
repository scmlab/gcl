{-# LANGUAGE OverloadedStrings #-}
module Syntax.Common.Instances.Json where

import Syntax.Common.Types
import Data.Aeson 
import Data.Aeson.Types (Parser)
import Data.Loc (Loc(..), Pos)


instance ToJSON Name
instance ToJSONKey Name

instance FromJSON Name
instance FromJSONKey Name

instance ToJSON ChainOp
instance FromJSON ChainOp

instance ToJSON ArithOp
instance FromJSON ArithOp

instance ToJSON Op
instance FromJSON Op

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