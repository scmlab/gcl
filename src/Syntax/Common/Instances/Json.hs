{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
module Syntax.Common.Instances.Json where

import Syntax.Common.Types
import Data.Aeson 
import Data.Aeson.Types (Parser)
import Data.Loc (Loc(..), Pos (..))


instance ToJSON Name where
  toJSON :: Name -> Value
  toJSON (Name text loc) = object
    [ "symbol" .= String text
    , "location" .= toJSON loc
    ]
instance ToJSONKey Name

instance ToJSON ChainOp

instance ToJSON ArithOp

instance ToJSON TypeOp

instance ToJSON Op

instance ToJSON Loc where
  toJSON :: Loc -> Value
  toJSON NoLoc = Null
  toJSON (Loc (Pos filePath line column _) (Pos _ line' column' _)) = object
    [ "filePath" .= toJSON filePath
    , "start"    .= object
      ["line"       .= (line - 1)
      , "character" .= (column - 1)]
    , "end"      .= object
      ["line"       .= (line' - 1)
      , "character" .= (column' - 1)]
    ]
