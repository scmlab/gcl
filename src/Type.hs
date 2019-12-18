{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Type where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.Loc
import GHC.Generics

import Syntax.Parser.TokenStream (PrettyToken(..))
import Syntax.Parser.Util ()

--------------------------------------------------------------------------------
-- | Instances of ToJSON

instance ToJSON Pos where
  toJSON (Pos filepath line column offset) = object
    [ "filepath"  .= filepath
    , "line"      .= line
    , "column"    .= column
    , "offset"    .= offset
    ]

  toEncoding (Pos filepath line column offset) = pairs
      $   "filepath"  .= filepath
      <>  "line"      .= line
      <>  "column"    .= column
      <>  "offset"    .= offset

instance ToJSON Loc where
  toJSON NoLoc = object
    [ "tag"    .= ("NoLoc" :: String)
    ]
  toJSON (Loc start end) = object
    [ "tag"       .= ("Loc" :: String)
    , "contents"  .= object
      [ "start"    .= start
      , "end"      .= end
      ]
    ]
