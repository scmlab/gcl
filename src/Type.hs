{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Type where

import Data.Aeson
import Data.Loc

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
