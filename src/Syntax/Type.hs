{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Syntax.Type where

import Data.Aeson
import Data.Loc
import GHC.Generics

data SyntaxError
  = LexicalError    Pos
  | SyntacticError [(Pos, String)]
  | TransformError TransformError
  deriving (Generic)

instance Show SyntaxError where
  show (LexicalError pos) = "LexicalError " ++ show pos
  show (SyntacticError xs) = "SyntacticError\n" ++ unlines (map showPair xs)
    where
      showPair :: (Pos, String) -> String
      showPair (pos, msg) = msg ++ " at " ++ show pos
  show (TransformError e) = "TransformError " ++ show e

data TransformError
  = MissingAssertion Loc
  | MissingBound     Loc
  | ExcessBound      Loc
  | MissingPostcondition
  | DigHole Loc
  | Panic String
  deriving (Show, Generic)

instance ToJSON TransformError where
instance ToJSON SyntaxError where

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
