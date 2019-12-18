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
import Syntax.Parser.Lexer (Tok(..))

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

--------------------------------------------------------------------------------
-- | Instances of PrettyToken

instance PrettyToken Tok where
  prettyTokens (x:|[])  = fromMaybe ("'" <> show (unLoc x) <> "'") (prettyToken' (unLoc x))
  prettyTokens xs       = "\"" <> concatMap (f . unLoc) (NE.toList xs) <> "\""
    where
      f tok =
        case prettyToken' tok of
          Nothing     -> show tok
          Just pretty -> "<" <> pretty <> ">"

-- | If the given character has a pretty representation, return that,
-- otherwise 'Nothing'. This is an internal helper.

prettyToken' :: Tok -> Maybe String
prettyToken' tok = case tok of
  TokNewline -> Just "newline"
  TokWhitespace -> Just "space"
  TokEOF -> Just "end of file"
  _      -> Nothing
