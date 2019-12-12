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
-- | Site of Error

data Site
  = Global Loc      -- source file
  | Local  Loc Int  -- spec-specific (no pun intended)
  deriving (Show, Generic)

instance ToJSON Site where

--------------------------------------------------------------------------------
-- | Error

data Error
  = LexicalError    Pos
  | SyntacticError  Loc String
  | ConvertError    ConvertError
  deriving (Show, Generic)

instance Located Error where
  locOf (LexicalError pos) = Loc pos pos
  locOf (SyntacticError loc _) = loc
  locOf (ConvertError e) = locOf e

fromLocalError :: Int -> Error -> (Site, Error)
fromLocalError i e = (Local (locOf e) i, e)

fromGlobalError :: Error -> (Site, Error)
fromGlobalError e = (Global (locOf e), e)

instance ToJSON Error where

--------------------------------------------------------------------------------
-- | Convert Error

data ConvertError
  = MissingAssertion Loc
  | MissingBound     Loc
  | ExcessBound      Loc
  | MissingPostcondition
  | DigHole Loc
  | Panic String
  deriving (Show, Generic)

instance Located ConvertError where
  locOf (MissingAssertion loc) = loc
  locOf (MissingBound loc) = loc
  locOf (ExcessBound loc) = loc
  locOf MissingPostcondition = NoLoc
  locOf (DigHole loc) = loc
  locOf (Panic _) = NoLoc

instance ToJSON ConvertError where

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
