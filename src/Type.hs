{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Type where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.Loc
import GHC.Generics
import Data.Foldable (fold)

import Text.Megaparsec hiding (Pos, State, ParseError, parse)
import qualified Text.Megaparsec as Mega

import Syntax.Parser.TokenStream (PrettyToken(..))
import Syntax.Parser.Util ()
import Syntax.Parser.Lexer (TokStream, Tok(..))

data Site
  = Global Loc      -- source file
  | Local  Loc Int  -- spec-specific (no pun intended)
  deriving (Show, Generic)

data Error
  = LexicalError    Pos
  | SyntacticError  SyntacticError
  | ConvertError    ConvertError
  deriving (Show, Generic)


instance Located Error where
  locOf (LexicalError pos) = Loc pos pos
  locOf (SyntacticError x) = locOf x
  locOf (ConvertError e) = locOf e


fromLocalError :: Int -> Error -> (Site, Error)
fromLocalError i e = (Local (locOf e) i, e)

fromGlobalError :: Error -> (Site, Error)
fromGlobalError e = (Global (locOf e), e)

instance ToJSON Site where
instance ToJSON Error where

data SyntacticError = SynErr
  { synErrLocation :: Loc
  , synErrMessage :: String
  }
  deriving (Generic)

instance Show SyntacticError where
  show (SynErr _ msg) = msg

instance Located SyntacticError where
  locOf (SynErr loc _) = loc

-- data SyntaxError
--   = LexicalError   Pos
--   | SyntacticError SyntacticError
--   | ConvertError ConvertError
--   deriving (Generic)

-- instance Show SyntaxError where
--   show (LexicalError pos) = "LexicalError " ++ show pos
--   show (SyntacticError x) = "SyntacticError " ++ show x
--   show (ConvertError e) = "ConvertError " ++ show e

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

instance ToJSON SyntacticError where
instance ToJSON ConvertError where
-- instance ToJSON SyntaxError where

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

fromParseErrorBundle :: ShowErrorComponent e
                   => ParseErrorBundle TokStream e
                   -> [SyntacticError]
fromParseErrorBundle (ParseErrorBundle errors posState)
  = snd $ foldr f (posState, []) errors
  where
    f :: ShowErrorComponent e
      => Mega.ParseError TokStream e
      -> (PosState TokStream, [SyntacticError])
      -> (PosState TokStream, [SyntacticError])
    f err (initial, accum) =
        let (_, next) = reachOffset (errorOffset err) initial
        in (next, (SynErr (getLoc err) (parseErrorTextPretty err)):accum)

    getLoc :: ShowErrorComponent e
      => Mega.ParseError TokStream e
      -> Loc
    -- get the Loc of all unexpected tokens
    getLoc (TrivialError _ (Just (Tokens xs)) _) = fold $ fmap locOf xs
    getLoc _ = mempty
