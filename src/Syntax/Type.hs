{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Syntax.Type where

import qualified Data.List.NonEmpty as NE
import Data.Aeson
import Data.Loc
import GHC.Generics

import Text.Megaparsec hiding (Pos, State, ParseError, parse)
import qualified Text.Megaparsec as Mega

import Syntax.Parser.Util ()
import Syntax.Parser.Lexer (TokStream)

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
        in (next, (SynErr (getLocs err) (parseErrorTextPretty err)):accum)

    getLocs :: ShowErrorComponent e
      => Mega.ParseError TokStream e
      -> [Loc]
    getLocs (TrivialError _ (Just (Tokens xs)) _) = NE.toList $ fmap locOf xs
    getLocs _ = []

data SyntacticError = SynErr
  { synErrLocations :: [Loc]
  , synErrMessage :: String
  }
  deriving (Generic)

instance Show SyntacticError where
  show (SynErr _ msg) = msg

data SyntaxError
  = LexicalError    Pos
  | SyntacticError [SyntacticError]
  | TransformError TransformError
  deriving (Generic)

instance Show SyntaxError where
  show (LexicalError pos) = "LexicalError " ++ show pos
  show (SyntacticError xs) = "SyntacticError\n" ++ unlines (map show xs)
  show (TransformError e) = "TransformError " ++ show e

data TransformError
  = MissingAssertion Loc
  | MissingBound     Loc
  | ExcessBound      Loc
  | MissingPostcondition
  | DigHole Loc
  | Panic String
  deriving (Show, Generic)

instance ToJSON SyntacticError where
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
