{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Error where

import Data.Aeson
import Data.Loc
import GHC.Generics

import Syntax.Parser.Util ()
import Type ()

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
  | TypeError       Loc String
  | ConvertError    ConvertError
  deriving (Show, Generic)

instance Located Error where
  locOf (LexicalError pos) = Loc pos pos
  locOf (SyntacticError loc _) = loc
  locOf (TypeError loc _) = loc
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

-- --------------------------------------------------------------------------------
-- -- | Type Error
--
-- data TypeError
--   = NotInScope Text Loc
--   | UnifyFailed Type Type Loc
--   | RecursiveType TVar Type Loc
--   | NotFunction Type Loc
--   deriving (Show, Eq)
