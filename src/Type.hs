{-# LANGUAGE DeriveGeneric #-}

module Type where

import Syntax.Parser
import Data.Aeson
import GHC.Generics
import Data.Loc

data Site
  = Global Loc      -- source file
  | Local  Loc Int  -- spec-specific (no pun intended)
  deriving (Show, Generic)

data Error = SyntaxError SyntaxError
  deriving (Show, Generic)

fromLocalSyntaxError :: Int -> SyntaxError -> (Site, Error)
fromLocalSyntaxError i e = (Local (locOf e) i, SyntaxError e)

fromGlobalSyntaxError :: SyntaxError -> (Site, Error)
fromGlobalSyntaxError e = (Global (locOf e), SyntaxError e)

instance ToJSON Site where
instance ToJSON Error where
