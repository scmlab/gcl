{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Error where

import Control.Exception (IOException)
import Data.Aeson
import Data.Loc
import GHC.Generics

import Syntax.Parser.Util ()
import Type ()
import Syntax.Parser.Lexer (LexicalError)
import Syntax.Parser (SyntacticError)
-- import Syntax.Abstract (ConvertError)
import GCL.Type (TypeError)
-- import GCL.Exec.ExecMonad (ExecError)
import GCL.WP (StructError)
import GCL.WP2 (StructError2)

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
  = LexicalError    LexicalError
  | SyntacticError  SyntacticError
  | TypeError       TypeError
  | StructError     StructError
  | StructError2    StructError2
  | CannotReadFile  FilePath
  -- | ExecError       ExecError
  deriving (Eq, Show, Generic)

instance Located Error where
  locOf (LexicalError pos) = Loc pos pos
  locOf (SyntacticError (loc, _)) = loc
  locOf (TypeError e) = locOf e
  locOf (StructError e) = locOf e
  locOf (StructError2 e) = locOf e
  locOf (CannotReadFile _) = NoLoc
  -- locOf (ExecError e) = locOf e

fromLocalError :: Int -> Error -> (Site, Error)
fromLocalError i e = (Local (locOf e) i, e)

fromGlobalError :: Error -> (Site, Error)
fromGlobalError e = (Global (locOf e), e)

instance ToJSON Error where
