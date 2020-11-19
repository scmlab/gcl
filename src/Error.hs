{-# LANGUAGE DeriveGeneric #-}

module Error where

import Data.Aeson
import Data.Loc
-- import Data.ByteString.Lazy (ByteString)

-- import Syntax.Abstract (ConvertError)
import GCL.Type (TypeError)
-- import GCL.Exec.ExecMonad (ExecError)
import GCL.WP (StructError)
import GHC.Generics
import Syntax.Parser (SyntacticError)
import Syntax.Parser.Lexer (LexicalError)
import Syntax.Parser.Util ()
import Type ()

--------------------------------------------------------------------------------

-- | Site of Error
data Site
  = Global Loc -- source file
  | Local Loc Int -- spec-specific (no pun intended)
  deriving (Show, Generic)

instance ToJSON Site

--------------------------------------------------------------------------------

-- | Error
data Error
  = LexicalError LexicalError
  | SyntacticError [SyntacticError]
  | TypeError TypeError
  | StructError StructError
  | CannotReadFile FilePath
  | NotLoaded
  deriving (Eq, Show, Generic)

instance Located Error where
  locOf (LexicalError pos) = Loc pos pos
  locOf (SyntacticError es) = foldl (\l (m, _) -> l <--> m) NoLoc es
  locOf (TypeError e) = locOf e
  locOf (StructError e) = locOf e
  locOf (CannotReadFile _) = NoLoc
  locOf NotLoaded = NoLoc

localError :: Int -> Error -> (Site, Error)
localError i e = (Local (locOf e) i, e)

globalError :: Error -> (Site, Error)
globalError e = (Global (locOf e), e)

instance ToJSON Error
