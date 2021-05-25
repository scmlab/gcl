{-# LANGUAGE DeriveGeneric #-}

module Error where

import Data.Aeson
import Data.Loc
import GCL.Type (TypeError)
import GCL.WP (StructError)
import GHC.Generics
import Syntax.Common ()
import Syntax.Parser.Util (SyntacticError)

--------------------------------------------------------------------------------

-- | Error
data Error
  = SyntacticError SyntacticError
  | TypeError TypeError
  | StructError StructError
  | CannotReadFile FilePath
  | Others String
  deriving (Eq, Show, Generic)

instance Located Error where
  locOf (SyntacticError (l, _)) = l
  locOf (TypeError e) = locOf e
  locOf (StructError e) = locOf e
  locOf (CannotReadFile _) = NoLoc
  locOf (Others _) = NoLoc

instance ToJSON Error
