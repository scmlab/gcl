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
import Syntax.Parser.Util ( SyntacticError )
import Syntax.Common ()

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
