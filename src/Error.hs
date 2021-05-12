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
import Render (Block)

--------------------------------------------------------------------------------

-- | Error
data Error
  = SyntacticError [SyntacticError]
  | TypeError TypeError
  | StructError StructError
  | CannotReadFile FilePath
  | Others Block
  deriving (Eq, Show, Generic)

instance Located Error where
  locOf (SyntacticError es) = foldl (\l (m, _) -> l <--> m) NoLoc es
  locOf (TypeError e) = locOf e
  locOf (StructError e) = locOf e
  locOf (CannotReadFile _) = NoLoc
  locOf (Others _) = NoLoc

instance ToJSON Error
