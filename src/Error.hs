{-# LANGUAGE DeriveGeneric #-}

module Error where

import           GCL.Type                       ( TypeError )
import           GHC.Generics
import           Syntax.Common                  ( )
import           Syntax.Parser.Util             ( SyntacticError )
import           GCL.WP.Type                    ( StructError )

--------------------------------------------------------------------------------

-- | Error
data Error
  = SyntacticError SyntacticError
  | TypeError TypeError
  | StructError StructError
  | CannotReadFile FilePath
  | Others String
  deriving (Eq, Show, Generic)
