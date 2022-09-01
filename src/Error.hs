{-# LANGUAGE DeriveGeneric #-}

module Error where

import           GCL.Type                       ( TypeError )
import           GCL.WP.Type                    ( StructError )
import           GHC.Generics
import           Syntax.Common                  ( )
import           Syntax.Parser.Error           ( ParseError )

--------------------------------------------------------------------------------

-- | Error
data Error
  = ParseError ParseError
  | TypeError TypeError
  | StructError StructError
  | CannotReadFile FilePath
  | Others String
  deriving (Eq, Show, Generic)
