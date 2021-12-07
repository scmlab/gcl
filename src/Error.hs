{-# LANGUAGE DeriveGeneric #-}

module Error where

import           GCL.Type                       ( TypeError )
import           GCL.Scope                      ( ScopeError )
import           GHC.Generics
import           Syntax.Common                  ( )
import           Syntax.Parser.Util             ( SyntacticError )
import           GCL.WP.Type                    ( StructError )

--------------------------------------------------------------------------------

-- | Error
data Error
  = SyntacticError SyntacticError
  | ScopeError ScopeError
  | TypeError TypeError
  | StructError StructError
  | CannotReadFile FilePath
  | Others String
  deriving (Eq, Show, Generic)
