module Syntax.Abstract.Instances.Json where

import           Syntax.Abstract.Types
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )

instance ToJSON Endpoint
instance ToJSON Interval
instance ToJSON TBase
instance ToJSON Type
instance ToJSON Redex
instance ToJSON Expr
instance ToJSON CaseConstructor
instance ToJSON Pattern
instance ToJSON Lit

instance FromJSON Expr
instance FromJSON Redex
instance FromJSON CaseConstructor
instance FromJSON Pattern
instance FromJSON Lit
