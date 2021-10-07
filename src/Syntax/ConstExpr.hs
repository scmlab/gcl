module Syntax.ConstExpr where

import           Data.List                      ( partition )
import           Data.Maybe                     ( mapMaybe )
import qualified Data.Set                      as Set
import           GCL.Common                     ( fv )
import           Syntax.Abstract
import           Syntax.Abstract.Util           ( extractAssertion )

-- extract assertions from declarations
pickGlobals :: [Declaration] -> ([Expr], [Expr])
pickGlobals = partition hasNoFreeVars . mapMaybe extractAssertion
 where
  hasNoFreeVars :: Expr -> Bool
  hasNoFreeVars = Set.null . fv
