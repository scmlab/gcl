module Syntax.ConstExpr where

import           Data.List                      ( partition )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( mapMaybe )
import qualified Data.Set                      as Set
import           GCL.Common                     ( fv )
import           Syntax.Abstract
import           Syntax.Abstract.Util           ( extractLetBinding )

-- extracts assertion from declarations
seperateAssertions :: [Declaration] -> ([Expr], [Expr])
seperateAssertions = partition containsNoFreeVariable . mapMaybe extractAssertion
 where
  extractAssertion :: Declaration -> Maybe Expr
  extractAssertion (ConstDecl _ _ e _) = e
  extractAssertion (VarDecl   _ _ e _) = e

  containsNoFreeVariable :: Expr -> Bool
  containsNoFreeVariable = Set.null . fv

-- extract let bindings in declarations
pickLetBindings :: [LetDeclaration] -> Defns
pickLetBindings = Map.fromList . map extractLetBinding
