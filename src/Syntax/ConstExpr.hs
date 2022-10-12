module Syntax.ConstExpr where

import           Data.Char                      ( isLower )
import           Data.List                      ( partition )
import           Data.Maybe                     ( listToMaybe
                                                , mapMaybe
                                                )
import qualified Data.Set                      as Set
import qualified Data.Text                     as Text
import           GCL.Common                     ( freeVars )
import           Syntax.Abstract
import           Syntax.Common                  ( Name
                                                , nameToText
                                                )

pickGlobals :: [Declaration] -> ([Expr], [Expr])
pickGlobals = partition isGlobalProp . mapMaybe extractAssertion
  where
    -- An assertion is a global prop
    -- if all of its free variables are of CONSTANTS
    isGlobalProp :: Expr -> Bool
    isGlobalProp assertion = Set.null $ Set.filter nameIsVar (freeVars assertion)

    nameIsVar :: Name -> Bool
    nameIsVar name =
        maybe False isLower (listToMaybe (Text.unpack (nameToText name)))

    -- Extracts both the assertion and those declared names
    extractAssertion :: Declaration -> Maybe Expr
    extractAssertion (ConstDecl _ _ e _) = e
    extractAssertion (VarDecl   _ _ e _) = e
