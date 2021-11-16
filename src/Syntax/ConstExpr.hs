module Syntax.ConstExpr where

import           Data.Bifunctor                 ( bimap )
import           Data.List                      ( partition )
import           Data.Maybe                     ( mapMaybe )
import qualified Data.Set                      as Set
import           GCL.Common                     ( fv )
import           Syntax.Abstract
import           Syntax.Common                  ( Name )

pickGlobals :: [Declaration] -> ([Expr], [Expr])
pickGlobals =
    bimap (map snd) (map snd)
        . partition isGlobalProp
        . mapMaybe extractAssertion
  where
    -- An assertion is a global prop 
    -- if it has no free variables other than the accompanied declared names
    isGlobalProp :: ([Name], Expr) -> Bool
    isGlobalProp (names, assertion) =
        fv assertion `Set.isSubsetOf` Set.fromList names

    -- Extracts both the assertion and those declared names 
    extractAssertion :: Declaration -> Maybe ([Name], Expr)
    extractAssertion (ConstDecl _     _ Nothing  _) = Nothing
    extractAssertion (ConstDecl names _ (Just e) _) = Just (names, e)
    extractAssertion (VarDecl   _     _ Nothing  _) = Nothing
    extractAssertion (VarDecl   names _ (Just e) _) = Just (names, e)
