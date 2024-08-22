{-# LANGUAGE FlexibleInstances, FlexibleContexts,
             OverloadedStrings, LambdaCase, MonoLocalBinds #-}

module GCL.Substitution where

import           Data.Loc                       ( locOf )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import qualified Data.Maybe                    as Maybe
import qualified Data.Set                      as Set
import           Data.Set                       ( Set )
import           Data.Text                      ( Text )
import           GCL.Common                     ( Free(freeVars)
                                              --  , Counterous(..)
                                                , Fresh(..)
                                              --  , FreshState
                                                , freshPre
                                                )
-- import           GCL.Predicate                  ( PO(PO)
--                                                 , Pred(..)
--                                                 )
-- import           Syntax.Abstract                ( Chain(..)
--                                                 , CaseClause(..)
--                                                 , Expr(..)
--                                                 , FuncClause(..)
--                                                 , Mapping
--                                                 , Pattern(..)
--                                                 , extractBinder
--                                                 )
import qualified Syntax.Abstract as A
import qualified Syntax.Typed    as T
import           Syntax.Common                  ( Name(Name)
                                                , nameToText
                                                )
------------------------------------------------------------------

syntaxSubst :: [Name] -> [T.Expr] -> T.Expr -> T.Expr
syntaxSubst xs es e = T.Subst e (zip xs es)
    -- SCM: For now. A lot more to do.

------------------------------------------------------------------

type Mapping = Map Text A.Expr

mappingFromSubstitution :: [Name] -> [A.Expr] -> Mapping
mappingFromSubstitution xs es =
  Map.mapKeys nameToText $ Map.fromList $ zip xs es


-- produce a binder "renaming", if any binder is in the set of "capturableNames"
produceBinderRenamings :: Fresh m => Set Text -> [Name] -> m (Map Name Name)
produceBinderRenamings capturableNames binders = mconcat <$> mapM go binders
 where
  go :: Fresh m => Name -> m (Map Name Name)
  go binder = if Set.member (nameToText binder) capturableNames
    then do
      -- CAPTURED! returns the alpha renamed binder
      binder' <- Name <$> freshPre (nameToText binder) <*> pure
        (locOf binder)
      return $ Map.singleton binder binder'
    else
      -- not captured, returns the original binder
         return mempty

-- rename a binder with some "renaming"
renameBinder :: Map Name Name -> Name -> Name
renameBinder renamings binder =
  Maybe.fromMaybe binder (Map.lookup binder renamings)

-- convert a binder renaming (Map Name Name) to a Mapping (Map Text Expr)
renamingToMapping :: Map Name Name -> Mapping
renamingToMapping =
  Map.fromList
    . map (\(old, new) -> (nameToText old, A.Var new (locOf old)))
    . Map.toList

------------------------------------------------------------------
-- | Perform Alpha renaming only when necessary
-- returns a set of free names that is susceptible to capturing
-- also returns a Mapping that is reduced further with only free variables in "body"
getCapturableNamesAndShrinkMapping :: Mapping -> A.Expr -> (Set Text, Mapping)
getCapturableNamesAndShrinkMapping mapping body =
  let
    -- collect all free variables in "body"
    -- and reduce the mapping further with free variables in "body"
    shrinkedMapping = shrinkMapping body mapping
    -- collect all free varialbes in the mapped expressions
    mappedExprs     = Map.elems shrinkedMapping
    freeVarsInMappedExprs =
      Set.map nameToText $ Set.unions (map freeVars mappedExprs)
  in
    (freeVarsInMappedExprs, shrinkedMapping)


------------------------------------------------------------------
-- | Shrink the Mapping in Substs

shrinkMapping :: A.Expr -> Mapping -> Mapping
shrinkMapping expr mapping =
  let
      -- collect all free variables in the expression
      fv = Set.map nameToText (freeVars expr)
      -- restrict the mapping with only free variables
  in  Map.restrictKeys mapping fv