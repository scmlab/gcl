{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module GCL.Substitute
    ( run
    , Scope
    ) where

import           Control.Monad.RWS
import           Data.Loc                       ( locOf )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Data.Set                       ( Set )
import           Data.Text                      ( Text )
import           GCL.Common                     ( Free(fv)
                                                , Fresh(fresh, freshWithLabel)
                                                )
import           GCL.Predicate                  ( Pred(..) )
import           Syntax.Abstract                ( Expr(..), Mapping )
import           Syntax.Common                  ( Name(Name)
                                                , nameToText
                                                )

------------------------------------------------------------------

run
    :: (Substitutable a)
    => Scope -- declarations
    -> [Name] -- name of variables to be substituted
    -> [Expr] -- values to be substituted for  
    -> a
    -> a
run scope names exprs predicate = fst
    $ evalRWS (subst mapping predicate) scope 0
  where
    mapping :: Mapping
    mapping = mappingFromSubstitution names exprs

mappingFromSubstitution :: [Name] -> [Expr] -> Mapping
mappingFromSubstitution xs es =
    Map.mapKeys nameToText $ Map.fromList $ zip xs es

------------------------------------------------------------------

type Scope = Map Text (Maybe Expr)
type M = RWS Scope () Int

instance Fresh M where
    fresh = do
        i <- get
        put (succ i)
        return i

------------------------------------------------------------------

--  TODO: REVISE & FIX THESE RULES
--      f                       --expand-->     \binder . body
--      body [ x / binder ]     ~~~~~~~~~~>     y
-- --------------------------------------------------------------- [App-Expand-Lam]
--      f x                     --expand-->     y
-- 
--  
--      body [ x / binder ]     ~~~~~~~~~~>     y
-- --------------------------------------------------------------- [App-Lam]
--      (\binder . body) x      ~~~~~~~~~~>     y
--
--
-- --------------------------------------------------------------- [Others]
--      other constructs        ~~~~~~~~~~>     other constructs
-- 

-- perform substitution when there's a redex
reduce :: Expr -> M Expr
reduce expr = case expr of
    App f x l1 -> case f of
        -- [App-Expand-Lam]
        Expand before (Lam binder body l2) -> do
            let mapping = mappingFromSubstitution [binder] [x]
            -- Expand (App (Expand before (Lam binder body l2)) x l1) <$> subst mapping body

            -- distribute App inwards
            after <- reduce (App (Lam binder body l2) x l1)
            before' <- reduce (App before x l1)

            Expand (Expand before' after) <$> subst mapping body
        -- [App-Lam]
        Lam binder body _ -> do
            let mapping = mappingFromSubstitution [binder] [x]
            subst mapping body
        -- [Others]
        _ -> return expr
    -- [Others]

    _ -> return expr

------------------------------------------------------------------

class Substitutable a where
    subst :: Mapping -> a -> M a

instance Substitutable Expr where
    subst mapping expr = reduce =<< case expr of

-- 
--      e [../..]           ~~~~~~~~~~>     a
--      Paren a             ~~~~~~~~~~>     b
-- ---------------------------------------------------------------
--      Paren e [../..]     ~~~~~~~~~~>     b
-- 
        Paren e l  -> Paren <$> subst mapping e <*> pure l

-- 
-- ---------------------------------------------------------------
--      Lit a               ~~~~~~~~~~>     Lit a
-- 
        Lit{}      -> return expr

        Var name _ -> case Map.lookup (nameToText name) mapping of
            Just value -> return value
            Nothing    -> do
                scope <- ask
                case Map.lookup (nameToText name) scope of
                    Just (Just binding) -> do
                        after <- subst mapping binding
                        let before = Subst2 expr mapping
                        return $ Expand before after
                    Just Nothing -> return expr
                    Nothing      -> return expr

        Const name _ -> case Map.lookup (nameToText name) mapping of
            Just value -> return value
            Nothing    -> do
                scope <- ask
                case Map.lookup (nameToText name) scope of
                    Just (Just binding) -> do
                        after <- subst mapping binding
                        let before = Subst2 expr mapping
                        return $ Expand before after
                    Just Nothing -> return expr
                    Nothing      -> return expr

        Op{} -> return expr

        Chain a op b l ->
            Chain <$> subst mapping a <*> pure op <*> subst mapping b <*> pure l

        App f x l -> App <$> subst mapping f <*> subst mapping x <*> pure l

        Lam binder body l -> do

            -- rename the binder to avoid capturing only when necessary! 
            let (capturableNames, shrinkedMapping) =
                    getCapturableNames mapping body

            (binder', alphaRenameMapping) <- alphaRename capturableNames binder

            Lam binder'
                <$> subst (alphaRenameMapping <> shrinkedMapping) body
                <*> pure l

        Quant op binders range body l -> do
            -- rename binders to avoid capturing only when necessary! 
            let (capturableNames, shrinkedMapping) =
                    getCapturableNames mapping expr

            (binders', alphaRenameMapping) <-
                unzip <$> mapM (alphaRename capturableNames) binders

            -- combine individual renamings to get a new mapping 
            -- and use that mapping to rename other stuff
            let alphaRenameMappings = mconcat alphaRenameMapping

            Quant op binders'
                <$> subst (alphaRenameMappings <> shrinkedMapping) range
                <*> subst (alphaRenameMappings <> shrinkedMapping) body
                <*> pure l

        Subst{}  -> return expr
        Subst2{}  -> return expr
        Expand{}  -> return expr

        -- Expand (Expand e1 m1 _) m2 e3 -> traceShow "MERGE1" $ return (Expand e1 (m1 <> m2) e3)
        -- Expand e1 m1 (Expand _ m2 e3) -> traceShow "MERGE2" $return (Expand e1 (m1 <> m2) e3)

        -- Expand (Expand e1 m1 _) m2 e3 -> return (Expand e1 (m1 <> m2) e3)
        -- Expand e1 m1 (Expand _ m2 e3) -> return (Expand e1 (m1 <> m2) e3)

        -- Expand before after -> do 
        --     case before of 
        --         App (Expand before' after') x _ -> 

        --     return expr 
        --     -- Expand <$> subst mapping before <*> pure (mapping <> oldMapping) <*> subst mapping after

        ArrIdx array index l ->
            ArrIdx <$> subst mapping array <*> subst mapping index <*> pure l

        ArrUpd array index value l ->
            ArrUpd
                <$> subst mapping array
                <*> subst mapping index
                <*> subst mapping value
                <*> pure l

instance Substitutable Pred where
    subst mapping = \case
        Constant a    -> Constant <$> subst mapping a
        GuardIf   a l -> GuardIf <$> subst mapping a <*> pure l
        GuardLoop a l -> GuardLoop <$> subst mapping a <*> pure l
        Assertion a l -> Assertion <$> subst mapping a <*> pure l
        LoopInvariant a b l ->
            LoopInvariant <$> subst mapping a <*> subst mapping b <*> pure l
        Bound a l   -> Bound <$> subst mapping a <*> pure l
        Conjunct as -> Conjunct <$> mapM (subst mapping) as
        Disjunct as -> Disjunct <$> mapM (subst mapping) as
        Negate   a  -> Negate <$> subst mapping a


------------------------------------------------------------------
-- | Perform Alpha renaming only when necessary

-- rename a binder if it is in the set of "capturableNames"
-- returns the renamed binder and the mapping of alpha renaming (for renaming other stuff)
alphaRename :: Set Text -> Name -> M (Name, Mapping)
alphaRename capturableNames binder =
    if Set.member (nameToText binder) capturableNames
        -- CAPTURED! 
        -- returns the alpha renamed binder along with its mapping 
        then do
            binder' <- Name <$> freshWithLabel (nameToText binder) <*> pure
                (locOf binder)
            return
                ( binder'
                , Map.singleton (nameToText binder) (Var binder' (locOf binder))
                )
        -- not captured, returns the original binder 
        else return (binder, Map.empty)

-- returns a set of free names that is susceptible to capturing 
-- also returns a Mapping that is reduced further with free variables in "body" 
getCapturableNames :: Mapping -> Expr -> (Set Text, Mapping)
getCapturableNames mapping body =
    let
        -- collect all free variables in "body"
        freeVarsInBody  = Set.map nameToText (fv body)
        -- reduce the mapping further with free variables in "body" 
        shrinkedMapping = Map.restrictKeys mapping freeVarsInBody
        -- collect all free varialbes in the mapped expressions 
        mappedExprs     = Map.elems shrinkedMapping
        freeVarsInMappedExprs =
            Set.map nameToText $ Set.unions (map fv mappedExprs)
    in
        (freeVarsInMappedExprs, shrinkedMapping)
