{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module GCL.Substitute
    ( run
    , Scope
    , Binding(..)
    , scopeFromLetBindings
    , mappingFromSubstitution
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
import           Pretty                         ( (<+>)
                                                , Pretty(pretty)
                                                )
import           Syntax.Abstract                ( Bindings(..)
                                                , Expr(..)
                                                )
import           Syntax.Abstract.Util           ( bindingsToExpr )
import           Syntax.Common                  ( Name(Name)
                                                , nameToText
                                                )


------------------------------------------------------------------

run :: Subst a => Scope -> Mapping -> a -> a
run scope mapping predicate = fst $ evalRWS (subst mapping predicate) scope 0

-- runExpr :: [Scope] -> Expr -> Expr
-- runExpr scopes x = evalState (subst (mconcat scopes) x) 0

-- substScopes :: Mapping -> [Scope] -> [Scope]
-- substScopes mapping scopes = traceShow ("substScopes", pretty mapping, pretty scopes, pretty $ evalState (mapM (substScope mapping) scopes) 0) evalState (mapM (substScope mapping) scopes) 0

------------------------------------------------------------------

-- | A "Scope" is a mapping from names to Bindings 
type Scope = Map Text Binding

scopeFromLetBindings :: Map Name (Maybe Bindings) -> Scope
scopeFromLetBindings = Map.mapKeys nameToText . fmap toBinding
  where
    toBinding Nothing = NoBinding
    toBinding (Just (LetBinding x)) = UserDefinedBinding x
    toBinding (Just others) = UserDefinedBinding (bindingsToExpr others)

-- scopeFromSubstitution :: [Name] -> [Expr] -> Scope
-- scopeFromSubstitution xs es =
--     Map.mapKeys nameToText $ Map.fromList $ zip xs (map SubstitutionBinding es)

mappingFromSubstitution :: [Name] -> [Expr] -> Mapping
mappingFromSubstitution xs es =
    Map.mapKeys nameToText $ Map.fromList $ zip xs es

------------------------------------------------------------------

data Binding
    = UserDefinedBinding Expr
    | NoBinding
    deriving (Show)

type Mapping = Map Text Expr

type M = RWS Scope () Int

instance Fresh M where
    fresh = do
        i <- get
        put (succ i)
        return i

instance Free Binding where
    fv (UserDefinedBinding  x) = fv x
    fv NoBinding               = Set.empty

instance Pretty (Map Text Binding) where
    pretty = pretty . Map.toList

instance Pretty Binding where
    pretty (UserDefinedBinding expr) = "UserDefinedBinding" <+> pretty expr
    pretty NoBinding = "NoBinding"

------------------------------------------------------------------

-- perform substitution when there's a redex
-- reduceExpr :: Mapping -> Expr -> M Expr
-- reduceExpr mapping expr = case expr of
--     App f x _ -> case f of
--         Expand _ _ (Lam binder body _) -> do
--             let
--                 mapping' = Map.insert (nameToText binder)
--                                       (SubstitutionBinding x)
--                                       mapping
--             -- perform substitution
--             Expand [] expr <$> subst mapping' body

--         Lam binder body _ -> do
--             let
--                 mapping' = Map.insert (nameToText binder)
--                                       (SubstitutionBinding x)
--                                       mapping
--             -- perform substitution
--             subst mapping' body

--         _ -> return expr
--     _ -> return expr

-- perform substitution when there's a redex
reduce :: Mapping -> Expr -> M Expr
reduce mapping expr = case expr of
    App f x _ -> case f of
        Expand _ _ (Lam binder body _) -> do
            let mapping' = mappingFromSubstitution [binder] [x] <> mapping
            -- perform substitution
            Expand [] expr <$> subst mapping' body

        Lam binder body _ -> do
            let mapping' = mappingFromSubstitution [binder] [x] <> mapping
            subst mapping' body

        _ -> return expr
    _ -> return expr

------------------------------------------------------------------

class Subst a where
    subst :: Mapping -> a -> M a

instance Subst Pred where
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

instance Subst Expr where
    subst mapping expr = reduce mapping =<< case expr of

        Paren e l  -> Paren <$> subst mapping e <*> pure l

        Lit{}      -> return expr

        Var name _ -> case Map.lookup (nameToText name) mapping of
            Just value -> return value
            Nothing    -> do
                scope <- ask
                case Map.lookup (nameToText name) scope of
                    Just (UserDefinedBinding  binding) -> return $ Expand [] expr binding
                    Just NoBinding                     -> return expr
                    Nothing   -> return expr

        Const name _ -> case Map.lookup (nameToText name) mapping of
            Just value -> return value
            Nothing    -> do
                scope <- ask
                case Map.lookup (nameToText name) scope of
                    Just (UserDefinedBinding  binding) -> return $ Expand [] expr binding
                    Just NoBinding                     -> return expr
                    Nothing   -> return expr

        Op{} -> return expr

        Chain a op b l ->
            Chain
                <$> subst mapping a
                <*> pure op
                <*> subst mapping b
                <*> pure l

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

        Expand{} -> return expr

        ArrIdx array index l ->
            ArrIdx
                <$> subst mapping array
                <*> subst mapping index
                <*> pure l

        ArrUpd array index value l ->
            ArrUpd
                <$> subst mapping array
                <*> subst mapping index
                <*> subst mapping value
                <*> pure l

-- subst2 :: Scope -> Mapping -> Expr -> M Expr
-- subst2 scope mapping expr = reduceExpr mapping =<< case expr of
--     _ -> return expr 


------------------------------------------------------------------

-- substScope :: Mapping -> Scope -> M Scope
-- substScope mapping = mapM (substBinding mapping)

-- substBinding :: Mapping -> Binding -> M Binding
-- substBinding _ NoBinding = return NoBinding
-- substBinding mapping (SubstitutionBinding expr) =
--     SubstitutionBinding <$> subst mapping expr
-- substBinding mapping (UserDefinedBinding expr) =
--     UserDefinedBinding <$> subst mapping expr

-- substPred :: Mapping -> Pred -> M Pred
-- substPred mapping = \case
--     Constant a    -> Constant <$> subst mapping a
--     GuardIf   a l -> GuardIf <$> subst mapping a <*> pure l
--     GuardLoop a l -> GuardLoop <$> subst mapping a <*> pure l
--     Assertion a l -> Assertion <$> subst mapping a <*> pure l
--     LoopInvariant a b l ->
--         LoopInvariant <$> subst mapping a <*> subst mapping b <*> pure l
--     Bound a l   -> Bound <$> subst mapping a <*> pure l
--     Conjunct as -> Conjunct <$> mapM (substPred mapping) as
--     Disjunct as -> Disjunct <$> mapM (substPred mapping) as
--     Negate   a  -> Negate <$> substPred mapping a


-- subst :: Mapping -> Expr -> M Expr
-- subst mapping expr = reduceExpr mapping =<< case expr of

--     Paren e l  -> Paren <$> subst mapping e <*> pure l

--     Lit{}      -> return expr

--     Var name _ -> case Map.lookup (nameToText name) mapping of
--         Nothing                            -> return expr
--         Just (UserDefinedBinding  binding) -> return $ Expand [] expr binding
--         Just (SubstitutionBinding binding) -> return binding
--         Just NoBinding                     -> return expr

--     Const name _ -> case Map.lookup (nameToText name) mapping of
--         Nothing                            -> return expr
--         Just (UserDefinedBinding  binding) -> return $ Expand [] expr binding
--         Just (SubstitutionBinding binding) -> return binding
--         Just NoBinding                     -> return expr

--     Op{} -> return expr

--     Chain a op b l ->
--         Chain
--             <$> subst mapping a
--             <*> pure op
--             <*> subst mapping b
--             <*> pure l

--     App f x l -> App <$> subst mapping f <*> subst mapping x <*> pure l

--     Lam binder body l -> do

--         -- rename the binder to avoid capturing only when necessary! 
--         let (capturableNames, shrinkedMapping) =
--                 getCapturableNames mapping body

--         (binder', alphaRenameMapping) <- alphaRename capturableNames binder

--         Lam binder'
--             <$> subst (alphaRenameMapping <> shrinkedMapping) body
--             <*> pure l

--     Quant op binders range body l -> do
--         -- rename binders to avoid capturing only when necessary! 
--         let (capturableNames, shrinkedMapping) =
--                 getCapturableNames mapping expr

--         (binders', alphaRenameMapping) <-
--             unzip <$> mapM (alphaRename capturableNames) binders

--         -- combine individual renamings to get a new mapping 
--         -- and use that mapping to rename other stuff
--         let alphaRenameMappings = mconcat alphaRenameMapping

--         Quant op binders'
--             <$> subst (alphaRenameMappings <> shrinkedMapping) range
--             <*> subst (alphaRenameMappings <> shrinkedMapping) body
--             <*> pure l

--     Subst{}  -> return expr

--     Expand{} -> return expr

--     ArrIdx array index l ->
--         ArrIdx
--             <$> subst mapping array
--             <*> subst mapping index
--             <*> pure l

--     ArrUpd array index value l ->
--         ArrUpd
--             <$> subst mapping array
--             <*> subst mapping index
--             <*> subst mapping value
--             <*> pure l

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
