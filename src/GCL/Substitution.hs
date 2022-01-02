{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module GCL.Substitution
  ( run
  , buildRedexMap
  , step
  , Scope
  -- TODO: don't export these
  , Substitutable
  , Reducible
  , CollectRedexes
  ) where

import           Control.Monad.RWS
import           Data.Foldable                  ( toList )
import qualified Data.IntMap                   as IntMap
import           Data.IntMap                    ( IntMap )
import qualified Data.List.NonEmpty            as NonEmpty
import           Data.Loc                       ( locOf )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import qualified Data.Maybe                    as Maybe
import qualified Data.Set                      as Set
import           Data.Set                       ( Set )
import           Data.Text                      ( Text )
import           GCL.Common                     ( Free(fv)
                                                , Fresh(..)
                                                , FreshState
                                                , fresh
                                                , freshWithLabel
                                                )
import           GCL.Predicate                  ( PO(PO)
                                                , Pred(..)
                                                )
import           Syntax.Abstract                ( CaseClause(..)
                                                , Expr(..)
                                                , FuncClause(..)
                                                , Mapping
                                                , Pattern(..)
                                                , extractBinder
                                                )
import           Syntax.Common                  ( Name(Name)
                                                , nameToText
                                                )
------------------------------------------------------------------

run
  :: MonadState FreshState m
  => (Substitutable a, Reducible a, CollectRedexes a)
  => Scope -- declarations
  -- -> Int -- initial redex ID counter
  -> [Name] -- name of variables to be substituted
  -> [Expr] -- values to be substituted for
  -> a
  -> m (a, IntMap (Int, Expr))
run scope names exprs predicate = runM scope $ do
  output <- subst mapping predicate >>= reduce
  return (output, buildRedexMap output)
 where
  mapping :: Mapping
  mapping = mappingFromSubstitution names exprs

buildRedexMap :: CollectRedexes a => a -> IntMap (Int, Expr)
buildRedexMap = IntMap.fromList . map (\(i, e) -> (i, (i, e))) . collectRedexes

step :: MonadState FreshState m => Scope -> Expr -> m Expr
step scope expr = runM scope $ go expr
 where
  go :: Expr -> M Expr
  go (App f x l) = App <$> go f <*> pure x <*> pure l >>= reduce
  go (RedexKernel _ value _ mappings) =
    foldM (flip subst) value (reverse $ toList mappings) >>= reduce
  go (RedexShell _ e) = go e
  go others           = return others


mappingFromSubstitution :: [Name] -> [Expr] -> Mapping
mappingFromSubstitution xs es =
  Map.mapKeys nameToText $ Map.fromList $ zip xs es

------------------------------------------------------------------

type Scope = Map Text (Maybe Expr)
type M = RWS Scope () Int

instance Fresh M where
  getCounter = get
  setCounter = put

runM :: MonadState FreshState m => Scope -> M b -> m b
runM scope p = do
  counter <- get
  let (output, counter', _) = runRWS p scope counter
  put counter'
  return output

------------------------------------------------------------------

class CollectRedexes a where
    collectRedexes :: a -> [(Int, Expr)]

instance CollectRedexes PO where
  collectRedexes (PO pre post _ _ _) =
    collectRedexes pre <> collectRedexes post

instance CollectRedexes Pred where
  collectRedexes predicate = case predicate of
    Constant x          -> collectRedexes x
    GuardIf   x _       -> collectRedexes x
    GuardLoop x _       -> collectRedexes x
    Assertion x _       -> collectRedexes x
    LoopInvariant x y _ -> collectRedexes x <> collectRedexes y
    Bound x _           -> collectRedexes x
    Conjunct xs         -> xs >>= collectRedexes
    Disjunct xs         -> xs >>= collectRedexes
    Negate   x          -> collectRedexes x

instance CollectRedexes Expr where
  collectRedexes expr = case expr of
    App x y _       -> collectRedexes x <> collectRedexes y
    Lam _ x _       -> collectRedexes x
    Quant _ _ _ x _ -> collectRedexes x
    RedexKernel{}   -> []
    RedexShell i e  -> [(i, e)]
    ArrIdx x y _    -> collectRedexes x <> collectRedexes y
    ArrUpd x y z _  -> collectRedexes x <> collectRedexes y <> collectRedexes z
    Case _ xs _     -> xs >>= collectRedexes
    _               -> []

instance CollectRedexes CaseClause where
  collectRedexes (CaseClause _ x) = collectRedexes x

------------------------------------------------------------------


class Reducible a where
    reduce :: a -> M a

instance Reducible Expr where
    -- perform substitution when there's a redex
  reduce expr = case expr of
    App f x l1 -> do
      f' <- reduce f
      x' <- reduce x
      case f' of
        RedexShell _ e -> RedexShell <$> fresh <*> reduce (App e x' l1)
        Lam n body _   -> subst (mappingFromSubstitution [n] [x']) body
        _              -> return $ App f' x' l1
    Lam binder body l -> Lam binder <$> reduce body <*> return l
    Quant op binders range body l ->
      Quant op binders range <$> reduce body <*> return l
    RedexShell i e       -> RedexShell i <$> reduce e
    ArrIdx array index l -> ArrIdx <$> reduce array <*> reduce index <*> pure l
    ArrUpd array index value l ->
      ArrUpd <$> reduce array <*> reduce index <*> reduce value <*> pure l
    _ -> return expr

instance Reducible Pred where
  reduce = \case
    Constant a          -> Constant <$> reduce a
    GuardIf   a l       -> GuardIf <$> reduce a <*> pure l
    GuardLoop a l       -> GuardLoop <$> reduce a <*> pure l
    Assertion a l       -> Assertion <$> reduce a <*> pure l
    LoopInvariant a b l -> LoopInvariant <$> reduce a <*> reduce b <*> pure l
    Bound a l           -> Bound <$> reduce a <*> pure l
    Conjunct as         -> Conjunct <$> mapM reduce as
    Disjunct as         -> Disjunct <$> mapM reduce as
    Negate   a          -> Negate <$> reduce a

------------------------------------------------------------------

class Substitutable a where
    subst :: Mapping -> a -> M a

instance Substitutable Expr where
  subst mapping expr = case expr of

    Lit{}      -> return expr

    Var name _ -> case Map.lookup (nameToText name) mapping of
      Just value -> return value
      Nothing    -> do
        scope <- ask
        index <- fresh
        case Map.lookup (nameToText name) scope of
          Just (Just binding) -> do
            let e = RedexKernel
                  name
                  binding
                  (fv binding)
                  -- NonEmpty.singleton is only available after base-4.15
                  (NonEmpty.fromList [shrinkMapping binding mapping])
            return $ RedexShell index e
          Just Nothing -> return expr
          Nothing      -> return expr

    Const name _ -> case Map.lookup (nameToText name) mapping of
      Just value -> reduce value
      Nothing    -> do
        scope <- ask
        index <- fresh
        case Map.lookup (nameToText name) scope of
          Just (Just binding) -> do
            let e = RedexKernel
                  name
                  binding
                  (fv binding)
                  -- NonEmpty.singleton is only available after base-4.15
                  (NonEmpty.fromList [shrinkMapping binding mapping])
            return $ RedexShell index e
          Just Nothing -> return expr
          Nothing      -> return expr

    Op{}              -> return expr

    App f      x    l -> App <$> subst mapping f <*> subst mapping x <*> pure l

    Lam binder body l -> do

      -- we need to rename binders to avoid capturing
      -- only the free vars that is also present in `body` will be renamed 
      let (capturableNames, shrinkedMapping) =
            getCapturableNamesAndShrinkMapping mapping body

      -- rename captured binder 
      renamings <- produceBinderRenamings capturableNames [binder]
      let renamedBinder = renameBinder renamings binder

      Lam renamedBinder
        <$> subst (renamingToMapping renamings <> shrinkedMapping) body
        <*> pure l

    Func name clauses l ->
      Func name <$> mapM (subst mapping) clauses <*> pure l

    Tuple es                      -> Tuple <$> mapM (subst mapping) es

    Quant op binders range body l -> do
      -- rename binders to avoid capturing only when necessary!
      let (capturableNames, shrinkedMapping) =
            getCapturableNamesAndShrinkMapping mapping expr

      -- rename captured binder 
      renamings <- produceBinderRenamings capturableNames binders
      let renamedBinders = map (renameBinder renamings) binders

      Quant op renamedBinders
        <$> subst (renamingToMapping renamings <> shrinkedMapping) range
        <*> subst (renamingToMapping renamings <> shrinkedMapping) body
        <*> pure l

        -- apply new mappings on the outside instead of merging them (Issue #54)
        -- NOTE:
        --      when shrinking the applied outer new mapping
        --      free variables occured from the inner old mapping
        --      should be taken into consideration
    RedexKernel name e freeVars mappings ->
      let
        removeSubstitutedVars :: Mapping -> Set Name -> Set Name
        removeSubstitutedVars m =
          Set.filter (\v -> not $ Set.member (nameToText v) (Map.keysSet m))


        outermostMapping = NonEmpty.head mappings

        freeVars' =
          freeVars <> Set.unions (map fv $ Map.elems outermostMapping)
        newFreeVars = removeSubstitutedVars mapping freeVars
          <> Set.unions (map fv $ Map.elems outermostMapping)

        shrinkedMapping =
          Map.restrictKeys mapping (Set.map nameToText freeVars')
      in
        return $ RedexKernel name
                             e
                             newFreeVars
                             (NonEmpty.cons shrinkedMapping mappings)

    RedexShell _ e -> RedexShell <$> fresh <*> subst mapping e

    ArrIdx array index l ->
      ArrIdx <$> subst mapping array <*> subst mapping index <*> pure l

    ArrUpd array index value l ->
      ArrUpd
        <$> subst mapping array
        <*> subst mapping index
        <*> subst mapping value
        <*> pure l

    -- apply subst on body of cases only
    Case e cases l -> do
      cases' <- forM cases
        $ \(CaseClause patt body) -> CaseClause patt <$> subst mapping body
      return $ Case e cases' l

instance Substitutable FuncClause where
  subst mapping (FuncClause patterns body) = do
    -- we need to rename binders to avoid capturing
    -- only the free vars that is also present in `body` will be renamed 
    let (capturableNames, shrinkedMapping) =
          getCapturableNamesAndShrinkMapping mapping body

    -- renaming captured binders in patterns 
    let bindersInPatterns = patterns >>= extractBinder
    renamings <- produceBinderRenamings capturableNames bindersInPatterns
    let renamedPatterns = map (renameBindersInPattern renamings) patterns

    FuncClause renamedPatterns
      <$> subst (renamingToMapping renamings <> shrinkedMapping) body

renameBindersInPattern :: Map Name Name -> Pattern -> Pattern
renameBindersInPattern renamings patt = case patt of
  PattLit      _      -> patt
  PattBinder   binder -> PattBinder $ renameBinder renamings binder
  PattWildcard _      -> patt
  PattConstructor name patts ->
    PattConstructor name $ map (renameBindersInPattern renamings) patts

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

-- produce a binder "renaming", if any binder is in the set of "capturableNames"
produceBinderRenamings :: Set Text -> [Name] -> M (Map Name Name)
produceBinderRenamings capturableNames binders = mconcat <$> mapM go binders
 where
  go :: Name -> M (Map Name Name)
  go binder = if Set.member (nameToText binder) capturableNames
    then do
      -- CAPTURED! returns the alpha renamed binder
      binder' <- Name <$> freshWithLabel (nameToText binder) <*> pure
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
    . map (\(old, new) -> (nameToText old, Var new (locOf old)))
    . Map.toList

------------------------------------------------------------------
-- | Perform Alpha renaming only when necessary
-- returns a set of free names that is susceptible to capturing
-- also returns a Mapping that is reduced further with only free variables in "body"
getCapturableNamesAndShrinkMapping :: Mapping -> Expr -> (Set Text, Mapping)
getCapturableNamesAndShrinkMapping mapping body =
  let
    -- collect all free variables in "body"
    -- and reduce the mapping further with free variables in "body"
    shrinkedMapping = shrinkMapping body mapping
    -- collect all free varialbes in the mapped expressions
    mappedExprs     = Map.elems shrinkedMapping
    freeVarsInMappedExprs =
      Set.map nameToText $ Set.unions (map fv mappedExprs)
  in
    (freeVarsInMappedExprs, shrinkedMapping)


------------------------------------------------------------------
-- | Shrink the Mapping in Substs

shrinkMapping :: Expr -> Mapping -> Mapping
shrinkMapping expr mapping =
  let
      -- collect all free variables in the expression
      freeVars = Set.map nameToText (fv expr)
      -- restrict the mapping with only free variables
  in  Map.restrictKeys mapping freeVars
