{-# LANGUAGE FlexibleInstances #-}
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
import qualified Data.Set                      as Set
import           Data.Set                       ( Set )
import           Data.Text                      ( Text )
import           GCL.Common                     ( Free(fv)
                                                , Fresh(..)
                                                )
import           GCL.Predicate                  ( PO(PO)
                                                , Pred(..)
                                                )
import           Syntax.Abstract                ( CaseConstructor
                                                  ( CaseConstructor
                                                  )
                                                , Expr(..)
                                                , Mapping
                                                , Redex(..)
                                                )
import           Syntax.Common                  ( Name(Name)
                                                , nameToText
                                                )
------------------------------------------------------------------

run
  :: Fresh m
  => (Substitutable a, Reducible a, CollectRedexes a)
  => Scope -- declarations
  -- -> Int -- initial redex ID counter
  -> [Name] -- name of variables to be substituted
  -> [Expr] -- values to be substituted for
  -> a
  -> m (a, IntMap Redex)
run scope names exprs predicate = runM scope $ do
  output <- subst mapping predicate >>= reduce
  return (output, buildRedexMap output)
 where
  mapping :: Mapping
  mapping = mappingFromSubstitution names exprs

buildRedexMap :: CollectRedexes a => a -> IntMap Redex
buildRedexMap =
  IntMap.fromList . map (\redex -> (redexID redex, redex)) . collectRedexes

step :: Fresh m => Scope -> Expr -> m Expr
step scope expr = runM scope $ go expr
 where
  go :: Expr -> M Expr
  go (App f x l) = App <$> go f <*> pure x <*> pure l >>= reduce
  go (RedexStem _ value _ mappings) =
    foldM (flip subst) value (reverse $ toList mappings) >>= reduce
  go (Redex redex) = go (redexExpr redex)
  go others        = return others


mappingFromSubstitution :: [Name] -> [Expr] -> Mapping
mappingFromSubstitution xs es =
  Map.mapKeys nameToText $ Map.fromList $ zip xs es

------------------------------------------------------------------

type Scope = Map Text (Maybe Expr)
type M = RWS Scope () Int

runM :: Fresh m => Scope -> M b -> m b
runM scope p = do
  counter <- getCounter
  let (output, counter', _) = runRWS p scope counter
  setCounter counter'
  return output

-- for alpha-renaming 
instance Fresh M where
  getCounter = get
  setCounter = put

------------------------------------------------------------------

class CollectRedexes a where
    collectRedexes :: a -> [Redex]

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
    RedexStem{}     -> []
    Redex redex     -> [redex]
    ArrIdx x y _    -> collectRedexes x <> collectRedexes y
    ArrUpd x y z _  -> collectRedexes x <> collectRedexes y <> collectRedexes z
    Case _ xs _     -> xs >>= collectRedexes
    _               -> []

instance CollectRedexes CaseConstructor where
  collectRedexes (CaseConstructor _ _ x) = collectRedexes x

------------------------------------------------------------------



--      a                  x    ~~~~~~~~~~~>    b
--             (\n . body) x    ~~~~~~~~~~~>           c
-- --------------------------------------------------------------- [reduce-App-Expand-Lam]
--      (a ===> \n . body) x    ~~~~~~~~~~~>    b ===> c
--
--
--      body                    ~[ x / n ]~>    b
-- --------------------------------------------------------------- [reduce-App-Lam]
--      (\n . body) x           ~~~~~~~~~~~>    b
--
--
-- --------------------------------------------------------------- [reduce-Others]
--      other constructs        ~~~~~~~~~~~>    other constructs
--

class Reducible a where
    reduce :: a -> M a

instance Reducible Expr where
    -- perform substitution when there's a redex
  reduce expr = case expr of
    App f x l1 -> do
      f' <- reduce f
      x' <- reduce x
      case f' of
          -- [reduce-App-Expand-Lam]
        Redex (Rdx _ e) -> Redex <$> (Rdx <$> fresh <*> reduce (App e x' l1))
        -- [reduce-App-Lam]
        Lam n body _    -> subst (mappingFromSubstitution [n] [x']) body
        -- [Others]
        _               -> return $ App f' x' l1
    Lam binder body l -> Lam binder <$> reduce body <*> return l
    Quant op binders range body l ->
      Quant op binders range <$> reduce body <*> return l
    Redex redex          -> Redex <$> reduce redex
    ArrIdx array index l -> ArrIdx <$> reduce array <*> reduce index <*> pure l
    ArrUpd array index value l ->
      ArrUpd <$> reduce array <*> reduce index <*> reduce value <*> pure l
    _ -> return expr

instance Reducible Redex where
  reduce (Rdx index expr) = Rdx index <$> reduce expr

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

--
-- ---------------------------------------------------------------[subst-Lit]
--      Lit a               ~[.../...]~>    Lit a
--
    Lit{}      -> return expr

--
--      a                   ~~~~~~~~~~~>    a'
-- ---------------------------------------------------------------[subst-Var-substituted]
--      Var x               ~[ a / x ]~>    a'
--
--
--      x                   is defined as   a
--      a                   ~[.../...]~>    a'
-- ---------------------------------------------------------------[subst-Var-defined]
--      Var x               ~[.../...]~>    Var x ===> a'
--
--
--      x                   is not defined
-- ---------------------------------------------------------------[subst-Var-not-defined]
--      Var x               ~[.../...]~>    Var x
--
    Var name _ -> case Map.lookup (nameToText name) mapping of
      Just value -> return value -- [subst-Var-substituted]
      Nothing    -> do
        scope <- ask
        index <- fresh
        case Map.lookup (nameToText name) scope of
            -- [subst-Var-defined]
          Just (Just binding) -> do
            let e = RedexStem
                  name
                  binding
                  (fv binding)
                  -- NonEmpty.singleton is only available after base-4.15
                  (NonEmpty.fromList [shrinkMapping binding mapping])
            return $ Redex (Rdx index e)
          -- [subst-Var-defined]
          Just Nothing -> return expr
          Nothing      -> return expr

--
--      a                   ~~~~~~~~~~~>    a'
-- ---------------------------------------------------------------[subst-Const-substituted]
--      Const x             ~[ a / x ]~>    a'
--
--
--      x                   is defined as   a
--      a                   ~[.../...]~>    a'
-- ---------------------------------------------------------------[subst-Const-defined]
--      Const x             ~[.../...]~>    Const x ===> a'
--
--
--      x                   is not defined
-- ---------------------------------------------------------------[subst-Const-not-defined]
--      Const x             ~[.../...]~>    Const x
--
    Const name _ -> case Map.lookup (nameToText name) mapping of
      Just value -> reduce value -- [subst-Const-substituted]
      Nothing    -> do
        scope <- ask
        index <- fresh
        case Map.lookup (nameToText name) scope of
            -- [subst-Const-defined]
          Just (Just binding) -> do
            let e = RedexStem
                  name
                  binding
                  (fv binding)
                  -- NonEmpty.singleton is only available after base-4.15
                  (NonEmpty.fromList [shrinkMapping binding mapping])
            return $ Redex (Rdx index e)
          -- [subst-Const-not-defined]
          Just Nothing -> return expr
          Nothing      -> return expr

--
-- ---------------------------------------------------------------[subst-Op]
--      Op a                ~[.../...]~>    Op a
--
    Op{}              -> return expr

--
--      a                   ~[.../...]~>    a'
--      b                   ~[.../...]~>    b'
-- ---------------------------------------------------------------[subst-Chain]
--      Chan a op b         ~[.../...]~>    Op a' op b'
--
    --Chain a op b l ->
        --Chain <$> subst mapping a <*> pure op <*> subst mapping b <*> pure l

--
--      f                   ~[.../...]~>    f'
--      x                   ~[.../...]~>    x'
--      f' x'               ~~~~~~~~~~~>    y
-- ---------------------------------------------------------------[subst-App]
--      f  x                ~[.../...]~>    y
--
    App f      x    l -> App <$> subst mapping f <*> subst mapping x <*> pure l

--
--      n                   ~~~rename~~>    n'
--      body                ~[.../...]~>    body'
-- ---------------------------------------------------------------[subst-Lam]
--      \n . body           ~[.../...]~>    \n' . body'
--
    Lam binder body l -> do

        -- rename the binder to avoid capturing only when necessary!
      let (capturableNames, shrinkedMapping) = getCapturableNames mapping body

      (binder', alphaRenameMapping) <- rename capturableNames binder

      Lam binder'
        <$> subst (alphaRenameMapping <> shrinkedMapping) body
        <*> pure l

--
--      ns                  ~~~rename~~>    ns'
--      a                   ~[.../...]~>    a'
--      b                   ~[.../...]~>    b'
-- ---------------------------------------------------------------[subst-Quant]
--      Quant op ns a b     ~[.../...]~>    Quant op ns' a' b'
--
    Quant op binders range body l -> do
        -- rename binders to avoid capturing only when necessary!
      let (capturableNames, shrinkedMapping) = getCapturableNames mapping expr

      (binders', alphaRenameMapping) <-
        unzip <$> mapM (rename capturableNames) binders

      -- combine individual renamings to get a new mapping
      -- and use that mapping to rename other stuff
      let alphaRenameMappings = mconcat alphaRenameMapping

      Quant op binders'
        <$> subst (alphaRenameMappings <> shrinkedMapping) range
        <*> subst (alphaRenameMappings <> shrinkedMapping) body
        <*> pure l

--
-- ---------------------------------------------------------------[subst-Subst]
--      Subst a mapping     ~[.../...]~>    Subst (Subst a mapping) mapping'
--

        -- apply new mappings on the outside instead of merging them (Issue #54)
        -- NOTE:
        --      when shrinking the applied outer new mapping
        --      free variables occured from the inner old mapping
        --      should be taken into consideration
    RedexStem name e freeVars mappings ->
      let
        removeSubstitutedVars :: Mapping -> Set Name -> Set Name
        removeSubstitutedVars m = Set.filter (\v -> not $ Set.member (nameToText v) (Map.keysSet m))

    
        outermostMapping = NonEmpty.head mappings

        freeVars' = freeVars <> Set.unions (map fv $ Map.elems outermostMapping)
        newFreeVars = removeSubstitutedVars mapping freeVars <> Set.unions (map fv $ Map.elems outermostMapping)

        shrinkedMapping =
          Map.restrictKeys mapping (Set.map nameToText freeVars')
      in
        return $ RedexStem name
                           e
                           newFreeVars
                           (NonEmpty.cons shrinkedMapping mappings)
--
--      a                   ~[.../...]~>    a'
--      b                   ~[.../...]~>    b'
-- ---------------------------------------------------------------[subst-Expand]
--      a ===> b            ~[.../...]~>    a' ===> b'
--
    Redex (Rdx _ e) -> Redex <$> (Rdx <$> fresh <*> subst mapping e)

--
--      a                   ~[.../...]~>    a'
--      b                   ~[.../...]~>    b'
-- ---------------------------------------------------------------[subst-ArrIdx]
--      ArrIdx a b          ~[.../...]~>    ArrIdx a b
--
    ArrIdx array index l ->
      ArrIdx <$> subst mapping array <*> subst mapping index <*> pure l

--
--      a                   ~[.../...]~>    a'
--      b                   ~[.../...]~>    b'
--      c                   ~[.../...]~>    c'
-- ---------------------------------------------------------------[subst-ArrUpd]
--      ArrUpd a b c        ~[.../...]~>    ArrUpd a b c
--
    ArrUpd array index value l ->
      ArrUpd
        <$> subst mapping array
        <*> subst mapping index
        <*> subst mapping value
        <*> pure l

    -- apply subst on body of cases only
    Case e cases l -> do
      cases' <- forM cases $ \(CaseConstructor ctor binders body) ->
        CaseConstructor ctor binders <$> subst mapping body
      return $ Case e cases' l

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
rename :: Set Text -> Name -> M (Name, Mapping)
rename capturableNames binder =
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
