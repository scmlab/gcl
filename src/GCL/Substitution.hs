{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module GCL.Substitution
  ( run
  , Scope
  -- TODO: don't export these 
  , Substitutable
  , Reducible
  , CollectRedexes(collectRedexes)
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
import           GCL.Predicate                  ( PO(PO)
                                                , Pred(..)
                                                )
import           Syntax.Abstract                ( Case(CaseConstructor)
                                                , Expr(..)
                                                , Mapping
                                                )
import           Syntax.Common                  ( Name(Name)
                                                , nameToText
                                                )
------------------------------------------------------------------

run
  :: (Substitutable a, Reducible a, CollectRedexes a)
  => Scope -- declarations
  -> Int -- initial redex ID counter
  -> [Name] -- name of variables to be substituted
  -> [Expr] -- values to be substituted for
  -> a
  -> (a, [(Int, Expr)], Int)
run scope index names exprs predicate =
  let (output, (_, index'), _) =
        runRWS (subst mapping predicate >>= reduce) scope (0, index)
  in  (output, collectRedexes output, index')
 where
  mapping :: Mapping
  mapping = mappingFromSubstitution names exprs

mappingFromSubstitution :: [Name] -> [Expr] -> Mapping
mappingFromSubstitution xs es =
  Map.mapKeys nameToText $ Map.fromList $ zip xs es

------------------------------------------------------------------

type Scope = Map Text (Maybe Expr)
type M = RWS Scope () (Int, Int)

-- for alpha-renaming 
instance Fresh M where
  fresh = do
    (x, i) <- get
    put (succ x, i)
    return x

-- for indexing redexes
freshRedexIndex :: M Int
freshRedexIndex = do
  (x, i) <- get
  put (x, succ i)
  return i

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
    App x y _                   -> collectRedexes x <> collectRedexes y
    Lam _ x _                   -> collectRedexes x
    Quant _ _ _ x _             -> collectRedexes x
    DisplaySubst x     _      _ -> collectRedexes x
    Expand       index before _ -> [(index, before)]
    ArrIdx       x     y      _ -> collectRedexes x <> collectRedexes y
    ArrUpd x y z _ -> collectRedexes x <> collectRedexes y <> collectRedexes z
    Case _ xs _                 -> xs >>= collectRedexes
    _                           -> []

instance CollectRedexes Case where
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
        Expand index before (Lam n body l2) ->
          Expand index <$> reduce (App before x' l1) <*> reduce
            (App (Lam n body l2) x' l1)
        -- [reduce-App-Lam]
        Lam n body _ -> subst (mappingFromSubstitution [n] [x']) body
        -- [Others]
        _            -> return $ App f' x' l1
    Lam binder body l -> Lam binder <$> reduce body <*> return l
    Quant op binders range body l ->
      Quant op binders range <$> reduce body <*> return l
    DisplaySubst e freeVarsInE mapping ->
      DisplaySubst <$> reduce e <*> return freeVarsInE <*> return mapping
    Expand index before after ->
      Expand index <$> reduce before <*> reduce after
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
        index <- freshRedexIndex
        case Map.lookup (nameToText name) scope of
            -- [subst-Var-defined]
          Just (Just binding) -> do
            after <- subst mapping binding
            let before = DisplaySubst expr
                                      (fv binding)
                                      (shrinkMapping binding mapping)
            return $ Expand index before after
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
        index <- freshRedexIndex
        case Map.lookup (nameToText name) scope of
            -- [subst-Const-defined]
          Just (Just binding) -> do
            after <- subst mapping binding
            let before = DisplaySubst expr
                                      (fv binding)
                                      (shrinkMapping binding mapping)
            return $ Expand index before after
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
        --      when shrinking the applied outer new mapping (`mapping` in this case)
        --      free variables occured from the inner old mapping (`mapping'` in this case)
        --      should be taken into consideration
    DisplaySubst e freeVarsInE mapping' ->
      let
        freeVarsInMapping' = fv mapping'
        freeVars           = freeVarsInE <> freeVarsInMapping'
        shrinkedMapping =
          Map.restrictKeys mapping (Set.map nameToText freeVars)
      in
        return $ DisplaySubst (DisplaySubst e freeVarsInE mapping')
                              freeVars
                              shrinkedMapping
--
--      a                   ~[.../...]~>    a'
--      b                   ~[.../...]~>    b'
-- ---------------------------------------------------------------[subst-Expand]
--      a ===> b            ~[.../...]~>    a' ===> b'
--
    Expand index before after ->
      Expand index <$> subst mapping before <*> subst mapping after

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
