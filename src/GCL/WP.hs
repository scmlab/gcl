{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module GCL.WP where

import           Control.Arrow                  ( first )
import           Control.Monad.Except           ( Except
                                                , MonadError(throwError)
                                                , forM
                                                , forM_
                                                , runExcept
                                                , unless
                                                , when
                                                )
import           Control.Monad.RWS              ( MonadReader(ask)
                                                , MonadState(..)
                                                , MonadWriter(..)
                                                , RWST
                                                , evalRWST
                                                , withRWST
                                                )
import qualified Data.Hashable                 as Hashable
import qualified Data.List                     as List
import           Data.Loc                       ( Loc(..)
                                                , Located(..)
                                                )
import           Data.Loc.Range                 ( Range
                                                , fromLoc
                                                )
import qualified Data.Map                      as Map
import qualified Data.Text                     as Text
import           GCL.Common                     ( Fresh(..)
                                                , freshName'
                                                )
import           GCL.Predicate                  ( Origin(..)
                                                , PO(..)
                                                , Pred(..)
                                                , Spec(Specification)
                                                )
import           GCL.Predicate.Util             ( conjunct
                                                , disjunct
                                                , guardIf
                                                , guardLoop
                                                , toExpr
                                                )
import qualified GCL.Substitute                as Substitute
import           GCL.WP.Type
import           Numeric                        ( showHex )
import           Pretty                         ( toString, toText )
import qualified Syntax.Abstract               as A
import qualified Syntax.Abstract.Operator      as A
import qualified Syntax.Abstract.Util          as A
import           Syntax.Common                  ( Name(Name)
                                                , nameToText
                                                )

type TM = Except StructError

type WP
  = RWST [Substitute.Scope] ([PO], [Spec], [StructWarning]) (Int, Int, Int) TM

instance Fresh WP where
  fresh = do
    (i, j, k) <- get
    put (i, j, succ k)
    return k

runWP
  :: WP a
  -> [Substitute.Scope]
  -> Either StructError (a, ([PO], [Spec], [StructWarning]))
runWP p defs = runExcept $ evalRWST p defs (0, 0, 0)

sweep :: A.Program -> Either StructError ([PO], [Spec], [StructWarning])
sweep (A.Program decls _ _ stmts _) = do
  let scope = Map.mapKeys nameToText $ Map.map
        (maybe Substitute.NoBinding Substitute.UserDefinedBinding)
        (A.extractDeclarations decls)
  (_, (pos, specs, warnings)) <- runWP (structProgram stmts) [scope]
  -- update Proof Obligations with corresponding Proof Anchors


  let proofAnchors = stmts >>= \case
        A.Proof anchors _ -> anchors
        _                 -> []
  -- make a table of (#hash, range) from Proof Anchors 
  let table = Map.fromList
        $ map (\(A.ProofAnchor hash range) -> (hash, range)) proofAnchors
  let updatePO po = case Map.lookup (poAnchorHash po) table of
        Nothing    -> po
        Just range -> po { poAnchorLoc = Just range }

  let pos' = map updatePO pos

  return (pos', specs, warnings)

data ProgView
  = ProgViewEmpty
  | ProgViewOkay Pred [A.Stmt] Pred
  | ProgViewMissingPrecondition [A.Stmt] Pred
  | ProgViewMissingPostcondition Pred [A.Stmt]
  | ProgViewMissingBoth [A.Stmt]

progView :: [A.Stmt] -> ProgView
progView []               = ProgViewEmpty
progView [A.Assert pre l] = do
  ProgViewMissingPrecondition [] (Assertion pre l)
progView stmts = do
  case (head stmts, last (removeLastProofs stmts)) of
    (A.Assert pre l, A.Assert post m) -> do
      ProgViewOkay (Assertion pre l)
                   (init (tail (removeLastProofs stmts)))
                   (Assertion post m)
    (A.Assert pre l, _) -> do
      ProgViewMissingPostcondition (Assertion pre l)
                                   (tail (removeLastProofs stmts))
    (_, A.Assert post m) -> do
      ProgViewMissingPrecondition (init stmts) (Assertion post m)
    _ -> ProgViewMissingBoth stmts
 where
    -- ignore Proofs after the Postcondition
  removeLastProofs :: [A.Stmt] -> [A.Stmt]
  removeLastProofs = List.dropWhileEnd isProof

  isProof :: A.Stmt -> Bool
  isProof A.Proof{} = True
  isProof _         = False


-- alphaSubst :: (Substitutable A.Bindings b, AlphaRename WP b) => Subs A.Bindings -> b -> WP b
-- alphaSubst sub e = do
--   env <- ask
--   let ns = Map.keys env
--   env' <- alphaRename ns env
--   subst sub . subst env' <$> alphaRename ns e
  -- return . subst sub . subst env $ e


structProgram :: [A.Stmt] -> WP ()
structProgram stmts = do
  case progView stmts of
    ProgViewEmpty                -> return ()
    ProgViewOkay pre stmts' post -> structStmts True (pre, Nothing) stmts' post
    ProgViewMissingPrecondition stmts' post ->
      structStmts True (Constant A.true, Nothing) stmts' post
    ProgViewMissingPostcondition _ stmts' ->
      throwError . MissingPostcondition . locOf . last $ stmts'
    ProgViewMissingBoth stmts' ->
      throwError . MissingPostcondition . locOf . last $ stmts'

data SegElm = SAsrt A.Stmt
            | SSpec A.Stmt
            | SStmts [A.Stmt]

groupStmts :: [A.Stmt] -> [SegElm]
groupStmts []                            = []
groupStmts (s@(A.Assert _ _)    : stmts) = SAsrt s : groupStmts stmts
groupStmts (s@A.LoopInvariant{} : stmts) = SAsrt s : groupStmts stmts
groupStmts (s@(A.Spec _ _)      : stmts) = SSpec s : groupStmts stmts
groupStmts (s                   : stmts) = case groupStmts stmts of
  []                 -> [SStmts [s]]
  (SStmts ss : segs) -> SStmts (s : ss) : segs
  (s'        : segs) -> SStmts [s] : s' : segs

structStmts :: Bool -> (Pred, Maybe A.Expr) -> [A.Stmt] -> Pred -> WP ()
structStmts b pre = structSegs b pre . groupStmts

structSegs :: Bool -> (Pred, Maybe A.Expr) -> [SegElm] -> Pred -> WP ()
structSegs _ (pre, _) [] post = do
  case locOf pre of
    NoLoc  -> tellPO pre post (AtAssertion (locOf post))
    others -> tellPO pre post (AtAssertion others)
  return ()
structSegs True (pre, _) (SAsrt (A.Assert p l) : segs) post = do
  let assert = Assertion p l
  tellPO pre assert (AtAssertion (locOf pre))
  structSegs True (assert, Nothing) segs post
structSegs False (pre, bnd) (SAsrt (A.Assert _ _) : segs) post =
  structSegs False (pre, bnd) segs post
structSegs True (pre, _) (SAsrt (A.LoopInvariant p bnd l) : segs) post = do
  let loopInv = LoopInvariant p bnd l
  tellPO pre loopInv origin
  structSegs True (loopInv, Just bnd) segs post
 where
  startsWithDo :: [SegElm] -> Bool
  startsWithDo (SStmts (A.Do _ _ : _) : _) = True
  startsWithDo _                           = False
  origin = if startsWithDo segs then AtLoop l else AtAssertion l
structSegs False (pre, bnd) (SAsrt A.LoopInvariant{} : segs) post =
  structSegs False (pre, bnd) segs post
structSegs b (pre, bnd) [SStmts ss] post = structSStmts b (pre, bnd) ss post
structSegs b (pre, bnd) (SStmts ss : SAsrt (A.Assert p l) : segs) post = do
  structSStmts b (pre, bnd) ss (Assertion p l)
  structSegs b (Assertion p l, Nothing) segs post
structSegs b (pre, bnd) (SStmts ss : SAsrt (A.LoopInvariant p bd l) : segs) post
  = do
    structSStmts b (pre, bnd) ss (LoopInvariant p bd l)
    structSegs b (LoopInvariant p bd l, Just bd) segs post
structSegs b (pre, bnd) (SStmts ss : SSpec (A.Spec _ range) : segs) post = do
  post' <- wpSegs b segs post
  pre'  <- spSStmts b (pre, bnd) ss
  when b (tellSpec pre' post' range)
structSegs b (pre, _) (SSpec (A.Spec _ range) : segs) post = do
  post' <- wpSegs b segs post
  when b (tellSpec pre post' range)
structSegs _ _ _ _ = error "Missing case in structSegs"

 -- 'simple' version of struct stmts -- there are no assertions,
 -- invariants, or specs in the list of statements.
structSStmts :: Bool -> (Pred, Maybe A.Expr) -> [A.Stmt] -> Pred -> WP ()
structSStmts _ (pre, _) [] post = do
  case locOf pre of
    NoLoc  -> tellPO pre post (AtAssertion (locOf post))
    others -> tellPO pre post (AtAssertion others)
  return ()
structSStmts b (pre, bnd) (stmt : stmts) post = do
  post' <- wpSStmts b stmts post
  struct b (pre, bnd) stmt post'

struct :: Bool -> (Pred, Maybe A.Expr) -> A.Stmt -> Pred -> WP ()
struct b (pre, _) s@(A.Abort l) post = tellPO' (AtAbort l) pre =<< wp b s post
struct b (pre, _) s@(A.Skip l) post = tellPO' (AtSkip l) pre =<< wp b s post
struct b (pre, _) s@(A.Assign _ _ l) post = do
  tellPO' (AtAssignment l) pre =<< wp b s post
struct b (pre, _) s@(A.AAssign _ _ _ l) post = do
  tellPO' (AtAssignment l) pre =<< wp b s post
struct b (pre, _) (A.If gcmds l) post = do
  when b $ tellPO pre (disjunctGuards gcmds) (AtIf l)
  forM_ gcmds $ \(A.GdCmd guard body _) ->
    structStmts b (Conjunct [pre, guardIf guard], Nothing) body post
struct True (inv, Just bnd) (A.Do gcmds l) post = do
  let guards = A.getGuards gcmds
  tellPO (Conjunct (inv : map (Negate . guardLoop) guards)) post (AtLoop l)
  forM_ gcmds (structGdcmdInduct inv)
  tellPO (Conjunct (inv : map guardLoop guards))
         (Bound (bnd `A.gte` A.Lit (A.Num 0) NoLoc) NoLoc)
         (AtTermination l)
  forM_ gcmds (structGdcmdBnd inv bnd)
struct False (inv, _) (A.Do gcmds l) post = do
  let guards = A.getGuards gcmds
  tellPO (Conjunct (inv : map (Negate . guardLoop) guards)) post (AtLoop l)
struct _ (inv, Nothing) (A.Do gcmds l) post = do
  case fromLoc l of
    Nothing  -> return ()
    Just rng -> throwWarning (MissingBound rng)
  let guards = A.getGuards gcmds
  tellPO (Conjunct (inv : map (Negate . guardLoop) guards)) post (AtLoop l)
  forM_ gcmds (structGdcmdInduct inv)
--  tellPO (emptySubs, Conjunct (inv : map guardLoop guards)) (emptySubs, post) (AtTermination l)
-- struct b (pre, _) (A.Spec _ range) post = when b (tellSpec pre post range)
struct _ _ (A.Proof _ _) _ = return ()
struct _ _ _             _ = error "missing case in struct"

structGdcmdInduct :: Pred -> A.GdCmd -> WP ()
structGdcmdInduct inv (A.GdCmd guard body _) =
  structStmts True (Conjunct [inv, guardLoop guard], Nothing) body inv

structGdcmdBnd :: Pred -> A.Expr -> A.GdCmd -> WP ()
structGdcmdBnd inv bnd (A.GdCmd guard body _) = withFreshVar $ \oldbnd -> do
  -- oldbnd <- freshVar
  structStmts
    False
    (Conjunct [inv, Bound (bnd `A.eqq` oldbnd) NoLoc, guardLoop guard], Nothing)
    body
    (Bound (bnd `A.lt` oldbnd) NoLoc)

-- weakest precondition

wpStmts :: Bool -> [A.Stmt] -> Pred -> WP Pred
wpStmts b = wpSegs b . groupStmts

wpSegs :: Bool -> [SegElm] -> Pred -> WP Pred
wpSegs _ []                 post = return post
wpSegs b (SStmts ss : segs) post = do
  post' <- wpSegs b segs post
  wpSStmts b ss post'
wpSegs b (SSpec (A.Spec _ range) : segs) post = do
  post' <- wpSegs b segs post
  when b (tellSpec post' post' range)
  return post'
wpSegs b (SAsrt (A.Assert p l) : segs) post = do
  structSegs b (Assertion p l, Nothing) segs post
  return (Assertion p l)
wpSegs b (SAsrt (A.LoopInvariant p bd l) : segs) post = do
  structSegs b (LoopInvariant p bd l, Just bd) segs post
  return (Assertion p l) -- SCM: erasing bound information?
wpSegs _ _ _ = error "Missing case in wpSegs"

  -- "simple" version of wpStmts. need not deal with assertions and specs

wpSStmts :: Bool -> [A.Stmt] -> Pred -> WP Pred
wpSStmts _ []             post = return post
wpSStmts b (stmt : stmts) post = do
  post' <- wpSStmts b stmts post
  wp b stmt post'

wp :: Bool -> A.Stmt -> Pred -> WP Pred
wp _ (A.Abort _       ) _    = return (Constant A.false)
wp _ (A.Skip  _       ) post = return post
wp _ (A.Assign xs es _) post = do
  -- let sub = Map.fromList . zip xs . map A.AssignBinding $ es
  -- alphaSubst sub post

  scopes <- ask
  let assigmentBindings = Substitute.scopeFromSubstitution xs es
  return $ Substitute.reducePred (assigmentBindings : scopes) post

wp _ (A.AAssign (A.Var x _) i e _) post = do
  -- let sub = Map.fromList [(x, A.AssignBinding (A.ArrUpd (A.nameVar x) i e NoLoc))]
  -- alphaSubst sub post

  scopes <- ask
  let assigmentBindings = Substitute.scopeFromSubstitution
        [x]
        [A.ArrUpd (A.nameVar x) i e NoLoc]
  return $ Substitute.reducePred (assigmentBindings : scopes) post


wp _ (A.AAssign _ _ _ l) _    = throwError (MultiDimArrayAsgnNotImp l)
-- wp _ (A.Assert p l) post = do
--   tellPO (Assertion p l) post (AtAssertion l)
--   return (Assertion p l)
-- wp _ (A.LoopInvariant p b l) post = do
--   tellPO (LoopInvariant p b l) post (AtAssertion l)
--   return (LoopInvariant p b l)
wp _ (A.Do _     l     ) _    = throwError $ MissingAssertion l
wp b (A.If gcmds _     ) post = do
  pres <- forM gcmds $ \(A.GdCmd guard body _) ->
    Constant . (guard `A.imply`) . toExpr <$> wpStmts b body post
  return (conjunct (disjunctGuards gcmds : pres))
wp _ (A.Proof _ _         ) post = return post
wp _ (A.Alloc x (e : es) _) post = do
    {- wp (x := es) P = (forall x', (x' -> es) -* P[x'/x])-}

  scopes <- ask
  x'     <- freshName' (toText x) -- generate fresh name using the exisiting "x"
  let assigmentBindings = Substitute.scopeFromSubstitution [x] [A.nameVar x']
  let post' = Substitute.reduceExpr (assigmentBindings : scopes) (toExpr post)

  return $ Constant (A.forAll [x'] A.true (newallocs x' `A.sImp` post'))

 where
  newallocs x' = A.sconjunct
    ( (A.nameVar x' `A.pointsTo` e)
    : zipWith (\i -> A.pointsTo (A.nameVar x' `A.add` A.number i)) [1 ..] es
    )

  --  x' <- freshName'
  --  post' <- alphaSubst (sub x') (toExpr post)
  --  return $ Constant
  --    (A.forAll [x'] A.true
  --       (newallocs x' `A.sImp` post'))
  -- where sub x' = Map.fromList [(x, A.AssignBinding (A.nameVar x'))]
  --       newallocs x' = A.sconjunct (
  --         (A.nameVar x' `A.pointsTo` e) :
  --          zipWith (\i -> A.pointsTo (A.nameVar x' `A.add` A.number i))
  --              [1..] es)
wp _ (A.HLookup x e _) post = do
    {- wp (x := *e) P = (exists v . (e->v) * ((e->v) -* P[v/x])) -}

  scopes <- ask
  v     <- freshName' (toText x) -- generate fresh name using the exisiting "x"
  let assigmentBindings = Substitute.scopeFromSubstitution [x] [A.nameVar v]
  let post' = Substitute.reduceExpr (assigmentBindings : scopes) (toExpr post)
  return $ Constant
    (A.exists [v] A.true (entry v `A.sConj` (entry v `A.sImp` post')))
  where entry v = e `A.pointsTo` A.nameVar v
  --  v <- freshName'
  --  post' <- alphaSubst (sub v) (toExpr post)
  --  return $ Constant
  --   (A.exists [v] A.true
  --      (entry v `A.sConj` (entry v `A.sImp` post')))
  -- where sub v = Map.fromList [(x, A.AssignBinding (A.nameVar v))]
  --       entry v = e `A.pointsTo` A.nameVar v
wp _ (A.HMutate e1 e2 _) post = do
    {- wp (e1* := e2) P = (e1->_) * ((e1->e2) -* P) -}
  e1_allocated <- allocated e1
  return $ Constant
    (e1_allocated `A.sConj` ((e1 `A.pointsTo` e2) `A.sImp` toExpr post))
wp _ (A.Dispose e _) post = do
    {- wp (dispose e) P = (e -> _) * P -}
  e_allocated <- allocated e
  return $ Constant (e_allocated `A.sConj` toExpr post)
wp _ _ _ = error "missing case in wp"

allocated :: Fresh m => A.Expr -> m A.Expr
allocated e = do
  v <- freshName' "new"
  return (A.exists [v] A.true (e `A.pointsTo` A.nameVar v))
  -- allocated e = e -> _

disjunctGuards :: [A.GdCmd] -> Pred
disjunctGuards = disjunct . map guardIf . A.getGuards

-- strongest postcondition
spStmts :: Bool -> (Pred, Maybe A.Expr) -> [A.Stmt] -> WP Pred
spStmts b (pre, bnd) stmts = spSegs b (pre, bnd) (groupStmts stmts)

spSegs :: Bool -> (Pred, Maybe A.Expr) -> [SegElm] -> WP Pred
spSegs b (pre, bnd) segs = case split segs of
  (ls, Nothing                        ) -> spSegs' b (pre, bnd) ls
  (ls, Just (SAsrt (A.Assert p l), rs)) -> do
    structSegs b (pre, bnd) ls (Assertion p l)
    spSegs' b (Assertion p l, Nothing) rs
  (ls, Just (SAsrt (A.LoopInvariant p bd l), rs)) -> do
    structSegs b (pre, bnd) ls (Assertion p l)
    spSegs' b (Assertion p l, Just bd) rs
  (_, _) -> error "missing case in spSegs"
 where
  split :: [SegElm] -> ([SegElm], Maybe (SegElm, [SegElm]))
  split []                     = ([], Nothing)
  split (s@(SStmts _) : segs') = first (s :) (split segs')
  split (s@(SSpec  _) : segs') = first (s :) (split segs')
  split (s@(SAsrt  _) : segs') = case split segs' of
    (ls, Nothing   ) -> ([], Just (s, ls))
    (ls, r@(Just _)) -> (s : ls, r)

  -- spSeg' deals with a block with no assertions
  spSegs' :: Bool -> (Pred, Maybe A.Expr) -> [SegElm] -> WP Pred
  spSegs' _  (pre', _   ) []                  = return pre'
  spSegs' b' (pre', bnd') (SStmts ss : segs') = do
    pre'' <- spSStmts b' (pre', bnd') ss
    spSegs' b' (pre'', Nothing) segs'
  spSegs' b' (pre', bnd') (SSpec (A.Spec _ range) : segs') = do
    when b' (tellSpec pre' pre' range)
    spSegs' b' (pre', bnd') segs'
  spSegs' _ _ _ = error "missing case in spSegs'"

  -- the "simple" version
spSStmts :: Bool -> (Pred, Maybe A.Expr) -> [A.Stmt] -> WP Pred
spSStmts _ (pre, _  ) []             = return pre
spSStmts b (pre, bnd) (stmt : stmts) = do
  pre' <- sp b (pre, bnd) stmt
  spSStmts b (pre', Nothing) stmts

sp :: Bool -> (Pred, Maybe A.Expr) -> A.Stmt -> WP Pred
sp _ (_  , _) (A.Abort _       ) = return (Constant A.true)
sp _ (pre, _) (A.Skip  _       ) = return pre
sp _ (pre, _) (A.Assign xs es l) = do
  -- {P} x := E { (exists x' :: x = E[x'/x] && P[x'/x]) }

  -- generate fresh names from the assignees "xs"
  freshNames <- forM xs $ \x -> do 
    x' <- freshWithLabel (toText x)
    return $ Name x' NoLoc
  let freshVars = map (`A.Var` l) freshNames
  -- generate new scope for fresh names
  scopes <- ask
  let varsToFreshVars = Substitute.scopeFromSubstitution xs freshVars
  let freshVarToNothing = Map.fromList
        $ map (\name -> (nameToText name, Substitute.NoBinding)) freshNames
  let scopes' = varsToFreshVars : freshVarToNothing : scopes

  -- substitute "xs"s with fresh names in "pre"
  let pre'    = Substitute.reduceExpr scopes' (toExpr pre)
  -- 
  let predicate = A.conjunct $ zipWith
        (\x e -> A.nameVar x `A.eqq` Substitute.reduceExpr scopes' e)
        xs
        es

  return $ Constant (A.exists freshNames predicate pre')



  --   freshNames <- genFreshNames xs
  --   let sub = genSub xs freshNames
  --   pre' <- alphaSubst sub (toExpr pre)
  --   return $ Constant (
  --      A.exists freshNames (A.conjunct (zipWith (genEq sub) xs es)) pre')
  -- where
  --   genFreshNames :: [a] -> WP [Name]
  --   genFreshNames ys = map (`Name` l) <$> mapM (const freshText) ys
  --   -- genSub :: [Name] -> [Name] -> Subs A.Bindings
  --   genSub ys hs = Map.fromList . zip ys . map (A.AssignBinding . A.nameVar) $ hs
  --   -- genEq :: Map Name Bindings -> Name -> A.Expr -> A.Expr
  --   genEq sub x e = A.nameVar x `A.eqq` subst sub e
sp _ (pre, _) (A.AAssign (A.Var x _) i e _) = do
     -- {P} x[I] := E { (exist x' :: x = x'[I[x'/x] -> E[x'/x]] && P[x'/x]) }

  scopes <- ask
  x'     <- freshText
  let assigmentBindings = Substitute.scopeFromSubstitution [x] [A.variable x']
  let scopes'           = assigmentBindings : scopes
  let pre'              = Substitute.reduceExpr scopes' (toExpr pre)
  return $ Constant
    (A.exists
      [Name x' NoLoc]
      (       A.nameVar x
      `A.eqq` A.ArrUpd (A.variable x')
                       (Substitute.reduceExpr scopes' i)
                       (Substitute.reduceExpr scopes' e)
                       NoLoc
      )
      pre'
    )

  --  x' <- freshText
  --  let sub = Map.fromList [(x, A.AssignBinding (A.variable x'))]
  --  pre' <- alphaSubst sub (toExpr pre)
  --  return $ Constant (
  --    A.exists [Name x' NoLoc]
  --     (A.nameVar x `A.eqq` A.ArrUpd (A.variable x') (subst sub i) (subst sub e) NoLoc) pre')
sp _ (_  , _) (A.AAssign _ _ _ l) = throwError (MultiDimArrayAsgnNotImp l)
sp b (pre, _) (A.If gcmds _     ) = do
  posts <- forM gcmds $ \(A.GdCmd guard body _) ->
    Constant
      .   toExpr
      <$> spStmts b (Constant (guard `A.conj` toExpr pre), Nothing) body
  return (disjunct posts)
sp b (pre, bnd) (A.Do gcmds l) = do
  let guards = A.getGuards gcmds
  let post = Conjunct (pre : map (Negate . guardLoop) guards)
  struct b (pre, bnd) (A.Do gcmds l) post
  return post
sp _ (pre, _) _ = return pre

--

tellPO :: Pred -> Pred -> Origin -> WP ()
tellPO p q origin = unless (toExpr p == toExpr q) $ do

  scopes <- ask
  let p' = Substitute.reducePred scopes p
  let q' = Substitute.reducePred scopes q

  (i, j, k) <- get
  put (succ i, j, k)
  let anchorHash =
        Text.pack $ showHex (abs (Hashable.hash (toString (p', q')))) ""
  tell ([PO p' q' anchorHash Nothing origin], [], [])


tellPO' :: Origin -> Pred -> Pred -> WP ()
tellPO' l p q = tellPO p q l

tellSpec :: Pred -> Pred -> Range -> WP ()
tellSpec p q l = do

  scopes <- ask
  let p' = Substitute.reducePred scopes p
  let q' = Substitute.reducePred scopes q

  (i, j, k) <- get
  put (i, succ j, k)
  tell ([], [Specification j p' q' l], [])

throwWarning :: StructWarning -> WP ()
throwWarning warning = do
  tell ([], [], [warning])


withFreshVar :: (A.Expr -> WP a) -> WP a
withFreshVar f = do
  name <- freshName' "bnd"
  let var   = A.Var name NoLoc
  let scope = Map.singleton (nameToText name) Substitute.NoBinding
  withRWST (\scopes st -> (scope : scopes, st)) (f var)
