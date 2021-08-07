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
import           GCL.Predicate                  ( InfMode(..)
                                                , Origin(..)
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
import qualified GCL.Substitution              as Substitution
import           GCL.WP.Type
import           Numeric                        ( showHex )
import           Pretty                         ( toString
                                                , toText
                                                )
import           Render
import qualified Syntax.Abstract               as A
import qualified Syntax.Abstract.Operator      as A
import qualified Syntax.Abstract.Util          as A
import           Syntax.Common                  ( Name(Name)
                                                , nameToText
                                                )

type TM = Except StructError

type WP
  = RWST Substitution.Scope ([PO], [Spec], [StructWarning]) (Int, Int, Int) TM

instance Fresh WP where
  fresh = do
    (i, j, k) <- get
    put (i, j, succ k)
    return k

runWP
  :: WP a
  -> Substitution.Scope
  -> Either StructError (a, ([PO], [Spec], [StructWarning]))
runWP p decls = runExcept $ evalRWST p decls (0, 0, 0)

sweep :: A.Program -> Either StructError ([PO], [Spec], [StructWarning])
sweep (A.Program decls _ _ stmts _) = do
  let scope = Map.mapKeys nameToText $ A.extractDeclarations decls
  (_, (pos, specs, warnings)) <- runWP (structProgram stmts) scope
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
    ProgViewEmpty -> return ()
    ProgViewOkay pre stmts' post ->
      structStmts Primary (pre, Nothing) stmts' post
    ProgViewMissingPrecondition stmts' post ->
      structStmts Primary (Constant A.true, Nothing) stmts' post
    ProgViewMissingPostcondition _ stmts' ->
      throwError . MissingPostcondition . locOf . last $ stmts'
    ProgViewMissingBoth stmts' ->
      throwError . MissingPostcondition . locOf . last $ stmts'

--- grouping a sequence of statement by assertions and specs

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

--- removing assertions (while keeping loop invariants).
--- succeed if there are no specs.

stripAsserts :: [A.Stmt] -> Maybe [A.Stmt]
stripAsserts []                     = Just []
stripAsserts (A.Assert _ _ : stmts) = stripAsserts stmts
stripAsserts (s1@A.LoopInvariant{} : s2@A.Do{} : stmts) =
  (s1 :) <$> stripAsserts (s2 : stmts)
stripAsserts (A.LoopInvariant{} : stmts) = stripAsserts stmts
stripAsserts (A.Spec _     _    : _    ) = Nothing
stripAsserts (A.If   gcmds l    : stmts) = do
  gcmds' <- forM gcmds $ \(A.GdCmd guard body l') -> do
    body' <- stripAsserts body
    return (A.GdCmd guard body' l')
  stmts' <- stripAsserts stmts
  return (A.If gcmds' l : stmts')
stripAsserts (A.Do gcmds l : stmts) = do
  gcmds' <- forM gcmds $ \(A.GdCmd guard body l') -> do
    body' <- stripAsserts body
    return (A.GdCmd guard body' l')
  stmts' <- stripAsserts stmts
  return (A.Do gcmds' l : stmts')
stripAsserts (s : stmts) = (s :) <$> stripAsserts stmts

--- struct

structStmts :: InfMode -> (Pred, Maybe A.Expr) -> [A.Stmt] -> Pred -> WP ()
structStmts Primary pre stmts post = structSegs pre (groupStmts stmts) post
structStmts Secondary (pre, _) stmts post = case stripAsserts stmts of
  Nothing     -> return ()  -- skip if the program is incomplete
  Just stmts' -> do
    post' <- wpSStmts stmts' post
    tellPO
      pre
      post'
      (Explain { originHeader           = "Assertion (Secondary)"
               , originExplanation      = mempty
               , originInfMode          = Secondary
               , originHighlightPartial = False
               , originLoc              = locOf pre
               }
      )

structSegs :: (Pred, Maybe A.Expr) -> [SegElm] -> Pred -> WP ()
structSegs (pre, _) [] post = do
  case locOf pre of
    NoLoc  -> tellPO pre post (AtAssertion (locOf post))
    others -> tellPO pre post (AtAssertion others)
  return ()
structSegs (pre, _) (SAsrt (A.Assert p l) : segs) post = do
  let assert = Assertion p l
  tellPO pre assert (AtAssertion (locOf pre))
  structSegs (assert, Nothing) segs post
-- structSegs IgAsrt (pre, bnd) (SAsrt (A.Assert _ _) : segs) post =
--   structSegs IgAsrt (pre, bnd) segs post
structSegs (pre, _) (SAsrt (A.LoopInvariant p bnd l) : segs) post = do
  let loopInv = LoopInvariant p bnd l
  tellPO pre loopInv origin
  structSegs (loopInv, Just bnd) segs post
 where
  startsWithDo :: [SegElm] -> Bool
  startsWithDo (SStmts (A.Do _ _ : _) : _) = True
  startsWithDo _                           = False
  origin = if startsWithDo segs then AtLoop l else AtAssertion l
-- structSegs IgAsrt (pre, bnd) (SAsrt A.LoopInvariant {} : segs) post =
--   structSegs IgAsrt (pre, bnd) segs post
structSegs (pre, bnd) [SStmts ss] post = structSStmts (pre, bnd) ss post
structSegs (pre, bnd) (SStmts ss : SAsrt (A.Assert p l) : segs) post = do
  structSStmts (pre, bnd) ss (Assertion p l)
  structSegs (Assertion p l, Nothing) segs post
structSegs (pre, bnd) (SStmts ss : SAsrt (A.LoopInvariant p bd l) : segs) post
  = do
    structSStmts (pre, bnd) ss (LoopInvariant p bd l)
    structSegs (LoopInvariant p bd l, Just bd) segs post
structSegs (pre, bnd) (SStmts ss : SSpec (A.Spec _ range) : segs) post = do
  post' <- wpSegs segs post
  pre'  <- spSStmts (pre, bnd) ss
  tellSpec pre' post' range
structSegs (pre, _) (SSpec (A.Spec _ range) : segs) post = do
  post' <- wpSegs segs post
  tellSpec pre post' range
structSegs _ _ _ = error "Missing case in structSegs"

 -- 'simple' version of struct stmts -- there are no assertions,
 -- invariants, or specs in the list of statements.
structSStmts :: (Pred, Maybe A.Expr) -> [A.Stmt] -> Pred -> WP ()
structSStmts (pre, _) [] post = do
  case locOf pre of
    NoLoc  -> tellPO pre post (AtAssertion (locOf post))
    others -> tellPO pre post (AtAssertion others)
  return ()
structSStmts (pre, bnd) (stmt : stmts) post = do
  post' <- wpSStmts stmts post
  struct (pre, bnd) stmt post'

struct :: (Pred, Maybe A.Expr) -> A.Stmt -> Pred -> WP ()
struct (pre, _) s@(A.Abort l) post = tellPO' (AtAbort l) pre =<< wp s post
struct (pre, _) s@(A.Skip l) post = tellPO' (AtSkip l) pre =<< wp s post
struct (pre, _) s@(A.Assign vars exprs l) post = do
  tellPO' origin pre =<< wp s post
 where
  origin :: Origin
  origin = Explain
    { originHeader           = "Assigment"
    , originExplanation      = "After assignment, the postcondition"
                               <> (codeE . render) post
                               <> "should be implied by the precondition"
                               <> (codeE . render) pre
                               <> "after free variables"
                               <> sepByCommaE (map (codeE . render) vars)
                               <> "have been substituted with"
                               <> sepByCommaE (map (codeE . render) exprs)
    , originInfMode          = Primary
    , originHighlightPartial = False
    , originLoc              = l
    }
struct (pre, _) s@(A.AAssign _ _ _ l) post = do
  tellPO' (AtAssignment l) pre =<< wp s post
struct (pre, _) (A.If gcmds l) post = do
  tellPO pre (disjunctGuards gcmds) (AtIf l)
  forM_ gcmds $ \(A.GdCmd guard body _) ->
    structStmts Primary (Conjunct [pre, guardIf guard], Nothing) body post
struct (inv, Just bnd) (A.Do gcmds l) post = do
  let guards = A.getGuards gcmds
  let explainAfterLoop = Explain
        { originHeader           = "After Loop"
        , originExplanation      = "The loop invariant"
                                   <> (codeE . render) inv
                                   <> "should remain true while all the guards"
                                   <> sepByCommaE (map (codeE . render) guards)
                                   <> "become false after executing the loop"
        , originInfMode          = Primary
        , originHighlightPartial = True
        , originLoc              = l
        }
  let explainTermination = Explain
        { originHeader      = "Loop Termination"
        , originExplanation =
          "When the loop invariant"
          <> (codeE . render) inv
          <> "and one of the guards"
          <> sepByCommaE (map (codeE . render) guards)
          <> "remain true (that is, whilst looping), the bound"
          <> (codeE . render) bnd
          <> "should be greater then"
          <> (codeE . render) (A.Lit (A.Num 0) NoLoc)
        , originInfMode     = Primary
        , originHighlightPartial = True
        , originLoc         = l
        }
  tellPO (Conjunct (inv : map (Negate . guardLoop) guards))
         post
         explainAfterLoop
  forM_ gcmds (structGdcmdInduct inv)
  tellPO (Conjunct [inv, Disjunct (map guardLoop guards)])
         (Bound (bnd `A.gte` A.Lit (A.Num 0) NoLoc) NoLoc)
         explainTermination
  forM_ gcmds (structGdcmdBnd inv bnd)
struct (inv, Nothing) (A.Do gcmds l) post = do
  case fromLoc l of
    Nothing  -> return ()
    Just rng -> throwWarning (MissingBound rng)
  let guards = A.getGuards gcmds
  tellPO (Conjunct (inv : map (Negate . guardLoop) guards)) post (AtLoop l)
  forM_ gcmds (structGdcmdInduct inv)
struct _ (A.Proof _ _) _ = return ()
struct _ _             _ = error "missing case in struct"

structGdcmdInduct :: Pred -> A.GdCmd -> WP ()
structGdcmdInduct inv (A.GdCmd guard body _) =
  structStmts Primary (Conjunct [inv, guardLoop guard], Nothing) body inv

structGdcmdBnd :: Pred -> A.Expr -> A.GdCmd -> WP ()
structGdcmdBnd inv bnd (A.GdCmd guard body _) = withFreshVar $ \oldbnd -> do
  -- oldbnd <- freshVar
  structStmts
    Secondary
    (Conjunct [inv, Bound (bnd `A.eqq` oldbnd) NoLoc, guardLoop guard], Nothing)
    body
    (Bound (bnd `A.lt` oldbnd) NoLoc)

-- weakest precondition

wpStmts :: [A.Stmt] -> Pred -> WP Pred
wpStmts = wpSegs . groupStmts

  -- handels segments without a precondition. 
  -- switches back to structSegs when seeing an assertion
wpSegs :: [SegElm] -> Pred -> WP Pred
wpSegs []                 post = return post
wpSegs (SStmts ss : segs) post = do
  post' <- wpSegs segs post
  wpSStmts ss post'
wpSegs (SSpec (A.Spec _ range) : segs) post = do
  post' <- wpSegs segs post
  tellSpec post' post' range
  return post'
wpSegs (SAsrt (A.Assert p l) : segs) post = do
  structSegs (Assertion p l, Nothing) segs post
  return (Assertion p l)
wpSegs (SAsrt (A.LoopInvariant p bd l) : segs) post = do
  structSegs (LoopInvariant p bd l, Just bd) segs post
  return (Assertion p l) -- SCM: erasing bound information?
wpSegs _ _ = error "Missing case in wpSegs"

  -- "simple" version of wpStmts. 
  -- no assertions and specs (in the outer level), 
  -- but may contain invariants in secondary run

wpSStmts :: [A.Stmt] -> Pred -> WP Pred
wpSStmts [] post = return post
wpSStmts (A.LoopInvariant inv _ _ : A.Do gcmds _ : stmts) post = do  -- this happens only in secondary run
  post' <- wpSStmts stmts post
  let guards = A.getGuards gcmds
  return
    .        Constant
    $        inv
    `A.conj` (           (inv `A.conj` A.conjunct (map A.neg guards))
             `A.implies` toExpr post'
             )
wpSStmts (stmt : stmts) post = do
  post' <- wpSStmts stmts post
  wp stmt post'

wp :: A.Stmt -> Pred -> WP Pred
wp (A.Abort _       ) _    = return (Constant A.false)
wp (A.Skip  _       ) post = return post

wp (A.Assign xs es _) post = do
  scope <- ask
  return $ Substitution.run scope xs es post

wp (A.AAssign (A.Var x _) i e _) post = do
  scope <- ask
  return $ Substitution.run scope [x] [A.ArrUpd (A.nameVar x) i e NoLoc] post

wp (A.AAssign _ _ _ l) _    = throwError (MultiDimArrayAsgnNotImp l)

wp (A.Do _     l     ) _    = throwError $ MissingAssertion l -- shouldn't happen

wp (A.If gcmds _     ) post = do
  pres <- forM gcmds $ \(A.GdCmd guard body _) ->
    Constant . (guard `A.imply`) . toExpr <$> wpStmts body post
  return (conjunct (disjunctGuards gcmds : pres))

wp (A.Proof _ _         ) post = return post

wp (A.Alloc x (e : es) _) post = do -- non-empty
    {- wp (x := es) P = (forall x', (x' -> es) -* P[x'/x])-}
  x'    <- freshName' (toText x) -- generate fresh name using the exisiting "x"
  scope <- ask
  let post' = Substitution.run scope [x] [A.nameVar x'] (toExpr post)

  return $ Constant (A.forAll [x'] A.true (newallocs x' `A.sImp` post'))
 where
  newallocs x' = A.sconjunct
    ( (A.nameVar x' `A.pointsTo` e)
    : zipWith (\i -> A.pointsTo (A.nameVar x' `A.add` A.number i)) [1 ..] es
    )

wp (A.HLookup x e _) post = do
    {- wp (x := *e) P = (exists v . (e->v) * ((e->v) -* P[v/x])) -}
  v     <- freshName' (toText x) -- generate fresh name using the exisiting "x"
  scope <- ask
  let post' = Substitution.run scope [x] [A.nameVar v] (toExpr post)

  return $ Constant
    (A.exists [v] A.true (entry v `A.sConj` (entry v `A.sImp` post')))
  where entry v = e `A.pointsTo` A.nameVar v

wp (A.HMutate e1 e2 _) post = do
    {- wp (e1* := e2) P = (e1->_) * ((e1->e2) -* P) -}
  e1_allocated <- allocated e1
  return $ Constant
    (e1_allocated `A.sConj` ((e1 `A.pointsTo` e2) `A.sImp` toExpr post))

wp (A.Dispose e _) post = do
    {- wp (dispose e) P = (e -> _) * P -}
  e_allocated <- allocated e
  return $ Constant (e_allocated `A.sConj` toExpr post)

wp _ _ = error "missing case in wp"

allocated :: Fresh m => A.Expr -> m A.Expr
allocated e = do
  v <- freshName' "new"
  return (A.exists [v] A.true (e `A.pointsTo` A.nameVar v))
  -- allocated e = e -> _

disjunctGuards :: [A.GdCmd] -> Pred
disjunctGuards = disjunct . map guardIf . A.getGuards

-- strongest postcondition
spStmts :: (Pred, Maybe A.Expr) -> [A.Stmt] -> WP Pred
spStmts (pre, bnd) = spSegs (pre, bnd) . groupStmts

spSegs :: (Pred, Maybe A.Expr) -> [SegElm] -> WP Pred
spSegs (pre, bnd) segs = case split segs of
  (ls, Nothing                        ) -> spSegs' (pre, bnd) ls
  (ls, Just (SAsrt (A.Assert p l), rs)) -> do
    structSegs (pre, bnd) ls (Assertion p l)
    spSegs' (Assertion p l, Nothing) rs
  (ls, Just (SAsrt (A.LoopInvariant p bd l), rs)) -> do
    structSegs (pre, bnd) ls (Assertion p l)
    spSegs' (Assertion p l, Just bd) rs
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
  spSegs' :: (Pred, Maybe A.Expr) -> [SegElm] -> WP Pred
  spSegs' (pre', _   ) []                  = return pre'
  spSegs' (pre', bnd') (SStmts ss : segs') = do
    pre'' <- spSStmts (pre', bnd') ss
    spSegs' (pre'', Nothing) segs'
  spSegs' (pre', bnd') (SSpec (A.Spec _ range) : segs') = do
    tellSpec pre' pre' range
    spSegs' (pre', bnd') segs'
  spSegs' _ _ = error "missing case in spSegs'"

  -- the "simple" version
spSStmts :: (Pred, Maybe A.Expr) -> [A.Stmt] -> WP Pred
spSStmts (pre, _  ) []             = return pre
spSStmts (pre, bnd) (stmt : stmts) = do
  pre' <- sp (pre, bnd) stmt
  spSStmts (pre', Nothing) stmts

sp :: (Pred, Maybe A.Expr) -> A.Stmt -> WP Pred
sp (_  , _) (A.Abort _       ) = return (Constant A.true)

sp (pre, _) (A.Skip  _       ) = return pre

sp (pre, _) (A.Assign xs es l) = do
      -- {P} x := E { (exists x' :: x = E[x'/x] && P[x'/x]) }
  -- generate fresh names from the assignees "xs"
  freshNames <- forM xs $ \x -> do
    x' <- freshWithLabel (toText x)
    return $ Name x' NoLoc
  let freshVars = map (`A.Var` l) freshNames
  -- substitute "xs"s with fresh names in "pre"
  scope <- ask
  let pre' = Substitution.run scope xs freshVars (toExpr pre)
  -- 
  let predicate = A.conjunct $ zipWith
        (\x e -> A.nameVar x `A.eqq` Substitution.run scope xs freshVars e)
        xs
        es

  return $ Constant (A.exists freshNames predicate pre')

sp (pre, _) (A.AAssign (A.Var x _) i e _) = do
     -- {P} x[I] := E { (exist x' :: x = x'[I[x'/x] -> E[x'/x]] && P[x'/x]) }
  scope <- ask
  x'    <- freshText

  let pre' = Substitution.run scope [x] [A.variable x'] (toExpr pre)
  return $ Constant
    (A.exists
      [Name x' NoLoc]
      (       A.nameVar x
      `A.eqq` A.ArrUpd (A.variable x')
                       (Substitution.run scope [x] [A.variable x'] i)
                       (Substitution.run scope [x] [A.variable x'] e)
                       NoLoc
      )
      pre'
    )

sp (_  , _) (A.AAssign _ _ _ l) = throwError (MultiDimArrayAsgnNotImp l)

sp (pre, _) (A.If gcmds _     ) = do
  posts <- forM gcmds $ \(A.GdCmd guard body _) ->
    Constant
      .   toExpr
      <$> spStmts (Constant (guard `A.conj` toExpr pre), Nothing) body
  return (disjunct posts)
sp (pre, bnd) (A.Do gcmds l) = do
  let guards = A.getGuards gcmds
  let post = Conjunct (pre : map (Negate . guardLoop) guards)
  struct (pre, bnd) (A.Do gcmds l) post
  return post
sp (pre, _) _ = return pre

--

tellPO :: Pred -> Pred -> Origin -> WP ()
tellPO p q origin = unless (toExpr p == toExpr q) $ do

  scope <- ask
  let p' = Substitution.run scope [] [] p
  let q' = Substitution.run scope [] [] q

  (i, j, k) <- get
  put (succ i, j, k)
  let anchorHash =
        Text.pack $ showHex (abs (Hashable.hash (toString (p', q')))) ""
  tell ([PO p' q' anchorHash Nothing origin], [], [])


tellPO' :: Origin -> Pred -> Pred -> WP ()
tellPO' l p q = tellPO p q l

tellSpec :: Pred -> Pred -> Range -> WP ()
tellSpec p q l = do

  scope <- ask
  let p' = Substitution.run scope [] [] p
  let q' = Substitution.run scope [] [] q

  (i, j, k) <- get
  put (i, succ j, k)
  tell ([], [Specification j p' q' l], [])

throwWarning :: StructWarning -> WP ()
throwWarning warning = do
  tell ([], [], [warning])


withFreshVar :: (A.Expr -> WP a) -> WP a
withFreshVar f = do
  name <- freshName' "bnd"
  let var = A.Var name NoLoc
  withRWST (\scope st -> (Map.insert (nameToText name) Nothing scope, st))
           (f var)
