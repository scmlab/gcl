{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module GCL.WP where

import Control.Monad.Except (Except, MonadError (throwError), forM, forM_, runExcept, unless, when)
import Control.Monad.RWS (MonadReader (ask), MonadState (..), MonadWriter (..), RWST, evalRWST)
import Control.Arrow((***))
import Data.Aeson (ToJSON)
import Data.Loc (Loc (..), Located (..))
import Data.Loc.Range (Range, fromLoc)
import qualified Data.Map as Map
import GCL.Common (Fresh (fresh, freshText), Subs, Substitutable (subst), alphaRename)
import GCL.Predicate (Origin (..), PO (..), Pred (..), Spec (Specification))
import GCL.Predicate.Util (conjunct, disjunct, guardIf, guardLoop, toExpr)
import GHC.Generics (Generic)
import qualified Syntax.Abstract as A
import qualified Syntax.Abstract.Operator as A
import qualified Syntax.Abstract.Util as A
import qualified Syntax.ConstExpr as A
import Syntax.Common (Name (Name))
import qualified Data.Set as Set

type TM = Except StructError

type WP = RWST (Subs A.Expr) ([PO], [Spec], [StructWarning]) (Int, Int, Int) TM

instance Fresh WP where
  fresh = do
    (i, j, k) <- get
    put (i, j, succ k)
    return k

runWP :: WP a -> Subs A.Expr -> Either StructError (a, ([PO], [Spec], [StructWarning]))
runWP p defs = runExcept $ evalRWST p defs (0, 0, 0)

sweep :: A.Program -> Either StructError ([PO], [Spec], [StructWarning])
sweep (A.Program decls _ ds stmts _) = do
  snd <$> runWP (structProgram decls stmts) ds

data ProgView
  = ProgViewEmpty
  | ProgViewOkay Pred [A.Stmt] Pred
  | ProgViewMissingPrecondition [A.Stmt] Pred
  | ProgViewMissingPostcondition Pred [A.Stmt]
  | ProgViewMissingBoth [A.Stmt]

progView :: [A.Stmt] -> ProgView
progView [] = ProgViewEmpty
progView [A.Assert pre l] = do
  ProgViewMissingPrecondition [] (Assertion pre l)
progView stmts = do
  case (head stmts, last stmts) of
    (A.Assert pre l, A.Assert post m) -> do
      ProgViewOkay (Assertion pre l) (init (tail stmts)) (Assertion post m)
    (A.Assert pre l, _) -> do
      ProgViewMissingPostcondition (Assertion pre l) (tail stmts)
    (_, A.Assert post m) -> do
      ProgViewMissingPrecondition (init stmts) (Assertion post m)
    _ -> ProgViewMissingBoth stmts

alphaRenameDefns :: [A.Declaration] -> A.Defns -> WP A.Defns
alphaRenameDefns decls dfns = do
  let ns = Set.fromList $ Map.keys dfns ++ concatMap extractNames (A.pickDeclarations decls)
  mapM (alphaRename ns) dfns
  where
    extractNames (A.ConstDecl ns _ _ _) = ns
    extractNames (A.VarDecl ns _ _ _) = ns
    extractNames (A.LetDecl n _ _ _) = [n]

structProgram :: [A.Declaration] -> [A.Stmt] -> WP ()
structProgram decls stmts = do
  env <- Map.map Right <$> (ask >>= alphaRenameDefns decls):: WP (Subs A.Bindings)

  case progView (subst env stmts) of
    ProgViewEmpty -> return ()
    ProgViewOkay pre stmts' post -> structStmts True (pre,Nothing) stmts' post
    ProgViewMissingPrecondition stmts' post -> structStmts True (Constant A.true, Nothing) stmts' post
    ProgViewMissingPostcondition _ stmts' -> throwError . MissingPostcondition . locOf . last $ stmts'
    ProgViewMissingBoth stmts' -> throwError . MissingPostcondition . locOf . last $ stmts'

data SegElm = SAsrt A.Stmt
            | SSpec A.Stmt
            | SStmts [A.Stmt]

groupStmts :: [A.Stmt] -> [SegElm]
groupStmts [] = []
groupStmts (s@(A.Assert _ _) : stmts) = SAsrt s : groupStmts stmts
groupStmts (s@(A.LoopInvariant _ _ _) : stmts) = SAsrt s : groupStmts stmts
groupStmts (s@(A.Spec _ _) : stmts) = SSpec s : groupStmts stmts
groupStmts (s : stmts) = case groupStmts stmts of
    [] -> [SStmts [s]]
    (SStmts ss : segs) -> SStmts (s:ss) : segs
    (s' : segs) -> SStmts [s] : s' : segs

structStmts :: Bool -> (Pred, Maybe A.Expr) -> [A.Stmt] -> Pred -> WP ()
structStmts b pre stmts post =
  structSegs b pre (groupStmts stmts) post

structSegs :: Bool -> (Pred, Maybe A.Expr) -> [SegElm] -> Pred -> WP ()
structSegs _ (pre, _) [] post = do
  case locOf pre of
    NoLoc -> tellPO pre post (AtAssertion (locOf post))
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
 where startsWithDo :: [SegElm] -> Bool
       startsWithDo (SStmts (A.Do _ _ : _) : _) = True
       startsWithDo _ = False
       origin = if startsWithDo segs then AtLoop l else AtAssertion l
structSegs False (pre, bnd) (SAsrt (A.LoopInvariant {}) : segs) post =
  structSegs False (pre, bnd) segs post
structSegs b (pre, bnd) [SStmts ss] post =
  structSStmts b (pre,bnd) ss post
structSegs b (pre, bnd) (SStmts ss : SAsrt (A.Assert p l) : segs) post = do
  structSStmts b (pre, bnd) ss (Assertion p l)
  structSegs b (Assertion p l, Nothing) segs post
structSegs b (pre, bnd) (SStmts ss : SAsrt (A.LoopInvariant p bd l) : segs) post = do
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
    NoLoc -> tellPO pre post (AtAssertion (locOf post))
    others -> tellPO pre post (AtAssertion others)
  return ()
structSStmts b (pre, bnd) (stmt : stmts) post = do
  post' <- wpSStmts b stmts post
  struct b (pre, bnd) stmt post'

struct :: Bool -> (Pred, Maybe A.Expr) -> A.Stmt -> Pred -> WP ()
struct _ (pre, _) (A.Abort l) _ = tellPO pre (Constant A.false) (AtAbort l)
struct _ (pre, _) (A.Skip l) post = tellPO pre post (AtSkip l)
struct _ (pre, _) (A.Assign xs es l) post = do
  let sub = Map.fromList . zip xs . map Left $ es :: Subs A.Bindings
  tellPO pre (subst sub post) (AtAssignment l)
struct _ (pre, _) (A.AAssign (A.Var x _) i e l) post = do
     let sub = Map.fromList [(x, Left (A.ArrUpd (A.nameVar x) i e l))] :: Subs A.Bindings
     tellPO pre (subst sub post) (AtAssignment l)
struct _ (_, _) (A.AAssign _ _ _ l) _ =
  throwError (MultiDimArrayAsgnNotImp l)
struct b (pre, _) (A.If gcmds l) post = do
  when b $ tellPO pre (disjunctGuards gcmds) (AtIf l)
  forM_ gcmds $ \(A.GdCmd guard body _) ->
    structStmts b (Conjunct [pre, guardIf guard], Nothing) body post
struct True (inv, Just bnd) (A.Do gcmds l) post = do
  let guards = A.getGuards gcmds
  tellPO (Conjunct (inv : map (Negate . guardLoop) guards)) post (AtLoop l)
  forM_ gcmds (structGdcmdInduct inv)
  tellPO
    (Conjunct (inv : map guardLoop guards))
    (Bound (bnd `A.gte` A.Lit (A.Num 0) NoLoc) NoLoc)
    (AtTermination l)
  forM_ gcmds (structGdcmdBnd inv bnd)
struct False (inv, _) (A.Do gcmds l) post = do
  let guards = A.getGuards gcmds
  tellPO (Conjunct (inv : map (Negate . guardLoop) guards)) post (AtLoop l)
struct _ (inv, Nothing) (A.Do gcmds l) post = do
  case fromLoc l of
    Nothing -> return ()
    Just rng -> throwWarning (MissingBound rng)
  let guards = A.getGuards gcmds
  tellPO (Conjunct (inv : map (Negate . guardLoop) guards)) post (AtLoop l)
  forM_ gcmds (structGdcmdInduct inv)
--  tellPO (Conjunct (inv : map guardLoop guards)) post (AtTermination l)
-- struct b (pre, _) (A.Spec _ range) post = when b (tellSpec pre post range)
struct _ _ (A.Proof _) _ = return ()
struct _ _ _ _ = error "missing case in struct"

structGdcmdInduct :: Pred -> A.GdCmd -> WP ()
structGdcmdInduct inv (A.GdCmd guard body _) =
  structStmts True (Conjunct [inv, guardLoop guard], Nothing) body inv

structGdcmdBnd :: Pred -> A.Expr -> A.GdCmd -> WP ()
structGdcmdBnd inv bnd (A.GdCmd guard body _) = do
  oldbnd <- freshText
  structStmts
    False
    ( Conjunct
        [ inv,
          Bound (bnd `A.eqq` A.variable oldbnd) NoLoc,
          guardLoop guard
        ],
    Nothing )
    body
    (Bound (bnd `A.lt` A.variable oldbnd) NoLoc)

-- weakest precondition

wpStmts :: Bool -> [A.Stmt] -> Pred -> WP Pred
wpStmts b stmts post =
  wpSegs b (groupStmts stmts) post

wpSegs :: Bool -> [SegElm] -> Pred -> WP Pred
wpSegs _ [] post = return post
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
wpSStmts _ [] post = return post
wpSStmts b (stmt : stmts) post = do
  post' <- wpSStmts b stmts post
  wp b stmt post'

wp :: Bool -> A.Stmt -> Pred -> WP Pred
wp _ (A.Abort _) _ = return (Constant A.false)
wp _ (A.Skip _) post = return post
wp _ (A.Assign xs es _) post = do
  let sub = Map.fromList . zip xs . map Left $ es :: Subs A.Bindings
  return $ subst sub post
wp _ (A.AAssign (A.Var x _) i e _) post = do
  let sub = Map.fromList [(x, Left (A.ArrUpd (A.nameVar x) i e NoLoc))] :: Subs A.Bindings
  return $ subst sub post
wp _ (A.AAssign _ _ _ l) _ = throwError (MultiDimArrayAsgnNotImp l)
wp _ (A.Do _ l) _ = throwError $ MissingAssertion l
wp b (A.If gcmds _) post = do
  pres <- forM gcmds $ \(A.GdCmd guard body _) ->
    Constant . (guard `A.imply`)
      . toExpr
      <$> wpStmts b body post
  return (conjunct (disjunctGuards gcmds : pres))
wp _ (A.Proof _) post = return post
wp _ _ _ = error "missing case in wp"

disjunctGuards :: [A.GdCmd] -> Pred
disjunctGuards = disjunct . map guardIf . A.getGuards

-- strongest postcondition
spStmts :: Bool -> (Pred, Maybe A.Expr) -> [A.Stmt] -> WP Pred
spStmts b (pre, bnd) stmts =
   spSegs b (pre, bnd) (groupStmts stmts)

spSegs :: Bool -> (Pred, Maybe A.Expr) -> [SegElm] -> WP Pred
spSegs b (pre, bnd) segs = case split segs of
    (ls, Nothing) -> spSegs' b (pre, bnd) ls
    (ls, Just (SAsrt (A.Assert p l), rs)) -> do
       structSegs b (pre, bnd) ls (Assertion p l)
       spSegs' b (Assertion p l, Nothing) rs
    (ls, Just (SAsrt (A.LoopInvariant p bd l), rs)) -> do
      structSegs b (pre, bnd) ls (Assertion p l)
      spSegs' b (Assertion p l, Just bd) rs
    (_, _) -> error "missing case in spSegs"
 where
  split :: [SegElm] -> ([SegElm], Maybe (SegElm, [SegElm]))
  split [] = ([], Nothing)
  split (s@(SStmts _):segs) = ((s:) *** id) (split segs)
  split (s@(SSpec _):segs) = ((s:) *** id) (split segs)
  split (s@(SAsrt _):segs) =
          case split segs of
            (ls, Nothing) -> ([], Just (s, ls))
            (ls, r@(Just _)) -> (s:ls, r)

  -- spSeg' deals with a block with no assertions
  spSegs' :: Bool -> (Pred, Maybe A.Expr) -> [SegElm] -> WP Pred
  spSegs' _ (pre, _) [] = return pre
  spSegs' b (pre, bnd) (SStmts ss : segs) = do
    pre' <- spSStmts b (pre, bnd) ss
    spSegs' b (pre', Nothing) segs
  spSegs' b (pre, bnd) (SSpec (A.Spec _ range) : segs) = do
    when b (tellSpec pre pre range)
    spSegs' b (pre, bnd) segs
  spSegs' _ _ _ = error "missing case in spSegs'"

  -- the "simple" version
spSStmts :: Bool -> (Pred, Maybe A.Expr) -> [A.Stmt] -> WP Pred
spSStmts _ (pre, _) [] = return pre
spSStmts b (pre, bnd) (stmt : stmts) = do
  pre' <- sp b (pre, bnd) stmt
  spSStmts b (pre', Nothing) stmts

sp :: Bool -> (Pred, Maybe A.Expr) -> A.Stmt -> WP Pred
sp _ (_, _) (A.Abort _) = return (Constant A.true)
sp _ (pre, _) (A.Skip _) = return pre
sp _ (pre, _) (A.Assign xs es l) = do
      -- {P} x := E { (exists x' :: x = E[x'/x] && P[x'/x]) }
    frNames <- genFrNames xs
    let sub = genSub xs frNames
    return $ Constant (
       A.exists frNames (A.conjunct (zipWith (genEq sub) xs es))
          (subst sub (toExpr pre)))
  where
    genFrNames :: [a] -> WP [Name]
    genFrNames ys = map (\x -> Name x l) <$> mapM (const freshText) ys
    genSub :: [Name] -> [Name] -> Subs A.Bindings
    genSub ys hs = Map.fromList . zip ys . map (Left . A.nameVar) $ hs
    -- genEq :: Name -> A.Expr -> A.Expr
    genEq sub x e = A.nameVar x `A.eqq` (subst sub e)
sp _ (pre, _) (A.AAssign (A.Var x _) i e _) = do
     -- {P} x[I] := E { (exist x' :: x = x'[I[x'/x] -> E[x'/x]] && P[x'/x]) }
   x' <- freshText
   let sub = Map.fromList [(x, Left (A.variable x'))] :: Subs A.Bindings
   return $ Constant (
     A.exists [Name x' NoLoc]
      (A.nameVar x `A.eqq` A.ArrUpd (A.variable x') (subst sub i) (subst sub e) NoLoc)
      (subst sub (toExpr pre)))
sp _ (_, _) (A.AAssign _ _ _ l) = throwError (MultiDimArrayAsgnNotImp l)
sp b (pre, _) (A.If gcmds _) = do
  posts <- forM gcmds $ \(A.GdCmd guard body _) ->
    Constant . toExpr <$>
      spStmts b (Constant (guard `A.conj` toExpr pre), Nothing) body
  return (disjunct posts)
sp b (pre, bnd) (A.Do gcmds l) = do
  let guards = A.getGuards gcmds
  let post = Conjunct (pre : map (Negate . guardLoop) guards)
  struct b (pre, bnd) (A.Do gcmds l) post
  return post
sp _ (pre, _) _ = return pre

--

tellPO :: Pred -> Pred -> Origin -> WP ()
tellPO p q l = unless (toExpr p == toExpr q) $ do
  (i, j, k) <- get
  put (succ i, j, k)
  tell ([PO i p q l], [], [])

tellSpec :: Pred -> Pred -> Range -> WP ()
tellSpec p q l = do
  (i, j, k) <- get
  put (i, succ j, k)
  tell ([], [Specification j p q l], [])

throwWarning :: StructWarning -> WP ()
throwWarning warning = do
  tell ([], [], [warning])

data StructWarning
  = MissingBound Range
  | ExcessBound Range
  deriving (Eq, Show, Generic)

instance Located StructWarning where
  locOf (MissingBound rng) = locOf rng
  locOf (ExcessBound rng) = locOf rng

data StructError
  = MissingAssertion Loc
  | MissingPostcondition Loc
  | MultiDimArrayAsgnNotImp Loc
     -- Assignment to multi-dimensional array not implemented.
     -- SCM: will remove this when we figure out how.
  deriving (Eq, Show, Generic)

instance Located StructError where
  locOf (MissingAssertion l) = l
  locOf (MissingPostcondition l) = l
  locOf (MultiDimArrayAsgnNotImp l) = l

instance ToJSON StructError
