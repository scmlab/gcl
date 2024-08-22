{-# LANGUAGE OverloadedStrings #-}

module GCL.WP.Struct where

import           Control.Arrow                  ( first, second )
import           Control.Monad.Except           ( forM_ )
import           Data.Text                      ( Text )
import           Data.Loc                       ( Loc(..)
                                                , Located(..)
                                                )
import           Data.Loc.Range                 ( fromLoc )
import           Data.Map                       ( fromList )
import           GCL.Predicate                  ( InfMode(..)
                                                , Pred
                                                , Origin(..))
import           GCL.Common                     ( freshName )
import           GCL.WP.Types
import           GCL.WP.Explanation
import           GCL.WP.Util
import           Syntax.Abstract.Operator       ( tInt )
import           Syntax.Typed
import           Syntax.Typed.Util              ( getGuards, declaredNamesTypes )
import           Syntax.Typed.Operator          ( eqq , gte , lt, neg
                                                , disjunct
                                                , conj, conjunct
                                                )
import Syntax.Typed.Instances.Substitution     ()
import Syntax.Common.Types                     ( Name(..), nameToText )
import Syntax.Substitution
import           Syntax.Abstract.Types          ( Type(..), Lit (Num) )

type RecFns = (TwpSegs, TwpSStmts, Twp, TspSStmts)

structFunctions :: (TwpSegs, TwpSStmts, Twp, TspSStmts)
                -> (TstructStmts, TstructSegs, Tstruct)
structFunctions (wpSegs, wpSStmts, wp, spSStmts) =
     (structStmts, structSegs, struct)

 where

 structStmts :: InfMode -> (Pred, Maybe Expr) -> [Stmt] -> Pred -> WP ()
 structStmts Primary pre stmts post = structSegs pre (groupStmts stmts) post
 structStmts Secondary (pre, _) stmts post = case stripAsserts stmts of
   Nothing     -> return ()  -- skip if the program is incomplete
   Just stmts' -> do
     post' <- wpSStmts stmts' post
     tellPO
       pre
       post'
       (emptyExplain "Assertion (Secondary)" (locOf pre))

 structSegs :: (Pred, Maybe Expr) -> [SegElm] -> Pred -> WP ()
 structSegs (pre, _) [] post = do
  case locOf pre of
    NoLoc  -> tellPO pre post (AtAssertion (locOf post))
    others -> tellPO pre post (AtAssertion others)
 structSegs (pre, _) (SAsrt (Assert p _) : segs) post = do
   tellPO pre p (AtAssertion (locOf pre))
   structSegs (p, Nothing) segs post
 structSegs (pre, _) (SAsrt (LoopInvariant p bnd l) : segs) post = do
  tellPO pre p origin
  structSegs (p, Just bnd) segs post
  where
   startsWithDo :: [SegElm] -> Bool
   startsWithDo (SStmts (Do _ _ : _) : _) = True
   startsWithDo _                           = False
   origin = if startsWithDo segs then AtLoop l else AtAssertion l
 structSegs (pre, bnd) [SStmts ss] post =
   structSStmts (pre, bnd) ss post
 structSegs (pre, bnd) (SStmts ss : SAsrt (Assert p _) : segs) post = do
  structSStmts (pre, bnd) ss p
  structSegs (p, Nothing) segs post
 structSegs (pre, bnd) (SStmts ss : SAsrt (LoopInvariant p bd _) : segs) post
  = do
    structSStmts (pre, bnd) ss p
    structSegs (p, Just bd) segs post
 structSegs (pre, bnd) (SStmts ss : SSpec (Spec _ range tenv) : segs) post = do
  post' <- wpSegs segs post
  pre'  <- spSStmts (pre, bnd) ss
  tellSpec pre' post' tenv range
 structSegs (pre, _) (SSpec (Spec _ range tenv) : segs) post = do
  post' <- wpSegs segs post
  tellSpec pre post' tenv range
 structSegs _ _ _ = error "Missing case in structSegs"

 -- 'simple' version of struct stmts -- there are no assertions,
 -- invariants, or specs in the list of statements.

 structSStmts ::  (Pred, Maybe Expr) -> [Stmt] -> Pred -> WP ()
 structSStmts (pre, _) [] post = do
   case locOf pre of
     NoLoc  -> tellPO pre post (AtAssertion (locOf post))
     others -> tellPO pre post (AtAssertion others)
 structSStmts (pre, bnd) (stmt : stmts) post = do
   post' <- wpSStmts stmts post
   struct (pre, bnd) stmt post'

 struct :: (Pred, Maybe Expr) -> Stmt -> Pred -> WP ()
 struct (pre, _) s@(Abort l) post = tellPO' (AtAbort l) pre =<< wp s post
 struct (pre, _) s@(Skip l) post = tellPO' (AtSkip l) pre =<< wp s post
 struct (pre, _) s@(Assign vars exprs l) post = do
   tellPO' origin pre =<< wp s post
  where
   origin :: Origin
   origin = explainAssignment pre post vars exprs l
 struct (pre, _) s@(AAssign _ _ _ l) post = do
   tellPO' (AtAssignment l) pre =<< wp s post
 struct (pre, _) (If gcmds l) post = do
   tellPO pre (disjunctGuards gcmds) (AtIf l)
   forM_ gcmds $ \(GdCmd guard body _) ->
     structStmts Primary (pre `conj` guard, Nothing) body post
 struct (inv, Just bnd) (Do gcmds l) post = do
  let guards = getGuards gcmds
  tellPO (conjunct (inv : map neg guards))
         post
         (explainAfterLoop inv guards l)
  forM_ gcmds (structGdcmdInduct inv)
  tellPO (inv `conj` disjunct guards)
         (bnd `gte` Lit (Num 0) tInt NoLoc)
         (explainTermination inv guards bnd l)
  forM_ gcmds (structGdcmdBnd inv bnd)

 struct (inv, Nothing) (Do gcmds l) post = do
  case fromLoc l of
    Nothing  -> return ()
    Just rng -> throwWarning (MissingBound rng)
  let guards = getGuards gcmds
  tellPO (conjunct (inv : map neg guards)) post (AtLoop l)
  forM_ gcmds (structGdcmdInduct inv)
 struct _        (Proof _ _ _)     _    = return ()

 struct (pre, _) (Block prog _)  post = structBlock pre prog post

 struct (pre, _) s@(Alloc _ _ l) post =
   tellPO' (AtAbort l) pre =<< wp s post
 struct (pre, _) s@(HLookup _ _ l) post =
   tellPO' (AtAbort l) pre =<< wp s post
 struct (pre, _) s@(HMutate _ _ l) post =
   tellPO' (AtAbort l) pre =<< wp s post
 struct (pre, _) s@(Dispose _ l) post =
   tellPO' (AtAbort l) pre =<< wp s post

 struct _        _                 _    = error "missing case in struct"

 structGdcmdInduct :: Pred -> GdCmd -> WP ()
 structGdcmdInduct inv (GdCmd guard body _) =
   structStmts Primary (inv `conj` guard, Nothing) body inv

 structGdcmdBnd :: Pred -> Expr -> GdCmd -> WP ()
 structGdcmdBnd inv bnd (GdCmd guard body _) = withFreshVar tInt $ \oldbnd -> do
  structStmts
    Secondary
    (conjunct [inv, bnd `eqq` oldbnd,guard], Nothing)
    body
    (bnd `lt` oldbnd)

 structBlock :: Pred -> Program -> Pred -> WP ()
 structBlock pre (Program _ decls _props stmts _) post = do
   let localNames = declaredNamesTypes decls
   (xs, ys) <- withLocalScopes (\scopes ->
                withScopeExtension (map (nameToText . fst) localNames)
                  (calcLocalRenaming (concat scopes) localNames))
   stmts' <- subst (toSubst ys) stmts
   withScopeExtension (xs ++ (map (nameToText . fst . snd) ys))
     (structStmts Primary (pre, Nothing) stmts' post)
  where toSubst = fromList . map (\(n, (n',t)) -> (n, Var n' t (locOf n')))

calcLocalRenaming :: [Text] -> [(Name, Type)] -> WP ([Text], [(Text, (Name, Type))])
calcLocalRenaming _ [] = return ([], [])
calcLocalRenaming scope ((x,t):xs)
  | tx `elem` scope = do
        x' <- freshName tx (locOf x)
        second ((tx,(x',t)) :) <$> calcLocalRenaming scope xs
  | otherwise =
        first (tx:) <$> calcLocalRenaming scope xs
 where tx = nameToText x

-- debugging

-- pp :: Pretty a => a -> String
-- pp = renderString . layoutPretty defaultLayoutOptions . pretty
