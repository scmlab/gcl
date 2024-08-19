{-# LANGUAGE OverloadedStrings #-}

module GCL.WP.WP where

import           Control.Arrow                  ( first, second )
import           Control.Monad.Except           ( MonadError(throwError)
                                                , forM
                                                )
import           Data.Text                      ( Text )
import           Data.Loc                       ( Loc(..), locOf )
import           Data.Map                       ( fromList )
import           GCL.Predicate                  ( Pred )
import           GCL.Common                     ( Fresh(..)
                                                , freshName
                                                , freshName'
                                                )
import           GCL.Substitution               ( syntaxSubst )
import           GCL.WP.Types
import           GCL.WP.Util
import           Syntax.Abstract.Operator       ( tInt )
import           Syntax.Typed
import           Syntax.Typed.Util              ( getGuards, declaredNamesTypes )
import           Syntax.Typed.Operator          ( nameVar, number, add
                                                , false, true, neg
                                                , conj, conjunct, implies
                                                , forAll, exists
                                                , pointsTo, sConj, sImp
                                                , sconjunct
                                                )
import           Syntax.Common.Types            ( Name(..)
                                                , nameToText )
import           Syntax.Typed.Instances.Substitution   ()
import           Syntax.Substitution
import           Syntax.Abstract.Types          ( Type(..) )

wpFunctions :: TstructSegs
            -> (TwpSegs, TwpSStmts, Twp)
wpFunctions structSegs = (wpSegs, wpSStmts, wp)
 where
 wpStmts :: [Stmt] -> Pred -> WP Pred
 wpStmts = wpSegs . groupStmts

  -- handels segments without a precondition.
  -- switches back to structSegs when seeing an assertion
 wpSegs :: [SegElm] -> Pred -> WP Pred
 wpSegs []                 post = return post
 wpSegs (SStmts ss : segs) post = do
  post' <- wpSegs segs post
  wpSStmts ss post'
 wpSegs (SSpec (Spec _ range tenv) : segs) post = do
  post' <- wpSegs segs post
  tellSpec post' post' tenv range
  return post'
 wpSegs (SAsrt (Assert p _) : segs) post = do
  structSegs (p, Nothing) segs post
  return p
 wpSegs (SAsrt (LoopInvariant p bd _) : segs) post = do
  structSegs (p, Just bd) segs post
  return p -- SCM: erasing bound information?
 wpSegs _ _ = error "Missing case in wpSegs"

  -- "simple" version of wpStmts.
  -- no assertions and specs (in the outer level),
  -- but may contain invariants in secondary run

 wpSStmts :: [Stmt] -> Pred -> WP Pred
 wpSStmts [] post = return post
 wpSStmts (LoopInvariant inv _ _ : Do gcmds _ : stmts) post = do  -- this happens only in secondary run
  post' <- wpSStmts stmts post
  let guards = getGuards gcmds
  return $ inv `conj`
           ((inv `conj` conjunct (map neg guards)) `implies` post')
 wpSStmts (stmt : stmts) post = do
  post' <- wpSStmts stmts post
  wp stmt post'

 wp :: Stmt -> Pred -> WP Pred
 wp (Abort _       ) _    = return false
 wp (Skip  _       ) post = return post

 wp (Assign xs es _) post = return $ syntaxSubst xs es post

 wp (AAssign (Var x t _) i e _) post =
  return $ syntaxSubst [x] [ArrUpd (nameVar x t) i e NoLoc] post

 wp (AAssign _ _ _ l) _    = throwError (MultiDimArrayAsgnNotImp l)

 wp (Do _     l     ) _    = throwError $ MissingAssertion l -- shouldn't happen

 wp (If gcmds _     ) post = do
  pres <- forM gcmds $ \(GdCmd guard body _) ->
    (guard `implies`) <$> wpStmts body post
  return (conjunct (disjunctGuards gcmds : pres))

 wp (Proof _ _ _       ) post = return post

 wp (Alloc x (e : es) _) post = do -- non-empty
    {- wp (x := es) P = (forall x', (x' -> es) -* P[x'/x])-}
   x'    <- freshName' (nameToText x) -- generate fresh name using the existing "x"
   let post' = syntaxSubst [x] [nameVar x' tInt] post

   return $ forAll [x'] true (newallocs x' `sImp` post')
  where
   newallocs x' = sconjunct
    ( (nameVar x' tInt `pointsTo` e)
    : zipWith (\i -> pointsTo (nameVar x' tInt `add` number i)) [1 ..] es
    )

 wp (HLookup x e _) post = do
    {- wp (x := *e) P = (exists v . (e->v) * ((e->v) -* P[v/x])) -}
  v     <- freshName' (nameToText x) -- generate fresh name using the exisiting "x"
  let post' = syntaxSubst [x] [nameVar v tInt] post

  return $ exists [v] true (entry v `sConj` (entry v `sImp` post'))
  where entry v = e `pointsTo` nameVar v tInt

 wp (HMutate e1 e2 _) post = do
    {- wp (e1* := e2) P = (e1->_) * ((e1->e2) -* P) -}
  e1_allocated <- allocated e1
  return $ e1_allocated `sConj` ((e1 `pointsTo` e2) `sImp` post)

 wp (Dispose e _) post = do
    {- wp (dispose e) P = (e -> _) * P -}
  e_allocated <- allocated e
  return $ e_allocated `sConj` post

-- TODO:
 wp (Block prog _) post = wpBlock prog post
 wp _         _    = error "missing case in wp"

 wpBlock :: Program -> Pred -> WP Pred
 wpBlock (Program _ decls _props stmts _) post = do
   let localNames = declaredNamesTypes decls
   (xs, ys) <- withLocalScopes (\scopes ->
                withScopeExtension (map (nameToText . fst) localNames)
                 (calcLocalRenaming (concat scopes) localNames))
   stmts' <- subst (toSubst ys) stmts
   withScopeExtension (xs ++ (map (nameToText . fst . snd) ys))
     (wpStmts stmts' post)
   -- if any (`member` (fv pre)) (declaredNames decls)
   --   then throwError (LocalVarExceedScope l)
   --   else return pre
  where toSubst = fromList . map (\(n, (n', t)) -> (n, Var n' t (locOf n')))

calcLocalRenaming :: [Text] -> [(Name, Type)] -> WP ([Text], [(Text, (Name, Type))])
calcLocalRenaming _ [] = return ([], [])
calcLocalRenaming scope ((x, t):xs)
  | tx `elem` scope = do
        x' <- freshName tx (locOf x)
        second ((tx,(x',t)) :) <$> calcLocalRenaming scope xs
  | otherwise =
        first (tx:) <$> calcLocalRenaming scope xs
 where tx = nameToText x


-- toMapping :: [(Text, Name)] -> A.Mapping
-- toMapping = fromList . map cvt
--   where cvt (x, y) = (x, A.Var y (locOf y))

allocated :: Fresh m => Expr -> m Expr
allocated e = do
  v <- freshName' "new"
  return (exists [v] true (e `pointsTo` nameVar v tInt))
  -- allocated e = e -> _

-- debugging
-- pp :: Pretty a => a -> String
-- pp = renderString . layoutPretty defaultLayoutOptions . pretty
