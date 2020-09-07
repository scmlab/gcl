{-# LANGUAGE OverloadedStrings, FlexibleContexts, DeriveGeneric, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}


module GCL.WP2 where

import           Control.Monad.State     hiding ( guard )
import           Control.Monad.Writer    hiding ( guard )
import           Control.Monad.Except    hiding ( guard )

import qualified Data.Map                      as Map
import           Data.Loc                       ( Loc(..)
                                                , Located(..)
                                                , L(..)
                                                )
import           Data.Aeson
import           GHC.Generics

import           Syntax.Concrete                ( Expr
                                                , Name
                                                )
import qualified Syntax.Concrete               as C
import           Syntax.Abstract                ( Fresh(..) )
-- import qualified Syntax.Predicate as P
import           Syntax.Predicate
import           Syntax.Location                ( Hydratable(..) )

import           Pretty.Concrete                ( )
import           Pretty.Predicate               ( )

assignmentEnv :: [Name] -> [Expr] -> C.Subst
assignmentEnv xs es = Map.fromList (zip (map C.nameToText xs) es)

--------------------------------------------------------------------------------
-- | Monad for calculating the weakest precondition

type WPM = ExceptT StructError2 (State Int)

runWPM :: WPM a -> Either StructError2 a
runWPM f = evalState (runExceptT f) 0

instance Fresh WPM where
  fresh = do
    i <- get
    put (succ i)
    return i

--------------------------------------------------------------------------------
-- | Origin of Proof Obligations

originOfStmt :: Stmt -> Origin
originOfStmt (Abort l     ) = AtAbort (locOf l)
originOfStmt (Skip  l     ) = AtSkip (locOf l)
originOfStmt (Assign l _ _) = AtAssignment (locOf l)
originOfStmt (If l _      ) = AtIf (locOf l)
originOfStmt (Do l _ _    ) = AtLoop (locOf l)
originOfStmt (Spec l _    ) = AtSpec (locOf l)

-- get the Origin of the first statement in a Struct
originOfStruct :: Struct -> Origin
originOfStruct (Struct _ [] next) = AtAssertion (locOf $ extractAssertion next)
originOfStruct (Struct _ (stmt : _) _) = originOfStmt stmt
originOfStruct (Postcond p) = AtAssertion (locOf p)

--------------------------------------------------------------------------------
-- | Obligation

-- Monad on top of WPM, for generating proof obligations
type POM = WriterT [PO] (StateT Int WPM)

instance Fresh POM where
  fresh = do
    i <- get
    put (succ i)
    return i

runPOM :: POM a -> WPM [PO]
runPOM f = evalStateT (execWriterT f) 0

tellPO :: Pred -> Pred -> Origin -> POM ()
tellPO p q l = unless (C.predEq (toExpr p) (toExpr q)) $ do
  -- NOTE: this could use some love
  i <- get
  put (succ i)
  tell [PO i p q l]

genPO :: Struct -> POM ()
genPO (Postcond _        ) = return ()
genPO (Struct pre [] next) = do
  tellPO pre
         (extractAssertion next)
         (AtAssertion (locOf $ extractAssertion next))
  genPO next
genPO (Struct pre (stmt : stmts) next) = do
  tellPO pre (precond stmt) (originOfStmt stmt)

  case stmt of
      -- inductive case
    If _ gdCmds     -> mapM_ (genPO . gdCmdBody) gdCmds
    Do l bnd gdCmds -> do
      let loc           = locOf l
      let guards        = map gdCmdGuard gdCmds
      -- the loop invariant has already been stored in Struct
      let loopInvariant = pre
      -- post condition of the current DO statement
      let post = case stmts of
            []      -> extractAssertion next
            (x : _) -> precond x

      -- base case
      tellPO (conjunct (loopInvariant : map Negate guards)) post (AtLoop loc)

      -- inductive case
      mapM_ (genPO . gdCmdBody) gdCmds

      -- termination
      bndVar <- (C.Var . hydrate) <$> freshVar "bnd" <*> pure NoLoc
      tellPO (conjunct (loopInvariant : guards))
             (Bound (bndVar `C.gte` C.number 0) NoLoc)
             (AtTermination loc)

      -- bound decrement
      forM_ gdCmds $ \(GdCmd _ body) -> do
        let start = Bound (bnd `C.eqq` bndVar) loc
        let pre   = [start, extractAssertion body]
        let post  = Bound (bnd `C.lt` bndVar) NoLoc

        body' <- lift $ lift $ updateStruct pre body post

        genPO body'

    _ -> return ()

  genPO next

--------------------------------------------------------------------------------
-- | Specification

-- Monad on top of WPM, for generating specifications
type SpecM = WriterT [Spec] (StateT Int WPM)

instance Fresh SpecM where
  fresh = do
    i <- get
    put (succ i)
    return i

tellSpec :: L Pred -> Pred -> SpecM ()
tellSpec (L l pre) post = do
  i <- get
  put (succ i)
  tell [Specification i pre post l]

runSpecM :: SpecM a -> WPM [Spec]
runSpecM f = evalStateT (execWriterT f) 0


genSpec :: Struct -> SpecM ()
genSpec (Postcond _                 ) = return ()
genSpec (Struct _   []          next) = genSpec next
genSpec (Struct pre (stmt : xs) next) = do
  case stmt of
    Spec p q      -> tellSpec p q
    If   _ gdCmds -> mapM_ (genSpec . gdCmdBody) gdCmds
    Do _ _ gdCmds -> mapM_ (genSpec . gdCmdBody) gdCmds
    _             -> return ()
  genSpec (Struct pre xs next)


--------------------------------------------------------------------------------
-- | StructError

data StructError2 = MissingLoopInvariant Loc
                 | MissingBound Loc
                 | MissingPostcondition Loc
                 | PreconditionUnknown Loc
                 | DigHole Loc
                deriving (Eq, Show, Generic)

instance Located StructError2 where
  locOf (MissingLoopInvariant loc) = loc
  locOf (MissingBound         loc) = loc
  locOf (MissingPostcondition loc) = loc
  locOf (PreconditionUnknown  loc) = loc
  locOf (DigHole              loc) = loc

instance ToJSON StructError2 where

--------------------------------------------------------------------------------
-- | Struct

data ProgView 
    = ProgViewEmpty 
    | ProgViewOkay Pred [C.Stmt] Pred
    | ProgViewMissingPrecondition [C.Stmt] Pred
    | ProgViewMissingPostcondition Pred [C.Stmt]
    | ProgViewMissingBoth [C.Stmt]

progView :: [C.Stmt] -> ProgView
progView [] = ProgViewEmpty
progView (C.Assert pre l:[]) = ProgViewMissingPrecondition [] (Assertion pre l)
progView (C.LoopInvariant pre bnd l:[]) = ProgViewMissingPrecondition [] (LoopInvariant pre bnd l)
progView stmts = case (head stmts, last stmts) of 
  (C.Assert pre l, C.Assert post m) -> ProgViewOkay (Assertion pre l) (init (tail stmts)) (Assertion post m)
  (C.LoopInvariant pre bnd l, C.Assert post m) -> ProgViewOkay (LoopInvariant pre bnd l) (init (tail stmts)) (Assertion post m)
  (C.Assert pre l, _) -> ProgViewMissingPostcondition (Assertion pre l) (tail stmts)
  (C.LoopInvariant pre bnd l, _) -> ProgViewMissingPostcondition (LoopInvariant pre bnd l) (tail stmts)
  (_, C.Assert post m) -> ProgViewMissingPrecondition (init stmts) (Assertion post m)
  (_, _) -> ProgViewMissingBoth stmts

programToStruct :: C.Program -> WPM (Maybe Struct)
programToStruct (C.Program _ _ _ statements _) = case progView statements of 
  ProgViewEmpty -> return Nothing
  ProgViewOkay pre stmts post -> Just <$> wpStmts [pre] stmts post
  -- Missing precondition, insert { True } instead 
  ProgViewMissingPrecondition stmts post ->Just <$> wpStmts [Constant C.true] stmts post
  ProgViewMissingPostcondition _pre stmts -> throwError (MissingPostcondition (locOf (last stmts)))
  ProgViewMissingBoth stmts -> throwError (MissingPostcondition (locOf (last stmts)))

--------------------------------------------------------------------------------
-- | Concrete Statements -> Struct

wpStmts :: [Pred] -> [C.Stmt] -> Pred -> WPM Struct
wpStmts imposed stmts post = do
  accum <- wpStmts' imposed stmts post
  return $ fromAccum (conjunct $ reverse imposed) accum
 where
  wpStmts' :: [Pred] -> [C.Stmt] -> Pred -> WPM Accum
  wpStmts' _       []             post = return (Accum [] (Postcond post))
  wpStmts' imposed (stmt : stmts) post = case stmt of
    C.Assert p l -> do
      accum <- wpStmts' [Assertion p l] stmts post
      return (Accum [] (fromAccum (Assertion p l) accum))
    C.LoopInvariant p b l -> do
      accum <- wpStmts' [LoopInvariant p b l] stmts post
      return (Accum [] (fromAccum (LoopInvariant p b l) accum))
    otherStmt -> do
      xs <- wpStmts' [] stmts post
      x  <- wp imposed otherStmt (precondAccum xs)
      return $ insertAccum x xs


  wp :: [Pred] -> C.Stmt -> Pred -> WPM Stmt
  wp imposed stmt post = case stmt of
    C.Abort l               -> return $ Abort (L l (Constant C.false))
    C.Skip  l               -> return $ Skip (L l post)
    C.Assert _ _            -> error "[ panic ] Assert in wp"
    C.LoopInvariant _  _  _ -> error "[ panic ] LoopInvariant in wp"
    C.Assign        xs es l -> do
      pre <- subst (assignmentEnv xs es) post
      return $ Assign (L l pre) xs es

    C.If gdCmds l -> do
      gdCmds' <- forM gdCmds $ \(C.GdCmd guard body m) -> do
        let imposed' = guardIf guard : imposed
        struct <- wpStmts imposed' body post
        return $ GdCmd (GuardIf guard m) struct

      let pre = disjunct (map guardIf (C.getGuards gdCmds))
      return $ If (L l pre) gdCmds'

    C.Do gdCmds l -> case imposed of
      (LoopInvariant _ bnd _ : _) -> do

        let loopInvariant = conjunct $ reverse imposed

        -- use the loop invariant as the postcondition of the branch
        gdCmds' <- forM gdCmds $ \(C.GdCmd guard body m) -> do
          let imposed' = guardLoop guard : imposed
          struct <- wpStmts imposed' body loopInvariant
          return $ GdCmd (GuardLoop guard m) struct

        return $ Do (L l loopInvariant) bnd gdCmds'

      _ -> throwError (MissingLoopInvariant l)

    C.SpecQM l -> throwError (DigHole l)
    C.Spec   l -> if null imposed
      then return $ Spec (L l post) post
      else return $ Spec (L l (conjunct (reverse imposed))) post


--------------------------------------------------------------------------------
-- | Struct -> Struct

-- update the stored preconditions and postconditions of a struct
updateStruct :: [Pred] -> Struct -> Pred -> WPM Struct
updateStruct _       (Postcond _dumped         ) post = return $ Postcond post
updateStruct imposed (Struct _dumped stmts next) post = do
  next' <- updateStruct [] next post
  accum <- updateAccum imposed stmts next'
  return $ fromAccum (conjunct $ reverse imposed) accum

 where
  updateAccum :: [Pred] -> [Stmt] -> Struct -> WPM Accum
  updateAccum _       []             next = return $ Accum [] next
  updateAccum imposed (stmt : stmts) next = do
    xs <- updateAccum [] stmts next
    x  <- updateStmt imposed stmt (precondAccum xs)
    return $ insertAccum x xs

  updateStmt :: [Pred] -> Stmt -> Pred -> WPM Stmt
  updateStmt imposed stmt post = case stmt of
    Skip  l        -> return $ Skip (updateL l post)
    Abort l        -> return $ Abort l
    Assign l xs es -> do
      pre <- subst (assignmentEnv xs es) post
      return $ Assign (updateL l pre) xs es
    If l gdCmds -> do
      gdCmds' <- forM gdCmds $ \(GdCmd guard body) -> do
        let imposed' = guard : imposed
        body' <- updateStruct imposed' body post
        return $ GdCmd guard body'
      return $ If l gdCmds'
    Do (L l inv) bnd gdCmds -> do
      gdCmds' <- forM gdCmds $ \(GdCmd guard body) -> do
        let imposed' = guard : imposed
        body' <- updateStruct imposed' body inv
        return $ GdCmd guard body'
      return $ Do (L l inv) bnd gdCmds'
    Spec l _ -> if null imposed
      then return $ Spec (updateL l post) post
      else return $ Spec (updateL l (conjunct (reverse imposed))) post

  updateL :: L Pred -> Pred -> L Pred
  updateL (L loc _) = L loc

--------------------------------------------------------------------------------
-- | Accum, temporary datatype for "incomplete" Structs

data Accum = Accum [Stmt] Struct

fromAccum :: Pred -> Accum -> Struct
fromAccum pre (Accum xs next) = Struct pre xs next

insertAccum :: Stmt -> Accum -> Accum
insertAccum x (Accum xs ys) = Accum (x : xs) ys

precondAccum :: Accum -> Pred
precondAccum (Accum []         xs) = extractAssertion xs
precondAccum (Accum (stmt : _) _ ) = precond stmt
