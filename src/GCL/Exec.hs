{-# LANGUAGE LambdaCase, MultiParamTypeClasses, FlexibleContexts,
             TypeSynonymInstances, FlexibleInstances #-}

module GCL.Exec where

import Prelude hiding (Ordering(..))
import Data.Loc
import Control.Monad.Except
import Control.Monad.State hiding (guard)
-- import Control.Monad.Trans.List  -- not always correct, but does't matter.

import Syntax.Abstract

type Store = [(Var, Val)]

data Val = VNum Int | VBol Bool | VChr Char
         | VFun (Val -> Val)
         | VArr Int [Val]

class (MonadPlus m, MonadError e m)=> ExecMonad e m where
  lookupStore :: Loc -> Var -> m Val
  updateStore :: Loc -> Var -> Val -> m ()

  -- make evalExpr monadic for now,
  --   since it may want to throw errors (e.g. div by zero).

evalExpr :: ExecMonad e m => Loc -> Expr -> m Val
evalExpr _ (Lit v) = return (litToVal v)
evalExpr l (Const x) = lookupStore l x
evalExpr l (Var x) = lookupStore l x
evalExpr _ (Op op) = evalOp op
evalExpr l (App e1 e2) =
   evalExpr l e1 >>= \case
    VFun f -> f <$> evalExpr l e2
    _ -> error "type error, shouldn't happen"
evalExpr _ (Quant _ _ _ _) = error "not supported"
evalExpr _ (Hole _ _) = error "shouldn't happen"

litToVal :: Lit -> Val
litToVal (Num n) = VNum n
litToVal (Bol a) = VBol a
litToVal (Chr c) = VChr c

---

data ExecError = Aborted Loc
               | AllFailedInIf Loc
 deriving (Show, Eq)


execStmt :: ExecMonad ExecError m => Stmt -> m ()
execStmt (Skip _)  = return ()
execStmt (Abort l) = throwError (Aborted l)
execStmt (Assign xs es l) = mapM_ (execAsgn l) (zip xs es)
execStmt (Assert _ _) = return ()
execStmt (Spec _) = error "spec cannot be executed"
execStmt (If _ gcmds l) =
  pickGCmds l (throwError (AllFailedInIf l)) gcmds
execStmt (Do _ _ gcmds l) =
  pickGCmds l (return ()) gcmds

execStmts :: ExecMonad ExecError m => [Stmt] -> m ()
execStmts [] = return ()
execStmts (s:ss) = execStmt s >> execStmts ss

execAsgn :: ExecMonad ExecError m => Loc -> (Var, Expr) -> m ()
execAsgn l (x, e) = do
  v <- evalExpr l e
  updateStore l x v

 -- SCM: Not sure whether it is more complicated than necessary,
 --      but I need a way to distinguish between "all choices failed"
 --      and "end of choices after some succssful executions".

pickGCmds :: ExecMonad ExecError m =>
             Loc -> m () -> [GdCmd] -> m ()
pickGCmds _ ex [] = ex
pickGCmds l ex (GdCmd g cmds : gs) =
  evalExpr l g >>= \case
    VBol False -> pickGCmds l ex gs
    VBol True ->  execStmts cmds `mplus` pickGCmds' gs
    _ -> error "type error, shouldn't happen"
 where -- pickGCmds -- some choices have succeeded
       pickGCmds' [] = mzero
       pickGCmds' (GdCmd g' cmds' : gs') =
         evalExpr l g' >>= \case
           VBol False -> pickGCmds' gs'
           VBol True ->  execStmts cmds' `mplus` pickGCmds' gs'
           _ -> error "type error, shouldn't happen"

-- should these be written with dependent type, or type family?

evalOp :: ExecMonad e m => Op -> m Val
evalOp EQ  = return (liftOp2IRel (==))
evalOp NEQ = return (liftOp2IRel (/=))
evalOp LT  = return (liftOp2IRel (<))
evalOp LTE = return (liftOp2IRel (<=))
evalOp GTE = return (liftOp2IRel (>=))
evalOp GT  = return (liftOp2IRel (>))
evalOp Implies  = return (liftOp2Bool (\p q -> not p || q))
evalOp Conj     = return (liftOp2Bool (&&))
evalOp Disj     = return (liftOp2Bool (||))
evalOp Neg      = return (liftOpBool not)
evalOp Add = return (liftOp2Int (+))
evalOp Sub = return (liftOp2Int (-))
evalOp Mul = return (liftOp2Int (*))
evalOp Div = return (liftOp2Int div)

liftOpInt :: (Int -> Int) -> Val
liftOpInt f = VFun (\case VNum v -> VNum (f v)
                          _ -> error "type error, shouldn't happen" )

liftOpBool :: (Bool -> Bool) -> Val
liftOpBool f = VFun (\case VBol v -> VBol (f v)
                           _ -> error "type error, shouldn't happen" )

liftOp2Int :: (Int -> Int -> Int) -> Val
liftOp2Int f =
  VFun (\case VNum v -> liftOpInt (f v)
              _ -> error "type error, shouldn't happen" )

liftOp2Bool :: (Bool -> Bool -> Bool) -> Val
liftOp2Bool f =
  VFun (\case VBol v -> liftOpBool (f v)
              _ -> error "type error, shouldn't happen" )

liftOp2IRel :: (Int -> Int -> Bool) -> Val
liftOp2IRel f =
  VFun (\case VNum v1 -> VFun (\case VNum v2 -> VBol (f v1 v2)
                                     _ -> error "type error, shouldn't happen" )
              _ -> error "type error, shouldn't happen" )

-- Some choices for ExecMonad

type ExNondet = ExceptT ExecError (StateT Store [])

-- StateT s (ExceptT e m) =  s -> m (Either e (s, a))
-- ExceptT e (StateT s m) =  s -> m (s, Either e a)
--  which do we want?

  -- do we need these? why?
instance Semigroup ExecError where
   (<>) = undefined
instance Monoid ExecError where
   mempty = undefined
   mappend = undefined

runExNondet :: ExNondet a -> [(Either ExecError a, Store)]
runExNondet m = runStateT (runExceptT m) []

instance ExecMonad ExecError ExNondet where
  lookupStore _ x =
    (lookup x <$> get) >>= \case
     Nothing -> error "shouldn't happen"
     Just v -> return v
  updateStore _ x v = do
     store <- get
     put ((x,v) : filter (not . (==x) . fst) store)
