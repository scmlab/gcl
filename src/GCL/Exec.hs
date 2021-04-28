{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GCL.Exec
  ( module GCL.Exec,
    module GCL.Exec.ExecMonad,
  )
where

import Control.Monad.Except
import Data.Loc
import GCL.Exec.ExecMonad
import Syntax.Abstract
import Syntax.Common
import Prelude hiding (Ordering (..))

-- make evalExpr monadic for now,
--   since it may want to throw errors (e.g. div by zero).

evalExpr :: ExecMonad m => Expr -> m Val
evalExpr (Lit v _) = return (litToVal v)
evalExpr (Const (Name x l) _) = lookupStore l x
evalExpr (Var (Name x l) _) = lookupStore l x
evalExpr (Op op l) = evalOp op l
evalExpr (App e1 e2 _) =
  evalExpr e1 >>= \case
    VFun f -> evalExpr e2 >>= \v -> liftEither (f v)
    _ -> error "type error, shouldn't happen"
evalExpr (Lam _ _ _) = error "to be implemented"
evalExpr (Quant _ _ _ _ _) = error "not supported"
evalExpr (Hole _) = error "shouldn't happen"
evalExpr (Subst _ _) = error "not supported"

litToVal :: Lit -> Val
litToVal (Num n) = VNum n
litToVal (Bol a) = VBol a
litToVal (Chr c) = VChr c

---

execStmt :: ExecMonad m => Stmt -> m ()
execStmt (Skip _) = return ()
execStmt (Abort l) = throwError (Aborted l)
execStmt (Assign xs es l) = execAsgn xs es l
execStmt (Assert _ _) = return ()
execStmt (LoopInvariant _ _ _) = return ()
execStmt (Spec _ _) = error "spec cannot be executed"
execStmt (Proof _) = error "proof cannot be executed"
execStmt (If gcmds l) =
  shuffle gcmds >>= pickGCmds (return ()) (throwError (AllFailedInIf l))
execStmt (Do gcmds l) =
  shuffle gcmds >>= pickGCmds (execStmt (Do gcmds l)) (return ())

execStmts :: ExecMonad m => [Stmt] -> m ()
execStmts [] = return ()
execStmts (s : ss) = execStmt s >> execStmts ss

execAsgn :: ExecMonad m => [Name] -> [Expr] -> Loc -> m ()
execAsgn xs es l = do
  vs <- mapM evalExpr es
  mapM_ (uncurry (updateStore l)) (zip (map nameToText xs) vs)

-- SCM: Not sure whether it is more complicated than necessary,
--      but I need a way to distinguish between "all choices failed"
--      and "end of choices after some succssful executions".

pickGCmds :: ExecMonad m => m () -> m () -> [GdCmd] -> m ()
pickGCmds _ ex [] = ex -- no branch has succeeded
pickGCmds cont ex (GdCmd g cmds _ : gs) =
  evalExpr g >>= \case
    VBol False -> pickGCmds cont ex gs
    VBol True -> (execStmts cmds >> cont) `mplus` pickGCmds' gs
    _ -> error "type error, shouldn't happen"
  where
    -- pickGCmds -- some branch has succeeded
    pickGCmds' [] = mzero
    pickGCmds' (GdCmd g' cmds' _ : gs') =
      evalExpr g' >>= \case
        VBol False -> pickGCmds' gs'
        VBol True -> (execStmts cmds' >> cont) `mplus` pickGCmds' gs'
        _ -> error "type error, shouldn't happen"

execProg :: ExecMonad m => Program -> m ()
execProg (Program decls _ _ stmts _) = do
  mapM_ declare decls
  execStmts stmts

declare :: ExecMonad m => Declaration -> m ()
declare (ConstDecl cs _ _ _) =
  mapM_ (\x -> updateStore NoLoc x Undef) (map nameToText cs)
declare (VarDecl xs _ _ _) =
  mapM_ (\x -> updateStore NoLoc x Undef) (map nameToText xs)
declare (LetDecl c _ _ _) = updateStore NoLoc (nameToText c) Undef

-- Lifting primitive operators.
-- Should these be written with dependent type, or type family?

evalOp :: ExecMonad m => Op -> Loc -> m Val
evalOp EQ _ = return (liftOp2IRel (==))
evalOp NEQ _ = return (liftOp2IRel (/=))
evalOp LT _ = return (liftOp2IRel (<))
evalOp LTE _ = return (liftOp2IRel (<=))
evalOp GTE _ = return (liftOp2IRel (>=))
evalOp GT _ = return (liftOp2IRel (>))
evalOp Implies _ = return (liftOp2Bool (\p q -> not p || q))
evalOp Conj _ = return (liftOp2Bool (&&))
evalOp Disj _ = return (liftOp2Bool (||))
evalOp Neg _ = return (liftOpBool not)
evalOp Add _ = return (liftOp2Int (+))
evalOp Sub _ = return (liftOp2Int (-))
evalOp Mul _ = return (liftOp2Int (*))
evalOp Div l =
  return $
    VFun
      ( \case
          (VNum v1) ->
            ( Right
                ( VFun
                    ( \case
                        (VNum 0) -> Left (DivByZero l)
                        (VNum v2) -> Right (VNum (v1 `div` v2))
                        _ -> error "type error, shouldn't happen"
                    )
                )
            )
          _ -> error "type error, shouldn't happen"
      )
evalOp Mod l = return $ modVFun l

modVFun :: Loc -> Val
modVFun l =
  VFun
    ( \case
        (VNum v1) ->
          ( Right
              ( VFun
                  ( \case
                      (VNum 0) -> Left (DivByZero l)
                      (VNum v2) -> Right (VNum (v1 `mod` v2))
                      _ -> error "type error, shouldn't happen"
                  )
              )
          )
        _ -> error "type error, shouldn't happen"
    )

liftOpInt :: (Int -> Int) -> Val
liftOpInt f =
  VFun
    ( \case
        (VNum v) -> Right . VNum . f $ v
        _ -> error "type error, shouldn't happen"
    )

liftOpBool :: (Bool -> Bool) -> Val
liftOpBool f =
  VFun
    ( \case
        VBol v -> Right . VBol . f $ v
        _ -> error "type error, shouldn't happen"
    )

liftOp2Int :: (Int -> Int -> Int) -> Val
liftOp2Int f =
  VFun
    ( \case
        VNum v -> Right . liftOpInt . f $ v
        _ -> error "type error, shouldn't happen"
    )

liftOp2Bool :: (Bool -> Bool -> Bool) -> Val
liftOp2Bool f =
  VFun
    ( \case
        VBol v -> Right . liftOpBool . f $ v
        _ -> error "type error, shouldn't happen"
    )

liftOp2IRel :: (Int -> Int -> Bool) -> Val
liftOp2IRel f =
  VFun
    ( \case
        VNum v1 ->
          Right
            ( VFun
                ( \case
                    VNum v2 -> (Right (VBol (f v1 v2)))
                    _ -> error "type error, shouldn't happen"
                )
            )
        _ -> error "type error, shouldn't happen"
    )

-- prelude :: Store
-- prelude = map (pack *** id)
--  [("mod", modVFun NoLoc)]
