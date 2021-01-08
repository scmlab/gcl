{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module GCL.Type where

import Control.Monad.Except
import Control.Monad.State hiding (guard)
-- import Control.Monad (when)
import Data.Aeson (ToJSON)
import Data.Loc
import Data.Text.Lazy (Text)
import Data.Bifunctor (second)
import GHC.Generics (Generic)
import Syntax.Abstract
import Syntax.Concrete hiding
  ( Expr (..),
    Lit (..),
    Type (..),
  )
import qualified Syntax.Concrete as C
import Syntax.Location (depart)
import Prelude hiding (Ordering (..))
import Data.Traversable (foldMapDefault)

type TCxt = [(Text, Type)]

type SubstT = [(TVar, Type)]

type TM = ExceptT TypeError (State (SubstT, Int))

-- SCM: When run, this monad yields
--    (SubstT, Int) -> (Either TypeError a, (SubstT, Int))
--  rather than Either TypeError (a, (SubstT,Int)).
--  The reason is that I think I want a SubstT to debug in
--  case of error. But is this correct?

instance Fresh TM where
  fresh = do
    (theta, i) <- get
    put (theta, 1 + i)
    return i

exceptM :: Monad m => Maybe a -> e -> (a -> ExceptT e m b) -> ExceptT e m b
exceptM (Just x) _ f = f x
exceptM Nothing e _ = throwError e

lookupCxt :: Loc -> Text -> TCxt -> TM Type
lookupCxt l v cxt = exceptM (lookup v cxt) (NotInScope v l) substTM

runTM :: TM a -> Either TypeError a
runTM m = evalState (runExceptT m) ([], 0)

runTM' :: TM a -> (Either TypeError a, (SubstT, Int))
runTM' m = runState (runExceptT m) ([], 0)

--- type inference and checking

inferL :: C.Lit -> Type
inferL (C.Num _) = TBase TInt
inferL (C.Bol _) = TBase TBool
inferL (C.Chr _) = TBase TChar

inferE :: TCxt -> C.Expr -> TM Type
inferE cxt (C.Var (Name x l) _) = lookupCxt l x cxt
inferE cxt (C.Const (Name x l) _) = lookupCxt l x cxt
inferE _ (C.Lit v _) = return (inferL v)
inferE _ (C.Op op _) = return (opTypes op)
inferE cxt (C.App e1 e2 l) = do
  t <- inferE cxt e1
  case t of
    TFunc t1 t2 -> do
      t1' <- inferE cxt e2
      unify_ l t1 t1'
      substTM t2
    _ -> throwError (NotFunction t l)
inferE _ C.Lam {} = error "to be implemented" -- SCM
inferE _ (C.Hole _) = TVar <$> freshVar "t"
inferE cxt (C.Quant op xs rng trm l) = do
  tOp <- inferE cxt op
  tR <- TVar <$> freshVar "t"
  unify_ l tOp (tR `TFunc` (tR `TFunc` tR))
  tR' <- substTM tR
  cxt' <- zip (map depart xs) . map TVar <$> freshVars "t" (length xs)
  checkE (cxt' ++ cxt) rng tBool
  checkE (cxt' ++ cxt) trm tR'
  return tR'
inferE _ (C.Subst _ _) = error "SCM: to be implemented"

checkE :: TCxt -> C.Expr -> Type -> TM ()
checkE cxt e t = do
  t' <- inferE cxt e
  unify_ (locOf e) t t'
  return ()

checkS :: TCxt -> Stmt -> TM ()
checkS _ (Skip _) = return ()
checkS _ (Abort _) = return ()
checkS cxt (Assign vs es _) = mapM_ (checkAsgn cxt) (zip vs es)
checkS cxt (Assert p _) = checkE cxt p tBool
checkS cxt (LoopInvariant p b _) = checkE cxt p tBool >> checkE cxt b tInt
checkS cxt (Do gcmds _) = mapM_ (checkGdCmd cxt) gcmds
checkS cxt (If gcmds _) = mapM_ (checkGdCmd cxt) gcmds
checkS _ (SpecQM _) = return ()
checkS _ (Spec _) = return ()
checkS _ (Proof _) = return ()

checkSs :: TCxt -> [Stmt] -> TM ()
checkSs cxt = mapM_ (checkS cxt)

checkAsgn :: TCxt -> (Name, C.Expr) -> TM ()
checkAsgn cxt (Name v lv, e) = do
  t <- lookupCxt lv v cxt
  checkE cxt e t

checkGdCmd :: TCxt -> GdCmd -> TM ()
checkGdCmd cxt (GdCmd g cmds _) = do
  checkE cxt g tBool
  checkSs cxt cmds

checkProg :: Program -> TM ()
checkProg (Program decls _ _ stmts _) = do
  cxt <- foldl (>>=) (return ([] :: TCxt)) (map f decls)
  checkSs cxt stmts
  where
    f :: Declaration -> TCxt -> TM TCxt
    f (ConstDecl cs t _ _) cxt' = return ([(c, depart t) | Name c _ <- cs] ++ cxt')
    f (VarDecl vs t _ _) cxt' = return ([(v, depart t) | Name v _ <- vs] ++ cxt')
    f (LetDecl (Name c _) _ e _) cxt' = inferE cxt' e >>= \t -> return ((c,t) : cxt')
-- substitution

substT :: SubstT -> Type -> Type
substT _ (TBase t) = TBase t
-- NOTE: banacorn: I've added `interval` to the AST
substT theta (TArray interval t) = TArray interval (substT theta t)
substT theta (TFunc t1 t2) = TFunc (substT theta t1) (substT theta t2)
substT theta (TVar x) = case lookup x theta of
  Just t -> t
  Nothing -> TVar x

substTM :: Type -> TM Type
substTM t = gets $ flip substT t . fst
  -- do
  -- (theta, _) <- get
  -- return (substT theta t)

occursT :: TVar -> Type -> Bool
occursT _ (TBase _) = False
occursT x (TArray _ t) = occursT x t
occursT x (TFunc t1 t2) = occursT x t1 || occursT x t2
occursT x (TVar y) = x == y

-- unification
unify_ :: Loc -> Type -> Type -> TM ()
unify_ l t1 t2 = void $ unify l t1 t2

unify :: Loc -> Type -> Type -> TM Type
unify l (TBase t1) (TBase t2)
  | t1 == t2 = return (TBase t1)
  | otherwise = throwError (UnifyFailed (TBase t1) (TBase t2) l)
unify l (TArray i1 t1) (TArray i2 t2)
  | i1 == i2 = TArray i1 <$> unify l t1 t2
  | otherwise = throwError (UnifyFailed (TArray i1 t1) (TArray i2 t2) l)
unify l (TFunc t1 t2) (TFunc t3 t4) = 
  TFunc <$> unify l t1 t3 <*> unify l t2 t4
unify l (TVar x) t = do
  ty <- substTM t
  tx <- substTM (TVar x)
  case (tx, ty) of 
    (TVar x', TVar y) -> extSubstM x' (TVar y)
    (TVar x', _)
      | occursT x' ty -> throwError (RecursiveType x' ty l)
      | otherwise -> extSubstM x' ty
    (_, TVar y) 
      | occursT y tx -> throwError (RecursiveType y tx l)
      | otherwise -> extSubstM y tx
    _ -> unify l tx ty
unify l t (TVar x) = unify l (TVar x) t
unify l t1 t2 = throwError (UnifyFailed t1 t2 l)

extSubstM :: TVar -> Type -> TM Type
extSubstM x t = do
  (theta, i) <- get
  let theta' = map (second $ substT ((x, t) : theta)) theta
  put (theta', i)
  return t

-- -- types of built-in operators

opTypes :: Op -> Type
opTypes EQ = tInt `TFunc` (tInt `TFunc` tBool)
opTypes NEQ = tInt `TFunc` (tInt `TFunc` tBool)
opTypes LTE = tInt `TFunc` (tInt `TFunc` tBool)
opTypes GTE = tInt `TFunc` (tInt `TFunc` tBool)
opTypes LT = tInt `TFunc` (tInt `TFunc` tBool)
opTypes GT = tInt `TFunc` (tInt `TFunc` tBool)
opTypes Add = tInt `TFunc` (tInt `TFunc` tInt)
opTypes Sub = tInt `TFunc` (tInt `TFunc` tInt)
opTypes Mul = tInt `TFunc` (tInt `TFunc` tInt)
opTypes Div = tInt `TFunc` (tInt `TFunc` tInt)
opTypes Mod = tInt `TFunc` (tInt `TFunc` tInt)
opTypes Implies = tBool `TFunc` (tBool `TFunc` tBool)
opTypes Conj = tBool `TFunc` (tBool `TFunc` tBool)
opTypes Disj = tBool `TFunc` (tBool `TFunc` tBool)
opTypes Neg = tBool `TFunc` tBool

--------------------------------------------------------------------------------

-- | Type Error
data TypeError
  = NotInScope Text Loc
  | UnifyFailed Type Type Loc
  | RecursiveType TVar Type Loc
  | NotFunction Type Loc
  deriving (Show, Eq, Generic)

instance ToJSON TypeError

instance Located TypeError where
  locOf (NotInScope _ loc) = loc
  locOf (UnifyFailed _ _ loc) = loc
  locOf (RecursiveType _ _ loc) = loc
  locOf (NotFunction _ loc) = loc
