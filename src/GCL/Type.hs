{-# LANGUAGE DeriveGeneric, TypeSynonymInstances, FlexibleInstances #-}

module GCL.Type where

import Control.Monad.State hiding (guard)
import Control.Monad.Except
import Data.Aeson (ToJSON)
import Data.Text.Lazy (Text)
import Data.Loc
import Prelude hiding (Ordering(..))
import GHC.Generics (Generic)


import Syntax.Abstract

type TCxt   = [(Text, Type)]
type SubstT = [(TVar, Type)]

type TM = ExceptT TypeError (State (SubstT, Int))
data TypeError
  = NotInScope Text Loc
  | UnifyFailed Type Type Loc
  | RecursiveType TVar Type Loc
  | NotFunction Type Loc
  deriving (Show, Eq, Generic)
instance ToJSON TypeError where


exceptM :: Monad m => Maybe a -> e -> (a -> ExceptT e m b) -> ExceptT e m b
exceptM (Just x) _ f = f x
exceptM Nothing  e _ = throwError e

lookupCxt :: Loc -> Var -> TCxt -> TM Type
lookupCxt l v cxt = exceptM (lookup v cxt) (NotInScope v l) substTM

runTM :: TM a -> Either TypeError a
runTM m = evalState (runExceptT m) ([],0)

--- type inference and checking

inferE :: Loc -> TCxt -> Expr -> TM Type
inferE l cxt (Var x)   = lookupCxt l x cxt
inferE l cxt (Const x) = lookupCxt l x cxt
inferE _ _   (Lit (Num _)) = return TInt
inferE _ _   (Lit (Bol _)) = return TBool
inferE _ _   (Op op) = return (opTypes op)
inferE l cxt (App e1 e2) = do
  t <- inferE l cxt e1
  case t of
     TFunc t1 t2 -> do t1' <- inferE l cxt e2
                       unify_ l t1 t1'
                       substTM t2
     _ -> throwError (NotFunction t l)
inferE _ _ (Hole _ _) = TVar <$> freshVar "t"
inferE l cxt (Quant op xs rng trm) = do
  tOp <- inferE l cxt op
  tR <- TVar <$> freshVar "t"
  unify_ l tOp ((tR `TFunc` tR) `TFunc` tR)
  tR' <- substTM tR
  cxt' <- zip xs . map TVar <$> freshVars "t" (length xs)
  checkE l (cxt' ++ cxt) rng TBool
  checkE l (cxt' ++ cxt) trm tR'
  return tR'

checkE :: Loc -> TCxt -> Expr -> Type -> TM ()
checkE l cxt e t = do
   t' <- inferE l cxt e
   unify_ l t t'
   return ()

checkS :: TCxt -> Stmt -> TM ()
checkS _ (Skip _)  = return ()
checkS _ (Abort _) = return ()
checkS cxt (Assign vs es l) =
  mapM_ (checkAsgn l cxt) (zip vs es)
checkS cxt (Assert p l) =
  checkE l cxt p TBool
checkS cxt (Do inv bnd gcmds l) = do
  checkE l cxt inv TBool
  checkE l cxt bnd TInt
  mapM_ (checkGdCmd l cxt) gcmds
checkS cxt (If Nothing gcmds l) =
  mapM_ (checkGdCmd l cxt) gcmds
checkS cxt (If (Just pre) gcmds l) = do
  checkE l cxt pre TBool
  mapM_ (checkGdCmd l cxt) gcmds
checkS _ (Spec _) = return ()

checkSs :: TCxt -> [Stmt] -> TM ()
checkSs cxt = mapM_ (checkS cxt)

checkAsgn :: Loc -> TCxt -> (Var, Expr) -> TM ()
checkAsgn l cxt (v,e) = do
  t <- lookupCxt l v cxt
  checkE l cxt e t

checkGdCmd :: Loc -> TCxt -> GdCmd -> TM ()
checkGdCmd l cxt (GdCmd g cmds)= do
  checkE l cxt g TBool
  checkSs cxt cmds

checkProg :: Program -> TM ()
checkProg (Program _ Nothing) = return ()
checkProg (Program decls (Just (stmts, post, loc))) = do
  checkSs cxt stmts
  checkE loc cxt post TBool  -- we need a location here!
 where cxt = concat (map f decls)
       f (ConstDecl cs t) = [(c,t) | c <- cs]
       f (VarDecl   vs t) = [(v,t) | v <- vs]

-- substitution

substT :: SubstT -> Type -> Type
substT _ TInt  = TInt
substT _ TBool = TBool
substT theta (TArray t)   = TArray (substT theta t)
substT theta (TFunc t1 t2) =
  TFunc (substT theta t1) (substT theta t2)
substT theta (TVar x) =
  case lookup x theta of
    Just t  -> t
    Nothing -> TVar x

occursT :: TVar -> Type -> Bool
occursT _ TInt  = False
occursT _ TBool = False
occursT x (TArray t) = occursT x t
occursT x (TFunc t1 t2) = occursT x t1 || occursT x t2
occursT x (TVar y) = x == y

-- unification

unify_ :: Loc -> Type -> Type -> TM ()
unify_ l t1 t2 = unify l t1 t2 >> return ()

unify :: Loc -> Type -> Type -> TM Type
unify _ TInt  TInt  = return TInt
unify _ TBool TBool = return TBool
unify l (TFunc t1 t2) (TFunc t3 t4) = do
    t1' <- unify l t1 t3
    t2' <- unify l t2 t4
    return (TFunc t1' t2')
unify l (TVar x) t = do
  t' <- substTM t
  if occursT x t' then throwError (RecursiveType x t' l)
    else do extSubstM x t'
            return t'
unify l t (TVar x) = unify l (TVar x) t
unify l t1 t2 = throwError (UnifyFailed t1 t2 l)

-- monad operations

substTM :: Type -> TM Type
substTM t = do (theta, _) <- get
               return (substT theta t)

extSubstM :: TVar -> Type -> TM ()
extSubstM x t = do
   (theta, i) <- get
   put ((x,t):theta, i)

instance Fresh TM where
  fresh = do (theta, i) <- get
             put (theta, 1+i)
             return i

-- types of built-in operators

opTypes :: Op -> Type
opTypes EQ  = TInt `TFunc` (TInt `TFunc` TBool)
opTypes NEQ = TInt `TFunc` (TInt `TFunc` TBool)
opTypes LTE = TInt `TFunc` (TInt `TFunc` TBool)
opTypes GTE = TInt `TFunc` (TInt `TFunc` TBool)
opTypes LT  = TInt `TFunc` (TInt `TFunc` TBool)
opTypes GT  = TInt `TFunc` (TInt `TFunc` TBool)

opTypes Add  = TInt `TFunc` (TInt `TFunc` TInt)
opTypes Sub = TInt `TFunc` (TInt `TFunc` TInt)
opTypes Mul   = TInt `TFunc` (TInt `TFunc` TInt)
opTypes Div   = TInt `TFunc` (TInt `TFunc` TInt)

opTypes Implies = TBool `TFunc` (TBool `TFunc` TBool)
opTypes Conj    = TBool `TFunc` (TBool `TFunc` TBool)
opTypes Disj    = TBool `TFunc` (TBool `TFunc` TBool)
opTypes Neg     = TBool `TFunc` TBool

--

instance Located TypeError where
  locOf (NotInScope _ loc)      = loc
  locOf (UnifyFailed _ _ loc)   = loc
  locOf (RecursiveType _ _ loc) = loc
  locOf (NotFunction _ loc)     = loc
