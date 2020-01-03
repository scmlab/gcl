{-# LANGUAGE DeriveGeneric, TypeSynonymInstances, FlexibleInstances #-}

module GCL.Type where

import Control.Arrow ((***))
import Control.Monad.State hiding (guard)
import Control.Monad.Except
import Data.Aeson (ToJSON)
import Data.Text.Lazy (Text, pack)
import Data.Loc
import Prelude hiding (Ordering(..))
import GHC.Generics (Generic)


import Syntax.Abstract

type TCxt   = [(Text, Type)]
type SubstT = [(TVar, Type)]

type TM = ExceptT TypeError (State (SubstT, Int))

instance Fresh TM where
  fresh = do (theta, i) <- get
             put (theta, 1+i)
             return i
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

runTM' :: TM a -> (Either TypeError a, (SubstT, Int))
runTM' m = runState (runExceptT m) ([],0)

--- type inference and checking

inferL :: Lit -> TBase
inferL (Num _) = TInt
inferL (Bol _) = TBool
inferL (Chr _) = TChar

inferE :: Loc -> TCxt -> Expr -> TM Type
inferE l cxt (Var x)   = lookupCxt l x cxt
inferE l cxt (Const x) = lookupCxt l x cxt
inferE _ _   (Lit v) = return (TBase (inferL v)) -- Lit not always a base type!
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
  unify_ l tOp (tR `TFunc` (tR `TFunc` tR))
  tR' <- substTM tR
  cxt' <- zip xs . map TVar <$> freshVars "t" (length xs)
  checkE l (cxt' ++ cxt) rng tBool
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
  checkE l cxt p tBool
checkS cxt (Do inv bnd gcmds l) = do
  checkE l cxt inv tBool
  checkE l cxt bnd tInt
  mapM_ (checkGdCmd l cxt) gcmds
checkS cxt (If Nothing gcmds l) =
  mapM_ (checkGdCmd l cxt) gcmds
checkS cxt (If (Just pre) gcmds l) = do
  checkE l cxt pre tBool
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
  checkE l cxt g tBool
  checkSs cxt cmds

checkProg :: Program -> TM ()
checkProg (Program _ Nothing) = return ()
checkProg (Program decls (Just (stmts, post, loc))) = do
  checkSs cxt stmts
  checkE loc cxt post tBool  -- we need a location here!
 where cxt = concat (map f decls)
       f (ConstDecl cs t) = [(c,t) | c <- cs]
       f (VarDecl   vs t) = [(v,t) | v <- vs]

-- substitution

substT :: SubstT -> Type -> Type
substT _ (TBase t)  = (TBase t)
substT theta (TArray t)   = TArray (substT theta t)
substT theta (TFunc t1 t2) =
  TFunc (substT theta t1) (substT theta t2)
substT theta (TVar x) =
  case lookup x theta of
    Just t  -> t
    Nothing -> TVar x

occursT :: TVar -> Type -> Bool
occursT _ (TBase _)  = False
occursT x (TArray t) = occursT x t
occursT x (TFunc t1 t2) = occursT x t1 || occursT x t2
occursT x (TVar y) = x == y

-- unification

unify_ :: Loc -> Type -> Type -> TM ()
unify_ l t1 t2 = unify l t1 t2 >> return ()

unify :: Loc -> Type -> Type -> TM Type
unify l (TBase t1) (TBase t2)
   | t1 == t2  = return (TBase t1)
   | otherwise = throwError (UnifyFailed (TBase t1) (TBase t2) l)
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
   case lookup x theta of
     Nothing -> put ((x,t):theta, i)
     Just t' -> if t == t' then return ()
                  else error "SCM: duplicated entry in unification. Fix this."

-- types of built-in operators

opTypes :: Op -> Type
opTypes EQ  = tInt `TFunc` (tInt `TFunc` tBool)
opTypes NEQ = tInt `TFunc` (tInt `TFunc` tBool)
opTypes LTE = tInt `TFunc` (tInt `TFunc` tBool)
opTypes GTE = tInt `TFunc` (tInt `TFunc` tBool)
opTypes LT  = tInt `TFunc` (tInt `TFunc` tBool)
opTypes GT  = tInt `TFunc` (tInt `TFunc` tBool)

opTypes Add  = tInt `TFunc` (tInt `TFunc` tInt)
opTypes Sub = tInt `TFunc` (tInt `TFunc` tInt)
opTypes Mul   = tInt `TFunc` (tInt `TFunc` tInt)
opTypes Div   = tInt `TFunc` (tInt `TFunc` tInt)

opTypes Implies = tBool `TFunc` (tBool `TFunc` tBool)
opTypes Conj    = tBool `TFunc` (tBool `TFunc` tBool)
opTypes Disj    = tBool `TFunc` (tBool `TFunc` tBool)
opTypes Neg     = tBool `TFunc` tBool

--

instance Located TypeError where
  locOf (NotInScope _ loc)      = loc
  locOf (UnifyFailed _ _ loc)   = loc
  locOf (RecursiveType _ _ loc) = loc
  locOf (NotFunction _ loc)     = loc

-- tests.
-- try: runTM' $ inferE NoLoc cxt0 exp3
--      runTM  $ subst subs0 exp3

cxt0 :: TCxt
cxt0 = map (pack *** id)
       [("x", tInt), ("y", tInt), ("p", tBool), ("q", tBool),
        ("a", tChar), ("b", tChar),
        ("shift", tChar `TFunc` (tInt `TFunc` tChar)),
        ("f", tInt `TFunc` tInt)]

exp1 :: Expr
exp1 = var "x" `plus` var "y"

exp2 :: Expr
exp2 = ((var "shift") `App` var "a") `App` litN 3

exp3 :: Expr
exp3 = Quant (Op Add) (map pack ["i"])
         ((litN 0 `lte` var "i") `conj` (var "i" `lte` var "x"))
         ((var "f" `App` var "i") `plus` litN 1)

subs0 :: Subst
subs0 = map (pack *** id)
          [("x", var "i" `plus` litN 1),
           ("i", var "y" `plus` litN 1)]
