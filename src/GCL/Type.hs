module GCL.Type where

import Prelude hiding (Ordering(..))
import Data.Text.Lazy (Text)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State hiding (guard)
import Control.Monad.Trans.Except

import Syntax.Abstract

type TCxt = Map Text Type
type SubstT = [(TVar, Type)]

type M = ExceptT Err (State (SubstT, Int))
type Err = () -- no information yet. to be extended later.

exceptM :: Monad m => Maybe a -> e -> ExceptT e m a
exceptM (Just x) _ = return x
exceptM Nothing e  = throwE e

inferE :: TCxt -> Expr -> M Type
inferE cxt (Var x)   =
  exceptM (Map.lookup x cxt)
          () --- undeclared var
inferE cxt (Const x) =
  exceptM (Map.lookup x cxt)
          () --- undeclared constant
inferE _   (Lit (Num _)) = return TInt
inferE _   (Lit (Bol _)) = return TBool
inferE _   (Op op) = return (opTypes op)
inferE cxt (App e1 e2) = do
  t <- inferE cxt e1
  case t of
     TFun t1 t2 -> do t1' <- inferE cxt e2
                      _   <- unify t1 t1'
                      substTM t2
     _ -> throwE ()  -- err: not a function type
inferE _ (Hole _ _) = TVar <$> freshTVar

checkE :: TCxt -> Expr -> Type -> M ()
checkE cxt e t = do
   t' <- inferE cxt e
   _  <- unify t t'
   return ()

checkS :: TCxt -> Stmt -> M ()
checkS _ (Skip _)  = return ()
checkS _ (Abort _) = return ()
checkS cxt (Assign vs es _) =
  mapM_ (checkAsgn cxt) (zip vs es)
checkS cxt (Assert p _) =
  checkE cxt p TBool
checkS cxt (Do inv bnd gcmds _) = do
  checkE cxt inv TBool
  checkE cxt bnd TInt
  mapM_ (checkGdCmd cxt) gcmds
checkS cxt (If Nothing gcmds _) =
  mapM_ (checkGdCmd cxt) gcmds
checkS cxt (If (Just pre) gcmds _) = do
  checkE cxt pre TBool
  mapM_ (checkGdCmd cxt) gcmds
checkS _ (Spec _) = return ()

checkSs :: TCxt -> [Stmt] -> M ()
checkSs cxt = mapM_ (checkS cxt)

checkAsgn :: TCxt -> (Var, Expr) -> M ()
checkAsgn cxt (v,e) = do
  t <- exceptM (Map.lookup v cxt)  -- should extend to other L-value
               () -- err: variable undeclared
  checkE cxt e t

checkGdCmd :: TCxt -> GdCmd -> M ()
checkGdCmd cxt (GdCmd g cmds)= do
  checkE cxt g TBool
  checkSs cxt cmds

checkProg :: Program -> M ()
checkProg (Program _ Nothing) = return ()
checkProg (Program decls (Just (stmts, post))) = do
  checkSs cxt stmts
  checkE cxt post TBool
 where cxt = Map.fromList (concat (map f decls))
       f (ConstDecl cs t) = [(c,t) | c <- cs]
       f (VarDecl   vs t) = [(v,t) | v <- vs]

-- substitution

substT :: SubstT -> Type -> Type
substT _ TInt  = TInt
substT _ TBool = TBool
substT theta (TArray t)   = TArray (substT theta t)
substT theta (TFun t1 t2) =
  TFun (substT theta t1) (substT theta t2)
substT theta (TVar x) =
  case lookup x theta of
    Just t  -> t
    Nothing -> TVar x

occursT :: TVar -> Type -> Bool
occursT _ TInt  = False
occursT _ TBool = False
occursT x (TArray t) = occursT x t
occursT x (TFun t1 t2) = occursT x t1 || occursT x t2
occursT x (TVar y) = x == y

-- unification

unify :: Type -> Type -> M Type
unify TInt TInt = return TInt
unify TBool TBool = return TBool
unify (TFun t1 t2) (TFun t3 t4) = do
    t1' <- unify t1 t3
    t2' <- unify t2 t4
    return (TFun t1' t2')
unify (TVar x) t = do
  t' <- substTM t
  if occursT x t' then throwE () --- err: recursive type
    else
         return t'
unify t (TVar x) = unify (TVar x) t
unify _ _ = throwE () --- err: type mismatch

-- monad operations

substTM :: Type -> M Type
substTM t = do (theta, _) <- get
               return (substT theta t)

extSubstM :: TVar -> Type -> M ()
extSubstM x t = do
   (theta, i) <- get
   put ((x,t):theta, i)

freshTVar :: M TVar
freshTVar = do
  (theta, i) <- get
  put (theta, 1+i)
  return i

-- types of built-in operators

opTypes :: Op -> Type
opTypes EQ  = TInt `TFun` (TInt `TFun` TBool)
opTypes LTE = TInt `TFun` (TInt `TFun` TBool)
opTypes GTE = TInt `TFun` (TInt `TFun` TBool)
opTypes LT  = TInt `TFun` (TInt `TFun` TBool)
opTypes GT  = TInt `TFun` (TInt `TFun` TBool)

opTypes Plus  = TInt `TFun` (TInt `TFun` TInt)
opTypes Minus = TInt `TFun` (TInt `TFun` TInt)
opTypes Mul   = TInt `TFun` (TInt `TFun` TInt)
opTypes Div   = TInt `TFun` (TInt `TFun` TInt)

opTypes Implies = TBool `TFun` (TBool `TFun` TBool)
opTypes Conj    = TBool `TFun` (TBool `TFun` TBool)
opTypes Disj    = TBool `TFun` (TBool `TFun` TBool)
opTypes Neg     = TBool `TFun` TBool
