module GCL.Type where

import Prelude hiding (Ordering(..))
import Data.Text.Lazy (Text)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State hiding (guard)
import Control.Monad.Trans.Except
import Data.Loc

import Syntax.Abstract

type TCxt = Map Text Type
type SubstT = [(TVar, Type)]

type M = ExceptT TErr (State (SubstT, Int))
data TErr = NotInScope Text Loc
          | UnifyFailed Type Type Loc
          | RecursiveType TVar Type Loc
          | NotFunction Type Loc
      deriving (Show, Eq)

exceptM :: Monad m => Maybe a -> e -> ExceptT e m a
exceptM (Just x) _ = return x
exceptM Nothing e  = throwE e

runM :: M a -> Either TErr a
runM m = evalState (runExceptT m) ([],0)

--- type inference and checking

inferE :: Loc -> TCxt -> Expr -> M Type
inferE l cxt (Var x)   =
  exceptM (Map.lookup x cxt)
          (NotInScope x l)
inferE l cxt (Const x) =
  exceptM (Map.lookup x cxt)
          (NotInScope x l)
inferE _ _   (Lit (Num _)) = return TInt
inferE _ _   (Lit (Bol _)) = return TBool
inferE _ _   (Op op) = return (opTypes op)
inferE l cxt (App e1 e2) = do
  t <- inferE l cxt e1
  case t of
     TFun t1 t2 -> do t1' <- inferE l cxt e2
                      _   <- unify l t1 t1'
                      substTM t2
     _ -> throwE (NotFunction t l)
inferE _ _ (Hole _ _) = TVar <$> freshTVar

checkE :: Loc -> TCxt -> Expr -> Type -> M ()
checkE l cxt e t = do
   t' <- inferE l cxt e
   _  <- unify l t t'
   return ()

checkS :: TCxt -> Stmt -> M ()
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

checkSs :: TCxt -> [Stmt] -> M ()
checkSs cxt = mapM_ (checkS cxt)

checkAsgn :: Loc -> TCxt -> (Var, Expr) -> M ()
checkAsgn l cxt (v,e) = do
  t <- exceptM (Map.lookup v cxt)  -- should extend to other L-value
               (NotInScope v l)
  checkE l cxt e t

checkGdCmd :: Loc -> TCxt -> GdCmd -> M ()
checkGdCmd l cxt (GdCmd g cmds)= do
  checkE l cxt g TBool
  checkSs cxt cmds

checkProg :: Program -> M ()
checkProg (Program _ Nothing) = return ()
checkProg (Program decls (Just (stmts, post))) = do
  checkSs cxt stmts
  checkE NoLoc cxt post TBool  -- we need a location here!
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

unify :: Loc -> Type -> Type -> M Type
unify _ TInt TInt = return TInt
unify _ TBool TBool = return TBool
unify l (TFun t1 t2) (TFun t3 t4) = do
    t1' <- unify l t1 t3
    t2' <- unify l t2 t4
    return (TFun t1' t2')
unify l (TVar x) t = do
  t' <- substTM t
  if occursT x t' then throwE (RecursiveType x t' l)
    else do extSubstM x t'
            return t'
unify l t (TVar x) = unify l (TVar x) t
unify l t1 t2 = throwE (UnifyFailed t1 t2 l)

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
