{-# LANGUAGE DeriveGeneric, TypeSynonymInstances, FlexibleInstances #-}

module GCL.Type where

import Control.Monad.State hiding (guard)
import Control.Monad.Except
import Data.Aeson (ToJSON)
import Data.Text.Lazy (Text)
import Data.Loc
import Prelude hiding (Ordering(..))
import GHC.Generics (Generic)

import Syntax.Concrete hiding (Type(..), Expr(..), Base(..), Lit(..))
import qualified Syntax.Concrete as C
import Syntax.Abstract
import Syntax.Abstract.Location (depart)

type TCxt   = [(Text, Type)]
type SubstT = [(TVar, Type)]

type TM = ExceptT TypeError (State (SubstT, Int))
    -- SCM: When run, this monad yields
    --    (SubstT, Int) -> (Either TypeError a, (SubstT, Int))
    --  rather than Either TypeError (a, (SubstT,Int)).
    --  The reason is that I think I want a SubstT to debug in
    --  case of error. But is this correct?

instance Fresh TM where
  fresh = do (theta, i) <- get
             put (theta, 1+i)
             return i

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

inferL :: C.Lit -> Type
inferL (C.Num _) = TBase TInt
inferL (C.Bol _) = TBase TBool
-- inferL (Chr _) = TChar

inferE :: TCxt -> C.Expr -> TM Type
inferE cxt (C.Var (Lower x l) _)   = lookupCxt l x cxt
inferE cxt (C.Const (Upper x l) _) = lookupCxt l x cxt
inferE _   (C.Lit v _) = return (inferL v)
inferE _   (C.Op op _) = return (opTypes op)
inferE cxt (C.App e1 e2 l) = do
  t <- inferE cxt e1
  case t of
     TFunc t1 t2 -> do t1' <- inferE cxt e2
                       unify_ l t1 t1'
                       substTM t2
     _ -> throwError (NotFunction t l)
-- inferE _ _ (Hole _ _) = TVar <$> freshVar "t"
-- inferE l cxt (Quant op xs rng trm) = do
--   tOp <- inferE l cxt op
--   tR <- TVar <$> freshVar "t"
--   unify_ l tOp (tR `TFunc` (tR `TFunc` tR))
--   tR' <- substTM tR
--   cxt' <- zip xs . map TVar <$> freshVars "t" (length xs)
--   checkE l (cxt' ++ cxt) rng tBool
--   checkE l (cxt' ++ cxt) trm tR'
--   return tR'

checkE :: Loc -> TCxt -> C.Expr -> Type -> TM ()
checkE l cxt e t = do
   t' <- inferE cxt e
   unify_ l t t'
   return ()

checkS :: TCxt -> Stmt -> TM ()
checkS _ (Skip _)  = return ()
checkS _ (Abort _) = return ()
checkS cxt (Assign vs es l) =
  mapM_ (checkAsgn l cxt) (zip vs es)
checkS cxt (Assert p l) =
  checkE l cxt p tBool
checkS cxt (AssertWithBnd p b l) =
  checkE l cxt p tBool >>
  checkE l cxt b tInt
checkS cxt (Do gcmds _) =
  mapM_ (checkGdCmd cxt) gcmds
checkS cxt (If gcmds _) =
  mapM_ (checkGdCmd cxt) gcmds
checkS _ (SpecQM _) = return ()
checkS _ (Spec   _) = return ()

checkSs :: TCxt -> [Stmt] -> TM ()
checkSs cxt = mapM_ (checkS cxt)

checkAsgn :: Loc -> TCxt -> (Lower, C.Expr) -> TM ()
checkAsgn l cxt (Lower v lv,e) = do
   t <- lookupCxt lv v cxt
   checkE l cxt e t

checkGdCmd :: TCxt -> GdCmd -> TM ()
checkGdCmd cxt (GdCmd g cmds l)= do
   checkE l cxt g tBool
   checkSs cxt cmds

checkProg :: Program -> TM ()
checkProg (Program decls stmts _) = do
  checkSs cxt stmts
 where cxt = concat (map f decls)
       f (ConstDecl cs t _) = [(c, depart t) | Upper c _ <- cs]
       f (VarDecl   vs t _) = [(v, depart t) | Lower v _ <- vs]

-- substitution

substT :: SubstT -> Type -> Type
substT _ (TBase t)  = (TBase t)
-- NOTE: banacorn: I've added `interval` to the AST
substT theta (TArray interval t) = TArray interval (substT theta t)
substT theta (TFunc t1 t2) =
  TFunc (substT theta t1) (substT theta t2)
substT theta (TVar x) =
  case lookup x theta of
    Just t  -> t
    Nothing -> TVar x

occursT :: TVar -> Type -> Bool
occursT _ (TBase _)  = False
occursT x (TArray _ t) = occursT x t
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

-- -- types of built-in operators

opTypes :: C.Op -> Type
opTypes (C.EQ  _) = tInt `TFunc` (tInt `TFunc` tBool)
opTypes (C.NEQ _) = tInt `TFunc` (tInt `TFunc` tBool)
opTypes (C.LTE _) = tInt `TFunc` (tInt `TFunc` tBool)
opTypes (C.GTE _) = tInt `TFunc` (tInt `TFunc` tBool)
opTypes (C.LT  _) = tInt `TFunc` (tInt `TFunc` tBool)
opTypes (C.GT  _) = tInt `TFunc` (tInt `TFunc` tBool)

opTypes (C.Add  _) = tInt `TFunc` (tInt `TFunc` tInt)
opTypes (C.Sub  _) = tInt `TFunc` (tInt `TFunc` tInt)
opTypes (C.Mul  _) = tInt `TFunc` (tInt `TFunc` tInt)
opTypes (C.Div  _) = tInt `TFunc` (tInt `TFunc` tInt)
opTypes (C.Mod  _) = tInt `TFunc` (tInt `TFunc` tInt)

opTypes (C.Implies _) = tBool `TFunc` (tBool `TFunc` tBool)
opTypes (C.Conj    _) = tBool `TFunc` (tBool `TFunc` tBool)
opTypes (C.Disj    _) = tBool `TFunc` (tBool `TFunc` tBool)
opTypes (C.Neg     _) = tBool `TFunc` tBool

--------------------------------------------------------------------------------
-- | Type Error

data TypeError
  = NotInScope Text Loc
  | UnifyFailed Type Type Loc
  | RecursiveType TVar Type Loc
  | NotFunction Type Loc
  deriving (Show, Eq, Generic)
instance ToJSON TypeError where

instance Located TypeError where
  locOf (NotInScope _ loc)      = loc
  locOf (UnifyFailed _ _ loc)   = loc
  locOf (RecursiveType _ _ loc) = loc
  locOf (NotFunction _ loc)     = loc
