{-# LANGUAGE DeriveGeneric, TypeSynonymInstances, FlexibleInstances #-}

module GCL.Type where

import Control.Monad.State hiding (guard)
import Control.Monad.Except
import Data.Aeson (ToJSON)
import Data.Text.Lazy (Text)
import Data.Loc
import Prelude hiding (Ordering(..))
import GHC.Generics (Generic)

import Syntax.Concrete hiding (Type(..), Expr(..), Lit(..), Fresh(..))
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

lookupCxt :: Loc -> Text -> TCxt -> TM Type
lookupCxt l v cxt = exceptM (lookup v cxt) (NotInScope v l) substTM

runTM :: TM a -> Either TypeError a
runTM m = evalState (runExceptT m) ([],0)

runTM' :: TM a -> (Either TypeError a, (SubstT, Int))
runTM' m = runState (runExceptT m) ([],0)

--- type inference and checking

inferL :: C.Lit -> Type
inferL (C.Num _) = TBase TInt
inferL (C.Bol _) = TBase TBool
inferL (C.Chr _) = TBase TChar

inferE :: TCxt -> C.Expr -> TM Type
inferE cxt (C.Var   (Lower x l) _) = lookupCxt l x cxt
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
inferE _   (C.Hole _) = TVar <$> freshVar "t"
inferE cxt (C.Quant op xs rng trm l) = do
  tOp <- inferE cxt op
  tR <- TVar <$> freshVar "t"
  unify_ l tOp (tR `TFunc` (tR `TFunc` tR))
  tR' <- substTM tR
  cxt' <- zip (map depart xs) . map TVar <$> freshVars "t" (length xs)
  checkE (cxt' ++ cxt) rng tBool
  checkE (cxt' ++ cxt) trm tR'
  return tR'

checkE :: TCxt -> C.Expr -> Type -> TM ()
checkE cxt e t = do
   t' <- inferE cxt e
   unify_ (locOf e) t t'
   return ()

checkS :: TCxt -> Stmt -> TM ()
checkS _ (Skip _)  = return ()
checkS _ (Abort _) = return ()
checkS cxt (Assign vs es _) =
  mapM_ (checkAsgn cxt) (zip vs es)
checkS cxt (Assert p _) =
  checkE cxt p tBool
checkS cxt (AssertWithBnd p b _) =
  checkE cxt p tBool >>
  checkE cxt b tInt
checkS cxt (Do gcmds _) =
  mapM_ (checkGdCmd cxt) gcmds
checkS cxt (If gcmds _) =
  mapM_ (checkGdCmd cxt) gcmds
checkS _ (SpecQM _) = return ()
checkS _ (Spec   _) = return ()

checkSs :: TCxt -> [Stmt] -> TM ()
checkSs cxt = mapM_ (checkS cxt)

checkAsgn :: TCxt -> (Lower, C.Expr) -> TM ()
checkAsgn cxt (Lower v lv,e) = do
   t <- lookupCxt lv v cxt
   checkE cxt e t

checkGdCmd :: TCxt -> GdCmd -> TM ()
checkGdCmd cxt (GdCmd g cmds _)= do
   checkE cxt g tBool
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

opTypes :: Op -> Type
opTypes EQ  = tInt `TFunc` (tInt `TFunc` tBool)
opTypes NEQ = tInt `TFunc` (tInt `TFunc` tBool)
opTypes LTE = tInt `TFunc` (tInt `TFunc` tBool)
opTypes GTE = tInt `TFunc` (tInt `TFunc` tBool)
opTypes LT  = tInt `TFunc` (tInt `TFunc` tBool)
opTypes GT  = tInt `TFunc` (tInt `TFunc` tBool)

opTypes Add  = tInt `TFunc` (tInt `TFunc` tInt)
opTypes Sub  = tInt `TFunc` (tInt `TFunc` tInt)
opTypes Mul  = tInt `TFunc` (tInt `TFunc` tInt)
opTypes Div  = tInt `TFunc` (tInt `TFunc` tInt)
opTypes Mod  = tInt `TFunc` (tInt `TFunc` tInt)

opTypes Implies = tBool `TFunc` (tBool `TFunc` tBool)
opTypes Conj    = tBool `TFunc` (tBool `TFunc` tBool)
opTypes Disj    = tBool `TFunc` (tBool `TFunc` tBool)
opTypes Neg     = tBool `TFunc` tBool

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
