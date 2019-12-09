module GCL.Type where

import Data.Text.Lazy (Text)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad

import Prelude hiding (Ordering(..))
import Syntax.Abstract

type TCxt = Map Text Type

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

inferE :: TCxt -> Expr -> Maybe Type
inferE cxt (Var x) = Map.lookup x cxt
inferE cxt (Const x) = Map.lookup x cxt
inferE _   (Lit (Num _)) = return TInt
inferE _   (Lit (Bol _)) = return TBool
inferE _   (Op op) = Just (opTypes op)
inferE cxt (App e1 e2) = do
  t <- inferE cxt e1
  case t of
     TFun t1 t2 -> do t1' <- inferE cxt e2
                      if t1 == t1' then return t2
                                   else mzero
     _ -> mzero
inferE _ (Hole _ _) = undefined --- what to do here?

checkE :: TCxt -> Expr -> Type -> Maybe ()
checkE cxt e t = do
   t' <- inferE cxt e
   guard (t == t')

checkS :: TCxt -> Stmt -> Maybe ()
checkS _ (Skip _) = return ()
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

checkSs :: TCxt -> [Stmt] -> Maybe ()
checkSs cxt = mapM_ (checkS cxt)

checkAsgn :: TCxt -> (Var, Expr) -> Maybe ()
checkAsgn cxt (v,e) = do
  t <- Map.lookup v cxt
  checkE cxt e t

checkGdCmd :: TCxt -> GdCmd -> Maybe ()
checkGdCmd cxt (GdCmd g cmds)= do
  checkE cxt g TBool
  checkSs cxt cmds

checkProg :: Program -> Maybe ()
checkProg (Program _ Nothing) = return ()
checkProg (Program decls (Just (stmts, post))) = do
  checkSs cxt stmts
  checkE cxt post TBool
 where cxt = Map.fromList (concat (map f decls))
       f (ConstDecl cs t) = [(c,t) | c <- cs]
       f (VarDecl   vs t) = [(v,t) | v <- vs]
