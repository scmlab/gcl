module GCL.Type where

import Prelude hiding (lookup)

import Data.Text.Lazy (Text, pack)
import Data.Map (Map, fromList, lookup)
import Control.Monad

import Syntax.Abstract

type TCxt = Map Text Type

opTypes :: Op -> Type
opTypes Eq  = TInt `TFun` (TInt `TFun` TBool)
opTypes LEq = TInt `TFun` (TInt `TFun` TBool)
opTypes GEq = TInt `TFun` (TInt `TFun` TBool)
opTypes LTh = TInt `TFun` (TInt `TFun` TBool)
opTypes GTh = TInt `TFun` (TInt `TFun` TBool)

opTypes Plus  = TInt `TFun` (TInt `TFun` TInt)
opTypes Minus = TInt `TFun` (TInt `TFun` TInt)
opTypes Mul   = TInt `TFun` (TInt `TFun` TInt)
opTypes Div   = TInt `TFun` (TInt `TFun` TInt)

opTypes Implies = TBool `TFun` (TBool `TFun` TBool)
opTypes Conj    = TBool `TFun` (TBool `TFun` TBool)
opTypes Disj    = TBool `TFun` (TBool `TFun` TBool)
opTypes Neg     = TBool `TFun` TBool

inferE :: TCxt -> Expr -> Maybe Type
inferE cxt (Var x) = lookup x cxt
inferE cxt (Const x) = lookup x cxt
inferE _   (Lit (Num _)) = return TInt
inferE _   (Lit (Bol _)) = return TBool
inferE cxt (Op op) = opTypes op
inferE cxt (ApE e1 e2) = do
  t <- inferE cxt e1
  case t of
     TFun t1 t2 -> do t1' <- inferE cxt e2
                      if t1 == t1' then return t2
                                   else mzero
     _ -> mzero
inferE _ (HoleE _ _) = undefined --- what to do here?
