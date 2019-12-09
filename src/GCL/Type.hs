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
