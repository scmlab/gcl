module GCL.Type where

import Prelude hiding (lookup)

import Data.Text.Lazy (Text, pack)
import Data.Map (Map, fromList, lookup)
import Control.Monad

import Syntax.Abstract

type TCxt = Map Text Type

preludeT :: TCxt
preludeT = fromList
 [(pack "+", TInt `TFun` (TInt `TFun` TInt)),
  (pack "-", TInt `TFun` (TInt `TFun` TInt)),
  (pack "*", TInt `TFun` (TInt `TFun` TInt)),
  (pack "/", TInt `TFun` (TInt `TFun` TInt)),
  (pack "<", TInt `TFun` (TInt `TFun` TBool)),
  (pack "<=", TInt `TFun` (TInt `TFun` TBool)),
  (pack ">", TInt `TFun` (TInt `TFun` TBool)),
  (pack ">=", TInt `TFun` (TInt `TFun` TBool)),
  (pack "=", TInt `TFun` (TInt `TFun` TBool))
 ]

inferE :: TCxt -> Expr -> Maybe Type
inferE cxt (VarE x) = lookup x cxt
inferE cxt (ConstE x) = lookup x cxt
inferE _   (LitE (Num _)) = return TInt
inferE _   (LitE (Bol _)) = return TBool
inferE cxt (ApE e1 e2) = do
  t <- inferE cxt e1
  case t of
     TFun t1 t2 -> do t1' <- inferE cxt e2
                      if t1 == t1' then return t2
                                   else mzero
     _ -> mzero
inferE _ (HoleE _ _) = undefined --- what to do here?
