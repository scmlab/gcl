module Syntax.Abstract.Located where

import Data.Loc
import Syntax.Common.Located()
import Syntax.Abstract
import Prelude hiding (Ordering(..))

instance Located Program where
  locOf (Program _ _ _ _ l) = l

instance Located Stmt where
  locOf (Skip l) = l
  locOf (Abort l) = l
  locOf (Assign _ _ l) = l
  locOf (Assert _ l) = l
  locOf (LoopInvariant _ _ l) = l
  locOf (Do _ l) = l
  locOf (If _ l) = l
  locOf (Spec _ l) = locOf l
  locOf (Proof l) = l

instance Located Endpoint where
  locOf (Including e) = locOf e
  locOf (Excluding e) = locOf e

instance Located Interval where
  locOf (Interval _ _ l) = l

instance Located Type where
  locOf (TBase _ l) = l
  locOf (TArray _ _ l) = l
  locOf (TFunc _ _ l) = l
  locOf (TVar _ l) = l

instance Located Expr where
  locOf (Paren e) = locOf e
  locOf (Var _ l) = l
  locOf (Const _ l) = l
  locOf (Lit _ l) = l
  locOf (Op op) = locOf op
  locOf (Chain _ _ _ l) = l
  locOf (App _ _ l) = l
  locOf (Lam _ _ l) = l
  locOf (Hole l) = l
  locOf (Quant _ _ _ _ l) = l
  locOf (Subst _ _ x) = locOf x

instance Located Lit where
  locOf _ = NoLoc
  
