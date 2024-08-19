module Syntax.Typed.Instances.Located where

import           Data.Loc
import           Prelude                 hiding ( Ordering(..) )
import           Syntax.Typed.Types
import           Syntax.Common                  ( )

instance Located Program where
  locOf (Program _ _ _ _ l) = l

instance Located Declaration where
  locOf (ConstDecl _ _ _ l) = l
  locOf (VarDecl   _ _ _ l) = l

instance Located Definition where
  locOf (TypeDefn    _ _ _ r) = r
  locOf (FuncDefnSig _ _ _ r) = r
  locOf (FuncDefn l r       ) = l <--> r

instance Located TypeDefnCtor where
  locOf (TypeDefnCtor l r) = l <--> r

instance Located Stmt where
  locOf (Skip  l            ) = l
  locOf (Abort l            ) = l
  locOf (Assign _ _ l       ) = l
  locOf (AAssign _ _ _ l    ) = l
  locOf (Assert _ l         ) = l
  locOf (LoopInvariant _ _ l) = l
  locOf (Do    _ l          ) = l
  locOf (If    _ l          ) = l
  locOf (Spec  _ r _        ) = locOf r
  locOf (Proof _ _ r        ) = locOf r
  locOf (Alloc   _ _ l      ) = l
  locOf (HLookup _ _ l      ) = l
  locOf (HMutate _ _ l      ) = l
  locOf (Dispose _ l        ) = l
  locOf (Block   _ l        ) = l

instance Located GdCmd where
  locOf (GdCmd _ _ l) = l

instance Located Expr where
  locOf (Lit   _ _     l) = l
  locOf (Var   _ _     l) = l
  locOf (Const _ _     l) = l
  locOf (Op op _        ) = locOf op
  locOf (Chain chain    ) = locOf chain
  locOf (App  _ _      l) = l
  locOf (Lam  _ _ _    l) = l
  locOf (Quant _ _ _ _ l) = l
  locOf (ArrIdx _ _    l) = l
  locOf (ArrUpd _ _ _  l) = l
  locOf (Case _ _ l     ) = l
  locOf (Subst _ _)       = NoLoc

instance Located Chain where
  locOf (Pure e      ) = locOf e
  locOf (More _ _ _ e) = locOf e
