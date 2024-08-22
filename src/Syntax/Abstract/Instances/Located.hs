module Syntax.Abstract.Instances.Located where

import           Data.Loc
import           Prelude                 hiding ( Ordering(..) )
import           Syntax.Abstract.Types
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
  locOf (Spec  _ l          ) = locOf l
  locOf (Proof _ _ r        ) = locOf r
  locOf (Alloc   _ _ l      ) = l
  locOf (HLookup _ _ l      ) = l
  locOf (HMutate _ _ l      ) = l
  locOf (Dispose _ l        ) = l
  locOf (Block   _ l        ) = l

instance Located GdCmd where
  locOf (GdCmd _ _ l) = l

instance Located Endpoint where
  locOf (Including e) = locOf e
  locOf (Excluding e) = locOf e

instance Located Interval where
  locOf (Interval _ _ l) = l

instance Located Type where
  locOf (TBase _ l   ) = l
  locOf (TArray _ _ l) = l
  locOf (TTuple _    ) = NoLoc
  locOf (TFunc _ _ l  ) = l
  locOf (TOp op      ) = locOf op
  locOf (TData _ l   ) = l
  locOf (TApp  _ _ l ) = locOf l
  locOf (TVar _ l    ) = l
  locOf (TMetaVar _ l) = l

instance Located Kind where
  locOf (KStar loc) = loc
  locOf (KFunc _ _ loc) = loc
  locOf (KMetaVar _) = NoLoc

instance Located Expr where
  locOf (Var   _ l           ) = l
  locOf (Const _ l           ) = l
  locOf (Lit   _ l           ) = l
  locOf (Op op               ) = locOf op
  locOf (Chain chain         ) = locOf chain
  locOf (App  _ _ l          ) = l
  locOf (Func _ _ l          ) = l
  locOf (Lam  _ _ l          ) = l
  locOf (Tuple _             ) = NoLoc
  locOf (Quant _ _ _ _ l     ) = l
  locOf (RedexKernel es _ _ _) = locOf es
  locOf (RedexShell _ x      ) = locOf x
  locOf (ArrIdx _ _ l        ) = l
  locOf (ArrUpd _ _ _ l      ) = l
  locOf (Case _ _ l          ) = l

instance Located Chain where
  locOf (Pure _ l            ) = l
  locOf (More _ _ _ l        ) = l

instance Located CaseClause where
  locOf (CaseClause l r) = l <--> r

instance Located Pattern where
  locOf (PattLit      l     ) = locOf l
  locOf (PattBinder   l     ) = locOf l
  locOf (PattWildcard l     ) = locOf l
  locOf (PattConstructor l r) = l <--> r

instance Located Lit where
  locOf _ = NoLoc
