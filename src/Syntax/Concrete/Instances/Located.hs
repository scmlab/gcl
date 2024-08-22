module Syntax.Concrete.Instances.Located where

import           Data.Loc                       ( (<-->)
                                                , Located(locOf)
                                                )
import           Syntax.Common                  ( )
import           Syntax.Concrete.Types

--------------------------------------------------------------------------------

instance Located a => Located (SepBy sep a) where
  locOf (Head a      ) = locOf a
  locOf (Delim a _ as) = a <--> locOf as

instance (Located a, Located c) => Located (a, b, c) where
  locOf (a, _, c) = a <--> c

--------------------------------------------------------------------------------

instance Located Program where
  locOf (Program a b) = a <--> b

instance Located Declaration where
  locOf (ConstDecl l r) = l <--> r
  locOf (VarDecl   l r) = l <--> r

instance Located TypeDefnCtor where
  locOf (TypeDefnCtor l r) = l <--> r

instance Located DefinitionBlock where
  locOf (DefinitionBlock l _ r) = l <--> r

instance Located DeclBase where
  locOf (DeclBase l _ r) = l <--> r

instance Located DeclProp where
  locOf (DeclProp l _ r) = l <--> r

instance Located DeclType where
  locOf (DeclType l r) = l <--> r

-------------------------------------------------------------------------------

instance Located Stmt where
  locOf (Skip  l                    ) = locOf l
  locOf (Abort l                    ) = locOf l
  locOf (Assign l _ r               ) = l <--> r
  locOf (AAssign l _ _ _ _ r        ) = l <--> r
  locOf (Assert l _ r               ) = l <--> r
  locOf (LoopInvariant l _ _ _ _ _ r) = l <--> r
  locOf (Do l _ r                   ) = l <--> r
  locOf (If l _ r                   ) = l <--> r
  locOf (SpecQM l                   ) = locOf l
  locOf (Spec  l _ r                ) = l <--> r
  locOf (Proof _ _ _ r                ) = locOf r
  locOf (Alloc l _ _ _ _ r          ) = l <--> r
  locOf (HLookup l _ _ r            ) = l <--> r
  locOf (HMutate l _ _ r            ) = l <--> r
  locOf (Dispose l r                ) = l <--> r
  locOf (Block l _ r                ) = l <--> r

--------------------------------------------------------------------------------

instance Located EndpointOpen where
  locOf (IncludingOpening l e) = l <--> e
  locOf (ExcludingOpening l e) = l <--> e

instance Located EndpointClose where
  locOf (IncludingClosing e l) = e <--> l
  locOf (ExcludingClosing e l) = e <--> l

instance Located Interval where
  locOf (Interval l _ r) = l <--> r

instance Located TBase where
  locOf (TInt  l) = locOf l
  locOf (TBool l) = locOf l
  locOf (TChar l) = locOf l

instance Located Type where
  locOf (TParen l _ r  ) = l <--> r
  locOf (TBase a       ) = locOf a
  locOf (TArray l _ _ r) = l <--> r
  locOf (TOp op        ) = locOf op
  locOf (TData _ l     ) = locOf l
  locOf (TApp l r      ) = l <--> r
  locOf (TMetaVar _ l  ) = locOf l

--------------------------------------------------------------------------------

instance Located Expr where
  locOf (Paren l _ r          ) = l <--> r
  locOf (Lit   x              ) = locOf x
  locOf (Var   x              ) = locOf x
  locOf (Const x              ) = locOf x
  locOf (Op    x              ) = locOf x
  locOf (Chain ch             ) = locOf ch
  locOf (Arr l _ _ r          ) = l <--> r
  locOf (App x y              ) = x <--> y
  locOf (Quant l _ _ _ _ _ _ r) = l <--> r
  locOf (Case l _ _ r         ) = l <--> r

instance Located Chain where
  locOf (Pure expr) = locOf expr
  locOf (More ch _ expr) = ch <--> expr

instance Located CaseClause where
  locOf (CaseClause l _ r) = l <--> r

--------------------------------------------------------------------------------

instance Located Pattern where
  locOf (PattLit l          ) = locOf l
  locOf (PattParen l _ r    ) = l <--> r
  locOf (PattBinder   l     ) = locOf l
  locOf (PattWildcard l     ) = locOf l
  locOf (PattConstructor l r) = l <--> r

--------------------------------------------------------------------------------

instance Located Lit where
  locOf (LitInt  _ l) = locOf l
  locOf (LitBool _ l) = locOf l
  locOf (LitChar _ l) = locOf l
