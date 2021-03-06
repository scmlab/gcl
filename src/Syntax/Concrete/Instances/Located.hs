module Syntax.Concrete.Instances.Located where

import Syntax.Concrete.Types
import Syntax.Common ()
import Data.Loc (Located (locOf), (<-->))

--------------------------------------------------------------------------------

instance Located a => Located (SepBy sep a) where
  locOf (Head a) = locOf a
  locOf (Delim a _ as) = a <--> locOf as

--------------------------------------------------------------------------------

instance Located Program where
  locOf (Program a b) = a <--> b

instance Located Declaration where
  locOf (ConstDecl l r) = l <--> r
  locOf (VarDecl l r) = l <--> r

instance Located BlockDeclaration where
  locOf (BlockDeclaration l _ r) = l <--> r

instance Located DeclBase where
  locOf (DeclBase l _ r) = l <--> r

instance Located DeclProp where
  locOf (DeclProp l _ r) = l <--> r

instance Located DeclBody where
  locOf (DeclBody l _ _ r) = l <--> r

instance Located DeclType where
  locOf (DeclType l r) = l <--> r

instance Located BlockDeclType where
  locOf (BlockDeclType l r) = l <--> r

-------------------------------------------------------------------------------

instance Located Stmt where
  locOf (Skip l) = locOf l
  locOf (Abort l) = locOf l
  locOf (Assign l _ r) = l <--> r
  locOf (AAssign l _ _ _ _ r) = l <--> r
  locOf (Assert l _ r) = l <--> r
  locOf (LoopInvariant l _ _ _ _ _ r) = l <--> r
  locOf (Do l _ r) = l <--> r
  locOf (If l _ r) = l <--> r
  locOf (SpecQM l) = l
  locOf (Spec l _ r) = l <--> r
  locOf (Proof l _ r) = l <--> r

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
  locOf (TInt l) = l
  locOf (TBool l) = l
  locOf (TChar l) = l

instance Located Type where
  locOf (TParen l _ r) = l <--> r
  locOf (TBase a) = locOf a
  locOf (TArray l _ _ r) = l <--> r
  locOf (TFunc l _ r) = l <--> r
  locOf (TVar x) = locOf x

--------------------------------------------------------------------------------

instance Located Expr where
  locOf (Paren l _ r) = l <--> r
  locOf (Lit x) = locOf x
  locOf (Var x) = locOf x
  locOf (Const x) = locOf x
  locOf (Op x) = locOf x
  locOf (Chain e1 op e2) = e1 <--> op <--> e2
  locOf (Arr l _ _ r) = l <--> r
  locOf (App x y) = x <--> y
  locOf (Quant l _ _ _ _ _ _ r) = l <--> r

--------------------------------------------------------------------------------

instance Located Lit where
  locOf (LitInt _ l) = l
  locOf (LitBool _ l) = l
  locOf (LitChar _ l) = l
