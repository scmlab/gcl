{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances,
             FlexibleContexts, MonoLocalBinds #-}
module Syntax.Typed.Instances.Free where

import           Data.Set                       ( Set
                                                , (\\)
                                                )
import qualified Data.Set                      as Set
import           Syntax.Typed.Types
import           GCL.Common
import           Syntax.Common

instance Free Expr where
 freeVars (Var   x _ _          ) = Set.singleton x
 freeVars (Const x _ _          ) = Set.singleton x
 freeVars (Op _  _              ) = mempty
 freeVars (Chain chain          ) = freeVars chain
 freeVars (Lit _ _ _            ) = mempty
 freeVars (App  e1 e2      _    ) = freeVars e1 <> freeVars e2
 freeVars (Lam  x _ e      _    ) = freeVars e \\ Set.singleton x
 freeVars (Quant op xs range term _) =
   (freeVars op <> freeVars range <> freeVars term) \\ Set.fromList xs
 freeVars (ArrIdx e1 e2 _      ) = freeVars e1 <> freeVars e2
 freeVars (ArrUpd e1 e2 e3 _   ) = freeVars e1 <> freeVars e2 <> freeVars e3
 freeVars (Case e cs _         ) = freeVars e <> Set.unions (map freeVars cs)
 freeVars (Subst e sb) = (freeVars e \\ Set.fromList dom) <> freeVars rng
    where dom = map fst sb
          rng = map snd sb

instance Free CaseClause where
  freeVars (CaseClause _ expr) = freeVars expr

instance Free Chain where
 freeVars (Pure expr)             = freeVars expr
 freeVars (More chain _op _ expr) = freeVars chain <> freeVars expr

instance Free Declaration where
  freeVars (ConstDecl ns _ expr _) =
    Set.fromList ns <> freeVars expr
  freeVars (VarDecl   ns _ expr _) =
    Set.fromList ns <> freeVars expr

instance Free Stmt where
  freeVars (Skip  _) = mempty
  freeVars (Abort _) = mempty
  freeVars (Assign ns es _) =
    Set.fromList ns <> Set.unions (map freeVars es)
  freeVars (AAssign a i e _) =
    freeVars a <> freeVars i <> freeVars e
  freeVars (Assert p _) = freeVars p
  freeVars (LoopInvariant p b _) = freeVars p <> freeVars b
  freeVars (Do gdcmds _) = Set.unions (map freeVars gdcmds)
  freeVars (If gdcmds _) = Set.unions (map freeVars gdcmds)
  freeVars (Spec  _ _ _) = mempty
  freeVars (Proof _ _ _) = mempty
  freeVars (Alloc x es _) =
     Set.singleton x <> Set.unions (map freeVars es)
  freeVars (HLookup x e _) =
     Set.singleton x <> freeVars e
  freeVars (HMutate e1 e2 _) = freeVars e1 <> freeVars e2
  freeVars (Dispose e _) = freeVars e
  freeVars (Block prog _) = freeVars prog

instance Free GdCmd where
  freeVars (GdCmd g stmts _) =
    freeVars g <> Set.unions (map freeVars stmts)

instance Free Program where
  freeVars (Program _defns decls props stmts _) =
    foldMap freeVars decls <> foldMap freeVars props <> foldMap freeVars stmts
   -- SCM: TODO: deal with defns later.
