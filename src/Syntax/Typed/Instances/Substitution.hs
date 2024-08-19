{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances,
             FlexibleContexts, MonoLocalBinds #-}
module Syntax.Typed.Instances.Substitution where

import           Control.Monad                  ( forM )
import           Prelude hiding                 ( lookup )
import           Data.Loc
import           Data.Map hiding                ( map )
import qualified Data.Map as Map
import           Prelude                 hiding ( Ordering(..) )
import           Syntax.Typed.Types
import           Syntax.Substitution
import           GCL.Common
import           Syntax.Common
import           Syntax.Typed.Instances.Free ()
import           Syntax.Abstract.Types          ( Type(..) )

instance Variableous Expr Type where
  isVar (Var   x t _) = Just (x, t)
  isVar (Const x t _) = Just (x, t)
  isVar _             = Nothing
  mkVar x t l         = Var x t l

instance Fresh m => Substitutable m Expr Expr where
  subst _ e@(Lit _ _ _) = return e
  subst sb (Var x t l) =
    return . maybe (Var   x t l) id $ Map.lookup (nameToText x) sb
  subst sb (Const x t l) =
    return . maybe (Const x t l) id $ Map.lookup (nameToText x) sb
  subst _ e@(Op _ _) = return e
  subst sb (Chain ch) = Chain <$> subst sb ch
  subst sb (App e1 e2 l) =
    App <$> subst sb e1 <*> subst sb e2 <*> pure l
  subst sb (Lam x t e l)
     | nameToText x `elem` keys sb = return (Lam x t e l)
     | otherwise = do
         (xs, e', _) <- substBinder sb [(x,t)] e
         let x' = fst (head xs)
         return (Lam x' t e' l)
  subst sb (Quant op xs ran body l) = do
       (xs', (ran', body'), _) <- undefined -- SCM, TODO substBinder sb xs (ran, body)
       return $ Quant op xs' ran' body' l
  subst sb (ArrIdx a i l) =
    ArrIdx <$> subst sb a <*> subst sb i <*> pure l
  subst sb (ArrUpd a i v l) =
    ArrUpd <$> subst sb a <*> subst sb i <*> subst sb v <*> pure l
  subst sb (Case expr clauses l) = Case <$> subst sb expr <*> subst sb clauses <*> pure l
  subst sb (Subst e tb) =
    Subst <$> subst sb e <*> forM tb (\(x, f) ->
        (,) x <$> subst sb f)

instance Fresh m => Substitutable m CaseClause Expr where
  subst sb (CaseClause pat rhs) = CaseClause pat <$> subst sb rhs

instance Fresh m => Substitutable m Chain Expr where
  subst sb (Pure expr) = Pure <$> subst sb expr
  subst sb (More ch' op t expr) = More <$> subst sb ch' <*> pure op <*> pure t <*> subst sb expr

instance Fresh m => Substitutable m Stmt Expr where
