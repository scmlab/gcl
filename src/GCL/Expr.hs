{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module GCL.Expr where

import GCL.EnumHole

import Data.Char(isAlpha)
import qualified Data.Map as Map
import Data.Map (Map)


type Idx = Int
type EIdx = Int

type OpName = String

data Lit = Num Int | Bol Bool
  deriving Eq

data Expr = Var String
          | Const String
          | Lit Lit
          | Op OpName [Expr]
          | HoleE (Maybe EIdx) [Subst]
  deriving Eq

instance EnumHole Expr where
  enumHole (Var x)    = return $ Var x
  enumHole (Const x)    = return $ Const x
  enumHole (Lit n)    = return $ Lit n
  enumHole (Op op es) = Op op <$> mapM enumHole es
  enumHole (HoleE Nothing subs) = do
    subs' <- mapM enumHole subs
    i <- freshHole
    return (HoleE (Just i) subs')
  enumHole (HoleE (Just i) subs) = HoleE (Just i) <$> mapM enumHole subs

type Subst = Map String Expr

instance EnumHole Subst where
  -- `const` for throwing away the key, we don't need it anyway
  enumHole = Map.traverseWithKey (const enumHole)

substE :: Subst -> Expr -> Expr
substE env (Var x) =
  case Map.lookup x env of
    Just e -> e
    Nothing -> Var x
substE env (Const x) =
  case Map.lookup x env of
    Just e -> e
    Nothing -> Const x
substE _   (Lit n)     = Lit n
substE env (Op op es)  = Op op (map (substE env) es)
substE env (HoleE idx subs) = HoleE idx (env:subs)

showLitS :: Lit -> ShowS
showLitS (Num n) = showsPrec 0 n
showLitS (Bol b) = showsPrec 0 b

showExprS :: Expr -> ShowS
showExprS (Var x) = (x ++)
showExprS (Const x) = (x ++)
showExprS (Lit n) = showLitS n
showExprS (Op op [x,y]) | not (isAlpha (head op)) =
    ('(':) . showExprS x . (' ':) . (op ++) . (' ':) . showExprS y . (')':)
showExprS (Op op es) =
  ('(':) . (op ++) . (' ':) . showArgs es . (')':)
showExprS (HoleE Nothing _subs) = ("[_]" ++)
showExprS (HoleE (Just i) _subs) =
  ('[':) . showsPrec 0 i . (']':)

showArgs :: [Expr] -> ShowS
showArgs [] = id
showArgs [x] = showExprS x
showArgs (x:xs) = showExprS x . (' ':) . showArgs xs

instance Show Expr where
  showsPrec _ = showExprS
