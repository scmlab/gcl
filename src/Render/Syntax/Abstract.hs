{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Render.Syntax.Abstract where

import Data.Loc (locOf)
import Data.Loc.Range
import qualified Data.Map as Map
import Pretty.Variadic (Variadic (..), var)
import Render.Class
import Render.Element
import Render.Syntax.Common ()
import Syntax.Abstract
import Syntax.Abstract.Located ()
import Syntax.Common (Fixity (..), Op, classify, Name)
import Data.Map (Map)

--------------------------------------------------------------------------------

-- instance Render Stmt where
--   render (Skip _) = "skip"
--   render (Abort _) = "abort"
--   render (Assign xs es _) =
--     mconcat (punctuateE ", " (map render xs))
--       <> ":= "
--       <> mconcat (punctuateE ", " (map render es))
--   render (Assert p _) =
--     "{ " <> render p <> " }"
--   render (LoopInvariant p bnd _) =
--     "{ " <> render p <> " , bnd: " <> render bnd <> " }"
--   render (Do gdCmds _) =
--     "do"
--       <> line
--       <> vsep (map (\x -> " |" <+> render x <> line) gdCmds)
--       <> "od"
--   render (If gdCmds _) =
--     "if"
--       <> line
--       <> vsep (map (\x -> " |" <+> render x <> line) gdCmds)
--       <> "fi"
--   render (Spec content _) = "[!" <> render content <> "!]"
--   render (Proof _) = "{-  -}"

--------------------------------------------------------------------------------

-- instance Render GdCmd where
--   render (GdCmd guard body _) =
--     render guard
--       <+> "->"
--       <+> mconcat (map render body)

------------------------------------------------------------------------------

-- | Literals
instance Render Lit where
  render (Num i) = render (show i)
  render (Bol b) = render (show b)
  render (Chr c) = render (show c)

--------------------------------------------------------------------------------

-- | Expr
instance RenderBlock Expr where
  renderBlock expr = blockE Nothing (fromLoc (locOf expr)) (render expr)

instance Render Expr where
  renderPrec n expr = case handleExpr n expr of
    Expect _ -> mempty
    Complete s -> s

handleExpr :: Int -> Expr -> Variadic Expr Inlines
handleExpr _ (Paren x) = return $ render x
handleExpr _ (Var x l) = return $ tempHandleLoc l $ render x
handleExpr _ (Const x l) = return $ tempHandleLoc l $ render x
handleExpr _ (Lit x l) = return $ tempHandleLoc l $ render x
handleExpr n (Op x) = handleOp n x
handleExpr _ (Chain a op b _) =
  return $
    render a
      <+> render op
      <+> render b
handleExpr n (App p q _) = case handleExpr n p of
  Expect f -> f q
  Complete s -> do
    t <- handleExpr n q
    -- see if the second argument is an application, apply parenthesis when needed
    return $ case q of
      App {} -> s <+> parensIf n (-1) t
      _ -> s <+> t
handleExpr _ (Lam p q _) = return $ "λ" <+> render p <+> "→" <+> render q
handleExpr _ (Hole _) = return "{!!}"
handleExpr _ (Quant op xs r t _) =
  return $
    "⟨"
      <+> render op
      <+> horzE (map render xs)
      <+> ":"
      <+> render r
      <+> ":"
      <+> render t
      <+> "⟩"
handleExpr _ (Subst before env after) =
  return $ substE (render before) (render env) (if isLam after then parensE (render after) else render after)
  where
    isLam :: Expr -> Bool
    isLam Lam {} = True
    isLam _ = False

instance Render Subst where
  render = render . fst . Map.mapEither id

instance Render (Map Name Expr) where
  render env
    | null env = mempty
    | otherwise = "[" <+> exprs <+> "/" <+> vars <+> "]"
      where
        vars = punctuateE "," $ map render $ Map.keys env
        exprs = punctuateE "," $ map render $ Map.elems env

--------------------------------------------------------------------------------

handleOp :: Int -> Op -> Variadic Expr Inlines
handleOp n op = case classify op of
  Infix m -> do
    p <- var
    q <- var
    return $
      parensIf n m $
        renderPrec (succ m) p
          <+> render op
          <+> renderPrec (succ m) q
  InfixL m -> do
    p <- var
    q <- var
    return $
      parensIf n m $
        renderPrec m p
          <+> render op
          <+> renderPrec (succ m) q
  InfixR m -> do
    p <- var
    q <- var
    return $
      parensIf n m $
        renderPrec (succ m) p
          <+> render op
          <+> renderPrec m q
  Prefix m -> do
    p <- var
    return $ parensIf n m $ render op <+> renderPrec m p
  Postfix m -> do
    p <- var
    return $ parensIf n m $ renderPrec m p <+> render op

--------------------------------------------------------------------------------

-- | Type
instance Render Type where
  render (TBase TInt _) = "Int"
  render (TBase TBool _) = "Bool"
  render (TBase TChar _) = "Char"
  render (TFunc a b _) = render a <+> "→" <+> render b
  render (TArray i b _) = "array" <+> render i <+> "of" <+> render b
  render (TVar i _) = "TVar" <+> render i

-- | Interval
instance Render Interval where
  render (Interval (Including a) (Including b) _) =
    "[" <+> render a <+> ".." <+> render b <+> "]"
  render (Interval (Including a) (Excluding b) _) =
    "[" <+> render a <+> ".." <+> render b <+> ")"
  render (Interval (Excluding a) (Including b) _) =
    "(" <+> render a <+> ".." <+> render b <+> "]"
  render (Interval (Excluding a) (Excluding b) _) =
    "(" <+> render a <+> ".." <+> render b <+> ")"

--------------------------------------------------------------------------------

parensIf :: Int -> Int -> Inlines -> Inlines
parensIf n m
  | n > m = parensE
  | otherwise = id
