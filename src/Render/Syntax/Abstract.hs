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
import Syntax.Common (ArithOp, Fixity (..), classifyArithOp)

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
handleExpr _ (Var x _) = return $ render x
handleExpr _ (Const x _) = return $ render x
handleExpr _ (Lit x _) = return $ render x
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
      App {} -> s <+> parensIf n 0 t
      _ -> s <+> t
handleExpr _ (Lam p q _) = return $ "λ" <+> render p <+> "→" <+> render q
handleExpr _ (Hole _) = return "{!!}"
handleExpr _ (Quant (Left op) xs r t _) =
  return $
    "⟨"
      <+> render op
      <+> horzE (map render xs)
      <+> ":"
      <+> render r
      <+> ":"
      <+> render t
      <+> "⟩"
handleExpr _ (Quant (Right op) xs r t _) =
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
  return $ substE (render before <+> render env) (if isLam after then parensE (render after) else render after)
  where
    isLam :: Expr -> Bool
    isLam Lam {} = True
    isLam _ = False

instance Render Subst where
  render env = "[" <+> exprs <+> "/" <+> vars <+> "]"
    where
      vars = punctuateE "," $ map render $ Map.keys env
      exprs = punctuateE "," $ map render $ Map.elems env

--------------------------------------------------------------------------------

handleOp :: Int -> ArithOp -> Variadic Expr Inlines
handleOp n op = case classifyArithOp op of
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
  render = renderLocatedAndPrettified

-- | Interval
instance Render Interval where
  render = renderLocatedAndPrettified

--------------------------------------------------------------------------------

parensIf :: Int -> Int -> Inlines -> Inlines
parensIf n m
  | n > m = parensE
  | otherwise = id
