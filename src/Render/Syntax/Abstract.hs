{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Render.Syntax.Abstract where

import qualified Data.Map                      as Map
import           Pretty.Variadic                ( Variadic(..)
                                                , var
                                                )
import           Render.Class
import           Render.Element
import           Render.Syntax.Common           ( )
import           Syntax.Abstract
-- import           Syntax.Abstract.Util           ( assignBindingToExpr )
import           Syntax.Common                  ( ArithOp(..)
                                                , Fixity(..)
                                                , Op(..)
                                                , classify
                                                )

------------------------------------------------------------------------------

-- | Literals
instance Render Lit where
  render (Num i) = render (show i)
  render (Bol b) = render (show b)
  render (Chr c) = render (show c)
  render Emp     = "emp"

--------------------------------------------------------------------------------

-- | Expr
instance Render Expr where
  renderPrec n expr = case handleExpr n expr of
    Expect   _ -> "<Render pending>"
    Complete s -> s

handleExpr :: Int -> Expr -> Variadic Expr Inlines
handleExpr n (Var   x l) = return $ tempHandleLoc l $ renderPrec n x
handleExpr n (Const x l) = return $ tempHandleLoc l $ renderPrec n x
handleExpr n (Lit   x l) = return $ tempHandleLoc l $ renderPrec n x
handleExpr n (Op x     ) = handleOp n x
handleExpr n (App (App (Op op@(ChainOp _)) p _) q _) = do
  return $ renderPrec n p <+> render op <+> renderPrec n q
handleExpr n (App p q _) = do
  case handleExpr (succ n) p of
    Expect   p' -> p' q
    Complete p' -> do

      -- function application is left-associative with the precedence of 9
      let precedence = 9
      t <- handleExpr (succ precedence) q
      return $ parensIf n precedence $ p' <+> t

handleExpr n (Lam p q _) =
  return $ parensIf n 0 $ "λ" <+> render p <+> "→" <+> render q
handleExpr _ (Quant op xs r t _) =
  return
    $   "⟨"
    <+> renderQOp op
    <+> horzE (map render xs)
    <+> ":"
    <+> render r
    <+> ":"
    <+> render t
    <+> "⟩"
 where
  renderQOp (Op (ArithOp (Conj  _))) = "∀"
  renderQOp (Op (ArithOp (ConjU _))) = "∀"
  renderQOp (Op (ArithOp (Disj  _))) = "∃"
  renderQOp (Op (ArithOp (DisjU _))) = "∃"
  renderQOp (Op (ArithOp (Add   _))) = "Σ"
  renderQOp (Op (ArithOp (Mul   _))) = "Π"
  renderQOp (Op op'                ) = render op'
  renderQOp op'                      = render op'
handleExpr n (DisplaySubst expr _ mapping) =
  return $ renderPrec n expr <+> render mapping
handleExpr n (Redex redex   ) = return $ renderPrec n redex
handleExpr _ (ArrIdx e1 e2 _) = return $ render e1 <> "[" <> render e2 <> "]"
handleExpr _ (ArrUpd e1 e2 e3 _) =
  return $ "(" <+> render e1 <+> ":" <+> render e2 <+> "↣" <+> render e3 <+> ")"
    -- SCM: need to print parenthesis around e1 when necessary.
handleExpr _ (Case expr cases _) =
  return $ "case" <+> render expr <+> "of" <+> vertE (map render cases)

instance Render Redex where
  renderPrec n (Rdx _index _ before after) =
    expandE (renderPrec n before) mempty (renderPrec n after)

instance Render Mapping where
  render env | null env  = mempty
             | otherwise = "[" <+> vars <+> "\\" <+> exprs <+> "]"
   where
    vars  = punctuateE "," $ map render $ Map.keys env
    exprs = punctuateE "," $ map render $ Map.elems env

--------------------------------------------------------------------------------

instance Render Case where
  render (CaseConstructor ctor binders body) =
    render ctor <+> horzE (map render binders) <+> "->" <+> render body

instance Render Pattern where
  render (PattBinder   a) = render a
  render (PattWildcard _) = "_"
  render (PattConstructor ctor patterns) =
    render ctor <+> horzE (map render patterns)

--------------------------------------------------------------------------------

handleOp :: Int -> Op -> Variadic Expr Inlines
handleOp n op = case classify op of
  Infix m -> do
    p <- var
    q <- var
    return
      $   parensIf n m
      $   renderPrec (succ m) p
      <+> render op
      <+> renderPrec (succ m) q
  InfixL m -> do
    p <- var
    q <- var
    return
      $   parensIf n m
      $   renderPrec m p
      <+> render op
      <+> renderPrec (succ m) q
  InfixR m -> do
    p <- var
    q <- var
    return
      $   parensIf n m
      $   renderPrec (succ m) p
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
  render (TBase TInt  _  ) = "Int"
  render (TBase TBool _  ) = "Bool"
  render (TBase TChar _  ) = "Char"
  render (TFunc  a b    _) = render a <+> "→" <+> render b
  render (TArray i b    _) = "array" <+> render i <+> "of" <+> render b
  render (TCon   n args _) = render n <+> horzE (map render args)
  render (TVar i _       ) = "TVar" <+> render i

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
parensIf n m | n > m     = parensE
             | otherwise = id
