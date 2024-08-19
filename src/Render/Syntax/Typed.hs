{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Render.Syntax.Typed where

import           Render.Class
import           Render.Element
import           Render.Syntax.Common           ( )
import           Render.Syntax.Abstract hiding ( handleExpr )
import           Syntax.Typed
import           Syntax.Common                  ( ArithOp(..)
                                                , Fixity(..)
                                                , Op(..)
                                                , classify
                                                )

------------------------------------------------------------------------------


-- | Expr
instance Render Expr where
  renderPrec prec expr = handleExpr prec expr

handleExpr :: PrecContext -> Expr -> Inlines
handleExpr _ (Lit   x _ l) = tempHandleLoc l $ render x
handleExpr _ (Var   x _ l) = tempHandleLoc l $ render x
handleExpr _ (Const x _ l) = tempHandleLoc l $ render x
handleExpr _ (Op _ _) = error "erroneous syntax given to render"
handleExpr _ (Chain ch) = render ch
handleExpr n (App (App (Op (ArithOp op) _) left _) right _) =  --binary operators
  parensIf n (Just (ArithOp op)) $
  renderPrec (HOLEOp (ArithOp op)) left
       <+> render op
  <+> renderPrec (OpHOLE (ArithOp op)) right
handleExpr n (App (Op (ArithOp op) _) e _) = case classify (ArithOp op) of --unary operators, this case shouldn't be former than the binary case
  (Prefix, _) -> parensIf n (Just (ArithOp op)) $ render op <+> renderPrec (OpHOLE (ArithOp op)) e
  (Postfix, _) -> parensIf n (Just (ArithOp op)) $ renderPrec (HOLEOp (ArithOp op)) e <+>  render op
  _ -> error "erroneous syntax given to render"
handleExpr n (App f e _) =  -- should only be normal applications
  parensIf n Nothing $ renderPrec HOLEApp f <+> renderPrec AppHOLE e

handleExpr prec (Lam p _ q _) =
  let ifparens = case prec of
        NoContext -> id
        _ -> parensE
  in
  ifparens $ "λ" <+> render p <+> "→" <+> render q
handleExpr _ (Quant op xs r t _) =
   "⟨"
    <+> renderQOp op
    <+> horzE (map render xs)
    <+> ":"
    <+> render r
    <+> ":"
    <+> render t
    <+> "⟩"
 where
  renderQOp (Op (ArithOp (Conj  _)) _) = "∀"
  renderQOp (Op (ArithOp (ConjU _)) _) = "∀"
  renderQOp (Op (ArithOp (Disj  _)) _) = "∃"
  renderQOp (Op (ArithOp (DisjU _)) _) = "∃"
  renderQOp (Op (ArithOp (Add   _)) _) = "Σ"
  renderQOp (Op (ArithOp (Mul   _)) _) = "Π"
  renderQOp (Op op'                 _) = render op'
  renderQOp op'                      = render op'
handleExpr _ (ArrIdx e1 e2 _) = render e1 <> "[" <> render e2 <> "]"
handleExpr _ (ArrUpd e1 e2 e3 _) =
  "(" <+> render e1 <+> ":" <+> render e2 <+> "↣" <+> render e3 <+> ")"
    -- SCM: need to print parenthesis around e1 when necessary.
handleExpr _ (Case e cs _) = "case" <+> render e <+> "of" <+> renderManySepByComma cs -- TODO: Use semicolon instead of comma
handleExpr n (Subst e subs) =
   parensIf n Nothing $
     handleExpr AppHOLE e <+> "[" <+>
      renderManySepByComma vs <+> "\\" <+>
      renderManySepByComma es <+> "]"
  where (vs, es) = unzip subs

instance Render CaseClause where
  render (CaseClause pat expr) = render pat <+> "→" <+> render expr

instance Render Chain where -- Hopefully this is correct.
  render (Pure expr) = render expr
  render (More ch op _ expr) = render ch <+> render op <+> render expr
