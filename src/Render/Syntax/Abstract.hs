{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Render.Syntax.Abstract where

import           Data.Foldable                  ( toList )
import qualified Data.Map                      as Map
import           Render.Class
import           Render.Element
import           Render.Syntax.Common           ( )
import           Syntax.Abstract
-- import           Syntax.Abstract.Util           ( assignBindingToExpr )
-- import           Syntax.Abstract.Util           ( assignBindingToExpr )
import           Syntax.Common                  ( ArithOp(..)
                                                , TypeOp(..)
                                                , Fixity(..)
                                                , Op(..)
                                                , classify
                                                , isAssocOp
                                                , sameOpSym
                                                , precOf
                                                , initOrderIndex
                                                )
import           Data.Loc                       ( Loc(NoLoc) )

------------------------------------------------------------------------------

-- | Literals
instance Render Lit where
  render (Num i) = render (show i)
  render (Bol b) = render (show b)
  render (Chr c) = render (show c)

--------------------------------------------------------------------------------

-- | Expr
instance Render Expr where
  renderPrec prec expr = handleExpr prec expr

handleExpr :: PrecContext -> Expr -> Inlines
handleExpr _ (Var   x l) = tempHandleLoc l $ render x
handleExpr _ (Const x l) = tempHandleLoc l $ render x
handleExpr _ (Lit   x l) = tempHandleLoc l $ render x
handleExpr _ (Op _     ) = error "erroneous syntax given to render"
handleExpr _ (Chain ch ) = render ch
handleExpr n (App (App (Op op) left _) right _) =  --binary operators
  parensIf n (Just (ArithOp op)) $ 
  renderPrec (HOLEOp (ArithOp op)) left 
       <+> render op
  <+> renderPrec (OpHOLE (ArithOp op)) right
handleExpr n (App (Op op) e _) = case classify (ArithOp op) of --unary operators, this case shouldn't be former than the binary case
  (Prefix, _) -> parensIf n (Just (ArithOp op)) $ render op <+> renderPrec (OpHOLE (ArithOp op)) e
  (Postfix, _) -> parensIf n (Just (ArithOp op)) $ renderPrec (HOLEOp (ArithOp op)) e <+>  render op
  _ -> error "erroneous syntax given to render"
handleExpr n (App f e _) =  -- should only be normal applications
  parensIf n Nothing $ renderPrec HOLEApp f <+> renderPrec AppHOLE e

handleExpr prec (Lam p q _) = 
  let ifparens = case prec of
        NoContext -> id
        _ -> parensE
  in
  ifparens $ "λ" <+> render p <+> "→" <+> render q
handleExpr _ (Func name _ _) = -- display only a Func's name 
  render name
handleExpr _ (Tuple ps) =
  "(" <+> punctuateE "," (map render ps) <+> ")"
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
  renderQOp (Op (Conj  _)) = "∀"
  renderQOp (Op (ConjU _)) = "∀"
  renderQOp (Op (Disj  _)) = "∃"
  renderQOp (Op (DisjU _)) = "∃"
  renderQOp (Op (Add   _)) = "Σ"
  renderQOp (Op (Mul   _)) = "Π"
  renderQOp (Op op'                ) = render op'
  renderQOp op'                      = render op'
handleExpr n (RedexKernel name _value _freeVars mappings) =
  renderPrec n name <+> mappings'
 where
  -- reverse the stack when printing it
  mappings' = punctuateE
    " "
    (map render $ filter (not . Map.null) $ reverse $ toList mappings)
handleExpr n (RedexShell index expr) =
  substE index (renderPrec n expr)
handleExpr _ (ArrIdx e1 e2 _) = render e1 <> "[" <> render e2 <> "]"
handleExpr _ (ArrUpd e1 e2 e3 _) =
  "(" <+> render e1 <+> ":" <+> render e2 <+> "↣" <+> render e3 <+> ")"
    -- SCM: need to print parenthesis around e1 when necessary.
handleExpr _ (Case expr cases _) =
  "case" <+> render expr <+> "of" <+> vertE (map render cases)

instance Render Chain where -- Hopefully this is correct.
  render (Pure expr _) = render expr
  render (More ch op expr _) = render ch <+> render op <+> render expr

instance Render Mapping where
  render env | null env  = mempty
             | otherwise = "[" <+> vars <+> "\\" <+> exprs <+> "]"
   where
    vars  = punctuateE "," $ map render $ Map.keys env
    exprs = punctuateE "," $ map render $ Map.elems env

--------------------------------------------------------------------------------

instance Render CaseClause where
  render (CaseClause patt body) = render patt <+> "->" <+> render body

instance Render Pattern where
  render (PattLit      a) = render a
  render (PattBinder   a) = render a
  render (PattWildcard _) = "_"
  render (PattConstructor ctor patterns) =
    render ctor <+> horzE (map render patterns)

--------------------------------------------------------------------------------

-- | Type
instance Render Type where
  renderPrec _ (TBase TInt  _) = "Int"
  renderPrec _ (TBase TBool _) = "Bool"
  renderPrec _ (TBase TChar _) = "Char"
  renderPrec _ (TArray i b  _) = "array" <+> render i <+> "of" <+> render b
  renderPrec _ (TTuple _     ) = "Tuple"
  renderPrec _ (TFunc l r _  ) = "Func" -- TODO: Change this to display proper information.
  renderPrec _ (TOp op       ) = render op
  -- TODO: Add support for more than one type operators.
  renderPrec n (TApp (TApp (TOp (Arrow _)) left _) right _) =
    parensIf n (Just . TypeOp $ Arrow NoLoc) $ 
      renderPrec (HOLEOp (TypeOp (Arrow NoLoc))) left 
        <+> render (Arrow NoLoc)
        <+> renderPrec (OpHOLE (TypeOp (Arrow NoLoc))) right
  renderPrec n (TApp l r _   ) = parensIf n Nothing $ renderPrec HOLEApp l <+> renderPrec AppHOLE r
  renderPrec _ (TData n _    ) = render n
  renderPrec _ (TVar i _     ) = render i
  renderPrec _ (TMetaVar n _ ) = render n

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

-- | Kind
instance Render Kind where
  renderPrec _ (KStar _) = "*"
  renderPrec n (KFunc a b _) =
    parensIf n (Just . TypeOp $ Arrow NoLoc) $ 
      renderPrec (HOLEOp . TypeOp $ Arrow NoLoc) a 
      <+> "→"
      <+> renderPrec (OpHOLE . TypeOp $ Arrow NoLoc) b
  renderPrec _ (KMetaVar i) = render i

--------------------------------------------------------------------------------


-- | The second argument: Nothing means the op at-issue is application.
parensIf :: PrecContext -> Maybe Op -> Inlines -> Inlines
parensIf pc mop = case isomerismOfContextAndCurrentOp pc mop of
  Nothing    -> 
    let conditionOfOmittingParens = case mop of
          Just (ArithOp (Neg _)) -> commonParenOmittingCondition
          Just (ArithOp (NegU _)) -> commonParenOmittingCondition
          -- negation is treated differently from other unary operators: 
          -- others, e.g., minus: "a * (- b)", but negation: "a && ~b" doesn't need parentheses
          _ -> case classify' mop of
            Prefix  -> case pc of
              AppHOLE  -> False
              OpHOLE _ -> False
              _        -> precOfPC pc >= precOf' mop
            Postfix -> commonParenOmittingCondition
                       || precOfPC pc == precOf' mop
            _       -> commonParenOmittingCondition
                       || (sameOpSym' pc mop && isAssocOp' mop)
    in
    if conditionOfOmittingParens
    then id
    else parensE
  Just Cis -> -- e.g., In "a -> (b -> c)", "(a - b) - c", parentheses can be omitted.
    if commonParenOmittingCondition || sameOpSym' pc mop
    then id
    else parensE
  Just Trans   -> -- e.g., In "a - (b - c)", the parentheses shouldn't be omitted;
                  --  but in "a * (b * c)", since "*" is associative, the parentheses can be omitted.
    if commonParenOmittingCondition || (sameOpSym' pc mop
                                        && isAssocOp' mop)
    then id
    else parensE

  where
    commonParenOmittingCondition = precOfPC pc > precOf' mop 
                                  || (isChainPC pc && isChainOp' mop)
    isChainPC :: PrecContext -> Bool
    isChainPC pc' = case pc' of
      NoContext -> False
      AppHOLE -> False
      HOLEApp -> False
      OpHOLE op -> isChainOp' (Just op)
      HOLEOp op -> isChainOp' (Just op)

    isChainOp' :: Maybe Op -> Bool
    isChainOp' mop' = case mop' of
      Nothing -> False
      Just op -> case op of
        ChainOp _ -> True
        ArithOp _ -> False
        TypeOp _ -> False

    -- In this scope, every "Nothing" case of "Maybe Op" means application.
    sameOpSym' :: PrecContext -> Maybe Op -> Bool
    sameOpSym' pc' Nothing = case pc' of
      AppHOLE -> True
      HOLEApp -> True
      _       -> False
    sameOpSym' pc' (Just op) = case pc' of
      OpHOLE pcop -> sameOpSym pcop op
      HOLEOp pcop -> sameOpSym pcop op
      _ -> False
    
    isAssocOp' :: Maybe Op -> Bool
    isAssocOp' Nothing = False
    isAssocOp' (Just op) = isAssocOp op

    precOf' :: Maybe Op -> Int
    precOf' Nothing = initOrderIndex -1
    precOf' (Just op) = precOf op

    precOfPC :: PrecContext -> Int
    precOfPC pc' = case pc' of
      NoContext -> 99999
      AppHOLE -> precOf' Nothing
      HOLEApp -> precOf' Nothing
      OpHOLE op -> precOf op
      HOLEOp op -> precOf op

data Isomerism = Cis | Trans deriving (Show) -- taking the concept from chemistry

-- | The Nothing in the second argument means application. 
-- Recalling that application is left associative "a b c" == "(a b) c"
isomerismOfContextAndCurrentOp :: PrecContext -> Maybe Op -> Maybe Isomerism
isomerismOfContextAndCurrentOp pc mop = case pc of
  NoContext -> Nothing
  AppHOLE -> case classify' mop of
    InfixL -> Just Trans -- a - {b - c}   ==> a - (b - c)
    InfixR -> Just Cis   -- a -> {b -> c} ==> a -> b -> c
    _      -> Nothing
  HOLEApp -> case classify' mop of
    InfixL -> Just Cis   -- {a - b} - c   ==> a - b - c
    InfixR -> Just Trans -- {a -> b} -> c ==> (a -> b) -> c
    _      -> Nothing
  OpHOLE _ -> isomerismOfContextAndCurrentOp AppHOLE mop
  HOLEOp _ -> isomerismOfContextAndCurrentOp HOLEApp mop

classify' :: Maybe Op -> Fixity
classify' Nothing = InfixL
classify' (Just op) = fst $ classify op


{- the original, proved version
parensIf :: PrecContext -> Maybe Op -> Inlines -> Inlines
parensIf NoContext _ = id
parensIf AppHOLE mop = case mop of
  Nothing -> parensE      -- a {b c} ==> a (b c)
  Just _ -> parensE       -- a {b+c} ==> a (b+c)
parensIf HOLEApp mop = case mop of
  Nothing -> id           -- {a b} c ==> a b c
  Just _ -> parensE       -- {a+b} c ==> (a+b) c
parensIf _ Nothing = id   -- a + {b c} ==> a + b c
parensIf pc (Just op) = case classify op of
  (Infix, precOf_op) ->  
    if sameOpSym' pc op || precOfPC pc <= precOf_op 
    then parensE          -- {a-b}-c; {a-b}*c ==> (a-b)-c; (a-b)*c
    else id               -- {a && b} => c    ==> a && b => c
  (Prefix, precOf_op) -> 
    if precOfPC pc < precOf_op
    then parensE          -- {¬ P} f ==> (¬ P) f --- but this case is already caught above, since only application has higher precedence
    else id               -- ¬ P ∧ Q
  (Postfix, precOf_op) -> 
    if precOfPC pc < precOf_op
    then parensE 
    else id
  (InfixR, precOf_op) -> case pc of 
    OpHOLE _ ->  
      if precOfPC pc > precOf_op || sameOpSym' pc op
        then id             -- a -> {b -> c} ==> a -> b -> c
      else if precOfPC pc < precOf_op || (precOfPC pc == precOf_op 
                                         && not (sameOpSym' pc op))
        then parensE        -- a ∧ {b => c}
      else nonExhaustiveGuardError
    HOLEOp _ -> 
      if precOfPC pc > precOf_op || (sameOpSym' pc op
                                    && isAssocOp op)
        then id             -- 
      else if precOfPC pc < precOf_op || (precOfPC pc == precOf_op 
                                         && (not (sameOpSym' pc op)
                                             || not (isAssocOp op)))
        then parensE        -- {a -> b} -> c ==> (a -> b) -> c
      else nonExhaustiveGuardError
    _ -> error "These cases should be caught in first 3 cases of parensIf."
  (InfixL, precOf_op) -> case pc of -- is this case just the reverse of InfixR?
    OpHOLE _ -> 
      if precOfPC pc > precOf_op || (sameOpSym' pc op
                                    && isAssocOp op)
        then id             -- a + {b * c}, a ∧ {b ∧ c}
      else if precOfPC pc < precOf_op || (precOfPC pc == precOf_op 
                                         && (not (sameOpSym' pc op)
                                             || not (isAssocOp op)))
        then parensE        -- a * {b + c}, a ∧ {b ∨ c}, a - {b - c}
      else nonExhaustiveGuardError
    HOLEOp _ -> 
      if precOfPC pc > precOf_op || sameOpSym' pc op
        then id             -- {a * b} + c, {a - b} - c
      else if precOfPC pc < precOf_op || (precOfPC pc == precOf_op 
                                         && not (sameOpSym' pc op))
        then parensE        -- {a + b} * c, {a ∧ b} ∨ c
      else nonExhaustiveGuardError
    _ -> error "These cases should be caught in first 3 cases of parensIf."

-}