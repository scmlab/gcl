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
import           Syntax.Common                  ( ArithOp(..)
                                                , Fixity(..)
                                                , Op(..)
                                                , classify
                                                , isAssocOp
                                                , sameOpSym
                                                , precOf
                                                , initOrderIndex
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
  renderPrec prec expr = handleExpr prec expr

handleExpr :: PrecContext -> Expr -> Inlines
handleExpr _ (Var   x l) = tempHandleLoc l $ render x
handleExpr _ (Const x l) = tempHandleLoc l $ render x
handleExpr _ (Lit   x l) = tempHandleLoc l $ render x
handleExpr _ (Op _     ) = error "erroneous syntax given to render"
handleExpr n (App (App (Op op@(ChainOp _)) p _) q _) = do
  renderPrec n p <+> render op <+> renderPrec n q
handleExpr n (App (App (Op op) left _) right _) =  --binary operators
  parensIf n (Just op) $ 
  renderPrec (HOLEOp op) left 
       <+> render op
  <+> renderPrec (OpHOLE op) right
handleExpr n (App (Op op) e _) = case classify op of --unary operators, this case shouldn't be former than the binary case
  (Prefix, _) -> parensIf n (Just op) $ render op <+> renderPrec (OpHOLE op) e
  (Postfix, _) -> parensIf n (Just op) $ renderPrec (HOLEOp op) e <+>  render op
  _ -> error "erroneous syntax given to render"
handleExpr n (App f e _) =  -- should only be normal applications
  parensIf n Nothing $ renderPrec HOLEApp f <+> renderPrec AppHOLE e
-- handleExpr n (App p q _) = do
--   case handleExpr HOLEApp p of
--     Expect   p' -> p' q
--     Complete p' -> do

--       -- function application is left-associative with the precedence of 0
--       let precedence = 0
--       t <- handleExpr AppHOLE q
--       parensIf n (Just Nothing) $ p' <+> t

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
  renderQOp (Op (ArithOp (Conj  _))) = "∀"
  renderQOp (Op (ArithOp (ConjU _))) = "∀"
  renderQOp (Op (ArithOp (Disj  _))) = "∃"
  renderQOp (Op (ArithOp (DisjU _))) = "∃"
  renderQOp (Op (ArithOp (Add   _))) = "Σ"
  renderQOp (Op (ArithOp (Mul   _))) = "Π"
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

-- handleOp :: Int -> Op -> Variadic Expr Inlines
-- handleOp n op = case classify op of
--   Infix m -> do
--     p <- var
--     q <- var
--     return
--       $   parensIf n op
--       $   renderPrec (succ m) p
--       <+> render op
--       <+> renderPrec (succ m) q
--   InfixL m -> do
--     p <- var
--     q <- var
--     return
--       $   parensIf n m
--       $   renderPrec m p
--       <+> render op
--       <+> renderPrec (succ m) q
--   InfixR m -> do
--     p <- var
--     q <- var
--     return
--       $   parensIf n m
--       $   renderPrec (succ m) p
--       <+> render op
--       <+> renderPrec m q
--   Prefix m -> do
--     p <- var
--     return $ parensIf n m $ render op <+> renderPrec m p
--   Postfix m -> do
--     p <- var
--     return $ parensIf n m $ renderPrec m p <+> render op

--------------------------------------------------------------------------------


-- | Type
instance Render Type where
  render (TBase TInt  _  ) = "Int"
  render (TBase TBool _  ) = "Bool"
  render (TBase TChar _  ) = "Char"
  render (TTuple es      ) = "(" <+> punctuateE "," (map render es) <+> ")"
  render (TFunc  a b    _) = render a <+> "→" <+> render b
  render (TArray i b    _) = "array" <+> render i <+> "of" <+> render b
  render (TCon   n args _) = render n <+> horzE (map render args)
  render (TVar i _       ) = "TVar" <+> render i
  render (TMetaVar n     ) = "TMetaVar" <+> render n

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


-- | The second argument: Nothing means the op at-issue is application.
parensIf :: PrecContext -> Maybe Op -> Inlines -> Inlines
parensIf pc mop = case isomerismOfContextAndCurrentOp pc mop of
  Nothing    -> 
    let conditionOfOmittingParens = case classify' mop of
          Prefix  -> case pc of
            AppHOLE  -> False
            OpHOLE _ -> False
            _        -> precOfPC pc >= precOf' mop
          Postfix -> precOfPC pc >= precOf' mop
          _       -> precOfPC pc > precOf' mop || (sameOpSym' pc mop
                                                   && isAssocOp' mop)
    in
    if conditionOfOmittingParens
    then id
    else parensE
  Just Cis -> 
    if precOfPC pc > precOf' mop || sameOpSym' pc mop
    then id
    else parensE
  Just Trans   -> 
    if precOfPC pc > precOf' mop || (sameOpSym' pc mop
                                      && isAssocOp' mop)
    then id
    else parensE

  where
    -- In this scope, every "Nothing" case of "Maybe Op" means application.
    sameOpSym' :: PrecContext -> Maybe Op -> Bool
    sameOpSym' pc Nothing = case pc of
      AppHOLE -> True
      HOLEApp -> True
      _       -> False
    sameOpSym' pc (Just op) = case pc of
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
    precOfPC pc = case pc of
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