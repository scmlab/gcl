{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module GCL.Substitute where

import           Data.Loc                       ( locOf )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Text                      ( Text )
import           GCL.Predicate                  ( Pred(..) )
import           Syntax.Abstract                ( Bindings(LetBinding)
                                                , Expr(..)
                                                , Reason(..)
                                                )
import           Syntax.Abstract.Util           ( bindingsToExpr )
import           Syntax.Common                  ( Name
                                                , nameToText
                                                )
import Debug.Trace
import Pretty

------------------------------------------------------------------

reducePred :: Scopes -> Pred -> Pred
reducePred scopes x = case x of
    Constant a          -> Constant (reduceExpr a)
    GuardIf   a l       -> GuardIf (reduceExpr a) l
    GuardLoop a l       -> GuardLoop (reduceExpr a) l
    Assertion a l       -> Assertion (reduceExpr a) l
    LoopInvariant a b l -> LoopInvariant (reduceExpr a) (reduceExpr b) l
    Bound a l           -> Bound (reduceExpr a) l
    Conjunct as         -> Conjunct (map (reducePred scopes) as)
    Disjunct as         -> Disjunct (map (reducePred scopes) as)
    Negate   a          -> Negate (reducePred scopes a)
  where
    reduceExpr :: Expr -> Expr
    reduceExpr = extract . reduce scopes . Value

------------------------------------------------------------------

extract :: Reason -> Expr
extract (ExpandContinue _ x              ) = extract x
extract (ExpandPause reasons before after) = Expand reasons before after
extract (ExpandStuck name                ) = Const name (locOf name)
extract (Congruence _ _ after            ) = extract after
extract (Value x                         ) = x

reduce' :: Scopes -> Expr -> (Reason, Expr)
reduce' scopes expr =
    let reason = reduceValue scopes expr in (reason, extract reason)


reduce :: Scopes -> Reason -> Reason
reduce scopes expr = case expr of
    Value val -> reduceValue scopes val
    others    -> others

reduceValue :: Scopes -> Expr -> Reason
reduceValue scopes expr = case expr of

    Paren e l ->
        let (eReason, e') = reduce' scopes e
        in  Congruence [eReason] expr (Value $ Paren e' l)

    Var name _ -> case lookupScopes scopes name of
        Nothing ->
            error $ "panic: " ++ show (nameToText name) ++ " is not in scope"
        Just (UserDefinedBinding binding) -> ExpandPause [] expr binding
        Just (AssignmentBinding binding) ->
            ExpandContinue name (reduce scopes binding)
        Just NoBinding -> ExpandStuck name

    Const name _ -> case lookupScopes scopes name of
        Nothing ->
            error $ "panic: " ++ show (nameToText name) ++ " is not in scope"
        Just (UserDefinedBinding binding) -> ExpandPause [] expr binding
        Just (AssignmentBinding binding) ->
            ExpandContinue name (reduce scopes binding)
        Just NoBinding -> ExpandStuck name

    Chain a op b l ->
        let (aReason, a') = reduce' scopes a
            (bReason, b') = reduce' scopes b
        in  Congruence [aReason, bReason] expr $ Value $ Chain a' op b' l

    App a b l ->
        let
            (aReason, a') = reduce' scopes a
            -- (bReason, b') = reduce' scopes b
            -- after         = App a' b' l
        in
            case a' of
                Expand _ _ (Lam n x _) -> 
                    let scopes' = Map.singleton (nameToText n)
                                            (AssignmentBinding bReason)
                            : scopes
                        (bReason, b') = reduce' scopes' b
                        after         = App a' b' l
                    in 
                        traceShow (pretty n, pretty x, pretty (reduceValue scopes' x), pretty scopes') $
                    let reason = reduceValue scopes' x
                    in  Congruence [aReason, bReason] expr
                            $ ExpandPause [reason] after (extract reason)

                -- "App a' b'" is a redex 
                Lam n x _ -> 
                    let scopes' = Map.singleton (nameToText n)
                                            (AssignmentBinding bReason)
                            : scopes
                        (bReason, _) = reduce' scopes' b
                    in 
                    Congruence [aReason, bReason] expr $ reduceValue scopes'
                    x
                -- "App a' b'" is not a redex 
                _ -> 
                    let (bReason, b') = reduce' scopes b
                        after         = App a' b' l
                    in 
                    Congruence [aReason, bReason] expr (Value after)

    Lam arg body l -> 
        let (bodyReason, body') = reduce' scopes body
        in Congruence [bodyReason] expr (Value $ Lam arg body' l)
            -- traceShow ("LAMBDA", pretty arg, pretty body, pretty scopes) undefined
        -- let scopes' = Map.singleton (nameToText n)
        --                                     (AssignmentBinding bReason)
        --                     : scopes
        --                 (bReason, b') = reduce' scopes' b
        --                 after         = App a' b' l
        --             in 

    others -> Value others



------------------------------------------------------------------

type Scopes = [Scope Binding]

data Binding
    = UserDefinedBinding Expr
    | NoBinding
    | AssignmentBinding Reason
    deriving (Show)

instance Pretty (Map Text Binding) where 
    pretty = pretty . Map.toList

instance Pretty Binding where 
    pretty (UserDefinedBinding expr) = "UserDefinedBinding" <+> pretty expr
    pretty (AssignmentBinding reason) = "AssignmentBinding" <+> pretty reason
    pretty NoBinding = "NoBinding"

instance Pretty Reason where 
    pretty (Value val) = "Value" <+> pretty val
    pretty (ExpandContinue name reason) = pretty name <+> "=Continue=>" <+> pretty reason
    pretty (ExpandPause _ before after) = pretty before <+> "=Pause=>" <+> pretty after
    pretty (ExpandStuck name) = "Stuck" <+> pretty name
    pretty (Congruence _ before after) = pretty before <+> "=Cong=>" <+> pretty after
    -- pretty (ExpandStuck name) = "Stuck" <+> pretty name
    -- | Congruence [Reason] Expr Reason

------------------------------------------------------------------

-- | A "Scope" is a mapping from names to values
type Scope a = Map Text a

-- | See if a name is in a series of scopes (from local to global)
-- | Return the first result (which should be the most local target)
lookupScopes :: [Scope a] -> Name -> Maybe a
lookupScopes scopes name = foldl findFirst Nothing scopes
  where
    findFirst :: Maybe a -> Scope a -> Maybe a
    findFirst (Just found) _     = Just found
    findFirst Nothing      scope = Map.lookup (nameToText name) scope

scopeFromLetBindings :: Map Name (Maybe Bindings) -> Scope Binding
scopeFromLetBindings = Map.mapKeys nameToText . fmap toBinding
  where
    toBinding Nothing = NoBinding
    toBinding (Just (LetBinding x)) = UserDefinedBinding x
    toBinding (Just others) = AssignmentBinding (Value $ bindingsToExpr others)

scopeFromAssignments :: [Name] -> [Expr] -> Scope Binding
scopeFromAssignments xs es = Map.mapKeys nameToText $ Map.fromList $ zip
    xs
    (map (AssignmentBinding . Value) es)
