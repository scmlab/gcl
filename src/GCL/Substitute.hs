module GCL.Substitute where

import           Data.Loc                       ( locOf )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Text                      ( Text )
import           Debug.Trace
import           GCL.Predicate                  ( Pred(..) )
import           Syntax.Abstract                ( Expr(..)
                                                , Reason(..)
                                                )
import           Syntax.Common                  ( Name
                                                , nameToText
                                                )

------------------------------------------------------------------

-- data Reduction = Reduction Expr Expr
            --    | 

-- reduce :: Expr -> Reduction
-- reduce expr = Reduction expr (betaReduction expr)

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
-- extract (Reduce _ x                      ) = extract x
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
            (bReason, b') = reduce' scopes b
            after         = App a' b' l
        in
            case a' of
                Expand _ _ (Lam n x _) ->
                    let
                        reason = reduceValue
                            ( Map.singleton (nameToText n)
                                            (AssignmentBinding bReason)
                            : scopes
                            )
                            x
                    in  Congruence [aReason, bReason] expr $ ExpandPause [reason] after (extract reason)

                -- "App a' b'" is a redex 
                Lam n x _ -> Congruence [aReason, bReason] expr $ reduceValue
                    ( Map.singleton (nameToText n) (AssignmentBinding bReason)
                    : scopes
                    )
                    x
                -- "App a' b'" is not a redex 
                _ -> Congruence [aReason, bReason] expr (Value after)

    others -> Value others



------------------------------------------------------------------

type Scopes = [Scope Binding]

data Binding
    = UserDefinedBinding Expr
    | NoBinding
    | AssignmentBinding Reason
    deriving (Show)

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

