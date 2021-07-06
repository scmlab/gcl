module GCL.Substitute where

import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Text                      ( Text )
import           Syntax.Abstract                ( Expr(..), Reason(..) )
import           Syntax.Common                  ( Name
                                                , nameToText
                                                )
import GCL.Predicate (Pred (..))
import Data.Loc (locOf)
import Debug.Trace

------------------------------------------------------------------

-- data Reduction = Reduction Expr Expr
            --    | 

-- reduce :: Expr -> Reduction
-- reduce expr = Reduction expr (betaReduction expr)

reducePred :: Scopes -> Pred -> Pred
reducePred scopes x = case x of
    Constant a -> Constant (reduceExpr a)
    GuardIf a l -> GuardIf (reduceExpr a) l
    GuardLoop a l -> GuardLoop (reduceExpr a) l
    Assertion a l -> Assertion (reduceExpr a) l
    LoopInvariant a b l -> LoopInvariant (reduceExpr a) (reduceExpr b) l
    Bound a l -> Bound (reduceExpr a) l
    Conjunct as -> Conjunct (map (reducePred scopes) as)
    Disjunct as -> Disjunct (map (reducePred scopes) as)
    Negate a -> Negate (reducePred scopes a)
    where
        reduceExpr :: Expr -> Expr
        reduceExpr = extract . reduce scopes . Value

------------------------------------------------------------------

extract :: Reason -> Expr
extract (ExpandOthers      _ x) = extract x
extract (ExpandUserDefined before after) = Expand after before (extract after)
extract (ExpandStuck name)      = Const name (locOf name)
extract (Reduce            _ x) = extract x
extract (ReduceSub         _ x) = x
extract (Value x              ) = x

reduce' :: Scopes -> Expr -> (Reason, Expr)
reduce' scopes expr =
    let reason = reduceValue scopes expr in (reason, extract reason)


reduce :: Scopes -> Reason -> Reason
reduce scopes expr = case expr of
    Value val -> reduceValue scopes val
    others -> others

reduceValue :: Scopes -> Expr -> Reason
reduceValue scopes expr = case expr of

    Paren e l ->
        let (eReason, e') = reduce' scopes e
        in  ReduceSub [eReason] $ Paren e' l

    Var name _ -> case lookupScopes scopes name of
        Nothing -> error $ "panic: " ++ show (nameToText name) ++ " is not in scope"
        Just (UserDefinedBinding binding) ->
            ExpandUserDefined expr (reduce scopes binding)
        Just (OthersBinding binding) -> ExpandOthers name (reduce scopes binding)
        Just NoBinding -> ExpandStuck name

    Const name _ -> case lookupScopes scopes name of
        Nothing -> error $ "panic: " ++ show (nameToText name) ++ " is not in scope"
        Just (UserDefinedBinding binding) ->
            ExpandUserDefined expr (reduce scopes binding)
        Just (OthersBinding binding) -> ExpandOthers name (reduce scopes binding)
        Just NoBinding -> ExpandStuck name

    Chain a op b l ->
        let (aReason, a') = reduce' scopes a
            (bReason, b') = reduce' scopes b
        in  ReduceSub [aReason, bReason] $ Chain a' op b' l

    App a b l ->
        -- reduce "a" all the way down 
        let (aReason, a') = reduce' scopes a
            (bReason, b') = reduce' scopes b
        in case a' of
            -- ExpandUserDefined name _ -> traceShow "ExpandUserDefined" $ do
            --     case a' of 
            --         -- "App a' b'" is a redex 
            --         Lam n x _ ->
            --             -- reduce "x" all the way down 
            --             traceShow "USER DEFINED LAM" $ 
            --                 ExpandUserDefined name $ reduceValue (Map.singleton (nameToText n) (OthersBinding bReason) : scopes) x
            --         -- "App a' b'" is not a redex 
            --         _ -> traceShow ("otherwise", a') $ ReduceSub [aReason, bReason] $ App a' b' l

            --     -- ExpandUserDefined name x

            -- traceShow "OTHERWISE" $ case a' of 


                -- "App a' b'" is a redex 
                Expand reasons before after ->

                    case after of 
                        -- "App after b'" is a redex 
                        Lam n x _ ->
                            let before = App a' b' l 
                                after = reduceValue (Map.singleton (nameToText n) (OthersBinding bReason) : scopes) x
                            in ExpandUserDefined before after
                            -- reduce "x" all the way down 
                            
                        -- "App after b'" is not a redex 
                        _ -> ReduceSub [aReason, bReason] $ App a' b' l
                    -- traceShow "LAM" $ reduceValue (Map.singleton (nameToText n) (OthersBinding bReason) : scopes) x

                -- "App a' b'" is a redex 
                Lam n x _ ->
                    -- reduce "x" all the way down 
                    traceShow "LAM" $ reduceValue (Map.singleton (nameToText n) (OthersBinding bReason) : scopes) x
                -- "App a' b'" is not a redex 
                _ -> ReduceSub [aReason, bReason] $ App a' b' l

-- extract (ExpandOthers      _ x) = extract x
-- extract (ExpandUserDefined y x) = Expand x y (extract x)
-- extract (ExpandStuck name)      = Const name (locOf name)
-- extract (Reduce            _ x) = extract x
-- extract (ReduceSub         _ x) = x
-- extract (Value x              ) = x

    -- App (Lam n x _) b l ->
    --     let (bReason, b') = reduce' scopes b
    --         (aReason, a') =
    --             reduce' (Map.singleton (nameToText n) (OthersBinding bReason) : scopes) x

    --     in  ReduceSub [aReason, bReason] $ App a' b' l

    -- -- is not a redex 
    -- App a b l ->
    --     let (aReason, a') = reduce' scopes a
    --         (bReason, b') = reduce' scopes b
    --     in  ReduceSub [aReason, bReason] $ App a' b' l

    others -> Value others



------------------------------------------------------------------

type Scopes = [Scope Binding]

data Binding 
    = UserDefinedBinding Reason 
    | NoBinding
    | OthersBinding Reason
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

