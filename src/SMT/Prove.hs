module SMT.Prove (makeProvable, Error(..), provableIsOriginal) where
-- Packages
import Data.SBV
import Data.Text.Internal ( Text )
import qualified Data.Functor as F
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Prelude                 hiding ( Ordering(..) )
import Control.Monad.State.Lazy
import Control.Monad.Except
import Control.Monad.Reader

-- GCL
import Pretty
import GCL.Common
import GCL.Predicate
import GCL.Predicate.Util
import Syntax.Abstract
import Syntax.Common
import Debug.Trace

makeProvable :: PO -> [Expr] -> Symbolic SBool
makeProvable po props = let e1 = toExpr $ poPre po
                            e2 = toExpr $ poPost po
                            in makeImplication e1 e2 props

makeImplication :: Expr -> Expr -> [Expr] -> Symbolic SBool
makeImplication e1 e2 props = do
    -- inspect the expressions (DEBUG)
    lift (print $ pretty e1)
    lift (print $ pretty e2)

    -- capture free variables
    let propVarSet = Set.unions $ map fv props
        varList = Set.toList $ Set.map nameToText (fv e1 <> fv e2 <> propVarSet)
        len = length varList

    -- create symbolic bindings
    environment <- generateEnvironment varList len

    -- create constraints from global properties
    mapM_ (\prop -> constrain' $ runTranslator (toSBool prop) environment) props
    constrain' $ runTranslator (toSBool e1) environment


    -- return the evaluated expression to prove
--    return $ runReader p1 environment
--         .=> runReader p2 environment
    let maybeConclusion = runTranslator (toSBool e2) environment
    case maybeConclusion of
        (Left e) -> do 
            lift $ print e
            return $ literal False
        (Right conclusion) -> do
            return conclusion

    
    where constrain' :: Either Error SBool -> SymbolicT IO ()
          constrain' (Left _) = return ()
          constrain' (Right b) = constrain b

type Name2Num = Map.Map Text SInteger
type Name2Bol = Map.Map Text SBool
type Name2Chr = Map.Map Text SChar

-- Error "function name" "first parameter"
data Error = 
    NotDefinedError Text |
    PONotFoundError |
    Z3NotFoundError
    deriving (Show)


type Environment = (Name2Num, Name2Bol, Name2Chr)
type Translator = ExceptT Error (Reader Environment)
runTranslator :: Translator a -> Environment -> Either Error a
runTranslator x = runReader (runExceptT x)

provableIsOriginal :: PO -> [Expr] -> Bool
provableIsOriginal po props =
    let e1 = toExpr $ poPre po
        e2 = toExpr $ poPost po
        in foldr (\e b -> checkSBool e && b) (checkSBool e1 && checkSBool e2) props

generateEnvironment :: [Text] -> Int -> SymbolicT IO Environment
generateEnvironment varList len = do
    intVars  <- mkFreeVars len
    bolVars <- mkFreeVars len
    chrVars  <- mkFreeVars len
    let intMap = Map.fromList $ zip varList intVars
        bolMap = Map.fromList $ zip varList bolVars
        chrMap = Map.fromList $ zip varList chrVars
    -- inspect the bindings (DEBUG)
    lift (traceIO $ show bolMap)
    lift (traceIO $ show intMap)
    return (bolMap, intMap, chrMap)

bindNum :: Name -> Translator SInteger
bindNum varName = do
    let nameText = nameToText varName
    (numMap, _, _) <- ask
    return (numMap Map.! nameText)

bindBol :: Name -> Translator SBool
bindBol varName = do
    let nameText = nameToText varName
    (_, bolMap, _) <- ask
    return (bolMap Map.! nameText)

-- takes an expression that evaluates to an integer
-- and convert it to a function of the environment
toSInteger :: Expr -> Translator SInteger
toSInteger (Lit (Num i) _)    = return $ literal (fromIntegral i :: Integer)
toSInteger (Var varName _)    = bindNum varName
toSInteger (Const varName _)  = bindNum varName
toSInteger (App (Op (ArithOp op)) e _) =
    handleUnaryArithNumOp op e
toSInteger (App (App (Op (ArithOp op)) e1 _) e2 _) =
    handleBinaryArithNumOp op e1 e2
toSInteger e                  = throwError (NotDefinedError $ toText e <>  toText "toSInteger") 

-- takes an expression that evaluates to a boolean value
-- and convert it to a function of the environment
toSBool :: Expr -> Translator SBool
toSBool (Lit (Bol b) _)    = return $ literal b
toSBool (Var varName _)    = bindBol varName
toSBool (Const varName _)  = bindBol varName
toSBool (App (App (Op (ChainOp (EQProp l))) e1 _) e2 _) =
    handleChainOp (EQProp l) e1 e2
toSBool (App (App (Op (ChainOp (EQPropU l))) e1 _) e2 _) =
    handleChainOp (EQProp l) e1 e2
toSBool (App (App (Op (ChainOp op)) e1 _) e2 _) =
    handleChainOp op e1 e2
toSBool (App (Op (ArithOp op)) e _) =
    handleUnaryArithBolOp op e
toSBool (App (App (Op (ArithOp op)) e1 _) e2 _) =
    handleBinaryArithBolOp op e1 e2
toSBool _                  = throwError (NotDefinedError $ toText "toSBool")

handleChainOp :: ChainOp -> Expr -> Expr -> Translator SBool
handleChainOp (EQProp _) e1 e2 = do
    v1 <- toSBool e1
    v2 <- toSBool e2
    case e1 of
        App (App (Op (ChainOp _)) _ _) e12 _ -> do
            b1 <- toSBool e1
            v12 <- toSBool e12
            let b2 = v12 .== v2
            return $ b1 .&& b2
        _ -> return $ v1 .== v2

handleChainOp op e1 e2 = do
    v2 <- toSInteger e2
    case e1 of
        App (App (Op (ChainOp op')) _ _) e12 _ -> do
            b1 <- toSBool e1
            v12 <- toSInteger e12
            b2 <- applyChainOp op' v12 v2
            return $ b1 .&& b2
        _ -> do
            v1 <- toSInteger e1
            applyChainOp op v1 v2
    where applyChainOp :: ChainOp -> SInteger -> SInteger -> Translator SBool
          applyChainOp (EQ      _) i1 i2 = return $ i1 .== i2
          applyChainOp (NEQ     _) i1 i2 = return $ i1 ./= i2
          applyChainOp (NEQU    _) i1 i2 = return $ i1 ./= i2
          applyChainOp (LTE     _) i1 i2 = return $ i1 .<= i2
          applyChainOp (LTEU    _) i1 i2 = return $ i1 .<= i2
          applyChainOp (GTE     _) i1 i2 = return $ i1 .>= i2
          applyChainOp (GTEU    _) i1 i2 = return $ i1 .>= i2
          applyChainOp (LT      _) i1 i2 = return $ i1 .<  i2
          applyChainOp (GT      _) i1 i2 = return $ i1 .>  i2
          applyChainOp (EQProp  _) _  _  = throwError (NotDefinedError $ toText "EQProp with int")
          applyChainOp (EQPropU _) _  _  = throwError (NotDefinedError $ toText "EQProp with int")

handleUnaryArithNumOp :: ArithOp -> Expr -> Translator SInteger
handleUnaryArithNumOp (NegNum _) e  = toSInteger e F.<&> negate
handleUnaryArithNumOp _ _ = throwError (NotDefinedError $ toText "handleUnaryArithNumOp undefined")

handleUnaryArithBolOp :: ArithOp -> Expr -> Translator SBool
handleUnaryArithBolOp (Neg _) e  = toSBool e F.<&> sNot
handleUnaryArithBolOp (NegU _) e = toSBool e F.<&> sNot
handleUnaryArithBolOp _ _ = throwError (NotDefinedError $ toText "handleUnaryArithBolOp")

handleBinaryArithNumOp :: ArithOp -> Expr -> Expr -> Translator SInteger
handleBinaryArithNumOp (Add _) e1 e2 = applyBinaryArithNumOp (+) e1 e2
handleBinaryArithNumOp (Sub _) e1 e2 = applyBinaryArithNumOp (-) e1 e2
handleBinaryArithNumOp (Mul _) e1 e2 = applyBinaryArithNumOp (*) e1 e2
handleBinaryArithNumOp (Div _) e1 e2 = applyBinaryArithNumOp sDiv e1 e2
handleBinaryArithNumOp (Mod _) e1 e2 = applyBinaryArithNumOp sMod e1 e2
handleBinaryArithNumOp (Max _) e1 e2 = applyBinaryArithNumOp smax e1 e2
handleBinaryArithNumOp (Min _) e1 e2 = applyBinaryArithNumOp smin e1 e2
--handleBinaryArithNumOp (Exp _) e1 e2 = applyBinaryArithNumOp (.^) e1 e2
handleBinaryArithNumOp _ _ _ = throwError (NotDefinedError $ toText "handleBinaryArithNumOp")

handleBinaryArithBolOp :: ArithOp -> Expr -> Expr -> Translator SBool
handleBinaryArithBolOp (Implies _)  e1 e2 = applyBinaryArithBolOp (.=>) e1 e2
handleBinaryArithBolOp (ImpliesU _) e1 e2 = applyBinaryArithBolOp (.=>) e1 e2
handleBinaryArithBolOp (Conj _)     e1 e2 = applyBinaryArithBolOp (.&&) e1 e2
handleBinaryArithBolOp (ConjU _)    e1 e2 = applyBinaryArithBolOp (.&&) e1 e2
handleBinaryArithBolOp (Disj _)     e1 e2 = applyBinaryArithBolOp (.||) e1 e2
handleBinaryArithBolOp (DisjU _)    e1 e2 = applyBinaryArithBolOp (.||) e1 e2
handleBinaryArithBolOp _ _ _ = throwError (NotDefinedError $ toText "handleBinaryArithBolOp")

applyBinaryArithNumOp :: (SInteger -> SInteger -> SInteger) -> Expr -> Expr -> Translator SInteger
applyBinaryArithNumOp op e1 e2 = do
    f1 <- toSInteger e1
    f2 <- toSInteger e2
    return $ f1 `op` f2

applyBinaryArithBolOp :: (SBool -> SBool -> SBool) -> Expr -> Expr -> Translator SBool
applyBinaryArithBolOp op e1 e2 = do
    f1 <- toSBool e1
    f2 <- toSBool e2
    return $ f1 `op` f2


-- takes an expression that evaluates to an integer
-- and convert it to a function of the environment
checkSInteger :: Expr -> Bool
checkSInteger (Lit (Num _) _) = True
checkSInteger (Var _ _) = True
checkSInteger (Const _ _)  = True
checkSInteger (App (Op (ArithOp op)) e _) =
    checkHandleUnaryArithNumOp op e
checkSInteger (App (App (Op (ArithOp op)) e1 _) e2 _) =
    checkHandleBinaryArithNumOp op e1 e2
checkSInteger _ = False

-- takes an expression that evaluates to a boolean value
-- and convert it to a function of the environment
checkSBool :: Expr -> Bool
checkSBool (Lit (Bol _) _)    = True
checkSBool (Var _ _)    = True
checkSBool (Const _ _)  = True
checkSBool (App (App (Op (ChainOp (EQProp l))) e1 _) e2 _) =
    checkHandleChainOp (EQProp l) e1 e2
checkSBool (App (App (Op (ChainOp (EQPropU l))) e1 _) e2 _) =
    checkHandleChainOp (EQProp l) e1 e2
checkSBool (App (App (Op (ChainOp op)) e1 _) e2 _) =
    checkHandleChainOp op e1 e2
checkSBool (App (Op (ArithOp op)) e _) =
    checkHandleUnaryArithBolOp op e
checkSBool (App (App (Op (ArithOp op)) e1 _) e2 _) =
    checkHandleBinaryArithBolOp op e1 e2
checkSBool _                  = False

checkHandleChainOp :: ChainOp -> Expr -> Expr -> Bool
checkHandleChainOp (EQProp _) e1 e2 = 
    case e1 of
        App (App (Op (ChainOp _)) _ _) e12 _ ->
            checkSBool e12 && checkSBool e2
        _ -> checkSBool e1 && checkSBool e2

checkHandleChainOp op e1 e2 = do
    let b2 = checkSInteger e2
    case e1 of
        App (App (Op (ChainOp op')) _ _) e12 _ ->
            let b1 = checkSBool e1
                b12 = checkSInteger e12
                b = checkApplyChainOp op'
            in b1 && b2 && b12 && b
        _ ->
            let b1 = checkSInteger e1
                in checkApplyChainOp op && b1 && b2
    where checkApplyChainOp :: ChainOp -> Bool
          checkApplyChainOp (EQ      _) = True
          checkApplyChainOp (NEQ     _) = True
          checkApplyChainOp (NEQU    _) = True
          checkApplyChainOp (LTE     _) = True
          checkApplyChainOp (LTEU    _) = True
          checkApplyChainOp (GTE     _) = True
          checkApplyChainOp (GTEU    _) = True
          checkApplyChainOp (LT      _) = True
          checkApplyChainOp (GT      _) = True
          checkApplyChainOp (EQProp  _) = False
          checkApplyChainOp (EQPropU _) = False

checkHandleUnaryArithNumOp :: ArithOp -> Expr -> Bool
checkHandleUnaryArithNumOp (NegNum _) _  = True
checkHandleUnaryArithNumOp _ _ = False

checkHandleUnaryArithBolOp :: ArithOp -> Expr -> Bool
checkHandleUnaryArithBolOp (Neg _) e  = checkSBool e
checkHandleUnaryArithBolOp (NegU _) e = checkSBool e
checkHandleUnaryArithBolOp _ _ = False

checkHandleBinaryArithNumOp :: ArithOp -> Expr -> Expr -> Bool
checkHandleBinaryArithNumOp (Add _) e1 e2 = checkApplyBinaryArithNumOp e1 e2
checkHandleBinaryArithNumOp (Sub _) e1 e2 = checkApplyBinaryArithNumOp e1 e2
checkHandleBinaryArithNumOp (Mul _) e1 e2 = checkApplyBinaryArithNumOp e1 e2
checkHandleBinaryArithNumOp (Div _) e1 e2 = checkApplyBinaryArithNumOp e1 e2
checkHandleBinaryArithNumOp (Mod _) e1 e2 = checkApplyBinaryArithNumOp e1 e2
checkHandleBinaryArithNumOp (Max _) e1 e2 = checkApplyBinaryArithNumOp e1 e2
checkHandleBinaryArithNumOp (Min _) e1 e2 = checkApplyBinaryArithNumOp e1 e2
--checkHandleBinaryArithNumOp (Exp _) e1 e2 = applyBinaryArithNumOp (.^) e1 e2
checkHandleBinaryArithNumOp _ _ _ = False

checkHandleBinaryArithBolOp :: ArithOp -> Expr -> Expr -> Bool
checkHandleBinaryArithBolOp (Implies _)  e1 e2 = checkApplyBinaryArithBolOp e1 e2
checkHandleBinaryArithBolOp (ImpliesU _) e1 e2 = checkApplyBinaryArithBolOp e1 e2
checkHandleBinaryArithBolOp (Conj _)     e1 e2 = checkApplyBinaryArithBolOp e1 e2
checkHandleBinaryArithBolOp (ConjU _)    e1 e2 = checkApplyBinaryArithBolOp e1 e2
checkHandleBinaryArithBolOp (Disj _)     e1 e2 = checkApplyBinaryArithBolOp e1 e2
checkHandleBinaryArithBolOp (DisjU _)    e1 e2 = checkApplyBinaryArithBolOp e1 e2
checkHandleBinaryArithBolOp _ _ _ =  False

checkApplyBinaryArithNumOp :: Expr -> Expr -> Bool
checkApplyBinaryArithNumOp e1 e2 = checkSInteger e1 && checkSInteger e2

checkApplyBinaryArithBolOp ::  Expr -> Expr -> Bool
checkApplyBinaryArithBolOp e1 e2 = checkSBool e1 && checkSBool e2
