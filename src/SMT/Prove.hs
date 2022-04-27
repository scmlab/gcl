module SMT.Prove (makeProvable) where
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
        p1 = toSBool e1
        p2 = toSBool e2

    -- create symbolic bindings
    environment <- generateEnvironment varList len

    -- create constraints from global properties
    mapM_ (\prop -> constrain' $ runTranslator (toSBool prop) environment) props


    -- return the evaluated expression to prove
--    return $ runReader p1 environment
--         .=> runReader p2 environment
    let maybeImplication = runTranslator (buildImplication p1 p2) environment
    case maybeImplication of
        (Left e) -> do 
            lift $ print e
            return $ literal False
        (Right implication) -> do
            return implication

    
    where constrain' :: Either Error SBool -> SymbolicT IO ()
          constrain' (Left _) = return ()
          constrain' (Right b) = constrain b

          buildImplication :: Translator SBool -> Translator SBool -> Translator SBool
          buildImplication p1 p2 = do
              b1 <- p1
              b2 <- p2
              return $ b1 .=> b2

type Name2Num = Map.Map Text SInteger
type Name2Bol = Map.Map Text SBool
type Name2Chr = Map.Map Text SChar

-- Error "function name" "first parameter"
newtype Error = NotDefinedError Text deriving (Show)
type Environment = (Name2Num, Name2Bol, Name2Chr)
type Translator = ExceptT Error (Reader Environment)
runTranslator :: Translator a -> Environment -> Either Error a
runTranslator x = runReader (runExceptT x)

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
handleBinaryArithNumOp (Exp _) e1 e2 = applyBinaryArithNumOp (.^) e1 e2
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
