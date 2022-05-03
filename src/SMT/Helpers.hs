
module SMT.Helpers where


import           Control.Monad.State            ( runState )
import           Control.Monad.Except
import qualified Data.IntMap                   as IntMap
import           Data.List.NonEmpty (toList)
import qualified Data.Text                     as Text
import           Error
import qualified GCL.Substitution              as Substitution
import           Server.Pipeline
import           Syntax.Abstract
import           Syntax.Abstract.Util           ( programToScopeForSubstitution
                                                )


-- The implimentation was copied from Server.Handler.CustomMethod.handleSubst
reduceRedex :: Int -> PipelineM Expr
reduceRedex i = do
  stage <- load
  logText $ Text.pack $ "Substituting Redex " <> show i
  --
  case stage of
    Swept  result -> do
      let program = convertedProgram
            (typeCheckedPreviousStage (sweptPreviousStage result))
      case IntMap.lookup i (sweptRedexes result) of
        Nothing         -> throwError [Others $ "Cannot find the redex with index: "<>show i]
        Just (_, redex) -> do
          let scope = programToScopeForSubstitution program
          let (newExpr, counter) =
                runState (Substitution.step scope redex) (sweptCounter result)
          let redexesInNewExpr = Substitution.buildRedexMap newExpr
          let newResult = result
                { sweptCounter = counter
                , sweptRedexes = sweptRedexes result <> redexesInNewExpr
                }
          save (Swept newResult)
          return newExpr

    _      -> throwError [Others "Invoked reduceRedex in a wrong stage."]
    -- TODO: extend applicable stages

-- | The returned Expr should contain no redex(any RedexShell or RedexKernel).
-- But this requirement isn't satisfied yet (see TODO in function definition).
eliminateRedexes :: Expr -> PipelineM Expr
eliminateRedexes (RedexShell n _) = reduceRedex n
eliminateRedexes (App ex1 ex2 l) = do
      -- this case might not be this simple?
      ex1' <- eliminateRedexes ex1
      ex2' <- eliminateRedexes ex2
      return (App ex1' ex2' l)
eliminateRedexes (Lam n ex l) = do
      ex' <- eliminateRedexes ex
      return $ Lam n ex' l
-- TODO: complete cases below:
-- eliminateRedexes (Func n cls l) = undefined
-- eliminateRedexes (Tuple exs)              = undefined
-- eliminateRedexes (Quant ex nas ex' ex2 l) = undefined
-- eliminateRedexes (ArrIdx ex ex' l)        = undefined
-- eliminateRedexes (ArrUpd ex ex' ex2 l)    = undefined 
-- eliminateRedexes (Case ex css l)          = undefined
eliminateRedexes x                = return x