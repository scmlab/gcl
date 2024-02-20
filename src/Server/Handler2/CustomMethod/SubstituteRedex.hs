
module Server.Handler2.CustomMethod.SubstituteRedex (handler) where

import qualified Syntax.Abstract as A
import Error (Error(..))

import Server.Monad (ServerM, LoadedProgram(..))
import Server.CustomMethod (ResKind(..))
import Server.Handler2.Utils
import Server.Handler2.CustomMethod.Utils

import qualified GCL.Substitution as Substitution

handler :: FilePath -> Int -> ([ResKind] -> ServerM ()) -> (Error -> ServerM ()) -> ServerM ()
handler filePath redexNumber onFinish onError = do
  maybeLoadedProgram <- dumpProgram filePath
  case maybeLoadedProgram of
    Nothing -> onError (Others "Please reload before inspect.")
    Just loadedProgram -> do
      let redexes :: IntMap (Int, A.Expr) = _redexes loadedProgram
      let abstract :: A.Program = _abstractProgram loadedProgram
      let variableCounter :: Int = _variableCounter loadedProgram
      case IntMap.lookup redexNumber redexes of
        Nothing -> onError (Others "Redex not found.")
        Just (_, redex) -> do
          let scope :: Map Text (Maybe Expr) = A.programToScopeForSubstitution abstract
          let (newExpr, variableCounter') =
                runState (Substitution.step scope redex) variableCounter
          let redexesInNewExpr = Substitution.buildRedexMap newExpr
          let loadedProgram' = loadedProgram
                { _variableCounter = variableCounter'
                , _redexes         = redexes <> redexesInNewExpr
                }
          _ <- cacheLoadedProgram filePath loadedProgram'
          onFinish [ResSubstitute i (render newExpr)]