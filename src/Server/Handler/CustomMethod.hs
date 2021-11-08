{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Server.Handler.CustomMethod where

import           Control.Monad.Except           ( throwError )
import           Control.Monad.State
import qualified Data.Aeson                    as JSON
import qualified Data.IntMap                   as IntMap
import           Data.Loc                       ( Located(locOf)
                                                , posCol
                                                )
import           Data.Loc.Range
import qualified Data.Text                     as Text
import           Data.Text                      ( Text )
import           Error                          ( Error(Others) )
import           GCL.Predicate
import qualified GCL.Substitution              as Substitution
import           Render                         ( Render(render) )
import           Server.CustomMethod
import           Server.DSL
import           Server.Monad
import           Syntax.Abstract                ( Redex(redexExpr) )
import           Syntax.Abstract.Util           ( programToScopeForSubstitution
                                                )

import qualified Language.LSP.Types            as J

handleInspect :: Range -> CmdM [ResKind]
handleInspect range = do
  setLastSelection range
  stage <- readCurrentStage
  generateResponseAndDiagnosticsFromCurrentStage stage

handleRefine :: Range -> CmdM [ResKind]
handleRefine range = do
  mute True
  setLastSelection range
  source               <- getSource
  (spec, payloadLines) <- refine source range

  -- remove the Spec
  let indentedPayload = case payloadLines of
        []  -> ""
        [x] -> x
        (x : xs) ->
          let indentation =
                Text.replicate (posCol (rangeStart (specRange spec)) - 1) " "
          in  Text.unlines $ x : map (indentation <>) xs
  source'              <- editText (specRange spec) indentedPayload

  (concrete, abstract) <- parseProgram source'
  typeCheck abstract
  mute False
  result <- sweep concrete abstract
  cacheCurrentStage (FinalStage result)
  generateResponseAndDiagnosticsFromCurrentStage (FinalStage result)

handleInsertAnchor :: Text -> CmdM [ResKind]
handleInsertAnchor hash = do
  -- mute the event listener before editing the source 
  mute True
  -- template of proof to be appended to the source  
  let template = "{- #" <> hash <> "\n\n-}"
  -- range for appending the template of proof 
  source        <- getSource
  (_, abstract) <- parseProgram source
  range         <- case fromLoc (locOf abstract) of
    Nothing -> throwError [Others "Cannot read the range of the program"]
    Just x  -> return x

  source' <- editText (Range (rangeEnd range) (rangeEnd range))
                      ("\n\n" <> template)
  (concrete', abstract') <- parseProgram source'
  typeCheck abstract'
  mute False
  result <- sweep concrete' abstract'
  cacheCurrentStage (FinalStage result)
  generateResponseAndDiagnosticsFromCurrentStage (FinalStage result)

handleSubst :: Int -> CmdM [ResKind]
handleSubst i = do
  result <- readCurrentStage
  case result of
    FirstStage _error -> return []
    FinalStage cache  -> do
      logText $ Text.pack $ "Substituting Redex " <> show i
      -- 
      case IntMap.lookup i (cacheRedexes cache) of
        Nothing    -> return []
        Just redex -> do
          let scope = programToScopeForSubstitution (cacheProgram cache)
          let (newExpr, counter) = runState
                (Substitution.step scope (redexExpr redex))
                (cacheCounter cache)
          let redexesInNewExpr = Substitution.buildRedexMap newExpr
          let newCache = cache
                { cacheCounter = counter
                , cacheRedexes = cacheRedexes cache <> redexesInNewExpr
                }
          cacheCurrentStage (FinalStage newCache)
          return [ResSubstitute i (render newExpr)]

handleCustomMethod :: ReqKind -> CmdM [ResKind]
handleCustomMethod = \case
  ReqInspect      range -> handleInspect range
  ReqRefine       range -> handleRefine range
  ReqInsertAnchor hash  -> handleInsertAnchor hash
  ReqSubstitute   i     -> handleSubst i
  ReqDebug              -> return $ error "crash!"

handler :: JSON.Value -> (Response -> ServerM ()) -> ServerM ()
handler params responder = do
    -- JSON Value => Request => Response
  case JSON.fromJSON params of
    JSON.Error msg -> do
      -- logText
      --   $  " --> CustomMethod: CannotDecodeRequest "
      --   <> Text.pack (show msg)
      --   <> " "
      --   <> Text.pack (show params)
      responder $ CannotDecodeRequest $ show msg ++ "\n" ++ show params
    JSON.Success requests -> handleRequests requests
 where
      -- make the type explicit to appease the type checker 
  handleRequests :: [Request] -> ServerM ()
  handleRequests = mapM_ handleRequest

  handleRequest :: Request -> ServerM ()
  handleRequest request@(Req filepath kind) = do
    -- convert Request to Response
    interpret (J.filePathToUri filepath)
              (customRequestResponder filepath responder)
      $ do
          logText $ " --> Custom Reqeust: " <> Text.pack (show request)
          handleCustomMethod kind

