{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Server.Handler.CustomMethod where

import           Control.Monad.Except           ( throwError )
import           Control.Monad.State            ( runState )
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
import           Server.Monad
import           Server.Pipeline
import           Syntax.Abstract                ( Redex(redexExpr) )
import           Syntax.Abstract.Util           ( programToScopeForSubstitution
                                                )

import qualified Language.LSP.Types            as J

handleInspect :: Range -> PipelineM [ResKind]
handleInspect range = do
  setLastSelection range
  stage <- load
  case stage of
    Swept swept -> generateResponseAndDiagnostics swept
    _           -> return []

handleRefine :: Range -> PipelineM [ResKind]
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
  source'     <- editText (specRange spec) indentedPayload
  parsed      <- parse source'

  converted   <- convert parsed
  typeChecked <- typeCheck converted
  mute False
  swept <- sweep typeChecked

  generateResponseAndDiagnostics swept

handleInsertAnchor :: Text -> PipelineM [ResKind]
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
  parsed      <- parse source'
  converted   <- convert parsed
  typeChecked <- typeCheck converted
  mute False
  swept <- sweep typeChecked
  generateResponseAndDiagnostics swept

handleSubst :: Int -> PipelineM [ResKind]
handleSubst i = do
  stage <- load
  logText $ Text.pack $ "Substituting Redex " <> show i
  -- 
  case stage of
    Raw         _      -> return []
    Parsed      _      -> return []
    Converted   _      -> return []
    TypeChecked _      -> return []
    Swept       result -> do
      let program = convertedProgram
            (typeCheckedPreviousStage (sweptPreviousStage result))
      case IntMap.lookup i (sweptRedexes result) of
        Nothing    -> return []
        Just redex -> do
          let scope = programToScopeForSubstitution program
          let (newExpr, counter) = runState
                (Substitution.step scope (redexExpr redex))
                (sweptCounter result)
          let redexesInNewExpr = Substitution.buildRedexMap newExpr
          let newResult = result
                { sweptCounter = counter
                , sweptRedexes = sweptRedexes result <> redexesInNewExpr
                }
          save (Swept newResult)
          return [ResSubstitute i (render newExpr)]

handleCustomMethod :: ReqKind -> PipelineM [ResKind]
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

  -- convert Request to Response
  handleRequest :: Request -> ServerM ()
  handleRequest request@(Req filepath kind) =
    interpret (J.filePathToUri filepath)
              (customRequestResponder filepath responder)
      $ do
          logText $ "\n ---> Custom Reqeust: " <> Text.pack (show request)
          handleCustomMethod kind

