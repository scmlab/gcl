{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}

module LSP.Handler (handlers) where

import Control.Monad.Except hiding (guard)
import qualified Data.Aeson as JSON
import Data.Loc (Loc (..), posCol)
import Data.Text (Text, pack)
import qualified Data.Text as Text
import Error
import GCL.Expr (expand, runSubstM)
import LSP.CustomMethod
import LSP.Diagnostic (ToDiagnostics (toDiagnostics), locToRange)
import LSP.ExportPO ()
import LSP.Monad
import Language.LSP.Diagnostics (partitionBySource)
import Language.LSP.Server
import Language.LSP.Types hiding (TextDocumentSyncClientCapabilities (..))
import qualified Syntax.Abstract as A
import Syntax.Predicate
  ( Spec (..),
  )

-- handlers of the LSP server
handlers :: Handlers ServerM
handlers =
  mconcat
    [ -- autocompletion
      requestHandler STextDocumentCompletion $ \req responder -> do
        let RequestMessage _ _ _ params = req
        let CompletionParams (TextDocumentIdentifier uri) position _progress _partial completionContext = params

        logStuff completionContext
        let triggered = case completionContext of
              (Just (CompletionContext CtTriggerCharacter (Just "\\"))) -> True
              _ -> False

        if triggered
          then do
            -- CompletionList
            let item =
                  CompletionItem
                    "⟨"
                    (Just CiValue)
                    Nothing 
                    (Just "left angle bracket")
                    (Just $ CompletionDocString "The Unicode variant of \"<|\"")
                    Nothing 
                    Nothing 
                    Nothing 
                    Nothing 
                    Nothing 
                    (Just PlainText)
                    Nothing 
                    Nothing 
                    (Just (List ["l"]))
                    Nothing 
                    Nothing
            let isComplete = True
            let completionList = CompletionList isComplete (List [item])
            responder $ Right $ InR completionList
          else responder $ Right $ InR $ CompletionList True (List []),
      -- custom methods, not part of LSP
      requestHandler (SCustomMethod "guacamole") $ \req responder -> do
        let RequestMessage _ i _ params = req
        -- JSON Value => Request => Response
        response <- case JSON.fromJSON params of
          JSON.Error msg -> do
            logText " --> CustomMethod: CannotDecodeRequest"
            return $ CannotDecodeRequest $ show msg ++ "\n" ++ show params
          JSON.Success request@(Req filepath kind) -> do
            logText $ " --> Custom Reqeust: " <> pack (show request)

            result <- readSavedSource filepath
            case result of
              Nothing -> pure NotLoaded
              Just source -> do
                -- send Diagnostics
                version <- case i of
                  IdInt n -> return n
                  IdString _ -> bumpCounter
                checkAndSendDiagnostics filepath source version
                -- convert Request to Response
                kinds <- case kind of
                  ReqInspect selStart selEnd -> do
                    updateSelection filepath (selStart, selEnd)
                    return $
                      asGlobalError $ do
                        (pos, _specs, _globalProps, _warnings) <- checkEverything filepath source (Just (selStart, selEnd))
                        return [ResInspect pos]
                  ReqRefine selStart selEnd -> do
                    result' <- readLatestSource filepath
                    case result' of
                      Nothing -> return [ResConsoleLog "no source"]
                      Just source' -> do
                        logText source'
                        let f = do
                              spec <- findPointedSpec filepath source' (selStart, selEnd)
                              case spec of
                                Nothing -> throwError $ Others "Cannot find pointed spec"
                                Just spec' -> do
                                  let payload = getSpecPayload source' spec'
                                  let indentationOfSpec = case specLoc spec' of
                                        NoLoc -> 0
                                        Loc pos _ -> posCol pos - 1
                                  let indentedPayload = Text.intercalate ("\n" <> Text.replicate indentationOfSpec " ") payload
                                  _ <- refine indentedPayload
                                  return (spec', indentedPayload)
                        case runM f of
                          Left err -> do
                            sendDiagnostics filepath 0 (toDiagnostics err)
                            return [ResError [globalError err]]
                          Right (spec, indentedPayload) -> do
                            let removeSpecOpen = TextEdit (locToRange (specLoc spec)) indentedPayload
                            let identifier = VersionedTextDocumentIdentifier (filePathToUri filepath) (Just 0)
                            let textDocumentEdit = TextDocumentEdit identifier (List [removeSpecOpen])
                            let change = InL textDocumentEdit
                            let workspaceEdit = WorkspaceEdit Nothing (Just (List [change]))
                            let applyWorkspaceEditParams = ApplyWorkspaceEditParams (Just "Resolve Spec") workspaceEdit
                            let responder' response = case response of
                                  Left _responseError -> return ()
                                  Right _value -> do
                                    -- ? ==> [!  !]
                                    result'' <- readLatestSource filepath
                                    case result'' of
                                      Nothing -> return ()
                                      Just source'' -> do
                                        logText "after"
                                        version' <- bumpCounter
                                        checkAndSendResponse filepath source'' version'
                                        checkAndSendDiagnostics filepath source'' version'

                            _ <- sendRequest SWorkspaceApplyEdit applyWorkspaceEditParams responder'
                            return []
                  ReqSubstitute index expr _subst -> return $
                    asGlobalError $ do
                      A.Program _ _ defns _ _ <- parseProgram filepath source
                      let expr' = runSubstM (expand (A.Subst expr _subst)) defns 1
                      return [ResSubstitute index expr']
                  ReqExportProofObligations -> return $
                    asGlobalError $ do
                      return [ResConsoleLog "Export"]
                  ReqDebug -> return $ error "crash!"
                return $ Res filepath kinds

        logText $ " <-- " <> pack (show response)
        responder $ Right $ JSON.toJSON response,
      -- when the client saved the document, store the text for later use
      notificationHandler STextDocumentDidSave $ \ntf -> do
        logText " --> TextDocumentDidSave"
        let NotificationMessage _ _ (DidSaveTextDocumentParams (TextDocumentIdentifier uri) text) = ntf
        case text of
          Nothing -> pure ()
          Just source ->
            case uriToFilePath uri of
              Nothing -> pure ()
              Just filepath -> do
                updateSource filepath source
                version <- bumpCounter
                checkAndSendResponse filepath source version
                checkAndSendDiagnostics filepath source version,
      -- when the client opened the document
      notificationHandler STextDocumentDidOpen $ \ntf -> do
        logText " --> TextDocumentDidOpen"
        let NotificationMessage _ _ (DidOpenTextDocumentParams (TextDocumentItem uri _ _ source)) = ntf
        case uriToFilePath uri of
          Nothing -> pure ()
          Just filepath -> do
            updateSource filepath source
            version <- bumpCounter
            checkAndSendResponse filepath source version
            checkAndSendDiagnostics filepath source version
    ]

checkAndSendResponse :: FilePath -> Text -> Int -> ServerM ()
checkAndSendResponse filepath source version = do
  lastSelection <- readLastSelection filepath
  let result = runM (checkEverything filepath source lastSelection)
  let response = case result of
        Left e -> Res filepath [ResError [globalError e]]
        Right (pos, specs, globalProps, warnings) -> Res filepath [ResOK (IdInt version) pos specs globalProps warnings]
  sendNotification (SCustomMethod "guacamole") $ JSON.toJSON response

checkAndSendDiagnostics :: FilePath -> Text -> Int -> ServerM ()
checkAndSendDiagnostics filepath source version = do
  -- send diagnostics
  diags <- do
    let result = runM $ checkEverything filepath source Nothing
    return $ case result of
      Left err -> toDiagnostics err
      Right (pos, _, _, warnings) -> concatMap toDiagnostics pos ++ concatMap toDiagnostics warnings
  sendDiagnostics filepath version diags

sendDiagnostics :: FilePath -> Int -> [Diagnostic] -> ServerM ()
sendDiagnostics filepath version diags = publishDiagnostics 100 (toNormalizedUri (filePathToUri filepath)) (Just version) (partitionBySource diags)
