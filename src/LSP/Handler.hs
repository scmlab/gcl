{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}

module LSP.Handler (handlers) where

import qualified Data.Aeson as JSON
import Data.Text (Text, pack)
import Error
import GCL.Expr (expand, runSubstM)
import LSP.CustomMethod
import LSP.Diagnostic (ToDiagnostics (toDiagnostics), locToRange)
import LSP.ExportPO ()
import LSP.Monad
import Language.LSP.Diagnostics (partitionBySource)
import Language.LSP.Server
import Language.LSP.Types hiding (TextDocumentSyncClientCapabilities (..))
import qualified Language.LSP.Types as LSP
import qualified Syntax.Abstract as A
import Syntax.Predicate
  ( Spec (..),
  )
import qualified Data.Text as Text

-- handlers of the LSP server
handlers :: Handlers ServerM
handlers =
  mconcat
    [ -- autocompletion
      requestHandler STextDocumentCompletion $ \req responder -> do
        let RequestMessage _ _ _ params = req
        let CompletionParams (TextDocumentIdentifier _uri) position _progress _partial completionContext = params

        -- only gets triggered when a backslash "\" is typed, or when the previous completion is incomplete
        let triggered = case completionContext of
              (Just (CompletionContext CtTriggerCharacter (Just "\\"))) -> True
              (Just (CompletionContext CtTriggerForIncompleteCompletions _)) -> True
              _ -> False
        if triggered
          then do
            let Position line col = position
            let replaceRange = LSP.Range (Position line (col - 1)) position
            let removeSlash = Just $ List [TextEdit replaceRange ""]

            let makeItem label kind symbol detail doc =
                  CompletionItem
                    label
                    kind
                    Nothing
                    (Just detail)
                    (Just $ CompletionDocString doc)
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    (Just symbol)
                    (Just PlainText)
                    Nothing
                    Nothing
                    removeSlash
                    Nothing
                    Nothing
                    Nothing

            let items =
                  [ makeItem "to" (Just CiOperator) "→" "\"→\" Rightwards Arrow" "The Unicode variant of \"->\"",
                    makeItem "neq" (Just CiOperator) "≠" "\"≠\" Not Equal To" "The Unicode variant of \"/=\"",
                    makeItem "gte" (Just CiOperator) "≥" "\"≥\" Greater-Than or Equal To" "The Unicode variant of \">=\"",
                    makeItem "lte" (Just CiOperator) "≤" "\"≤\" Less-Than or Equal To" "The Unicode variant of \"<=\"",
                    makeItem "imp" (Just CiOperator) "⇒" "\"⇒\" Rightwards Double Arrow" "The Unicode variant of \"=>\"",
                    makeItem "conj" (Just CiOperator) "∧" "\"∧\" Logical And" "The Unicode variant of \"&&\"",
                    makeItem "disj" (Just CiOperator) "∨" "\"∨\" Logical Or" "The Unicode variant of \"||\"",
                    makeItem "neg" (Just CiOperator) "¬" "\"¬\" Not Sign" "The Unicode variant of \"~\"",
                    makeItem "leftanglebracket" (Just CiValue) "⟨" "\"⟨\" Left Angle Bracket" "The Unicode variant of \"<|\"",
                    makeItem "rightanglebracket" (Just CiValue) "⟩" "\"⟩\" Right Angle Bracket" "The Unicode variant of \"|>\""
                  ]

            let isComplete = True
            let completionList = CompletionList isComplete (List items)
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
                        -- logText source'
                        case runM (refine filepath source' (selStart, selEnd)) of
                          Left err -> do
                            sendDiagnostics filepath 0 (toDiagnostics err)
                            return [ResError [globalError err]]
                          Right (spec, payload) -> do
                            logText " *** [ Refine ] Payload of the spec:"
                            logText payload
                            -- replace the Spec with its parsed payload
                            let removeSpec = TextEdit (locToRange (specLoc spec)) (Text.stripStart payload)
                            let identifier = VersionedTextDocumentIdentifier (filePathToUri filepath) (Just 0)
                            let textDocumentEdit = TextDocumentEdit identifier (List [InL removeSpec])
                            let change = InL textDocumentEdit
                            let workspaceEdit = WorkspaceEdit Nothing (Just (List [change])) Nothing
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
