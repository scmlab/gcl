{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}

module LSP.Handler (handlers) where

import Control.Monad (void)
import Data.Aeson (Value)
import qualified Data.Aeson as JSON
import Data.Loc
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
import qualified Language.LSP.Types as LSP
import qualified Syntax.Abstract as A
import Syntax.Predicate
  ( Spec (..),
  )
import Control.Concurrent
import Control.Monad.Cont (liftIO)

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
      requestHandler (SCustomMethod "guacamole") $ \req responderPrim -> do
        let responder = responderPrim . Right . JSON.toJSON
        let RequestMessage _ _ _ params = req
        -- JSON Value => Request => Response
        case JSON.fromJSON params of
          JSON.Error msg -> do
            logText " --> CustomMethod: CannotDecodeRequest"
            responder $ CannotDecodeRequest $ show msg ++ "\n" ++ show params
          JSON.Success request@(Req filepath kind) -> do
            logText $ " --> Custom Reqeust: " <> pack (show request)

            result <- readSavedSource filepath
            case result of
              Nothing -> responder NotLoaded
              Just source -> do
                -- convert Request to Response
                case kind of
                  -- Inspect
                  ReqInspect selStart selEnd -> do
                    updateSelection filepath (selStart, selEnd)

                    runStuff filepath (Just responder) dontDigHole $ do
                      (pos, _specs, _globalProps, warnings) <- checkEverything filepath source (Just (selStart, selEnd))
                      let diagnostics = concatMap toDiagnostics pos ++ concatMap toDiagnostics warnings
                      let responses = [ResInspect pos]
                      return (responses, diagnostics)

                  -- Refine
                  ReqRefine selStart selEnd -> do
                    result' <- readLatestSource filepath
                    case result' of
                      Nothing -> responder $ Res filepath [ResError [globalError (Others "no source")]]
                      Just source' -> do
                        case runM (refine filepath source' (selStart, selEnd)) of
                          Left (ToClient err) -> do
                            sendDiagnostics filepath (toDiagnostics err)
                            responder $ Res filepath [ResError [globalError err]]
                          Left (ToServer loc) -> do
                            logText "SHOULD DIG HOLE 2"
                            responder $ Res filepath []
                          Right (spec, payload) -> do
                            logText " *** [ Refine ] Payload of the spec:"
                            logText payload
                            -- replace the Spec with its parsed payload
                            replaceText filepath (specLoc spec) (Text.stripStart payload) $ do
                              result'' <- readLatestSource filepath
                              case result'' of
                                Nothing -> return ()
                                Just source'' -> do
                                  logText "after"
                                  checkAndSendResponse filepath source''

                            -- let removeSpec = TextEdit (locToRange (specLoc spec)) (Text.stripStart payload)
                            -- let identifier = VersionedTextDocumentIdentifier (filePathToUri filepath) (Just 0)
                            -- let textDocumentEdit = TextDocumentEdit identifier (List [InL removeSpec])
                            -- let change = InL textDocumentEdit
                            -- let workspaceEdit = WorkspaceEdit Nothing (Just (List [change])) Nothing
                            -- let applyWorkspaceEditParams = ApplyWorkspaceEditParams (Just "Resolve Spec") workspaceEdit
                            -- let responder' response = case response of
                            --       Left _responseError -> return ()
                            --       Right _value -> do
                            --         -- ? ==> [!  !]
                            --         result'' <- readLatestSource filepath
                            --         case result'' of
                            --           Nothing -> return ()
                            --           Just source'' -> do
                            --             logText "after"
                            --             version' <- bumpCounter
                            --             checkAndSendResponse filepath source'' version'

                            -- _ <- sendRequest SWorkspaceApplyEdit applyWorkspaceEditParams responder'

                  -- Substitute
                  ReqSubstitute index expr _subst -> do
                    let program = do
                          A.Program _ _ defns _ _ <- parseProgram filepath source
                          let expr' = runSubstM (expand (A.Subst expr _subst)) defns 1
                          return [ResSubstitute index expr']
                    case runM program of
                      Left (ToClient err) -> do
                        sendDiagnostics filepath (toDiagnostics err)
                        responder $ Res filepath [ResError [globalError err]]
                      Left (ToServer loc) -> do
                        logText "SHOULD DIG HOLE 3"
                        responder $ Res filepath []
                      Right res -> responder $ Res filepath res

                  -- ExportProofObligations
                  ReqExportProofObligations ->
                    responder $ Res filepath [ResConsoleLog "Export"]
                  ReqDebug -> return $ error "crash!",
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
                checkAndSendResponse filepath source,
      -- when the client opened the document
      notificationHandler STextDocumentDidOpen $ \ntf -> do
        logText " --> TextDocumentDidOpen"
        let NotificationMessage _ _ (DidOpenTextDocumentParams (TextDocumentItem uri _ _ source)) = ntf
        case uriToFilePath uri of
          Nothing -> pure ()
          Just filepath -> do
            updateSource filepath source
            checkAndSendResponse filepath source
    ]

checkAndSendResponse :: FilePath -> Text -> ServerM ()
checkAndSendResponse filepath source = do
  lastSelection <- readLastSelection filepath
  version <- bumpCounter
  runStuff filepath Nothing (digHole filepath) $ do
    (pos, specs, globalProps, warnings) <- checkEverything filepath source lastSelection
    let diagnostics = concatMap toDiagnostics pos ++ concatMap toDiagnostics warnings
    let responses = [ResOK (IdInt version) pos specs globalProps warnings]
    return (responses, diagnostics)

type Cont = ([ResKind], [Diagnostic]) -> ServerM ()

-- replace the question mark "?" with a hole "[!  !]"
digHole :: FilePath -> Loc -> Cont -> ServerM ()
digHole _ilepath NoLoc _ = return ()
digHole filepath (Loc start end) cont = do
  let indent = Text.replicate (posCol start - 1) " "
  let holeText = "[!\n" <> indent <> "\n" <> indent <> "!]"
  replaceText filepath (Loc start end) holeText $ do
  -- 
    result <- readLatestSource filepath
    case result of
      Nothing -> return ()
      Just source -> do
        version <- bumpCounter
        res <- runStuff' filepath (const (return ())) $ do
          (pos, specs, globalProps, warnings) <- genPOsandSpecsOnly filepath source Nothing
          let diagnostics = concatMap toDiagnostics pos ++ concatMap toDiagnostics warnings
          let responses = [ResOK (IdInt version) pos specs globalProps warnings]
          return (responses, diagnostics)
        cont res



dontDigHole :: Loc -> Cont -> ServerM ()
dontDigHole _ _ = return ()

-- runEff' :: FilePath -> Maybe (Response -> ServerM ()) -> Eff a -> ServerM (Maybe a)
-- runEff' filepath responderM program = do
--   case runEff filepath program of
--     Left loc -> do
--       logText "SHOULD DIG HOLE"
--       return Nothing
--     Right a -> return (Just a)

-- sendDiagnostics filepath diagnostics
-- -- send responses to the responder if available
-- -- else send as notification
-- case responderM of
--   Nothing -> sendCustomResponse filepath responses
--   Just responder -> responder $ Res filepath responses

runStuff :: FilePath -> Maybe (Response -> ServerM ()) -> (Loc -> Cont -> ServerM ()) -> M ([ResKind], [Diagnostic]) -> ServerM ()
runStuff filepath responderM dig program = do
  let cont (responses, diagnostics) = do
        sendDiagnostics filepath diagnostics
        case responderM of
          Nothing -> sendCustomResponse filepath responses
          Just responder -> responder $ Res filepath responses

  runStuff' filepath (`dig` cont) program >>= cont

runStuff' :: FilePath -> (Loc -> ServerM ()) -> M ([ResKind], [Diagnostic]) -> ServerM ([ResKind], [Diagnostic])
runStuff' filepath dig program =
  case runM program of
    Left (ToClient err) -> do
      sendDiagnostics filepath (toDiagnostics err)
      return ([ResError [globalError err]], [])
    Left (ToServer loc) -> do
      dig loc
      return ([], [])
    Right (responses, diagnostics) -> return (responses, diagnostics)

sendDiagnostics :: FilePath -> [Diagnostic] -> ServerM ()
sendDiagnostics filepath diags = do
  version <- bumpCounter
  publishDiagnostics 100 (toNormalizedUri (filePathToUri filepath)) (Just version) (partitionBySource diags)

sendCustomResponse :: FilePath -> [ResKind] -> ServerM ()
sendCustomResponse filepath responses = sendNotification (SCustomMethod "guacamole") $ JSON.toJSON $ Res filepath responses

replaceText :: FilePath -> Loc -> Text -> ServerM () -> ServerM ()
replaceText filepath loc text callback = do
  let removeSpec = TextEdit (locToRange loc) text
  let identifier = VersionedTextDocumentIdentifier (filePathToUri filepath) (Just 0)
  let textDocumentEdit = TextDocumentEdit identifier (List [InL removeSpec])
  let change = InL textDocumentEdit
  let workspaceEdit = WorkspaceEdit Nothing (Just (List [change])) Nothing
  let applyWorkspaceEditParams = ApplyWorkspaceEditParams (Just "Resolve Spec") workspaceEdit

  void $ sendRequest SWorkspaceApplyEdit applyWorkspaceEditParams $ const callback
