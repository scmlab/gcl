{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}

module Server.Handler (handlers) where

import qualified Data.Aeson as JSON
import Data.Text (Text, pack)
import qualified Data.Text as Text
import Language.LSP.Server
import Language.LSP.Types hiding
  ( TextDocumentSyncClientCapabilities (..),
  )
import qualified Language.LSP.Types as LSP
import Server.CustomMethod (ReqKind (..), Request (..))
-- import qualified Server.CustomMethod as Custom
import Server.Diagnostic
  ( ToDiagnostics (toDiagnostics),
    locToRange,
  )
import Server.Eff
import Server.ExportPO ()
import Server.Monad
import Syntax.Predicate (Spec (..))

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
            let effEnv = EffEnv filepath (Just responder)
            -- convert Request to Response
            case kind of
              -- Inspect
              ReqInspect selStart selEnd -> do
                updateSelection filepath (selStart, selEnd)
                interpret effEnv $ do
                  source <- savedSource
                  (pos, _specs, _globalProps, warnings) <- checkEverything source (Just (selStart, selEnd))
                  let diagnostics = concatMap toDiagnostics pos ++ concatMap toDiagnostics warnings
                  let responses = [ResInspect pos]
                  terminate responses diagnostics

              -- Refine
              ReqRefine selStart selEnd -> do
                lastSelection <- readLastSelection filepath
                interpret effEnv $ do
                  source <- latestSource
                  (spec, text) <- refine source (selStart, selEnd)
                  editText (locToRange $ specLoc spec) (Text.stripStart text)
                  source' <- latestSource
                  checkAndSendResponsePrim lastSelection source'

              -- logText " *** [ Refine ] Payload of the spec:"
              -- logText text
              -- replace the Spec with its parsed text
              --  $ do
              --   result'' <- readLatestSource filepath
              --   case result'' of
              --     Nothing -> return ()
              --     Just source'' -> do
              --       logText "after"
              --       checkAndSendResponse filepath source''

              -- let diagnostics = concatMap toDiagnostics pos ++ concatMap toDiagnostics warnings
              -- let responses = [ResInspect pos]
              -- terminate responses diagnostics

              -- result <- readLatestSource filepath
              -- case result of
              --   Nothing -> responder $ Res filepath [ResError [globalError (Others "no source")]]
              --   Just latestSource -> do
              --     let program = do
              --           refine filepath latestSource (selStart, selEnd)
              --           return ([], [])
              --     let next = final filepath (Just responder)
              --     runStuff filepath True program next

              -- Substitute
              ReqSubstitute index expr _subst -> do
                responder $ Res filepath []

              -- result <- readSavedSource filepath
              -- case result of
              --   Nothing -> responder NotLoaded
              --   Just savedSource -> do
              --     let next = final filepath (Just responder)
              --     let program = do
              --           A.Program _ _ defns _ _ <- parseProgram filepath savedSource
              --           let expr' = runSubstM (expand (A.Subst expr _subst)) defns 1
              --           return ([ResSubstitute index expr'], [])

              --     runStuff filepath True program next

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

checkAndSendResponsePrim :: Maybe (Int, Int) -> Text -> EffM ()
checkAndSendResponsePrim lastSelection source = do
  version <- bumpVersion
  (pos, specs, globalProps, warnings) <- checkEverything source lastSelection
  let diagnostics = concatMap toDiagnostics pos ++ concatMap toDiagnostics warnings
  let responses = [ResOK (IdInt version) pos specs globalProps warnings]
  terminate responses diagnostics

checkAndSendResponse :: FilePath -> Text -> ServerM ()
checkAndSendResponse filepath source = do
  lastSelection <- readLastSelection filepath
  let effEnv = EffEnv filepath Nothing
  interpret effEnv $ checkAndSendResponsePrim lastSelection source
