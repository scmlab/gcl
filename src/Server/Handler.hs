{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}

module Server.Handler (handlers) where

-- import qualified Server.CustomMethod as Custom

import Control.Monad.Except
import qualified Data.Aeson as JSON
import Data.Loc
import Data.Loc.Range
import Data.Text (Text, pack)
import qualified Data.Text as Text
import Error
import Language.LSP.Server
import Language.LSP.Types hiding
  ( Range,
    TextDocumentSyncClientCapabilities (..),
  )
import qualified Language.LSP.Types as LSP
import Server.CustomMethod
import Server.Diagnostic
  ( ToDiagnostics (toDiagnostics),
  )
import Server.ExportPO ()
import Server.Monad
import Syntax.Predicate (Spec (..))
import Render

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
                interpret effEnv $ do
                  updateLastMouseSelection (selStart, selEnd)
                  source <- savedSource
                  -- parse + type check + sweep
                  program <- parseProgram source
                  typeCheck program
                  (pos, _specs, _globalProps, warnings) <- sweep program
                  -- display all POs
                  let diagnostics = concatMap toDiagnostics pos ++ concatMap toDiagnostics warnings
                  -- response with only POs in the vinicity of the cursor
                  let responses = [ResInspect (filterPOs (selStart, selEnd) pos)]
                  terminate responses diagnostics

              -- Refine
              ReqRefine selStart selEnd -> do
                interpret effEnv $ do
                  updateLastMouseSelection (selStart, selEnd)
                  source <- latestSource
                  -- refine (parse + sweep)

                  (spec, content) <- refine source (selStart, selEnd)

                  -- logM "*** SPEC CONTENT ------"
                  -- logM text
                  -- logM "************"

                  -- remove the Spec
                  case specLoc spec of
                    NoLoc -> throwError [Others "NoLoc in ReqRefine"]
                    Loc start end -> do
                      source' <- editText (Range start end) (Text.stripStart content)
                      -- logM $ "*** AFTER REMOVING SPEC\n" <> source'
                      checkAndSendResponsePrim (Just (selStart, selEnd)) source'

              -- Substitute
              ReqSubstitute index expr subst -> do
                interpret effEnv $ do
                  source <- savedSource
                  program <- parseProgram source
                  let expr' = substitute program expr subst
                  terminate [ResSubstitute index expr'] []

              -- ExportProofObligations
              ReqExportProofObligations ->
                responder $ Res filepath [ResConsoleLog "Export"]
              ReqDebug -> return $ error "crash!",
      -- when the client saved the document, store the text for later use
      notificationHandler STextDocumentDidSave $ \ntf -> do
        logText " --> TextDocumentDidSave"
        let NotificationMessage _ _ (DidSaveTextDocumentParams (TextDocumentIdentifier uri) source') = ntf
        case source' of
          Nothing -> pure ()
          Just source ->
            case uriToFilePath uri of
              Nothing -> pure ()
              Just filepath -> do
                let effEnv = EffEnv filepath Nothing
                interpret effEnv $ do
                  updateSavedSource source
                  lastSelection <- readLastMouseSelection
                  checkAndSendResponsePrim lastSelection source,
      -- when the client opened the document
      notificationHandler STextDocumentDidOpen $ \ntf -> do
        logText " --> TextDocumentDidOpen"
        let NotificationMessage _ _ (DidOpenTextDocumentParams (TextDocumentItem uri _ _ source)) = ntf
        case uriToFilePath uri of
          Nothing -> pure ()
          Just filepath -> do
            let effEnv = EffEnv filepath Nothing
            interpret effEnv $ do
              updateSavedSource source
              lastSelection <- readLastMouseSelection
              checkAndSendResponsePrim lastSelection source
    ]

-- parse + type check + sweep
checkAndSendResponsePrim :: Maybe (Int, Int) -> Text -> EffM ()
checkAndSendResponsePrim lastSelection source = do
  version <- bumpVersion
  program <- parseProgram source
  typeCheck program
  (pos, specs, globalProps, warnings) <- sweep program
  let diagnostics = concatMap toDiagnostics pos ++ concatMap toDiagnostics warnings
  let filteredPOs = case lastSelection of
        Nothing -> pos
        Just sel -> filterPOs sel pos
  let warningsSection = if null warnings then [] else headerE "Warnings" : map renderBlock warnings
  let responses = [ResOK (IdInt version) filteredPOs specs globalProps warningsSection]

  terminate responses diagnostics