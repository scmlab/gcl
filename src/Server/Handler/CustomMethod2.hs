{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}

module Server.Handler.CustomMethod2 where

import qualified Language.LSP.Types as LSP
import qualified Language.LSP.VFS as LSP
import qualified Language.LSP.Server as LSP

import qualified Data.Aeson.Types as JSON

import Server.CustomMethod (Response (..), Request (..), ReqKind (..), ResKind)
import Server.Monad (ServerM)
import qualified Server.Monad (logText, sendDiagnosticsLSP, convertErrorsToResponsesAndDiagnostics)

import Data.Loc.Range (Range, rangeFile, rangeStart)
import Data.Text (Text)

import qualified GCL.WP                        as WP
import qualified Server.SrcLoc as SrcLoc
import qualified Syntax.Concrete as C
import qualified Syntax.Parser as Parser
import qualified Syntax.Abstract as A
import qualified Data.Text as Text
import Error (Error(..))
import Syntax.Concrete (ToAbstract(..))
import Control.Monad.Except (runExcept)
import Pretty (toText)
import Data.Loc (posCol)
import qualified Data.Maybe as Maybe
import GCL.Type (ScopeTreeZipper)
import qualified GCL.Type as TypeChecking

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
    JSON.Success request -> handleRequest request
  where
    -- convert Request to Response and Diagnostics 
    handleRequest :: Request -> ServerM ()
    handleRequest _request@(Req filePath reqKind) = do
      case reqKind of
        ReqReload           -> handleReload filePath respondResult respondError
        ReqHelloWorld range -> replaceWithHelloworld' range respondError
        _                   -> return $ error "Not yet implemented"
      where
        respondError :: Error -> ServerM ()
        respondError err = do
          (responsesFromError, diagnosticsFromError) <- Server.Monad.convertErrorsToResponsesAndDiagnostics [err]
          sendDiagnostics filePath diagnosticsFromError
          responder (Res filePath responsesFromError)

        respondResult :: [ResKind] -> ServerM ()
        respondResult results = responder (Res filePath results)

-- Basic Instructions for our ServerM programs --

getSource :: FilePath -> ServerM (Maybe Text)
getSource filepath = fmap LSP.virtualFileText
                      <$> LSP.getVirtualFile (LSP.toNormalizedUri (LSP.filePathToUri filepath))

logText :: Text -> ServerM ()
logText = Server.Monad.logText

sendDiagnostics :: FilePath -> [LSP.Diagnostic] -> ServerM ()
sendDiagnostics = Server.Monad.sendDiagnosticsLSP

editText :: Range -> Text -> ServerM () -> ServerM ()
editText range textToReplace onSuccess = do
  let requestParams :: LSP.ApplyWorkspaceEditParams
        = LSP.ApplyWorkspaceEditParams {
            _label = Just "Resolve Spec",
            _edit = LSP.WorkspaceEdit {
              _changes = Nothing,
              _documentChanges = Just (LSP.List [LSP.InL textDocumentEdit]),
              _changeAnnotations = Nothing
            }
          }
  _ <- LSP.sendRequest LSP.SWorkspaceApplyEdit requestParams (const onSuccess)
  return ()

  where
    filepath :: FilePath
    filepath = rangeFile range
    textDocumentEdit :: LSP.TextDocumentEdit
    textDocumentEdit = LSP.TextDocumentEdit {
      _textDocument = LSP.VersionedTextDocumentIdentifier (LSP.filePathToUri filepath) (Just 0),
      _edits = LSP.List [LSP.InL textEdit]
    }
    textEdit :: LSP.TextEdit
    textEdit = LSP.TextEdit {
      _range = SrcLoc.toLSPRange range,
      _newText = textToReplace
    }

-- ServerM program - example 1 --

replaceWithHelloworld :: FilePath -> Range -> (Error -> ServerM ()) -> ServerM ()
replaceWithHelloworld filepath range onError = do
  maybeSource <- getSource filepath
  case maybeSource of
    Nothing -> onError (CannotReadFile filepath)
    Just source -> do
      logText "before replacement"
      logText source
      logText "\n"
      editText range "Hello, World!" $ do
        maybeSource' <- getSource filepath
        case maybeSource' of
          Nothing -> onError (CannotReadFile filepath)
          Just source' -> do
            logText "after replacement"
            logText source'
            logText "\n"

replaceWithHelloworld' :: Range -> (Error -> ServerM ()) -> ServerM ()
replaceWithHelloworld' range onError = do
  let filepath :: FilePath = rangeFile range
  replaceWithHelloworld filepath range onError


-- Reload --

handleReload :: FilePath -> ([ResKind] -> ServerM ()) -> (Error -> ServerM ()) -> ServerM ()
handleReload filepath onFinsih onError = do
  maybeSource <- getSource filepath
  case maybeSource of
    Nothing     -> onError (CannotReadFile filepath)
    Just source -> 
      case parse filepath source of
        Left err       -> onError err
        Right concrete -> convert filepath concrete (\abstract -> do
            -- type check
            case typeCheck abstract of
              Left err -> onError err
              Right scopeTree -> 
                case WP.sweep abstract of
                  Left  err -> onError (StructError err)
                  Right (pos, specs, warings, redexes, counter) -> do
                    -- TODO convert sweep result to data or rendering
                    -- respond and send diagnostics
                    return ()
          ) onError

parse :: FilePath -> Text -> Either Error C.Program
parse filepath source =
  case Parser.scanAndParse Parser.program filepath source of
    Left  err   -> Left (ParseError err)
    Right program -> Right program

convert :: FilePath -> C.Program -> (A.Program -> ServerM ()) -> (Error -> ServerM ()) -> ServerM ()
convert filepath concrete onFinish onError = do
  case runExcept $ toAbstract concrete of
    Left rangeToDigHole -> do
      _ <- digHole rangeToDigHole do
        maybeSource <- getSource filepath
        case maybeSource of
          Nothing -> onError (CannotReadFile filepath)
          Just source' -> case parse filepath source' of
            Left err        -> onError err
            Right concrete' -> convert filepath concrete' onFinish onError
      return ()
    Right abstract -> onFinish abstract
  where
    digHole :: Range -> ServerM () -> ServerM ()
    digHole range _onFinish = do
      logText $ "    < DigHole " <> toText range
      let indent   = Text.replicate (posCol (rangeStart range) - 1) " "
      let holeText = "[!\n" <> indent <> "\n" <> indent <> "!]"
      editText range holeText _onFinish

typeCheck :: A.Program -> Either Error (ScopeTreeZipper A.Type)
typeCheck abstract = do
  case TypeChecking.runTypeCheck abstract of
    Left  e -> Left (TypeError e)
    Right (_, scopeTree) -> return scopeTree

-- -- | Converts abstract syntax to POs and other stuff
-- --   persists the result
-- sweep :: TypeCheckResult -> PipelineM SweepResult
-- sweep result = do
--   let abstract@(A.Program _ _ globalProps _ _) =
--         convertedProgram (typeCheckedPreviousStage result)
--   swept <- case WP.sweep abstract of
--     Left  e -> throwError [StructError e]
--     Right (pos, specs, warings, redexes, counter) -> return $ SweepResult
--       { sweptPreviousStage = result
--       , sweptPOs           = List.sort pos
--       , sweptSpecs         = sortOn locOf specs
--       , sweptProps         = globalProps
--       , sweptWarnings      = warings
--       , sweptRedexes       = redexes
--       , sweptCounter       = counter
--       }
--   save (Swept swept) -- save the current progress
--   return swept

--------------------------------------------------------------------------------
-- | Monad for handling the pipeline
--
--  Side effects:
--    Reader: FilePath
--    State: PipelineState
--    Exception: [Error]
--
--  Also, PipelineM is also a free monad of Instruction
--  which allows us to "compile" a PipelineM program into a series of Instructions
--  We can then decide to run these compiled Instructions in the real world
--                     or simulate them for testing

-- toReponse :: ([PO], [Spec], [StructWarning], IntMap (Int, A.Expr), Int) -> [ResKind]
-- toReponse (proofObligations, specs, warnings, redexes, counter) = 

-- toDiagnostics :: ([PO], [Spec], [StructWarning], IntMap (Int, A.Expr), Int) -> [LSP.Diagnostic]
-- toDiagnostics =

-- generateResponseAndDiagnostics :: SweepResult -> PipelineM [ResKind]
-- generateResponseAndDiagnostics result = do
--   let (SweepResult _ pos specs globalProps warnings _redexes _) = result

--   -- get Specs around the mouse selection
--   lastSelection <- getLastSelection
--   let overlappedSpecs = case lastSelection of
--         Nothing        -> specs
--         Just selection -> filter (withinRange selection) specs
--   -- get POs around the mouse selection (including their corresponding Proofs)

--   let withinPOrange sel po = case poAnchorLoc po of
--         Nothing     -> withinRange sel po
--         Just anchor -> withinRange sel po || withinRange sel anchor

--   let overlappedPOs = case lastSelection of
--         Nothing        -> pos
--         Just selection -> filter (withinPOrange selection) pos
--   -- render stuff
--   let warningsSections =
--         if null warnings then [] else map renderSection warnings
--   let globalPropsSections = if null globalProps
--         then []
--         else map
--           (\expr -> Section
--             Plain
--             [Header "Property" (fromLoc (locOf expr)), Code (render expr)]
--           )
--           globalProps
--   let specsSections =
--         if null overlappedSpecs then [] else map renderSection overlappedSpecs
--   let poSections =
--         if null overlappedPOs then [] else map renderSection overlappedPOs
--   let sections = mconcat
--         [warningsSections, specsSections, poSections, globalPropsSections]

--   version <- bumpVersion
--   let encodeSpec spec =
--         ( specID spec
--         , toText $ render (specPreCond spec)
--         , toText $ render (specPostCond spec)
--         , specRange spec
--         )

--   let rangesOfPOs = mapMaybe (fromLoc . locOf) pos
--   let responses =
--         [ ResDisplay version sections
--         , ResUpdateSpecs (map encodeSpec specs)
--         , ResMarkPOs rangesOfPOs
--         ]
--   let diagnostics = concatMap collect warnings
--   sendDiagnostics diagnostics

--   return responses
