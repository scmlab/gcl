{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}

module Server.Handler2.CustomMethod where

import qualified Data.Aeson.Types as JSON

import Server.CustomMethod (Response (..), Request (..), ReqKind (..), ResKind (..))
import Server.Monad (ServerM, convertErrorsToResponsesAndDiagnostics)

import Data.Loc.Range (Range (..), rangeFile, rangeStart, rangeEnd, withinRange, fromLoc)
import Data.Text (Text)

import qualified GCL.WP                        as WP
import qualified Syntax.Concrete as C
import qualified Syntax.Parser as Parser
import qualified Syntax.Abstract as A
import qualified Data.Text as Text
import Error (Error(..))
import Control.Monad.Except (runExcept)
import Pretty (toText)
import Data.Loc hiding ( fromLoc )
import GCL.Type (ScopeTreeZipper)
import qualified GCL.Type as TypeChecking
import qualified Data.List as List
import Syntax.Abstract (Expr)
import GCL.WP.Type (StructWarning)
import GCL.Predicate (PO (..), Spec (..))
import Data.IntMap (IntMap)
import Render (Section (..), Deco (..), Block (..), Render (..), RenderSection (..))
import Server.Handler.Diagnostic (makeDiagnostic, Collect (collect))
import Data.String (fromString)
import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import Server.Handler2.Utils

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
        ReqHelloWorld range -> handleHelloWorld range respondResult respondError
        _                   -> return $ error "Not yet implemented"
      where
        respondError :: Error -> ServerM ()
        respondError err = do
          (responsesFromError, diagnosticsFromError) <- Server.Monad.convertErrorsToResponsesAndDiagnostics [err]
          sendDiagnostics filePath diagnosticsFromError
          responder (Res filePath responsesFromError)

        respondResult :: [ResKind] -> ServerM ()
        respondResult results = responder (Res filePath results)

-- Hello World --

handleHelloWorld :: Range -> ([ResKind] -> ServerM ()) -> (Error -> ServerM ()) -> ServerM ()
handleHelloWorld range onFinish onError = do
  let filepath :: FilePath = rangeFile range
  replaceWithHelloworld filepath range (do
      let helloWorldRange = rangeAfterReplacedWithHelloWorld range
      let diagnosticOnHelloWorld = makeDiagnostic Nothing helloWorldRange "Hello, World?" "This is a warning"
      _ <- sendDiagnostics filepath [diagnosticOnHelloWorld]
      version <- bumpVersion
      onFinish [
          ResDisplay version [
            Section Blue [
              Header "Hello, world" Nothing
            , Paragraph $ fromString "LSP server successfully responded."
            ]
          ]
        ]
    ) onError

replaceWithHelloworld :: FilePath -> Range -> ServerM () -> (Error -> ServerM ()) -> ServerM ()
replaceWithHelloworld filepath range onFinish onError = do
  maybeSource <- getSource filepath
  case maybeSource of
    Nothing -> onError (CannotReadFile filepath)
    Just source -> do
      logText "before replacement"
      logText source
      logText "\n"
      editText range "Hello, World!\n" $ do
        maybeSource' <- getSource filepath
        case maybeSource' of
          Nothing -> onError (CannotReadFile filepath)
          Just source' -> do
            logText "after replacement"
            logText source'
            logText "\n"
            onFinish

rangeAfterReplacedWithHelloWorld :: Range -> Range
rangeAfterReplacedWithHelloWorld range =
  Range (rangeStart range) (addToCoff 13 $ rangeEnd range)
  where
    addToCoff :: Int -> Pos -> Pos
    addToCoff offset pos = Pos (posFile pos) (posLine pos) (posCol pos) (posCoff pos + offset)


-- Reload --

handleReload :: FilePath -> ([ResKind] -> ServerM ()) -> (Error -> ServerM ()) -> ServerM ()
handleReload filepath onFinsih onError = do
  -- load source
  maybeSource <- getSource filepath
  case maybeSource of
    Nothing     -> onError (CannotReadFile filepath)
    Just source -> 
      -- parse source into concrete syntax
      case parse filepath source of
        Left err       -> onError err
        Right concrete ->
          -- dig holes and convert to abstract syntax
          toAbstract filepath concrete (\abstract -> do
            -- type check
            case typeCheck abstract of
              Left err -> onError err
              Right scopeTree -> 
                -- calculate proof obligations, specs and other hints
                case WP.sweep abstract of
                  Left  err -> onError (StructError err)
                  Right (pos, specs, warnings, redexes, counter) -> do
                    -- send warnings as diagnostics
                    let diagnostics = concatMap collect warnings
                    sendDiagnostics filepath diagnostics

                    -- send all hints as reponses for client to render
                    let (A.Program _ _ globalProperties _ _) = abstract
                    version' <- bumpVersion
                    let response :: [ResKind] =
                          hintsToResponseBody
                            version'
                            Nothing
                            (globalProperties, List.sort pos, sortOn locOf specs, warnings, redexes, counter)
                    onFinsih response
          ) onError

parse :: FilePath -> Text -> Either Error C.Program
parse filepath source =
  case Parser.scanAndParse Parser.program filepath source of
    Left  err   -> Left (ParseError err)
    Right program -> Right program

toAbstract :: FilePath -> C.Program -> (A.Program -> ServerM ()) -> (Error -> ServerM ()) -> ServerM ()
toAbstract filepath concrete onFinish onError = do
  case runExcept $ C.toAbstract concrete of
    Left rangeToDigHole -> do
      _ <- digHole rangeToDigHole do
        maybeSource <- getSource filepath
        case maybeSource of
          Nothing -> onError (CannotReadFile filepath)
          Just source' -> case parse filepath source' of
            Left err        -> onError err
            Right concrete' -> toAbstract filepath concrete' onFinish onError
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

hintsToResponseBody :: Int -> Maybe Range -> ([Expr], [PO], [Spec], [StructWarning], IntMap (Int, A.Expr), Int) -> [ResKind]
hintsToResponseBody version rangeToInspect _hints@(globalProperties, proofObligations, specs, warnings, redexes, counter) =
  [ ResMarkPOs rangesOfPOs
  , ResUpdateSpecs (map encodeSpec specs)
  , ResDisplay version sections
  ]
  where
    rangesOfPOs :: [Range]
    rangesOfPOs = mapMaybe (fromLoc . locOf) proofObligations
    encodeSpec :: Spec -> (Int, Text, Text, Range)
    encodeSpec spec = 
      ( specID spec
      , toText $ render (specPreCond spec)
      , toText $ render (specPostCond spec)
      , specRange spec
      )
    sections :: [Section]
    sections = mconcat
      [ warningsSections
      , specsSections
      , poSections
      , globalPropertiesSections
      ]
      where 
        warningsSections :: [Section]
        warningsSections = map renderSection warnings
        specsSections :: [Section]
        specsSections = map renderSection specsWithinCurrentSelection
          where
            specsWithinCurrentSelection :: [Spec]
            specsWithinCurrentSelection = case rangeToInspect of
              Nothing            -> specs
              Just selectedRange -> filter (withinRange selectedRange) specs 
        poSections :: [Section]
        poSections = map renderSection posWithCurrentSelection
          where
            posWithCurrentSelection :: [PO]
            posWithCurrentSelection = case rangeToInspect of
              Nothing        -> proofObligations
              Just selectedRange -> filter (selectedRange `covers`) proofObligations
              where
                covers :: Range -> PO -> Bool
                covers range po = case poAnchorLoc po of
                  Nothing     -> withinRange range po
                  Just anchor -> withinRange range po || withinRange range anchor
        globalPropertiesSections :: [Section]
        globalPropertiesSections = map
                (\expr -> Section
                  Plain
                  [Header "Property" (fromLoc (locOf expr)), Code (render expr)]
                )
                globalProperties

-- Refine --

handleRefine :: Range -> Text -> ServerM ()
handleRefine specRange filledText = do
  -- TODO: refine
  return ()