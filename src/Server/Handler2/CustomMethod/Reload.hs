{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
module Server.Handler2.CustomMethod.Reload (handler, reload) where

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
import Data.List (sortOn)

import Server.CustomMethod (ResKind (..))
import Server.Monad (ServerM, LoadedProgram (..))
import Server.Highlighting (collectHighlighting)
import Server.Hover (collectHoverInfo)
import Server.GoToDefn (collectLocationLinks)

import Server.Handler2.Utils
import Server.Handler2.CustomMethod.Utils (sendDiagnosticsAndMakeResponseFromLoadedProgram)
import Data.Loc.Range (Range, rangeStart)


handler :: FilePath -> ([ResKind] -> ServerM ()) -> (Error -> ServerM ()) -> ServerM ()
handler filePath onFinsih onError = do
  reload filePath (\loadedProgram -> do
      response <- sendDiagnosticsAndMakeResponseFromLoadedProgram filePath loadedProgram Nothing
      onFinsih response
    ) onError

reload :: FilePath -> (LoadedProgram -> ServerM ()) -> (Error -> ServerM ()) -> ServerM ()
reload filepath onFinish onError = do
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
                    -- cache all results
                    let loadedProgram = LoadedProgram
                          { _concreteProgram   = concrete
                          , _highlightingInfos = collectHighlighting concrete
                          , _abstractProgram   = abstract
                          , _scopingInfo       = collectLocationLinks abstract
                          , _typeCheckingInfo  = collectHoverInfo abstract scopeTree
                          , _proofObligations  = List.sort pos
                          , _specifiations     = sortOn locOf specs
                          , _warnings          = warnings
                          , _redexes           = redexes
                          , _variableCounter   = counter
                          }
                    _ <- cacheProgram filepath loadedProgram
                    onFinish loadedProgram
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

