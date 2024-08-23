{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use join" #-}

module Server.Load where



import Server.Monad (ServerM, FileState(..), loadFileState, saveFileState, readSource, digHoles, logText, logText)
import Data.Text (Text)

import Data.Loc.Range (Range)

import qualified GCL.WP                        as WP
import qualified Syntax.Concrete as C
import qualified Syntax.Parser as Parser
import qualified Syntax.Abstract as A
import qualified Syntax.Typed as T
import Error (Error(..))
import Server.Highlighting (collectHighlighting)
import Server.GoToDefn (collectLocationLinks)
import Server.Hover (collectHoverInfo)
import Control.Monad.Except (runExcept)
import Server.PositionMapping (idDelta)
import qualified Server.SrcLoc                 as SrcLoc
import qualified GCL.Type as TypeChecking
import GCL.WP.Types (StructError, StructWarning)
import GCL.Predicate (PO, Spec)
import Server.Notification.Update (sendUpdateNotification)
import qualified Data.Text as Text
import qualified Data.Aeson as JSON
import Pretty (Pretty(..))
import Syntax.Concrete.Types (SepBy (..), GdCmd (..))
import Server.Notification.Error (sendErrorNotification)

load :: FilePath -> ServerM ()
load filePath = do
  logText "load: start\n"
  maybeFileState <- loadFileState filePath
  let currentVersion = case maybeFileState of
                        Nothing -> 0
                        Just FileState{editedVersion} -> editedVersion

  -- read source
  maybeSource <- readSource filePath
  case maybeSource of
    Nothing     -> do
      logText "  read error"
      onError (CannotReadFile filePath)
    Just source -> do
      logText "  source read \n"
      -- parse source into concrete syntax
      case parse filePath source of
        Left err       -> do
          logText "  parse error"
          onError err
        Right concrete -> do
          logText "  source parsed \n"
          case reportHolesOrToAbstract concrete of
            Left [] -> do
              logText "  should not happen\n"
              logText . Text.pack $ show concrete
            Left holes -> do
              logText "  should dig holes\n"
              digHoles filePath holes do
                logText "  holes digged\n"
                load filePath
            Right abstract -> do
              logText "  all holes digged\n"
              logText "  abstract program generated\n"
              case elaborate abstract of
                Left err        -> do
                  logText "  elaborate error\n"
                  onError err
                Right elaborated -> do
                  logText "  program elaborated\n"
                  case WP.sweep elaborated of
                    Left  err -> do
                      logText "  sweep error\n"
                      onError (StructError err)
                    Right (pos, specs, warnings, redexes, idCount) -> do
                      let fileState = FileState
                                        { refinedVersion   = currentVersion
                                        , specifications   = map (\spec -> (currentVersion, spec)) specs
                                        , proofObligations = map (\po -> (currentVersion, po)) pos
                                        , warnings         = map (\warning -> (currentVersion, warning)) warnings

                                        -- to support other LSP methods in a light-weighted manner
                                        , loadedVersion    = currentVersion
                                        , toOffsetMap      = SrcLoc.makeToOffset source
                                        , concrete         = concrete
                                        , semanticTokens   = collectHighlighting concrete
                                        , abstract         = abstract
                                        , idCount          = idCount
                                        , definitionLinks  = collectLocationLinks abstract
                                        , elaborated       = elaborated
                                        , hoverInfos       = collectHoverInfo elaborated

                                        , positionDelta   = idDelta
                                        , editedVersion    = currentVersion
                                        }
                      logText "  fileState created\n"
                      saveFileState filePath fileState
                      logText "  fileState updated\n"
                      onSuccess
  logText "load: end\n"
  where
    onSuccess :: ServerM ()
    onSuccess = do
      logText "load: success\n"
      sendUpdateNotification filePath
      -- clear errors
      sendErrorNotification filePath []
      logText "load: update notification sent\n"
    onError :: Error -> ServerM ()
    onError err = do
      logText "load: error\n\t"
      logText $ Text.pack (show $ JSON.encode err)
      logText "\n"
      sendErrorNotification filePath [err]
      logText "load: update notification sent\n"

parse :: FilePath -> Text -> Either Error C.Program
parse filepath source =
  case Parser.scanAndParse Parser.program filepath source of
    Left  err   -> Left (ParseError err)
    Right concrete -> Right concrete

reportHolesOrToAbstract :: C.Program -> Either [Range] A.Program
reportHolesOrToAbstract concrete =
  case collectHoles concrete of
    []    -> case runExcept $ C.toAbstract concrete of
      Left _         -> error "should dig all holes before calling Concrete.toAbstract"
      Right abstract -> Right abstract
    holes -> Left holes

collectHoles :: C.Program -> [Range]
collectHoles (C.Program _ statements) = collectHolesFromStatements statements

collectHolesFromStatements :: [C.Stmt] -> [Range]
collectHolesFromStatements statements = do
  statement <- statements
  case statement of
    C.SpecQM range      -> [range]
    C.Block _ program _ -> collectHoles program
    C.Do _ commands _   -> collectHolesFromGdCmd commands
    C.If _ commands _   -> collectHolesFromGdCmd commands
    _                   -> []

mapSepBy :: (a -> b) -> SepBy s a -> [b]
mapSepBy f (Head c)       = [f c]
mapSepBy f (Delim c _ cs) = f c : mapSepBy f cs

collectHolesFromGdCmd :: SepBy s C.GdCmd -> [Range]
collectHolesFromGdCmd s = do
  ranges <- mapSepBy (\(GdCmd _ _ statements) -> collectHolesFromStatements statements) s
  ranges

elaborate :: A.Program -> Either Error T.Program
elaborate abstract = do
  case TypeChecking.runElaboration abstract mempty of
    Left  e -> Left (TypeError e)
    Right typedProgram -> return typedProgram
