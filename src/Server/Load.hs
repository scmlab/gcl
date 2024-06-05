{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Load where

import qualified Language.LSP.Types             as LSP


import Server.Monad (ServerM, FileState(..), loadFileState, saveFileState, readSource, editTexts)
import Data.Text (Text)
import qualified Data.Text as Text

import Data.Loc.Range (Range, rangeStart)

import qualified GCL.WP                        as WP
import qualified Syntax.Concrete as C
import qualified Syntax.Parser as Parser
import qualified Syntax.Abstract as A
import qualified Syntax.Typed as T
import Error (Error(..))
import Data.Loc (posCol)
import Server.Highlighting (collectHighlighting)
import Server.GoToDefn (collectLocationLinks)
import Server.Hover (collectHoverInfo)
import Control.Monad.Except (runExcept)
import Server.PositionMapping (idDelta)
import qualified Server.SrcLoc                 as SrcLoc
import qualified GCL.Type as TypeChecking

load :: FilePath -> (FileState -> ServerM ()) -> (Error -> ServerM ()) -> ServerM ()
load filePath onSuccess onError = do
  
  maybeFileState <- loadFileState filePath
  let currentVersion = case maybeFileState of
                        Nothing -> 0
                        Just (FileState{editedVersion}) -> editedVersion

  -- read source
  maybeSource <- readSource filePath
  case maybeSource of
    Nothing     -> onError (CannotReadFile filePath)
    Just source -> 
      -- parse source into concrete syntax
      case parse filePath source of
        Left err       -> onError err
        Right concrete ->
          case reportHolesOrToAbstract concrete of
            Left holes -> digHoles filePath holes do
              load filePath onSuccess onError
            Right abstract -> case WP.sweep abstract of
              Left  err -> onError (StructError err)
              Right (pos, specs, warnings, redexes, counter) -> do
                case elaborate abstract of
                  Left err        -> onError err
                  Right elaborated -> do
                    let fileState = FileState 
                                      { refinedVersion   = currentVersion
                                      , specifications   = specs
                                      , proofObligations = pos
                                      , hasChangedOutsideSpecsSinceLastReload
                                                        = False

                                      -- to support other LSP methods in a light-weighted manner
                                      , loadedVersion    = currentVersion
                                      , toOffsetMap      = SrcLoc.makeToOffset source
                                      , concrete         = concrete
                                      , semanticTokens   = collectHighlighting concrete
                                      , abstract         = abstract
                                      , variableCounter  = counter
                                      , definitionLinks  = collectLocationLinks abstract
                                      , elaborated       = elaborated
                                      , hoverInfos       = collectHoverInfo elaborated

                                      , positionDelta    = idDelta
                                      , editedVersion    = currentVersion
                                      }
                    saveFileState filePath fileState
                    onSuccess fileState

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
collectHoles (C.Program _ statements) = do
  statement <- statements
  case statement of
    C.SpecQM range -> return range
    _ -> []

digHoles :: FilePath -> [Range] -> ServerM () -> ServerM ()
digHoles filePath ranges onFinish = do
  -- logText $ "    < DigHoles " <> (map ranges toText)
  let indent range = Text.replicate (posCol (rangeStart range) - 1) " "
  let diggedText range = "[!\n" <> indent range <> "\n" <> indent range <> "!]"
  editTexts filePath (map (\range -> (range, diggedText range)) ranges) onFinish

elaborate :: A.Program -> Either Error T.TypedProgram
elaborate abstract = do
  case TypeChecking.runElaboration abstract of
    Left  e -> Left (TypeError e)
    Right typedProgram -> return typedProgram