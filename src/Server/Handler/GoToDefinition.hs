{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use forM_" #-}

module Server.Handler.GoToDefinition where

import qualified Language.LSP.Types as LSP
import Server.Monad (ServerM, FileState (..), readSource, loadFileState, logText)


import qualified Server.SrcLoc as SrcLoc
import qualified Server.IntervalMap as IntervalMap
import Server.PositionMapping( PositionResult( PositionExact ), fromDelta, PositionDelta, toCurrentRange, PositionMapping(..), toCurrentRange' )

handler :: LSP.Uri -> LSP.Position -> ([LSP.LocationLink] -> ServerM ()) -> ServerM ()
handler uri lspPosition responder = do
  case LSP.uriToFilePath uri of
    Nothing       -> responder []
    Just filePath -> do
      maybeFileState <- loadFileState filePath
      case maybeFileState of
        Nothing -> responder []
        Just FileState
              { definitionLinks
              , positionDelta
              , toOffsetMap
              } -> do
          case (fromDelta positionDelta) lspPosition of
            PositionExact oldLspPosition -> do
              let oldPos = SrcLoc.fromLSPPosition toOffsetMap filePath oldLspPosition
              case IntervalMap.lookup oldPos definitionLinks of
                Nothing -> responder []
                Just locationLink -> responder $ translateLocationLinks positionDelta [locationLink]
            _                            -> responder []

-- TODO: currently, we assume source and target are in the same file
--  and translate both source and target with the same positionDelta
--  extend this translation to use positionDelta of other fileUri if target is in another file
translateLocationLinks :: PositionDelta -> [LSP.LocationLink] -> [LSP.LocationLink]
translateLocationLinks delta links = do
  link <- links
  case translateLocationLink delta link of
    Nothing   -> []
    Just link' -> [link']

translateLocationLink :: PositionDelta -> LSP.LocationLink -> Maybe LSP.LocationLink
translateLocationLink delta (LSP.LocationLink maybeSource targetUri targetRange targetSelection) = do
  let maybeSource' = do
                      source <- maybeSource
                      translateRange source
  targetRange'     <- translateRange targetRange
  targetSelection' <- translateRange targetSelection
  return (LSP.LocationLink maybeSource' targetUri targetRange' targetSelection')
  where
    translateRange :: LSP.Range -> Maybe LSP.Range
    translateRange = toCurrentRange' delta