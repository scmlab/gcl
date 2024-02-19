{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Server.Handler.GoToDefn
  ( handler
  ) where

import           Data.Maybe                     ( maybeToList )
import           Error                          ( Error )
import           Language.LSP.Types      hiding ( Range )
import qualified Language.LSP.Types            as LSP
import           Server.Monad            hiding ( logText )
import           Server.Pipeline
import qualified Server.SrcLoc                 as SrcLoc
import qualified Server.IntervalMap               as IntervalMap

ignoreErrors :: ([Error], Maybe [LSP.LocationLink]) -> [LSP.LocationLink]
ignoreErrors (_, Nothing) = []
ignoreErrors (_, Just xs) = xs

handler :: Uri -> Position -> ([LSP.LocationLink] -> ServerM ()) -> ServerM ()
handler uri position responder = do
  case uriToFilePath uri of
    Nothing       -> return ()
    Just filepath -> do
      interpret uri (responder . ignoreErrors) $ do
        logText "<-- Goto Definition"
        source <- getSource

        let table = SrcLoc.makeToOffset source
        let pos   = SrcLoc.fromLSPPosition table filepath position

        stage <- load
        let
          tokenMap = case stage of
            Raw       _       -> Nothing
            Parsed    _result -> Nothing
            Converted result  -> Just $ convertedIntervalMap result
            TypeChecked result ->
              Just $ convertedIntervalMap (typeCheckedPreviousStage result)
            Swept result ->
              Just $ convertedIntervalMap
                (typeCheckedPreviousStage (sweptPreviousStage result))

        return $ maybeToList $ case tokenMap of
          Nothing -> Nothing
          Just xs -> IntervalMap.lookup pos xs

handler' :: Uri -> Position -> ([LSP.LocationLink] -> ServerM ()) -> ServerM ()
handler' uri position responder = do
  case uriToFilePath uri of
    Nothing       -> return ()
    Just filePath -> do
      logText "<-- Goto Definition"
      source <- getSource filePath

      let table = SrcLoc.makeToOffset source
      let positions' = SrcLoc.fromLSPPosition table filepath position

      loadedProgram <- dumpProgram filePath

      case IntervalMap.lookup positions' (scopingInfo loadedProgram) of
        Nothing -> return ()
        Just locationLinks -> responder locationLinks

