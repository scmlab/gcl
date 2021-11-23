{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Server.Handler.GoToDefn
  ( handler
  ) where

import           Data.Maybe                     ( maybeToList )
import           Error                          ( Error )
import           Language.LSP.Types      hiding ( Range )
import           Server.Monad
import           Server.Pipeline
import qualified Server.SrcLoc                 as SrcLoc
import qualified Server.TokenMap as TokenMap

ignoreErrors :: ([Error], Maybe [LocationLink]) -> [LocationLink]
ignoreErrors (_, Nothing) = []
ignoreErrors (_, Just xs) = xs

handler :: Uri -> Position -> ([LocationLink] -> ServerM ()) -> ServerM ()
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
            Converted result  -> Just $ convertedTokenMap result
            TypeChecked result ->
              Just $ convertedTokenMap (typeCheckedPreviousStage result)
            Swept result ->
              Just $ convertedTokenMap
                (typeCheckedPreviousStage (sweptPreviousStage result))

        return $ maybeToList $ case tokenMap of
          Nothing -> Nothing
          Just xs -> TokenMap.lookup xs pos
