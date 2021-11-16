{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Server.Handler.Definition
  ( handler
  ) where

import           Data.Maybe                     ( maybeToList )
import           Error                          ( Error )
import           Language.LSP.Types      hiding ( Range )
import           Server.Pipeline
import           Server.Monad
import qualified Server.SrcLoc                 as SrcLoc
import           Server.TokenMap

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
        let tokenMap = case stage of
              Raw _ -> Nothing
              Parsed _result -> Nothing
              Converted result -> Just $ convertedTokenMap result
              Swept     result -> Just $ convertedTokenMap (sweptPreviousStage result)
              
        return $ maybeToList $ case tokenMap of
          Nothing -> Nothing
          Just xs -> do -- in Maybe monad
            info <- lookupIntervalMap xs pos
            tokenLocationLink info
