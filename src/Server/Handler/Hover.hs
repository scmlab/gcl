{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Server.Handler.Hover
  ( handler
  ) where

import           Error                          ( Error )
import           Server.Monad

import           Language.LSP.Types      hiding ( Range )
import           Server.DSL
import qualified Server.SrcLoc                 as SrcLoc
import           Server.TokenMap                ( Token(tokenHoverAndType)
                                                , lookupIntervalMap
                                                )

ignoreErrors :: Either [Error] (Maybe Hover) -> Maybe Hover
ignoreErrors (Left  _errors  ) = Nothing
ignoreErrors (Right locations) = locations

handler :: Uri -> Position -> (Maybe Hover -> ServerM ()) -> ServerM ()
handler uri position responder = case uriToFilePath uri of
  Nothing       -> return ()
  Just filepath -> do
    interpret uri (responder . ignoreErrors) $ do
      logText "<-- Hover"
      source <- getSource
      let table = SrcLoc.makeToOffset source
      let pos   = SrcLoc.fromLSPPosition table filepath position


      stage <- readCurrentStage

      let
        tokenMap = case stage of
          Parsed _result -> Nothing
          Converted result -> Just $ convertedTokenMap result
          Swept result -> Just $ convertedTokenMap (sweptPreviousStage result)

      return $ case tokenMap of
        Nothing -> Nothing
        Just xs -> do -- in Maybe Monad
          info          <- lookupIntervalMap xs pos
          (hover, _typ) <- tokenHoverAndType info
          return hover
