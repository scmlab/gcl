{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Server.Handler.Hover
  ( handler
  ) where

import           Error                          ( Error )
import           Server.Monad

import           Language.LSP.Types      hiding ( Range )
import           Server.Pipeline
import qualified Server.SrcLoc                 as SrcLoc
import qualified Server.TokenMap               as TokenMap

ignoreErrors :: ([Error], Maybe (Maybe Hover)) -> Maybe Hover
ignoreErrors (_, Nothing) = Nothing
ignoreErrors (_, Just xs) = xs

handler :: Uri -> Position -> (Maybe Hover -> ServerM ()) -> ServerM ()
handler uri position responder = case uriToFilePath uri of
  Nothing       -> return ()
  Just filepath -> do
    interpret uri (responder . ignoreErrors) $ do
      logText " ---> Hover"
      source <- getSource
      let table = SrcLoc.makeToOffset source
      let pos   = SrcLoc.fromLSPPosition table filepath position


      stage <- load

      let
        tokenMap = case stage of
          Raw         _      -> Nothing
          Parsed      _      -> Nothing
          Converted   _      -> Nothing
          TypeChecked result -> Just $ typeCheckedTokenMap result
          Swept result ->
            Just $ typeCheckedTokenMap (sweptPreviousStage result)

      return $ case tokenMap of
        Nothing -> Nothing
        Just xs -> do -- in Maybe Monad
          (hover, _typ) <- TokenMap.lookup xs pos
          return hover
