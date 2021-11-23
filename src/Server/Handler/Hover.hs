{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Server.Handler.Hover
  ( handler
  ) where

import           Error                          ( Error )
import           Server.Monad

import           Data.Loc                       ( posCoff )
import           Language.LSP.Types      hiding ( Range )
import           Pretty                         ( toText )
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
      source <- getSource
      let table = SrcLoc.makeToOffset source
      let pos   = SrcLoc.fromLSPPosition table filepath position
      logText $ " ---> Hover " <> toText (posCoff pos)

      stage <- load

      let
        tokenMap = case stage of
          Raw         _      -> Nothing
          Parsed      _      -> Nothing
          Converted   _      -> Nothing
          TypeChecked result -> Just $ typeCheckedTokenMap result
          Swept result ->
            Just $ typeCheckedTokenMap (sweptPreviousStage result)

      case tokenMap of
        Nothing -> return Nothing
        Just xs -> case TokenMap.lookup xs pos of
          Nothing -> do
            -- logText $ toText xs
            logText "    < Hover (not found)"
            return Nothing
          Just (hover, _) -> do
            logText $ "    < Hover " <> toText hover
            return (Just hover)

