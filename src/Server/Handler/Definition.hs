{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Server.Handler.Definition
  ( handler
  ) where

import           Data.Maybe                     ( maybeToList )
import           Error                          ( Error )
import           Language.LSP.Types      hiding ( Range )
import           Server.DSL
import           Server.Monad
import qualified Server.SrcLoc                 as SrcLoc
import           Server.TokenMap

ignoreErrors :: Either [Error] [LocationLink] -> [LocationLink]
ignoreErrors (Left  _errors  ) = []
ignoreErrors (Right locations) = locations

handler :: Uri -> Position -> ([LocationLink] -> ServerM ()) -> ServerM ()
handler uri position responder = do
  case uriToFilePath uri of
    Nothing       -> return ()
    Just filepath -> do
      interpret filepath (responder . ignoreErrors) $ do
        source <- getSource

        result <- readCachedResult
        let infos = case result of
              Nothing            -> mempty
              Just (Left  _    ) -> mempty
              Just (Right cache) -> cacheTokenMap cache
        let table = SrcLoc.makeToOffset source
        let pos   = SrcLoc.fromLSPPosition table filepath position

        return $ maybeToList $ do -- Maybe monad here 
          info <- lookupIntervalMap infos pos
          tokenLocationLink info
