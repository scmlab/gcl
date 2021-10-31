{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Server.Handler.Highlighting where

import qualified Language.LSP.Types            as J

import           Data.Foldable                  ( toList )
import           Error                          ( Error )
import           Server.DSL
import           Server.Monad

ignoreErrors
  :: Either [Error] (Either J.ResponseError (Maybe J.SemanticTokens))
  -> Either J.ResponseError (Maybe J.SemanticTokens)
ignoreErrors (Left  _errors) = Right Nothing
ignoreErrors (Right xs     ) = xs

handler
  :: J.Uri
  -> (Either J.ResponseError (Maybe J.SemanticTokens) -> ServerM ())
  -> ServerM ()
handler uri responder = case J.uriToFilePath uri of
  Nothing       -> return ()
  Just filepath -> do
    interpret filepath (responder . ignoreErrors) $ do
      result <- readCachedResult
      let highlightings = toList $ case result of
            Nothing            -> mempty
            Just (Left  _    ) -> mempty
            Just (Right cache) -> cacheHighlighings cache
      let legend = J.SemanticTokensLegend
            (J.List J.knownSemanticTokenTypes)
            (J.List J.knownSemanticTokenModifiers)
      let tokens = J.makeSemanticTokens legend highlightings
      case tokens of
        Left t -> return $ Left $ J.ResponseError J.InternalError t Nothing
        Right tokens' -> return $ Right $ Just tokens'
