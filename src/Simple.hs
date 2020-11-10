{-# LANGUAGE OverloadedStrings #-}

module Simple where

import qualified Data.Aeson as JSON
import Language.LSP.Server
import Language.LSP.Types

handlers :: Handlers (LspM ())
handlers =
  mconcat
    [ requestHandler (SCustomMethod "guacamole/load") $ \req responder -> do
        responder (Right $ JSON.toJSON req)
    ]
