{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}

module Server.Handler.GCL.Reload where

import qualified Data.Aeson.Types as JSON
import GHC.Generics ( Generic )
import Server.Monad (ServerM, FileState(..), Versioned, sendDebugMessage)
import Error (Error)
import GCL.Predicate (Spec, PO)
import Server.Load (load)

data ReloadParams = ReloadParams { filePath :: FilePath }
  deriving (Eq, Show, Generic)

instance JSON.FromJSON ReloadParams
instance JSON.ToJSON ReloadParams

handler :: ReloadParams -> (() -> ServerM ()) -> (() -> ServerM ()) -> ServerM ()
handler ReloadParams{filePath} onResult _ = do
  load filePath
  onResult ()
