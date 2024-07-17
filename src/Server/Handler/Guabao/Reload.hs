{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}

module Server.Handler.Guabao.Reload where

import qualified Data.Aeson.Types as JSON
import GHC.Generics ( Generic )
import Server.Monad (ServerM, FileState(..), Versioned)
import Error (Error)
import GCL.Predicate (Spec, PO)
import Server.Load (load)

data ReloadParams = ReloadParams { filePath :: FilePath }
  deriving (Eq, Show, Generic)

instance JSON.FromJSON ReloadParams

data ReloadResult = ReloadResult
  { specifications :: [Versioned Spec]
  , proofObligations :: [PO]
  }
  deriving (Eq, Show, Generic)

type ReloadError = Error

handler :: ReloadParams -> (() -> ServerM ()) -> (() -> ServerM ()) -> ServerM ()
handler ReloadParams{filePath} onResult onError = do
  load filePath
  onResult ()
