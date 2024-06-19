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
instance JSON.ToJSON ReloadResult

type ReloadError = Error

handler :: ReloadParams -> (ReloadResult -> ServerM ()) -> (ReloadError -> ServerM ()) -> ServerM ()
handler ReloadParams{filePath} onSuccess onError =
  load filePath (\FileState{..} -> onSuccess ReloadResult{..}) onError
