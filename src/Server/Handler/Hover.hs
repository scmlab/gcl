{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Server.Handler.Hover where

-- import           Data.Loc                       ( posCoff )
-- import           Language.LSP.Types      hiding ( Range )
-- import           Pretty                         ( toText )
-- import qualified Server.SrcLoc                 as SrcLoc
-- import qualified Server.IntervalMap               as IntervalMap

-- import Server.Handler2.Utils
-- import Server.Monad (ServerM, LoadedProgram (..))

-- handler :: Uri -> Position -> (Maybe Hover -> ServerM ()) -> ServerM ()
-- handler uri position responder = case uriToFilePath uri of
--   Nothing       -> responder Nothing
--   Just filepath -> do
--     maybeSource <- getSource filepath
--     case maybeSource of
--       Nothing -> responder Nothing
--       Just source -> do
--         let table = SrcLoc.makeToOffset source
--         let pos   = SrcLoc.fromLSPPosition table filepath position
--         logText $ " ---> Hover " <> toText (posCoff pos)
        
--         maybeLoadedProgram <- dumpProgram filepath
--         case maybeLoadedProgram of
--           Nothing -> responder Nothing
--           Just loadedProgram -> do
--             case IntervalMap.lookup pos (_typeCheckingInfo loadedProgram) of
--               Nothing -> do
--                   -- logText $ toText xs
--                   logText "    < Hover (not found)"
--                   responder Nothing
--               Just (hover, _) -> do
--                   logText $ "    < Hover " <> toText hover
--                   responder (Just hover)