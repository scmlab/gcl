{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Except hiding (guard)
-- import Control.Monad.IO.Class
import qualified Data.Aeson as JSON
import Data.ByteString.Lazy (fromStrict)
import Data.Text.Encoding (encodeUtf8)
import Error
import Language.LSP.Server
import Language.LSP.Types
import REPL

main :: IO Int
main =
  runServer $
    ServerDefinition
      { onConfigurationChange = const $ pure $ Right (),
        doInitialize = \env _req -> pure $ Right env,
        staticHandlers = handlers,
        interpretHandler = \env -> Iso (runLspT env) liftIO,
        options = defaultOptions
      }

handlers :: Handlers (LspM ())
handlers =
  mconcat
    [ requestHandler (SCustomMethod "guacamole") $ \req responder -> do
        let RequestMessage _ _ _ params = req
        response <- liftIO $ runREPLM (go params `catchError` handleError)
        case response of
          Left _ -> error "???"
          Right x -> responder (Right $ JSON.toJSON x)
    ]
  where
    go :: JSON.Value -> REPLM Response
    go (JSON.String raw) = do
      case JSON.eitherDecode $ fromStrict $ encodeUtf8 raw of
        Left msg -> throwError $ CannotDecodeRequest $ show raw
        Right x -> do
          res <- handleRequest x
          case res of
            Nothing -> error "???"
            Just x' -> return x'
    go raw = do
      case JSON.fromJSON raw of
        JSON.Error msg -> throwError $ CannotDecodeRequest $ show raw
        JSON.Success x -> do
          res <- handleRequest x
          case res of
            Nothing -> error "???"
            Just x' -> return x'

    handleError :: Error -> REPLM Response
    handleError err = return $ ResError [globalError err]