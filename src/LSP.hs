{-# LANGUAGE OverloadedStrings #-}

module LSP where

import Control.Monad.Except hiding (guard)
-- import Control.Monad.IO.Class
import qualified Data.Aeson as JSON
import Error
import Language.LSP.Server
import Language.LSP.Types
import REPL

run :: IO Int
run =
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
    go raw = do
      case JSON.fromJSON raw of
        JSON.Error msg -> throwError $ CannotDecodeRequest msg
        JSON.Success x -> do
          res <- handleRequest x
          case res of
            Nothing -> error "???"
            Just x' -> return x'

    handleError :: Error -> REPLM Response
    handleError err = return $ ResError [globalError err]