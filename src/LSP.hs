{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}

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
        let RequestMessage _ i _ params = req
        response <- liftIO $ runREPLM (go i params)
        case response of
          Left err -> responder (Right $ JSON.toJSON $ ResError [globalError err])
          Right x -> responder (Right $ JSON.toJSON x)
    ]
  where
    go :: LspId ( 'CustomMethod :: Method 'FromClient 'Request) -> JSON.Value -> REPLM Response
    go i raw = case JSON.fromJSON raw of
      JSON.Error msg -> throwError $ CannotDecodeRequest msg
      JSON.Success x -> handleRequest i x