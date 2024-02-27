
import qualified Data.Aeson.Types as JSON
import Server.Monad (ServerM)

handler :: JSON.Value -> (Response -> ServerM ()) -> ServerM ()
handler params responder = do
    -- TODO:
    return ()