
module Server.Handler2.Reload (handler) where

import qualified Data.Aeson.Types as JSON
import Server.Monad (ServerM)
import Error (Error)
import GCL.Predicate (PO (..), Spec (..))

handler :: JSON.Value -> (Either Error ([Spec], [PO]) -> ServerM ()) -> ServerM ()
handler params responder = do
    -- TODO:
    return ()