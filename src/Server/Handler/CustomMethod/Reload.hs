
module Server.Handler.CustomMethod.Reload where

import Server.Monad (ServerM)
import Server.Load (load)

-- data ReloadResponse
--   = Success [PO] [Spec]
--   | Fail Error

-- data Error
--   = ParseError ParseError
--   | TypeError TypeError
--   | StructError StructError
--   | CannotReadFile FilePath
--   | Others String
--   deriving (Eq, Show, Generic)


-- TODO: respond result or error
handler :: FilePath -> ServerM ()
handler filePath = do
  load filePath (\_ -> return ()) (\_ -> return ())