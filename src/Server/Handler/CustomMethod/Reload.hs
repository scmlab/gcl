
module Server.Handler.CustomMethod.Reload where

import Server.Monad (ServerM)


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

handler :: FilePath -> ServerM ()
handler filePath = do
  -- TODO
  return ()