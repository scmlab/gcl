{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Handler.Hover (handler) where

import Data.Loc.Range
import Pretty
import Server.Stab
import Server.Util
import qualified Server.Util as J
import Syntax.Abstract
import Server.Interpreter.RealWorld
import Error (Error)

import           Language.LSP.Types      hiding ( Range )
import Server.DSL


ignoreErrors :: Either [Error] (Maybe Hover) -> Maybe Hover
ignoreErrors (Left  _errors  ) = Nothing
ignoreErrors (Right locations) = locations

handler :: Uri -> Position -> (Maybe Hover -> ServerM ()) -> ServerM ()
handler uri pos responder = do
  case uriToFilePath uri of
    Nothing       -> return ()
    Just filepath -> do
      interpret filepath (responder . ignoreErrors) $ do
        source  <- getSource
        program <- parseProgram source
        return $ Nothing
          -- stabMaybe pos program

        -- runGotoM program $ stabM pos program


-- handler :: J.Uri -> J.Position -> ServerM (Maybe J.Hover)
-- handler uri position = do
--   (_, result) <- parseAndScopeCheck uri
--   case result of
--     Nothing -> return Nothing
--     Just program -> return $ stabMaybe position program

-- instance StabM HoverM Program Hover where
--   stab pos (Program decls _ _ stmts l) = whenInRange' pos l $ _
    -- whenInRange' pos l $ do
    -- decls' <- concat <$> mapM (stabM pos) decls
    -- stmts' <- concat <$> mapM (stabM pos) stmts
    -- return (decls' <> stmts')


      -- then
      --   let content = J.HoverContents $ J.markedUpContent "pill-language-server" (toText name)
      --    in [J.Hover content (Just (J.toRange (rangeOf name)))]
      -- else []

-- instance Stab Program Hover where
--   stab position (Program _ procDefns _) = concatMap (stab position) procDefns

-- instance Stab ProcDefn J.Hover where
--   stab position (ProcDefn name _ _ _) =
--     if position `stabbed` name
--       then
--         let content = J.HoverContents $ J.markedUpContent "pill-language-server" (toText name)
--          in [J.Hover content (Just (J.toRange (rangeOf name)))]
--       else []