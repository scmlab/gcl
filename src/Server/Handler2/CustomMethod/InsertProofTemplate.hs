{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Handler2.CustomMethod.InsertProofTemplate (slowHandler) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.List as List

import Data.Loc.Range (Range (..))
import Render.Predicate (exprOfPred)
import GCL.Predicate (PO(..))
import Pretty (docToText, Pretty (..))

import Server.Monad (ServerM, LoadedProgram(..))
import Server.Handler2.Utils
import Server.Handler2.CustomMethod.Reload as Reload
import Server.CustomMethod (ResKind)
import Error (Error (..))

slowHandler :: FilePath -> Range -> Text -> ([ResKind] -> ServerM ()) -> (Error -> ServerM ()) -> ServerM ()
slowHandler sourceFilePath rangeToInsertProof proofObligationHash onFinish onError = do
  -- reload source
  Reload.reload sourceFilePath (\loadedProgram -> do
      -- find proof obligation by hash
      let proofObligations :: [PO] = _proofObligations loadedProgram
      case findProofObligationByHash proofObligations proofObligationHash of
        Nothing -> onError (Others "Proof obligation not found.")
        Just proofObligation -> do
          -- insert proof template
          let proofTemplate :: Text = makeProofTemplate proofObligation
          editText rangeToInsertProof ("\n\n" <> proofTemplate) do
            -- reload, send diagnostics and respond hint updates
            Reload.handler sourceFilePath onFinish onError
    ) onError
  where
    findProofObligationByHash :: [PO] -> Text -> Maybe PO
    findProofObligationByHash proofObligations hash =
      List.find (\po -> poAnchorHash po == hash) proofObligations
    makeProofTemplate :: PO -> Text
    makeProofTemplate proofObligation =
      "{- #" <> poAnchorHash proofObligation <> "\n"
      <> preExpr <> "\n"
      <> "⇒" <> "\n"
      <> postExpr <> "\n"
      <> Text.pack (replicate len '=')
      <> "\n\n-}\n"
      where
        preExpr = docToText $ pretty $ exprOfPred $ poPre proofObligation
        postExpr = docToText $ pretty $ exprOfPred $ poPost proofObligation
        len = max (max (Text.length preExpr) (Text.length postExpr) - 2) 5
