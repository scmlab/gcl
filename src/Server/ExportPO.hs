module Server.ExportPO where 


-- import qualified Data.Text as Text
-- import Data.Text (Text)
-- import Language.LSP.Server (LspT)

-- exportFilepath :: Text
-- exportFilepath = Text.pack filepath <> ".md"

-- createPOFile :: LspT () ServerM ()
-- createPOFile = do
--     let uri = Uri exportFilepath
--     let createFile = CreateFile uri Nothing
--     let edit = WorkspaceEdit Nothing (Just (List [InR (InL createFile)]))
--     _ <- sendRequest SWorkspaceApplyEdit (ApplyWorkspaceEditParams (Just "create export file") edit) handleCreatePOFile
--     pure ()

-- handleCreatePOFile :: Either ResponseError ApplyWorkspaceEditResponseBody -> LspT () ServerM ()
-- handleCreatePOFile (Left (ResponseError _ message _)) = sendNotification SWindowShowMessage (ShowMessageParams MtError $ "Failed to export proof obligations: \n" <> message)
-- handleCreatePOFile (Right (ApplyWorkspaceEditResponseBody False Nothing)) = sendNotification SWindowShowMessage (ShowMessageParams MtWarning $ exportFilepath <> " already existed")
-- handleCreatePOFile (Right _) = exportPOs

-- exportPOs :: LspT () ServerM ()
-- exportPOs = do
--     let result = runM $ do
--         program <- parseProgram filepath source
--         (pos, _) <- genPO program
--         return pos
--     case result of
--         Left err -> do
--             let message = Text.pack $ show err
--             sendNotification SWindowShowMessage (ShowMessageParams MtError $ "Failed calculate proof obligations: \n" <> message)
--         Right pos -> do
--             let toMarkdown (PO i pre post _) = pretty i <> "." <+> pretty pre <+> "=>" <+> pretty post
--             let content = renderStrict $ concatWith (\x y -> x <> line <> y) $ map toMarkdown pos

--             let identifier = VersionedTextDocumentIdentifier (Uri exportFilepath) (Just 0)
--             let range = Range (Position 0 0) (Position 0 0)
--             let textEdits = [TextEdit range content]
--             -- let textEdits = map (TextEdit range . renderStrict . toMarkdown) pos
--             let textDocEdit = TextDocumentEdit identifier $ List textEdits
--             let edit = WorkspaceEdit Nothing (Just (List [InL textDocEdit]))
--             _ <- sendRequest SWorkspaceApplyEdit (ApplyWorkspaceEditParams (Just "writing proof obligations") edit) handleExportPos

--             -- sendNotification SWindowShowMessage (ShowMessageParams MtInfo $ "a\nb")
--             -- sendNotification SWindowLogMessage (LogMessageParams MtInfo $ "a\nb")

--             pure ()

-- handleExportPos :: Either ResponseError ApplyWorkspaceEditResponseBody -> LspT () ServerM ()
-- handleExportPos (Left (ResponseError _ message _)) = sendNotification SWindowShowMessage (ShowMessageParams MtError $ "Failed to write proof obligations: \n" <> message)
-- handleExportPos (Right message) = sendNotification SWindowShowMessage (ShowMessageParams MtWarning $ Text.pack $ show message)