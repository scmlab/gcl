{-# LANGUAGE OverloadedStrings #-}
module Server.Handler.CustomMethod where

import qualified Data.Aeson                    as JSON
import           Data.Loc.Range
import qualified Data.Text                     as Text
import           GCL.Predicate
import           Server.CustomMethod
import           Server.DSL
import           Server.Interpreter.RealWorld
import Data.Loc (posCol)

handleRefine :: Range -> CmdM [ResKind]
handleRefine range = do
    mute True
    setLastSelection range
    source               <- getSource
    (spec, payloadLines) <- refine source range

    -- remove the Spec
    let
        indentedPayload = case payloadLines of
            []  -> ""
            [x] -> x
            (x : xs) ->
                let
                    indentation = Text.replicate
                        (posCol (rangeStart (specRange spec)) - 1)
                        " "
                in  Text.unlines $ x : map (indentation <>) xs
    source' <- editText (specRange spec) indentedPayload


    program <- parseProgram source'
    typeCheck program
    mute False
    result <- sweep program
    cacheResult (Right result)
    generateResponseAndDiagnosticsFromResult (Right result)

-- handler :: JSON.Value -> ServerM Int 
handler :: JSON.Value -> (Response -> ServerM ()) -> ServerM ()
handler params responder = do
    -- JSON Value => Request => Response
    case JSON.fromJSON params of
        JSON.Error msg -> do
            logText " --> CustomMethod: CannotDecodeRequest"
            responder $ CannotDecodeRequest $ show msg ++ "\n" ++ show params
        JSON.Success request@(Req filepath kind) -> do
            logText $ " --> Custom Reqeust: " <> Text.pack (show request)
            -- convert Request to Response
            interpret filepath (Just responder) $ do
                case kind of
                  -- Inspect
                    ReqInspect range -> do
                        setLastSelection range
                        result <- readCachedResult
                        generateResponseAndDiagnosticsFromResult result
                    ReqRefine range -> handleRefine range
                    ReqDebug        -> return $ error "crash!"
