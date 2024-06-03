{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Server.Handler2.CustomMethod.HelloWorld (handler) where

import Data.String (fromString)
import Data.Loc.Range (Range (..), rangeFile, rangeStart, rangeEnd)

import Render (Section (..), Deco (..), Block (..))
import Error (Error(..))

import Server.Monad (ServerM)
import Server.Handler2.Utils
import Server.CustomMethod (ResKind (..))
import Data.Loc (Pos (..), posFile, posLine, posCol, posCoff)
import Server.Handler.Diagnostic (makeDiagnostic)


handler :: Range -> ([ResKind] -> ServerM ()) -> (Error -> ServerM ()) -> ServerM ()
handler range onFinish onError = do
  let filepath :: FilePath = rangeFile range
  replaceWithHelloworld filepath range (do
      let helloWorldRange = rangeAfterReplacedWithHelloWorld range
      let diagnosticOnHelloWorld = makeDiagnostic Nothing helloWorldRange "Hello, World?" "This is a warning"
      _ <- sendDiagnostics filepath [diagnosticOnHelloWorld]
      version <- bumpVersion
      onFinish [
          ResDisplay version [
            Section Blue [
              Header "Hello, world" Nothing
            , Paragraph $ fromString "LSP server successfully responded."
            ]
          ]
        ]
    ) onError

replaceWithHelloworld :: FilePath -> Range -> ServerM () -> (Error -> ServerM ()) -> ServerM ()
replaceWithHelloworld filepath range onFinish onError = do
  maybeSource <- getSource filepath
  case maybeSource of
    Nothing -> onError (CannotReadFile filepath)
    Just source -> do
      logText "before replacement"
      logText source
      logText "\n"
      editText range "Hello, World!\n" $ do
        maybeSource' <- getSource filepath
        case maybeSource' of
          Nothing -> onError (CannotReadFile filepath)
          Just source' -> do
            logText "after replacement"
            logText source'
            logText "\n"
            onFinish

rangeAfterReplacedWithHelloWorld :: Range -> Range
rangeAfterReplacedWithHelloWorld range =
  Range (rangeStart range) (addToCoff 13 $ rangeEnd range)
  where
    addToCoff :: Int -> Pos -> Pos
    addToCoff offset pos = Pos (posFile pos) (posLine pos) (posCol pos) (posCoff pos + offset)
