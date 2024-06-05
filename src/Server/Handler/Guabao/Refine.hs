{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Server.Handler.Guabao.Refine where

import qualified Data.Aeson.Types as JSON
import GHC.Generics ( Generic )
import Control.Monad.Except           ( runExcept )
import Server.Monad (ServerM, FileState(..), loadFileState)

import qualified Syntax.Parser                as Parser
import           Syntax.Parser.Error           ( ParseError(..) )
import Syntax.Parser.Lexer (TokStream(..), scan)
import Language.Lexer.Applicative              ( TokenStream(..))

import           Data.Bifunctor                 ( Bifunctor (second) )

import Error (Error)
import GCL.Predicate (Spec(..), PO)
import GCL.Common (TypeEnv)
import GCL.Type (Elab(..), TypeError, runElaboration, typeInfoToType)
import           Control.Monad.State.Lazy (get)
import Server.Load (load)
import Data.Loc.Range (Range)
import Server.PositionMapping (fromCurrentRange, PositionDelta(..), PositionMapping(..))
import Server.SrcLoc (fromLSPRange, toLSPRange)
import qualified Language.LSP.Types as LSP
import qualified Server.SrcLoc as SrcLoc
import Data.Text (Text)
import Data.List (find)
import Data.Loc (Pos(..), Loc(..), L(..))
import qualified Syntax.Concrete as C
import qualified Syntax.Abstract as A
import qualified Syntax.Typed    as T

data RefineParams = RefineParams
  { filePath     :: FilePath
  , range        :: Range
  , fragmentText :: Text
  }
  deriving (Eq, Show, Generic)

instance JSON.FromJSON RefineParams

data RefineResult = RefineResult
  { specifications :: [Spec]
  , proofObligations :: [PO]
  }
  deriving (Eq, Show, Generic)
instance JSON.ToJSON RefineResult

-- TODO customize refine error
type RefineError = Error

handler :: RefineParams -> (RefineResult -> ServerM ()) -> (RefineError -> ServerM ()) -> ServerM ()
handler params@RefineParams{filePath, range, fragmentText} onSuccess onError = do
  maybeFileState <- loadFileState filePath
  case maybeFileState of
    Nothing -> return () -- TODO: report error using onError
    Just fileState -> do
      let FileState{
        positionDelta,
        toOffsetMap,
        specifications
      } = fileState
      -- calculate rangeAtLastReload with positionDelta
      let lspRange = SrcLoc.toLSPRange range
      case fromCurrentRange' positionDelta lspRange of
        Nothing -> return ()
        Just lspRangeAtLastReload -> do
          let rangeAtLastReload = SrcLoc.fromLSPRange toOffsetMap filePath lspRangeAtLastReload
          -- find the spec with rangeAtLastReload
          case findSpecWithRange rangeAtLastReload specifications of
            Nothing   -> return () -- TODO: error "spec not found at range"
            Just spec -> do
              -- TODO
              return ()

fromCurrentRange' :: PositionDelta -> LSP.Range -> Maybe LSP.Range
fromCurrentRange' = fromCurrentRange . PositionMapping

findSpecWithRange :: Range -> [Spec] -> Maybe Spec
findSpecWithRange range = find (\Specification{specRange} -> specRange == range)

deleteSpecWithRange :: Range -> [Spec] -> [Spec]
deleteSpecWithRange range = filter (\Specification{specRange} -> specRange == range)

parseFragment :: Pos -> Text -> Either ParseError [C.Stmt]
parseFragment fragmentStart fragment = do
  let Pos filePath _ _ _ = fragmentStart
  case scan filePath fragment of
    Left  err    -> Left (LexicalError err)
    Right tokens -> do
      let tokens' = translateTokStream fragmentStart tokens
      case Parser.parse Parser.statements filePath tokens' of
        Left  (errors,logMsg) -> Left (SyntacticError errors logMsg)
        Right val             -> Right val
  where
    translateRange :: Pos -> Pos -> Pos
    translateRange fragmentStart@(Pos _ lineStart colStart coStart)
        (Pos path lineOffset colOffset coOffset)
      = Pos path line col co
      where
        line = lineStart + lineOffset - 1
        col = if lineOffset == 1
                then colStart + colOffset
                else colStart
        co = if lineOffset == 1
                then coStart + coOffset
                else coStart

    translateLoc :: Pos -> Loc -> Loc
    translateLoc fragmentStart (Loc left right)
      = Loc (translateRange fragmentStart left) (translateRange fragmentStart right)
    translateLoc fragmentStart NoLoc = NoLoc

    translateTokStream :: Pos -> TokStream -> TokStream
    translateTokStream fragmentStart (TsToken (L loc x) rest)
      = TsToken (L (translateLoc fragmentStart loc) x) (translateTokStream fragmentStart rest)
    translateTokStream fragmentStart TsEof = TsEof
    translateTokStream fragmentStart (TsError e) = TsError e

toAbstractFragment :: [C.Stmt] -> Maybe [A.Stmt]
toAbstractFragment concreteFragment = 
  case runExcept $ C.toAbstract concreteFragment of
    Left _                 -> Nothing
    Right abstractFragment -> Just abstractFragment

typeCheckFragment :: Elab a => TypeEnv -> a -> Either TypeError ()
typeCheckFragment typeEnv abstractFragment =
  case runElaboration abstractFragment of
    Left err -> Left err
    Right _  -> Right ()

instance Elab [A.Stmt] where
  elaborate stmts env = error "TODO"

sweepFragment :: [A.Stmt] -> Maybe ([PO], [Spec])
sweepFragment fragment = error "TODO find new POs and specs with StructStmt"

