{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Server.Handler.Guabao.Refine where

import qualified Data.Aeson as JSON
import GHC.Generics ( Generic )
import Data.Bifunctor ( bimap )
import Control.Monad.Except           ( runExcept )
import Server.Monad (ServerM, FileState(..), loadFileState, editTexts, pushSpecs, deleteSpec, Versioned, pushPos, updateIdCounter, logText)
import Server.Notification.Update (sendUpdateNotification)

import qualified Syntax.Parser                as Parser
import           Syntax.Parser.Error           ( ParseError(..) )
import Syntax.Parser.Lexer (TokStream(..), scan)
import Language.Lexer.Applicative              ( TokenStream(..))

import Error (Error (ParseError, TypeError, StructError))
import GCL.Predicate (Spec(..), PO, InfMode(..))
import GCL.Common (TypeEnv)
import GCL.Type (Elab(..), TypeError, runElaboration, Typed)
import Data.Loc.Range (Range (..))
import Data.Text (Text, split)
import Data.List (find, maximumBy)
import Data.Loc (Pos(..), Loc(..), L(..))
import qualified Data.Map        as Map
import qualified Syntax.Concrete as C
import qualified Syntax.Abstract as A
import qualified Syntax.Typed    as T
import GCL.WP.Types (StructError, StructWarning)
import GCL.WP
import qualified Data.Text as Text
import Pretty (pretty)

data RefineParams = RefineParams
  { filePath  :: FilePath
  , specRange :: Range
  , implText  :: Text -- brackets included
  }
  deriving (Eq, Show, Generic)

instance JSON.FromJSON RefineParams
instance JSON.ToJSON RefineParams

-- assumes specText is surrounded by "[!" and "!]"
handler :: RefineParams -> (() -> ServerM ()) -> (() -> ServerM ()) -> ServerM ()
handler _params@RefineParams{filePath, specRange, implText} onFinish _ = do
  -- 把上次 load 的資料拿出來
  logText "refine: start\n"
  maybeFileState <- loadFileState filePath
  case maybeFileState of
    Nothing -> do
      logText "  no fileState matched\n"
      return ()
    Just fileState -> do
      -- 把 specText 去掉頭尾的 [!!] 和 \t \n \s
      logText "  fileState loaded\n"
      logText "  digging holes\n"
      logText "    (before)"
      -- logText implText
      logText "\n"
      logText "    (before2)"
      -- 挖洞
      case digImplHoles filePath implText of
        Left err -> do
          logText "  parse error\n"
          onError (ParseError err)
        Right holelessImplText -> do
          logText "  holes digged\n"
          logText "\n    (after)"
          logText holelessImplText
          logText "\n"
          let (Range specStart _) = specRange
          -- text to concrete
          -- (use specStart as the starting position in parse/toAbstract/elaborate)
          case parseFragment specStart holelessImplText of
            Left err           -> onError (ParseError err)
            Right concreteImpl -> do
              -- concrete to abstract
              logText "  text parsed\n"
              case toAbstractFragment concreteImpl of
                Nothing           -> do
                  logText "  holes still found after digging all holes\n"
                  error "should not happen\n"
                Just abstractImpl -> do
                  logText "  abstracted\n"
                  -- get spec (along with its type environment)
                  let FileState{specifications} = fileState
                  case lookupSpecByRange specifications specRange of
                    Nothing   -> do
                      logText "  spec not found at range, should reload\n"
                    Just spec -> do
                      logText "  matching spec found\n"
                      -- elaborate
                      let typeEnv :: TypeEnv = specTypeEnv spec
                      logText " type env:\n"
                      logText (Text.pack $ show typeEnv)
                      logText "\n"
                      -- TODO:
                      -- 1. Load: 在 elaborate program 的時候，要把 specTypeEnv 加到 spec 裡 (Andy) ok
                      -- 2. Load: 在 sweep 的時候，改成輸入 elaborated program，把 elaborated program 裡面的 spec 的 typeEnv 加到輸出的 [Spec] 裡 (SCM)
                      -- 3. Refine: elaborateFragment 裡面要正確使用 typeEnv (Andy) ok
                      case elaborateFragment typeEnv abstractImpl of
                        Left err -> do
                          logText "  type error\n"
                          onError (TypeError err)
                        Right typedImpl -> do
                          -- get POs and specs
                          logText "  type checked\n"
                          let FileState{idCount} = fileState
                          case sweepFragment idCount spec typedImpl of
                            Left err -> onError (StructError err)
                            Right (innerPos, innerSpecs, warnings, idCount') -> do
                              logText "  swept\n"
                              -- edit source (dig holes + remove outer brackets)
                              editTexts filePath [(specRange, holelessImplText)] do
                                logText "  text edited (refine)\n"
                                -- delete outer spec (by id)
                                deleteSpec filePath spec
                                logText "  outer spec deleted (refine)\n"
                                -- add inner specs to fileState
                                let FileState{editedVersion} = fileState
                                updateIdCounter filePath idCount'
                                logText "  counter updated (refine)\n"
                                pushSpecs (editedVersion + 1) filePath innerSpecs
                                pushPos (editedVersion + 1) filePath innerPos
                                logText "  new specs and POs added (refine)\n"
                                -- send notification to update Specs and POs
                                logText "refine: success\n"
                                sendUpdateNotification filePath []
                                logText "refine: update notification sent\n"
                                onFinish ()
  logText "refine: end\n"
  where
    onError :: Error -> ServerM ()
    onError err = do
      logText "refine: error\n\t"
      logText $ Text.pack (show $ pretty err)
      logText "\n"
      sendUpdateNotification filePath [err]
      logText "refine: update notification sent\n"


-- assumes specText is surrounded by "[!" and "!]"
trimSpecBracketsAndSpaces :: Text -> Text
trimSpecBracketsAndSpaces specText
  = Text.strip $ Text.dropEnd 2 $ Text.drop 2 specText

getMaximumSpecId :: [Versioned Spec] -> Int
getMaximumSpecId specs = specID $ snd $ maximumBy (\(_,specA) (_,specB) -> compare (specID specA) (specID specB)) specs

lookupSpecByRange :: [Versioned Spec] -> Range -> Maybe Spec
lookupSpecByRange specs targetRange = do
  (_version, spec) <- find (\(_, Specification{specRange}) -> specRange == targetRange) specs
  return spec

collectFragmentHoles :: [C.Stmt] -> [Range]
collectFragmentHoles concreteFragment = do
  statement <- concreteFragment
  case statement of
    C.SpecQM range -> return range
    _ -> []

reportFragmentHolesOrToAbstract :: [C.Stmt] -> Either [Range] [A.Stmt]
reportFragmentHolesOrToAbstract concreteFragment =
  case collectFragmentHoles concreteFragment of
    []    -> case toAbstractFragment concreteFragment of
      Nothing               -> error "should dig all holes before calling Concrete.toAbstract"
      Just abstractFragment -> Right abstractFragment
    holes -> Left holes


digImplHoles :: FilePath -> Text -> Either ParseError Text
digImplHoles filePath implText =
  case parseFragment (Pos filePath 1 1 0) implText of
    Left err -> Left err
    Right concreteImpl ->
      case collectFragmentHoles concreteImpl of
        [] -> return implText
        Range start _:_ -> digImplHoles filePath $ digFragementHole start implText
  where
    digFragementHole :: Pos -> Text -> Text
    digFragementHole (Pos _path lineNumber col _charOff) fullText =
      Text.unlines linesEdited
      where
        allLines :: [Text]
        allLines = split (== '\n') fullText -- split fullText by '\n'
        lineToEdit :: Text
        lineToEdit = allLines !! lineNumber
        beforeHole = Text.take (col-1) lineToEdit
        afterHole = Text.drop col lineToEdit -- lineToEdit
        indentation = Text.replicate col " "
        lineEdited :: Text
        lineEdited = beforeHole <> "[!\n" <> indentation <> "\n" <> indentation <> "!]" <> afterHole
        linesEdited :: [Text]
        linesEdited = take (lineNumber - 1) allLines ++ [lineEdited] ++ drop lineNumber allLines

-- `fragmentStart :: Pos` is used to translate the locations in the parse result
parseFragment :: Pos -> Text -> Either ParseError [C.Stmt]
parseFragment fragmentStart fragment = do
  let Pos filePath _ _ _ = fragmentStart
  case Syntax.Parser.Lexer.scan filePath fragment of
    Left  err    -> Left (LexicalError err)
    Right tokens -> do
      let tokens' = translateTokStream fragmentStart tokens
      case Parser.parse Parser.statements1 filePath tokens' of
        Left  (errors,logMsg) -> Left (SyntacticError errors logMsg)
        Right val             -> Right val
  where
    translateRange :: Pos -> Pos -> Pos
    translateRange _fragmentStart@(Pos _ lineStart colStart coStart)
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
    translateLoc _ NoLoc = NoLoc

    translateTokStream :: Pos -> Syntax.Parser.Lexer.TokStream -> Syntax.Parser.Lexer.TokStream
    translateTokStream fragmentStart (TsToken (L loc x) rest)
      = TsToken (L (translateLoc fragmentStart loc) x) (translateTokStream fragmentStart rest)
    translateTokStream _ TsEof = TsEof
    translateTokStream _ (TsError e) = TsError e

toAbstractFragment :: [C.Stmt] -> Maybe [A.Stmt]
toAbstractFragment concreteFragment =
  case runExcept $ C.toAbstract concreteFragment of
    Left _                 -> Nothing
    Right abstractFragment -> Just abstractFragment

elaborateFragment :: Elab a => TypeEnv -> a -> Either TypeError (Typed a)
elaborateFragment typeEnv abstractFragment = do
  runElaboration abstractFragment typeEnv

instance Elab [A.Stmt] where
  -- elaborate :: a -> TypeEnv -> ElaboratorM (Maybe Type, Typed a, Subs Type)
  elaborate stmts env = do
    typed <- mapM (\stmt -> do
        (_, typed, _) <- elaborate stmt env
        return typed
        ) stmts
    return (Nothing, typed, mempty)


sweepFragment :: Int -> Spec -> [T.Stmt] -> Either StructError ([PO], [Spec], [StructWarning], Int)
sweepFragment counter (Specification _ pre post _ _) impl =
    bimap id (\(_, counter', (pos, specs, sws, _)) ->
               (pos, specs, sws, counter'))
     $ runWP (structStmts Primary (pre, Nothing) impl post)
             (Map.empty, [])  -- SCM: this can't be right.
             counter