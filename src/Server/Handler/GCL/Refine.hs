{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Server.Handler.GCL.Refine where

import qualified Data.Aeson as JSON
import GHC.Generics ( Generic )
import Data.Bifunctor ( bimap )
import Control.Monad.Except           ( runExcept, when )
import Server.Monad (ServerM, FileState(..), loadFileState, editTexts, pushSpecs, deleteSpec, Versioned, pushPos, updateIdCounter, logText, saveFileState, pushWarnings)
import Server.Notification.Update (sendUpdateNotification)
import Server.Notification.Error (sendErrorNotification)

import qualified Syntax.Parser                as Parser
import           Syntax.Parser.Error           ( ParseError(..) )
import Syntax.Parser.Lexer (TokStream(..), scan)
import Language.Lexer.Applicative              ( TokenStream(..))

import Error (Error (ParseError, TypeError, StructError, Others))
import GCL.Predicate (Spec(..), PO (..), InfMode(..), Origin (..))
import GCL.Common (TypeEnv, Index, TypeInfo)
import GCL.Type (Elab(..), TypeError, runElaboration, Typed)
import Data.Loc.Range (Range (..), rangeStart, toLoc)
import Data.Text (Text, split, unlines, lines)
import Data.List (find, maximumBy)
import Data.Loc (Pos(..), Loc(..), L(..))
import qualified Data.Map        as Map
import qualified Syntax.Concrete as C
import qualified Syntax.Abstract as A
import qualified Syntax.Typed    as T
import GCL.WP.Types (StructError, StructWarning (..))
import GCL.WP
import qualified Data.Text as Text
import Pretty (pretty)
import Data.Aeson (Value(Bool))

data RefineParams = RefineParams
  { filePath  :: FilePath
  , specText  :: Text
  , specLines :: Range
  , implStart :: Pos
  }
  deriving (Eq, Show, Generic)

instance JSON.FromJSON RefineParams
instance JSON.ToJSON RefineParams


-- Assumes. specLines contains all the lines from "[!" to "!]"
-- Assumes. specText is the text in specLines
-- Assumes. implStart is the start of the line following "[!"
handler :: RefineParams -> (() -> ServerM ()) -> (() -> ServerM ()) -> ServerM ()
handler _params@RefineParams{filePath, specLines, specText, implStart} onFinish _ = do
  logText "refine: start\n"
  logText "  params\n"
  logText . Text.pack . show $ _params
  logText "\n"
  logText "  specLines:\n"
  logText . Text.pack . show  $ specLines
  logText "\n"
  if not (bracketsOccupyOwnLines specText)
    then do
      logText "  specText invalid"
      onError (Others "Refine Error" "The opening \"[!\" and closing \"!]\" brackets must occupy their own lines. Any code on the same lines as the brackets is not allowed." (toLoc specLines))
    else do
      -- 把上次 load 的資料拿出來
      maybeFileState <- loadFileState filePath
      case maybeFileState of
        Nothing -> do
          logText "  no fileState matched\n"
          return ()
        Just fileState -> do
          let implText :: Text = removeFirstAndLastLine specText
          logText "  fileState loaded\n"
          logText "  implText:\n"
          logText implText
          logText "\n"
          -- 挖洞
          -- let (Range implStart _) = implLines
          -- let implStart = rangeStart implLines
          let (Range specStart _) = specLines
          -- let implStart = getImplStart specStart
          case digImplHoles implStart filePath implText of
            Left err -> do
              logText "  parse error\n"
              onError (ParseError err)
            Right holelessImplText -> do
              logText "  holes digged\n"
              logText "    (after)\n"
              logText holelessImplText
              logText "\n"
              -- text to concrete
              -- (use specStart as the starting position in parse/toAbstract/elaborate)
              logText "  parsing\n"
              logText "    implStart =\n"
              logText . Text.pack . show . pretty $ implStart
              logText "\n"
              case parseFragment implStart holelessImplText of
                Left err           -> onError (ParseError err)
                Right concreteImpl -> do
                  -- concrete to abstract
                  logText "  text parsed\n"
                  case toAbstractFragment concreteImpl of
                    Nothing           -> do
                      error "Holes still found after digging all holes. Should not happen\n"
                    Just abstractImpl -> do
                      logText "  abstracted\n"
                      -- get spec (along with its type environment)
                      let FileState{specifications} = fileState
                      logText "  looking for specs\n"
                      case lookupSpecByLines specifications specLines of
                        Nothing   -> do
                          logText "  spec not found at range, should reload\n"
                          onError (Others "Refine Error" "spec not found at range, should reload" NoLoc)
                        Just spec -> do
                          logText "  matching spec found\n"
                          -- elaborate
                          let typeEnv = specTypeEnv spec
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
                                Right (innerPos, innerSpecs, innerWarnings, idCount') -> do
                                  logText "  swept\n"
                                  -- delete outer spec (by id)
                                  deleteSpec filePath spec
                                  logText "  outer spec deleted (refine)\n"
                                  -- add inner specs to fileState
                                  let FileState{editedVersion} = fileState
                                  updateIdCounter filePath idCount'
                                  logText "  counter updated (refine)\n"
                                  let innerSpecs' = predictAndTranslateSpecRanges innerSpecs
                                  let innerPos' = predictAndTranslatePosRanges innerPos
                                  let innerWarnings' = predictAndTranslateWarningsRanges innerWarnings
                                  pushSpecs (editedVersion + 1) filePath innerSpecs'
                                  pushPos (editedVersion + 1) filePath innerPos'
                                  pushWarnings (editedVersion + 1) filePath innerWarnings'
                                  -- let FileState{specifications, proofObligations} = fileState
                                  -- let fileState' = fileState
                                  --                   { specifications = specifications ++ Prelude.map (\spec -> (editedVersion + 1, spec)) innerSpecs'
                                  --                   , proofObligations = proofObligations ++ Prelude.map (\po -> (editedVersion + 1, po)) innerPos'
                                  --                   }

                                  -- saveFileState filePath fileState'
                                  
                                  logText "  new specs and POs added (refine)\n"
                                  -- send notification to update Specs and POs
                                  logText "refine: success\n"
                                  sendUpdateNotification filePath
                                  -- -- clear errors
                                  sendErrorNotification filePath []
                                  logText "refine: update notification sent\n"
                                  -- edit source (dig holes + remove outer brackets)
                                  editTexts filePath [(specLines, holelessImplText)] do
                                    logText "  text edited (refine)\n"
                                    onFinish ()
  logText "refine: end\n"
  where
    onError :: Error -> ServerM ()
    onError err = do
      logText "refine: error\n\t"
      logText $ Text.pack (show $ pretty err)
      logText "\n"
      sendErrorNotification filePath [err]
      logText "refine: update notification sent\n"
    minusOneLine :: Pos -> Pos
    minusOneLine (Pos filePath line column byte) = Pos filePath (line - 1) column 0
    predictAndTranslateSpecRanges :: [Spec] -> [Spec]
    predictAndTranslateSpecRanges = map (\spec@Specification{specRange} -> spec{specRange = minusOneLine' specRange})
      where
        minusOneLine' :: Range -> Range
        minusOneLine' (Range start end) = Range (minusOneLine start) (minusOneLine end)
    predictAndTranslatePosRanges :: [PO] -> [PO]
    predictAndTranslatePosRanges = map (\po@PO{poOrigin} -> po{poOrigin = modifyOriginLocation minusOneLine' poOrigin})
      where
        minusOneLine' :: Loc -> Loc
        minusOneLine' NoLoc = NoLoc
        minusOneLine' (Loc start end) = Loc (minusOneLine start) (minusOneLine end)
        modifyOriginLocation :: (Loc -> Loc) -> Origin -> Origin
        modifyOriginLocation f (AtAbort       l) = AtAbort (f l)
        modifyOriginLocation f (AtSkip        l) = AtSkip (f l)
        modifyOriginLocation f (AtSpec        l) = AtSpec (f l)
        modifyOriginLocation f (AtAssignment  l) = AtAssignment (f l)
        modifyOriginLocation f (AtAssertion   l) = AtAssertion (f l)
        modifyOriginLocation f (AtIf          l) = AtIf (f l)
        modifyOriginLocation f (AtLoop        l) = AtLoop (f l)
        modifyOriginLocation f (AtTermination l) = AtTermination (f l)
        modifyOriginLocation f (Explain h e i p l) = Explain h e i p (f l)
    predictAndTranslateWarningsRanges :: [StructWarning] -> [StructWarning]
    predictAndTranslateWarningsRanges = map (\(MissingBound range) -> MissingBound (minusOneLine' range))
      where
        minusOneLine' :: Range -> Range
        minusOneLine' (Range start end) = Range (minusOneLine start) (minusOneLine end)
bracketsOccupyOwnLines :: Text -> Bool
bracketsOccupyOwnLines specText =
  hasAtLeastTwoLines specText
  && firstLine specText `onlyIncludes` ['!', '[', ' ', '\t', '\n', '\r']
  && lastLine specText `onlyIncludes` ['!', ']', ' ', '\t', '\n', '\r']

hasAtLeastTwoLines :: Text -> Bool
hasAtLeastTwoLines = (>= 2) . length . Text.lines

firstLine :: Text -> Text
firstLine = head . Text.lines

lastLine :: Text -> Text
lastLine = last . Text.lines

onlyIncludes :: Text -> [Char] -> Bool
onlyIncludes text cs = Text.null (Text.filter (`notElem` cs) text)

removeFirstAndLastLine :: Text -> Text
removeFirstAndLastLine = Text.intercalate "\n" . tail . init . Text.lines

lookupSpecByLines :: [Versioned Spec] -> Range -> Maybe Spec
lookupSpecByLines specs targetLines = do
  (_version, spec) <- find (\(_, Specification{specRange}) -> coverSameLines specRange targetLines) specs
  return spec
  where
    coverSameLines :: Range -> Range -> Bool
    coverSameLines (Range (Pos _ lineStart _ _) (Pos _ lineEnd _ _)) (Range (Pos _ lineStart' _ _) (Pos _ lineEnd' _ _))
      = (lineStart == lineStart') && (lineEnd == lineEnd')

collectFragmentHoles :: [C.Stmt] -> [Range]
collectFragmentHoles concreteFragment = do
  statement <- concreteFragment
  case statement of
    C.SpecQM range -> return range
    _ -> []

digImplHoles :: Pos -> FilePath -> Text -> Either ParseError Text
digImplHoles parseStart filePath implText =
  -- use `parseStart` for absolute positions in error messages
  case parseFragment parseStart implText of
    Left err -> Left err
    Right _ ->
      -- use `Pos filePath 1 1 0` for relative position of the hole reported
      case parseFragment (Pos filePath 1 1 0) implText of
        Left _ -> error "should not happen"
        Right concreteImpl ->
          case collectFragmentHoles concreteImpl of
            [] -> return implText
            Range start _ : _ -> digImplHoles parseStart filePath $ digFragementHole start implText
  where
    digFragementHole :: Pos -> Text -> Text
    digFragementHole (Pos _path lineNumber col _charOff) fullText =
      Text.intercalate "\n" linesEdited
      where
        allLines :: [Text]
        allLines = Text.lines fullText -- split fullText by '\n'
        lineToEdit :: Text
        lineToEdit = allLines !! (lineNumber - 1)
        beforeHole = Text.take (col-1) lineToEdit
        afterHole = Text.drop col lineToEdit -- lineToEdit
        indentation n = Text.replicate n " "
        lineEdited :: Text
        lineEdited = beforeHole <> "[!\n" <>
                    indentation (col-1) <> "\n" <>
                    indentation (col-1) <> "!]" <> afterHole
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
      case Parser.parse Parser.statements filePath tokens' of
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
                then colStart + colOffset - 1
                else colOffset
        co = coStart + coOffset

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

elaborateFragment :: Elab a => [(Index, TypeInfo)] -> a -> Either TypeError (Typed a)
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
