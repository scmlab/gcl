{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.DSL where

import           Control.Monad.Cont
import           Control.Monad.Except
import           Control.Monad.RWS       hiding ( state )
import           Control.Monad.Trans.Free
import           Data.IntMap                    ( IntMap )
import           Data.List                      ( find
                                                , sortOn
                                                )
import qualified Data.List                     as List
import           Data.Loc                hiding ( fromLoc )
import           Data.Loc.Range
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Error
import           GCL.Predicate
import           GCL.Predicate.Util             ( specPayloadWithoutIndentation
                                                )
import qualified GCL.Type                      as TypeChecking
import qualified GCL.WP                        as WP
import           GCL.WP.Type                    ( StructWarning )
import qualified Language.LSP.Types            as J
import           Prelude                 hiding ( span )
import           Pretty                         ( Pretty(pretty)
                                                , toText
                                                , vsep
                                                )
import           Render
import           Server.CustomMethod
import           Server.Handler.Diagnostic      ( Collect(collect) )
import           Server.Highlighting            ( collectHighlighting )
import           Server.TokenMap
import qualified Syntax.Abstract               as A
import           Syntax.Concrete                ( ToAbstract(toAbstract) )
import qualified Syntax.Concrete               as C
import           Syntax.Parser                  ( Parser
                                                , pProgram
                                                , pStmts
                                                , runParse
                                                )

--------------------------------------------------------------------------------

data ParseResult = ParseResult
  { parsedProgram      :: C.Program
  , parsedHighlighings :: [J.SemanticTokenAbsolute]
  }
  deriving (Show, Eq)

parse :: Text -> CmdM ParseResult
parse source = do
  program <- parseWithParser pProgram source
  let parsed = ParseResult { parsedProgram      = program
                           , parsedHighlighings = collectHighlighting program
                           }
  save (Parsed parsed)
  return parsed


data ConvertResult = ConvertResult
  { convertedPreviousStage :: ParseResult
  , convertedProgram       :: A.Program
  , convertedTokenMap      :: TokenMap
  }
  deriving (Show, Eq)

convert :: ParseResult -> CmdM ConvertResult
convert result = do
  converted <- case runExcept (toAbstract (parsedProgram result)) of
    Left  (Range start end) -> digHole (Range start end) >>= parse >>= convert
    Right program           -> return $ ConvertResult
      { convertedPreviousStage = result
      , convertedProgram       = program
      , convertedTokenMap      = collectTokenMap program
      }
  save (Converted converted)
  return converted


data SweepResult = SweepResult
  { sweptPreviousStage :: ConvertResult
  , sweptPOs           :: [PO]
    -- Specs (holes)
  , sweptSpecs         :: [Spec]
    -- Global properties
  , sweptProps         :: [A.Expr]
    -- Warnings 
  , sweptWarnings      :: [StructWarning]
    -- Redexes waiting to be reduce by the client on demand
  , sweptRedexes       :: IntMap A.Redex
    -- counter for generating fresh variables
  , sweptCounter       :: Int
  }
  deriving (Show, Eq)

sweep :: ConvertResult -> CmdM SweepResult
sweep convertedResult = do
  let abstract@(A.Program _ _ globalProps _ _) =
        convertedProgram convertedResult
  swept <- case WP.sweep abstract of
    Left  e -> throwError [StructError e]
    Right (pos, specs, warings, redexes, counter) -> return $ SweepResult
      { sweptPreviousStage = convertedResult
      , sweptPOs           = List.sort pos
      , sweptSpecs         = sortOn locOf specs
      , sweptProps         = globalProps
      , sweptWarnings      = warings
      , sweptRedexes       = redexes
      , sweptCounter       = counter
      }

  save (Swept swept)
  return swept

data Stage = Uninitialized FilePath
        | Parsed ParseResult
        | Converted ConvertResult
        | Swept SweepResult
        deriving (Show, Eq)

instance Pretty Stage where
  pretty stage = case stage of
    Uninitialized filepath -> "Uninitialized " <> pretty filepath
    Parsed        _result  -> "Parsed"
    Converted     _result  -> "Converted"
    Swept         result   -> pretty result

instance Pretty SweepResult where
  pretty result =
    "Sweep Result { "
      <> vsep
           [ "POs: " <> pretty (sweptPOs result)
           , "Specs: " <> pretty (sweptSpecs result)
           , "Props: " <> pretty (sweptProps result)
           , "Warnings: " <> pretty (sweptWarnings result)
           ]
      <> " }"

-- The "Syntax" of the DSL for handling LSP requests and responses
data Cmd next
  = EditText
      Range -- ^ Range to replace 
      Text -- ^ Text to replace with 
      (Text -> next) -- ^ Continuation with the text of the whole file after the edit
  | GetSource (Text -> next)
  | Log Text next
  | SendDiagnostics [J.Diagnostic] next
  deriving (Functor)

type CmdM = FreeT Cmd (RWST FilePath () CmdState (Except [Error]))
data CmdState = CmdState
  { cmdErrors    :: [Error]
  , cmdStage     :: Stage
  , cmdMute      :: Bool   -- state for indicating whether we should ignore events like `STextDocumentDidChange` 
  , cmdSelection :: Maybe Range -- text selections (including cursor position)
  , cmdCounter   :: Int -- counter for generating different IDs for Responses
  }
  deriving (Show, Eq)

initState :: FilePath -> CmdState
initState filepath = CmdState [] (Uninitialized filepath) False Nothing 0

runCmdM
  :: FilePath
  -> CmdState
  -> CmdM a
  -> Either [Error] (FreeF Cmd a (CmdM a), CmdState, ())
runCmdM filepath st p = runExcept (runRWST (runFreeT p) filepath st)

editText :: Range -> Text -> CmdM Text
editText range inserted = do

  let Range start end = range
  source <- getSource
  let (_, rest)     = Text.splitAt (posCoff start) source
  let (replaced, _) = Text.splitAt (posCoff end - posCoff start) rest


  case (Text.null replaced, Text.null inserted) of
    -- no-op
    (True, True) -> return ()
    -- deletion 
    (True, False) ->
      logText
        $  "      [ edit ] Delete "
        <> toText range
        <> " \""
        <> replaced
        <> "\""
    -- insertion 
    (False, True) ->
      logText
        $  "      [ edit ] Insert "
        <> toText range
        <> " \""
        <> inserted
        <> "\""
    -- replacement 
    (False, False) ->
      logText
        $  "      [ edit ] Replace "
        <> toText range
        <> " \""
        <> replaced
        <> "\" \n      with \""
        <> inserted
        <> "\""

  liftF (EditText range inserted id)

getFilePath :: CmdM FilePath
getFilePath = ask

getSource :: CmdM Text
getSource = liftF (GetSource id)

load :: CmdM Stage
load = do
  stage <- gets cmdStage
  case stage of
    Uninitialized _ -> logText "      [ load ] from Uninitialized"
    Parsed        _ -> logText "      [ load ] from Parsed"
    Converted     _ -> logText "      [ load ] from Converted"
    Swept         _ -> logText "      [ load ] from Swept"
  return stage

getErrors :: CmdM [Error]
getErrors = gets cmdErrors

isMuted :: CmdM Bool
isMuted = gets cmdMute

mute :: Bool -> CmdM ()
mute m = do
  if m then logText "      [ event ] mute" else logText "      [ event ] unmute"
  modify' $ \state -> state { cmdMute = m }

setErrors :: [Error] -> CmdM ()
setErrors e = modify' $ \state -> state { cmdErrors = e }

-- | Save current progress and remove existing Errors
save :: Stage -> CmdM ()
save stage = do
  case stage of
    Uninitialized _ -> logText "      [ save ] Uninitialized"
    Parsed        _ -> logText "      [ save ] Parsed"
    Converted     _ -> logText "      [ save ] Converted"
    Swept         _ -> logText "      [ save ] Swept"
  modify' $ \state -> state { cmdErrors = [], cmdStage = stage }

setLastSelection :: Range -> CmdM ()
setLastSelection selection = do
  logText $ "      [ save ] Mouse selection: " <> toText (ShortRange selection)
  modify' $ \state -> state { cmdSelection = Just selection }

getLastSelection :: CmdM (Maybe Range)
getLastSelection = do
  sel <- gets cmdSelection
  case sel of
    Nothing -> logText "      [ load ] Mouse selection: Nothing"
    Just r ->
      logText $ "      [ load ] Mouse selection: " <> toText (ShortRange r)

  return sel

logText :: Text -> CmdM ()
logText text = liftF (Log text ())

bumpVersion :: CmdM Int
bumpVersion = do
  i <- gets cmdCounter
  -- logText $ "    - Bump counter " <> toText i <> " => " <> toText (succ i)
  modify' $ \state -> state { cmdCounter = succ i }
  return i

sendDiagnostics :: [J.Diagnostic] -> CmdM ()
sendDiagnostics xs = do
  logText $ "    < Send Diagnostics " <> toText (length xs)
  liftF (SendDiagnostics xs ())

------------------------------------------------------------------------------

-- converts the "?" at a given location to "[!   !]"
-- and returns the modified source and the difference of source length
digHole :: Range -> CmdM Text
digHole range = do
  logText $ "    < DigHole " <> toText range
  let indent   = Text.replicate (posCol (rangeStart range) - 1) " "
  let holeText = "[!\n" <> indent <> "\n" <> indent <> "!]"
  editText range holeText

-- | Try to parse a piece of text in a Spec
refine :: Text -> Range -> CmdM (Spec, [Text])
refine source range = do
  result <- findPointedSpec
  case result of
    Nothing ->
      throwError [Others "Please place the cursor in side a Spec to refine it"]
    Just spec -> do
      source' <- getSource

      let payload = Text.unlines $ specPayloadWithoutIndentation source' spec
      -- HACK, `pStmts` will kaput if we feed empty strings into it
      let payloadIsEmpty = Text.null (Text.strip payload)
      if payloadIsEmpty
        then return ()
        else void $ parseWithParser pStmts payload
      return (spec, specPayloadWithoutIndentation source' spec)
 where
  findPointedSpec :: CmdM (Maybe Spec)
  findPointedSpec = do
    parsed    <- parse source
    converted <- convert parsed
    result    <- sweep converted
    let specs = sweptSpecs result
    return $ find (withinRange range) specs

typeCheck :: A.Program -> CmdM ()
typeCheck p = case TypeChecking.runTM (TypeChecking.checkProgram p) of
  Left  e -> throwError [TypeError e]
  Right v -> return v

--------------------------------------------------------------------------------

parseWithParser :: Parser a -> Text -> CmdM a
parseWithParser p source = do
  filepath <- getFilePath
  case runParse p filepath source of
    Left  errors -> throwError $ map SyntacticError errors
    Right val    -> return val

parseProgram :: Text -> CmdM (C.Program, A.Program)
parseProgram source = do
  concrete <- parseWithParser pProgram source
  case runExcept (toAbstract concrete) of
    Left  (Range start end) -> digHole (Range start end) >>= parseProgram
    Right abstract          -> return (concrete, abstract)



--------------------------------------------------------------------------------

generateResponseAndDiagnostics :: SweepResult -> CmdM [ResKind]
generateResponseAndDiagnostics result = do
  let (SweepResult _ pos specs globalProps warnings _redexes _) = result

  -- get Specs around the mouse selection
  lastSelection <- getLastSelection
  let overlappedSpecs = case lastSelection of
        Nothing        -> specs
        Just selection -> filter (withinRange selection) specs
  -- get POs around the mouse selection (including their corresponding Proofs)

  let withinPOrange sel po = case poAnchorLoc po of
        Nothing     -> withinRange sel po
        Just anchor -> withinRange sel po || withinRange sel anchor

  let overlappedPOs = case lastSelection of
        Nothing        -> pos
        Just selection -> filter (withinPOrange selection) pos
  -- render stuff
  let warningsSections =
        if null warnings then [] else map renderSection warnings
  logText $ toText (length globalProps)
  let globalPropsSections = if null globalProps
        then []
        else map
          (\expr -> Section
            Plain
            [Header "Property" (fromLoc (locOf expr)), Code (render expr)]
          )
          globalProps
  let specsSections =
        if null overlappedSpecs then [] else map renderSection overlappedSpecs
  let poSections =
        if null overlappedPOs then [] else map renderSection overlappedPOs
  let sections = mconcat
        [warningsSections, specsSections, poSections, globalPropsSections]

  version <- bumpVersion
  let encodeSpec spec =
        ( specID spec
        , toText $ render (specPreCond spec)
        , toText $ render (specPostCond spec)
        , specRange spec
        )

  let responses =
        [ResDisplay version sections, ResUpdateSpecs (map encodeSpec specs)]
  let diagnostics = concatMap collect pos ++ concatMap collect warnings
  sendDiagnostics diagnostics

  return responses
