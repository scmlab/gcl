{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Pipeline where

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
import           Data.Maybe                     ( mapMaybe )
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
import           Server.GoToDefn                ( collectLocationLinks )
import           Server.Handler.Diagnostic      ( Collect(collect) )
import           Server.Highlighting            ( collectHighlighting )
import           Server.Hover                   ( collectHoverInfo )
import           Server.IntervalMap                ( IntervalMap )
import qualified Syntax.Abstract               as A
import           Syntax.Abstract                ( Type )
import           Syntax.Concrete                ( ToAbstract(toAbstract) )
import qualified Syntax.Concrete               as C
import           Syntax.Parser                  ( Parser
                                                , pProgram
                                                , pStmts
                                                , runParse
                                                )
import           Data.SBV                       ( ThmResult
                                                , Symbolic
                                                , SBool
                                                )
import           SMT.Prove                      ( makeProvable )

--------------------------------------------------------------------------------
-- | Stages of the processing pipeline
--
--                      ┌───────────────────┐
--                      │        Raw        │ : Text
--                      └───────────────────┘
--                                │
--          parse error  ◀────────┤ parse
--                                │
--                                ▼
--                      ┌───────────────────┐
--           ┌─────────▶│      Parsed       │ : Concrete Syntax
--           │          └───────────────────┘   Highlighting info
--                                │
--  request to dig hole  ◀────────┤ toAbstract
--                                │
--                                ▼
--                      ┌───────────────────┐
--                      │     Converted     │ : Abstract Syntax
--                      └───────────────────┘   Scoping info
--                                │
--           type error  ◀────────┤ typeCheck
--                                │
--                                ▼
--                      ┌───────────────────┐
--                      │    TypeChecked    │ : Abstract Syntax
--                      └───────────────────┘   Typing info
--                                │
--          sweep error  ◀────────┤ "sweep"
--                                │
--                                ▼
--                      ┌───────────────────┐
--                      │       Swept       │ : POs & whatnots
--                      └───────────────────┘
--
data Stage = Raw FilePath
           | Parsed ParseResult
           | Converted ConvertResult
           | TypeChecked TypeCheckResult
           | Swept SweepResult
           deriving (Show, Eq)

instance Pretty Stage where
  pretty stage = case stage of
    Raw         filepath -> "Raw " <> pretty filepath
    Parsed      _result  -> "Parsed"
    Converted   _result  -> "Converted"
    TypeChecked _result  -> "TypeChecked"
    Swept       result   -> pretty result

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


--------------------------------------------------------------------------------

data ParseResult = ParseResult
  { parsedProgram      :: C.Program -- Concrete syntax of parsed program
  , parsedHighlighings :: [J.SemanticTokenAbsolute] -- Highlighting infos
  }
  deriving (Show, Eq)

-- | Converts raw Text to concrete syntax and Highlighting info
--   persists the result
parse :: Text -> PipelineM ParseResult
parse source = do
  program <- parseWithParser pProgram source
  let parsed = ParseResult { parsedProgram      = program
                           , parsedHighlighings = collectHighlighting program
                           }
  save (Parsed parsed)
  return parsed

--------------------------------------------------------------------------------

data ConvertResult = ConvertResult
  { convertedPreviousStage :: ParseResult
  , convertedProgram       :: A.Program -- abstract syntax
  , convertedIntervalMap      :: IntervalMap J.LocationLink -- scoping info
  }
  deriving (Show, Eq)

-- | Converts concrete syntax to abstract syntax and scoping info
--   persists the result
convert :: ParseResult -> PipelineM ConvertResult
convert result = do
  converted <- case runExcept (toAbstract (parsedProgram result)) of
    Left (Range start end) -> -- should dig hole!
      digHole (Range start end) >>= parse >>= convert
    Right program -> return $ ConvertResult
      { convertedPreviousStage = result
      , convertedProgram       = program
      , convertedIntervalMap      = collectLocationLinks program
      }
  save (Converted converted) -- save the current progress
  return converted

--------------------------------------------------------------------------------

data TypeCheckResult = TypeCheckResult
  { typeCheckedPreviousStage :: ConvertResult
  , typeCheckedIntervalMap      :: IntervalMap (J.Hover, Type) -- type checking info
  }
  deriving (Show, Eq)

-- | Converts concrete syntax to abstract syntax and scoping info
--   persists the result
typeCheck :: ConvertResult -> PipelineM TypeCheckResult
typeCheck result = do
  let program = convertedProgram result

  (_, scopeTree) <- case TypeChecking.runTypeCheck program of
    Left  e -> throwError [TypeError e]
    Right v -> return v

  let typeChecked = TypeCheckResult
        { typeCheckedPreviousStage = result
        , typeCheckedIntervalMap   = collectHoverInfo program scopeTree
        }
  save (TypeChecked typeChecked) -- save the current progress
  return typeChecked

--------------------------------------------------------------------------------

data SweepResult = SweepResult
  { sweptPreviousStage :: TypeCheckResult
  , sweptPOs           :: [PO]
    -- Specs (holes)
  , sweptSpecs         :: [Spec]
    -- Global properties
  , sweptProps         :: [A.Expr]
    -- Warnings
  , sweptWarnings      :: [StructWarning]
    -- Redexes waiting to be reduce by the client on demand
  , sweptRedexes       :: IntMap (Int, A.Expr)
    -- counter for generating fresh variables
  , sweptCounter       :: Int
  }
  deriving (Show, Eq)

-- | Converts abstract syntax to POs and other stuff
--   persists the result
sweep :: TypeCheckResult -> PipelineM SweepResult
sweep result = do
  let abstract@(A.Program _ _ globalProps _ _) =
        convertedProgram (typeCheckedPreviousStage result)
  swept <- case WP.sweep abstract of
    Left  e -> throwError [StructError e]
    Right (pos, specs, warings, redexes, counter) -> return $ SweepResult
      { sweptPreviousStage = result
      , sweptPOs           = List.sort pos
      , sweptSpecs         = sortOn locOf specs
      , sweptProps         = globalProps
      , sweptWarnings      = warings
      , sweptRedexes       = redexes
      , sweptCounter       = counter
      }
  save (Swept swept) -- save the current progress
  return swept

--------------------------------------------------------------------------------
-- | Monad for handling the pipeline
--
--  Side effects:
--    Reader: FilePath
--    State: PipelineState
--    Exception: [Error]
--
--  Also, PipelineM is also a free monad of Instruction
--  which allows us to "compile" a PipelineM program into a series of Instructions
--  We can then decide to run these compiled Instructions in the real world
--                     or simulate them for testing

type PipelineM
  = FreeT Instruction (RWST FilePath () PipelineState (Except [Error]))

runPipelineM
  :: FilePath
  -> PipelineState
  -> PipelineM a
  -> Either
       [Error]
       (FreeF Instruction a (PipelineM a), PipelineState, ())
runPipelineM filepath st p = runExcept (runRWST (runFreeT p) filepath st)

--------------------------------------------------------------------------------
-- | The State

data PipelineState = PipelineState
  { pipelineErrors         :: [Error]
  , pipelineStage          :: Stage
  , pipelineMute           :: Bool   -- state for indicating whether we should ignore events like `STextDocumentDidChange`
  , pipelineMouseSelection :: Maybe Range -- text selections (including cursor position)
  , pipelineCounter        :: Int -- counter for generating different IDs for Responses
  }
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- The "assembly code" for making LSP responses

data Instruction next
  = EditText
      Range -- ^ Range to replace
      Text -- ^ Text to replace with
      (Text -> next) -- ^ Continuation with the text of the whole file after the edit
  | GetSource (Text -> next) -- ^ Read the content of a file from the LSP filesystem
  | Log Text next -- ^ Make some noise
  | SendDiagnostics [J.Diagnostic] next -- ^ Send Diagnostics
  | Solve (Maybe (Symbolic SBool)) (ThmResult -> next) -- ^ Send the provable function
  deriving (Functor)

initState :: FilePath -> PipelineState
initState filepath = PipelineState [] (Raw filepath) False Nothing 0

--------------------------------------------------------------------------------
-- PipelineM functions

editText :: Range -> Text -> PipelineM Text
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

getFilePath :: PipelineM FilePath
getFilePath = ask

getSource :: PipelineM Text
getSource = liftF (GetSource id)

load :: PipelineM Stage
load = do
  stage <- gets pipelineStage
  case stage of
    Raw         _ -> logText "      [ load ] from Raw"
    Parsed      _ -> logText "      [ load ] from Parsed"
    Converted   _ -> logText "      [ load ] from Converted"
    TypeChecked _ -> logText "      [ load ] from TypeChecked"
    Swept       _ -> logText "      [ load ] from Swept"
  return stage

getErrors :: PipelineM [Error]
getErrors = gets pipelineErrors

isMuted :: PipelineM Bool
isMuted = gets pipelineMute

mute :: Bool -> PipelineM ()
mute m = do
  if m then logText "      [ event ] mute" else logText "      [ event ] unmute"
  modify' $ \state -> state { pipelineMute = m }

setErrors :: [Error] -> PipelineM ()
setErrors e = modify' $ \state -> state { pipelineErrors = e }

-- | Save current progress and remove existing Errors
save :: Stage -> PipelineM ()
save stage = do
  case stage of
    Raw         _ -> logText "      [ save ] Raw"
    Parsed      _ -> logText "      [ save ] Parsed"
    Converted   _ -> logText "      [ save ] Converted"
    TypeChecked _ -> logText "      [ save ] TypeChecked"
    Swept       _ -> logText "      [ save ] Swept"
  modify' $ \state -> state { pipelineErrors = [], pipelineStage = stage }

setLastSelection :: Range -> PipelineM ()
setLastSelection selection = do
  logText $ "      [ save ] Mouse selection: " <> toText (ShortRange selection)
  modify' $ \state -> state { pipelineMouseSelection = Just selection }

getLastSelection :: PipelineM (Maybe Range)
getLastSelection = do
  sel <- gets pipelineMouseSelection
  case sel of
    Nothing -> logText "      [ load ] Mouse selection: Nothing"
    Just r ->
      logText $ "      [ load ] Mouse selection: " <> toText (ShortRange r)

  return sel

logText :: Text -> PipelineM ()
logText text = liftF (Log text ())

solve :: Text -> PipelineM ThmResult
--solve hash = liftF (Solve hash (const ()))
solve hash = do
  pps <- gets pipelineStage
  case pps of 
    Swept result -> 
      let pos = sweptPOs result
          maybePo = findPO pos
          props = sweptProps result
      in case maybePo of
        Nothing -> liftF (Solve Nothing id)
        Just po -> liftF (Solve (Just (makeProvable po props)) id)
            
    _            -> throwError [Others "An unconsidered case in Server.Pipeline.solve"]

  where findPO :: [PO] -> Maybe PO
        findPO pos = find ((hash ==) . poAnchorHash) pos

bumpVersion :: PipelineM Int
bumpVersion = do
  i <- gets pipelineCounter
  -- logText $ "    - Bump counter " <> toText i <> " => " <> toText (succ i)
  modify' $ \state -> state { pipelineCounter = succ i }
  return i

sendDiagnostics :: [J.Diagnostic] -> PipelineM ()
sendDiagnostics xs = do
  logText $ "    < Send Diagnostics " <> toText (length xs)
  liftF (SendDiagnostics xs ())

------------------------------------------------------------------------------

-- converts the "?" at a given location to "[!   !]"
-- and returns the modified source and the difference of source length
digHole :: Range -> PipelineM Text
digHole range = do
  logText $ "    < DigHole " <> toText range
  let indent   = Text.replicate (posCol (rangeStart range) - 1) " "
  let holeText = "[!\n" <> indent <> "\n" <> indent <> "!]"
  editText range holeText

-- | Try to parse a piece of text in a Spec
refine :: Text -> Range -> PipelineM (Spec, [Text])
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
  findPointedSpec :: PipelineM (Maybe Spec)
  findPointedSpec = do
    parsed      <- parse source
    converted   <- convert parsed
    typeChecked <- typeCheck converted
    swept       <- sweep typeChecked
    let specs = sweptSpecs swept
    return $ find (withinRange range) specs

--------------------------------------------------------------------------------

parseWithParser :: Parser a -> Text -> PipelineM a
parseWithParser p source = do
  filepath <- getFilePath
  case runParse p filepath source of
    Left  errors -> throwError $ map SyntacticError errors
    Right val    -> return val

parseProgram :: Text -> PipelineM (C.Program, A.Program)
parseProgram source = do
  concrete <- parseWithParser pProgram source
  case runExcept (toAbstract concrete) of
    Left  (Range start end) -> digHole (Range start end) >>= parseProgram
    Right abstract          -> return (concrete, abstract)

--------------------------------------------------------------------------------

generateResponseAndDiagnostics :: SweepResult -> PipelineM [ResKind]
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

  let rangesOfPOs = mapMaybe (fromLoc . locOf) pos
  let responses =
        [ ResDisplay version sections
        , ResUpdateSpecs (map encodeSpec specs)
        , ResMarkPOs rangesOfPOs
        ]
  let diagnostics = concatMap collect warnings
  sendDiagnostics diagnostics

  return responses


-----------
generateSolveAndDiagnostics :: SweepResult -> Text -> String -> PipelineM [ResKind]
generateSolveAndDiagnostics result hash solveResult = do
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
        let ori_sections = if null overlappedPOs then [] else map renderSection overlappedPOs
            getHash (Section _ (HeaderWithButtons _ _ h _:_)) = h
            getHash _ = error "Entered a strange case in Server.Pipeline.generateSolveAndDiagnostics"
            replaceTheSolved sec@(Section deco blocks)
              | ((hash==) $ getHash sec) = Section deco $ init blocks <> [Paragraph (render solveResult)] <> [last blocks]
              | otherwise = sec
        in map replaceTheSolved ori_sections
  let sections = mconcat
        [warningsSections, specsSections, poSections, globalPropsSections]

  version <- bumpVersion
  let encodeSpec spec =
        ( specID spec
        , toText $ render (specPreCond spec)
        , toText $ render (specPostCond spec)
        , specRange spec
        )

  let rangesOfPOs = mapMaybe (fromLoc . locOf) pos
  let responses =
        [ ResDisplay version sections
        , ResUpdateSpecs (map encodeSpec specs)
        , ResMarkPOs rangesOfPOs
        ]
  let diagnostics = concatMap collect warnings
  sendDiagnostics diagnostics

  return responses