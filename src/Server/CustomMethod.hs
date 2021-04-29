{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}

module Server.CustomMethod where

import Control.Monad.Except hiding (guard)
import Control.Monad.Free
import Control.Monad.Writer
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as JSON
import Data.Foldable (find)
import Data.List (sort)
import Data.Loc (Loc (..), Located (locOf), posCoff)
import Data.Text (Text)
import qualified Data.Text as Text
import Error
import qualified GCL.Type as TypeChecking
import GCL.WP (StructWarning)
import qualified GCL.WP as POGen
import GHC.Generics (Generic)
import Language.LSP.Diagnostics (partitionBySource)
import Language.LSP.Server
import Language.LSP.Types hiding (TextDocumentSyncClientCapabilities (..))
import Server.ExportPO ()
import Server.Monad
import qualified Syntax.Abstract as A
import Syntax.Concrete (ToAbstract (toAbstract))
import Syntax.Parser
import Syntax.Predicate
  ( Origin (..),
    PO (..),
    Spec (..),
    specPayload,
  )

--------------------------------------------------------------------------------

-- | Request
data ReqKind
  = ReqInspect Int Int
  | ReqRefine Int Int
  | ReqSubstitute Int A.Expr A.Subst
  | ReqExportProofObligations
  | ReqDebug
  deriving (Generic)

instance FromJSON ReqKind

instance Show ReqKind where
  show (ReqInspect x y) = "Inspect " <> show x <> " " <> show y
  show (ReqRefine i x) = "Refine #" <> show i <> " " <> show x
  show (ReqSubstitute i x y) = "Substitute #" <> show i <> " " <> show x <> " => " <> show y
  show ReqExportProofObligations = "ExportProofObligations"
  show ReqDebug = "Debug"

data Request = Req FilePath ReqKind
  deriving (Generic)

instance FromJSON Request

instance Show Request where
  show (Req _path kind) = show kind

--------------------------------------------------------------------------------

-- | Given an interval of mouse selection, calculate POs within the interval, ordered by their vicinity
filterPOs :: (Int, Int) -> [PO] -> [PO]
filterPOs (selStart, selEnd) pos = opverlappedPOs
  where
    opverlappedPOs = reverse $ case overlapped of
      [] -> []
      (x : _) -> case locOf x of
        NoLoc -> []
        Loc start _ ->
          let same y = case locOf y of
                NoLoc -> False
                Loc start' _ -> start == start'
           in filter same overlapped
      where
        -- find the POs whose Range overlaps with the selection
        isOverlapped po = case locOf po of
          NoLoc -> False
          Loc start' end' ->
            let start = posCoff start'
                end = posCoff end' + 1
             in (selStart <= start && selEnd >= start) -- the end of the selection overlaps with the start of PO
                  || (selStart <= end && selEnd >= end) -- the start of the selection overlaps with the end of PO
                  || (selStart <= start && selEnd >= end) -- the selection covers the PO
                  || (selStart >= start && selEnd <= end) -- the selection is within the PO
                  -- sort them by comparing their starting position
        overlapped = reverse $ sort $ filter isOverlapped pos

type ID = LspId ('CustomMethod :: Method 'FromClient 'Request)

--------------------------------------------------------------------------------

data Eff next
  = ReportDiagnostic [Diagnostic] next
  | EditTextDocument Range Text (Free Eff next) next
  | SendResponse [ResKind]
  deriving (Functor)

type EffM = Free Eff

reponrtDiagnostic :: [Diagnostic] -> EffM ()
reponrtDiagnostic xs = liftF (ReportDiagnostic xs ())

editTextDocument :: Range -> Text -> EffM () -> EffM ()
editTextDocument range text again = liftF (EditTextDocument range text again ())

sendResponse :: [ResKind] -> EffM ()
sendResponse responses = liftF (SendResponse responses)

type Responder = Response -> ServerM ()

runEffM :: FilePath -> Maybe Responder -> EffM () -> ServerM ()
runEffM _ _ (Pure ()) = logText "Improper termination"
runEffM filepath responder (Free (ReportDiagnostic diagnostics next)) = do
  version <- bumpCounter
  publishDiagnostics 100 (toNormalizedUri (filePathToUri filepath)) (Just version) (partitionBySource diagnostics)
  runEffM filepath responder next
runEffM filepath responder (Free (EditTextDocument range text again next)) = do
  -- apply edit 
  let removeSpec = TextEdit range text
  let identifier = VersionedTextDocumentIdentifier (filePathToUri filepath) (Just 0)
  let textDocumentEdit = TextDocumentEdit identifier (List [InL removeSpec])
  let change = InL textDocumentEdit
  let workspaceEdit = WorkspaceEdit Nothing (Just (List [change])) Nothing
  let applyWorkspaceEditParams = ApplyWorkspaceEditParams (Just "Resolve Spec") workspaceEdit
  -- run "again" before continue on
  let callback _ = runEffM filepath responder $ again >> next
  void $ sendRequest SWorkspaceApplyEdit applyWorkspaceEditParams callback
runEffM filepath Nothing (Free (SendResponse responses)) =
  sendNotification (SCustomMethod "guacamole") $ JSON.toJSON $ Res filepath responses
runEffM filepath (Just responder) (Free (SendResponse responses)) =
  responder $ Res filepath responses

-- | TODO: refactor this
data Error2
  = ReportError Error
  | DigHole Loc
  | RefineSpec Spec Text
  deriving (Show, Eq)

type M = Except Error2

runM :: M a -> Either Error2 a
runM = runExcept

--------------------------------------------------------------------------------

-- | Parse with a parser
parse :: Parser a -> FilePath -> Text -> M a
parse p filepath = withExcept (ReportError . SyntacticError) . liftEither . runParse p filepath

-- | Parse the whole program
parseProgram :: FilePath -> Text -> M A.Program
parseProgram filepath source = do
  concrete <- parse pProgram filepath source
  case runExcept (toAbstract concrete) of
    Left loc -> throwError $ DigHole loc
    Right program -> return program

-- | Try to parse a piece of text in a Spec
refine :: FilePath -> Text -> (Int, Int) -> M ()
refine filepath source selection = do
  result <- findPointedSpec
  case result of
    Nothing ->
      throwError $ ReportError $ Others "Cannot find pointed spec"
    Just spec -> do
      --
      let payload = Text.unlines $ specPayload source spec
      void $ parse pStmts "<specification>" payload
      throwError $ RefineSpec spec payload
  where
    findPointedSpec :: M (Maybe Spec)
    findPointedSpec = do
      (_, specs, _) <- parseProgram filepath source >>= genPO
      return $ find (pointed selection) specs
      where
        pointed :: (Int, Int) -> Spec -> Bool
        pointed (start, end) spec = case specLoc spec of
          NoLoc -> False
          Loc open close ->
            (posCoff open <= start && start <= posCoff close)
              || (posCoff open <= end && end <= posCoff close)

-- | Type check + generate POs and Specs
checkEverything :: FilePath -> Text -> Maybe (Int, Int) -> M ([PO], [Spec], [A.Expr], [StructWarning])
checkEverything filepath source mouseSelection = do
  program@(A.Program _ globalProps _ _ _) <- parseProgram filepath source
  typeCheck program
  (pos, specs, warings) <- genPO program
  case mouseSelection of
    Nothing -> return (pos, specs, globalProps, warings)
    Just sel -> return (filterPOs sel pos, specs, globalProps, warings)

-- | Only generate POs and Specs
genPOsandSpecsOnly :: FilePath -> Text -> Maybe (Int, Int) -> M ([PO], [Spec], [A.Expr], [StructWarning])
genPOsandSpecsOnly filepath source mouseSelection = do
  program@(A.Program _ globalProps _ _ _) <- parseProgram filepath source
  (pos, specs, warings) <- genPO program
  case mouseSelection of
    Nothing -> return (pos, specs, globalProps, warings)
    Just sel -> return (filterPOs sel pos, specs, globalProps, warings)

typeCheck :: A.Program -> M ()
typeCheck = withExcept (ReportError . TypeError) . TypeChecking.checkProg

genPO :: A.Program -> M ([PO], [Spec], [StructWarning])
genPO = withExcept (ReportError . StructError) . liftEither . POGen.sweep

--------------------------------------------------------------------------------

-- | Response
data ResKind
  = ResOK ID [PO] [Spec] [A.Expr] [StructWarning]
  | ResInspect [PO]
  | ResError [(Site, Error)]
  | ResUpdateSpecPositions [Loc]
  | ResResolve Int -- resolves some Spec
  | ResSubstitute Int A.Expr
  | ResConsoleLog Text
  deriving (Generic)

instance ToJSON ResKind

instance Show ResKind where
  show (ResOK i pos specs props warnings) =
    "OK " <> show i <> " "
      <> show (length pos)
      <> " pos, "
      <> show (length specs)
      <> " specs, "
      <> show (length props)
      <> " props, "
      <> show (length warnings)
      <> " warnings"
  show (ResInspect pos) = "Inspect " <> show (length pos) <> " POs"
  show (ResError errors) = "Error " <> show (length errors) <> " errors"
  show (ResUpdateSpecPositions locs) = "UpdateSpecPositions " <> show (length locs) <> " locs"
  show (ResResolve i) = "Resolve " <> show i
  show (ResSubstitute i _) = "Substitute " <> show i
  show (ResConsoleLog x) = "ConsoleLog " <> show x

data Response
  = Res FilePath [ResKind]
  | CannotDecodeRequest String
  | NotLoaded
  deriving (Generic)

instance ToJSON Response

instance Show Response where
  show (Res _path kinds) = show kinds
  show (CannotDecodeRequest s) = "CannotDecodeRequest " <> s
  show NotLoaded = "NotLoaded"

--------------------------------------------------------------------------------

-- | Instances of ToJSON
instance ToJSON Origin

instance ToJSON PO

instance ToJSON Spec
