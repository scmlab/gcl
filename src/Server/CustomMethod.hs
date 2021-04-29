{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}

module Server.CustomMethod where

import Control.Monad.Except hiding (guard)
import Data.Aeson (FromJSON, ToJSON)
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
import Server.ExportPO ()
import Language.LSP.Types hiding (TextDocumentSyncClientCapabilities (..))
import qualified Syntax.Abstract as A
import Syntax.Concrete (ToAbstract (toAbstract))
import Syntax.Parser
import Syntax.Predicate
  ( Origin (..),
    PO (..),
    Spec (..),
    specPayload,
  )
import Control.Monad.Writer
import Server.Monad

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

-- | TODO: refactor this
data Error2
  = ReportError Error
  | DigHole Loc
  deriving (Show, Eq)

type M = Except Error2

runM :: M a -> Either Error2 a
runM = runExcept


type Eff a = ExceptT Loc (WriterT [Diagnostic] (WriterT [ResKind] ServerM)) a

runEff :: Eff a -> ServerM ((Either Loc a, [Diagnostic]), [ResKind])
runEff p = runWriterT (runWriterT (runExceptT p))

-- ignoreError :: M [ResKind] -> [ResKind]
-- ignoreError program =
--   case runM program of
--     Left _err -> []
--     Right val -> val

-- catches Error and convert it into a global ResError
-- asGlobalError :: M [ResKind] -> CustomError
-- asGlobalError program =
--   case runM program of
--     Left err -> ReportError $  [ResError [globalError err]]
--     Right val -> val

-- catches Error and convert it into a local ResError with Hole id
-- asLocalError :: Int -> M [ResKind] -> [ResKind]
-- asLocalError i program =
--   case runM program of
--     Left err -> [ResError [localError i err]]
--     Right val -> val

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
refine :: FilePath -> Text -> (Int, Int) -> M (Spec, Text)
refine filepath source selection = do
  result <- findPointedSpec
  case result of
    Nothing ->
      throwError $ ReportError $ Others "Cannot find pointed spec"
    Just spec -> do
      --
      let payload = Text.unlines $ specPayload source spec
      void $ parse pStmts "<specification>" payload
      return (spec, payload)
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
