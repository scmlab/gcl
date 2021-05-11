{-# LANGUAGE OverloadedStrings #-}

module Server.Diagnostic where

import Data.Loc
import Data.Text (Text)
import qualified Data.Text as Text
import Error (Error (..))
import GCL.Type (TypeError (..))
import GCL.WP (StructError (..), StructWarning (..))
import Language.LSP.Types hiding (TextDocumentSyncClientCapabilities (..), Range(..))
import qualified Language.LSP.Types as LSP
import Pretty
import Syntax.Predicate (Origin (..), PO (..))
import Data.Loc.Util (translate)
import Data.Loc.Range

class ToDiagnostics a where
  toDiagnostics :: a -> [Diagnostic]

instance ToDiagnostics StructError where
  toDiagnostics (MissingAssertion loc) = [makeError loc "Assertion Missing" "Assertion before the DO construct is missing"]
  toDiagnostics (MissingPostcondition loc) = [makeError loc "Postcondition Missing" "The last statement of the program should be an assertion"]

instance ToDiagnostics Error where
  toDiagnostics (LexicalError pos) = [makeError (Loc pos pos) "Lexical error" ""]
  toDiagnostics (SyntacticError errs) = map (\(loc, msg) -> makeError loc "Syntax error" (Text.pack msg)) errs
  toDiagnostics (StructError err) = toDiagnostics err
  toDiagnostics (TypeError err) = toDiagnostics err
  toDiagnostics _ = []

instance ToDiagnostics TypeError where
  toDiagnostics (NotInScope name loc) = [makeError loc "Not in scope" $ renderStrict $ "The definition " <> pretty name <> " is not in scope"]
  toDiagnostics (UnifyFailed s t loc) =
    [ makeError loc "Cannot unify types" $
        renderStrict $
          "Cannot unify:" <+> pretty s <> line
            <> "with        :" <+> pretty t
    ]
  toDiagnostics (RecursiveType var t loc) =
    [ makeError loc "Recursive type variable" $
        renderStrict $
          "Recursive type variable:" <+> pretty var <> line
            <> "in type             :" <+> pretty t
    ]
  toDiagnostics (NotFunction t loc) =
    [ makeError loc "Not a function" $
        renderStrict $
          "The type" <+> pretty t <+> "is not a function type"
    ]

instance ToDiagnostics StructWarning where
  toDiagnostics (MissingBound range) = [makeWarning (locOf range) "Bound Missing" "Bound missing at the end of the assertion before the DO construct \" , bnd : ... }\""]
  toDiagnostics (ExcessBound range) = [makeWarning (locOf range) "Excess Bound" "Unnecessary bound annotation at this assertion"]

instance ToDiagnostics PO where
  toDiagnostics (PO _i _pre _post origin) = [makeWarning loc title ""]
    where
      -- we only mark the opening tokens ("do" and "if") for loops & conditionals
      first2Char :: Loc -> Loc
      first2Char NoLoc = NoLoc
      first2Char (Loc start _) = Loc start (translate 1 start)

      loc :: Loc
      loc = case origin of
        -- we only mark the closing tokens ("od" and "fi") for loops & conditionals
        AtLoop l -> first2Char l
        AtTermination l -> first2Char l
        AtIf l -> first2Char l
        others -> locOf others

      title :: Text.Text
      title = case origin of
        AtAbort {} -> "Abort"
        AtSpec {} -> "Spec"
        AtAssignment {} -> "Assignment"
        AtAssertion {} -> "Assertion"
        AtIf {} -> "Conditional"
        AtLoop {} -> "Loop Invariant"
        AtTermination {} -> "Loop Termination"
        AtSkip {} -> "Skip"

makeError :: Loc -> Text -> Text -> Diagnostic
makeError = makeDiagnostic (Just DsError)

makeWarning :: Loc -> Text -> Text -> Diagnostic
makeWarning = makeDiagnostic (Just DsWarning)

makeDiagnostic :: Maybe DiagnosticSeverity -> Loc -> Text -> Text -> Diagnostic
makeDiagnostic severity loc title body =
  Diagnostic
    (locToRange loc)
    severity
    Nothing
    Nothing
    title
    Nothing
    (Just $ List [DiagnosticRelatedInformation (locToLocation loc) body])

locToRange :: Loc -> LSP.Range
locToRange NoLoc = LSP.Range (Position 0 0) (Position 0 0)
locToRange (Loc start end) = LSP.Range (posToPosition start) (posToPosition (translate 1 end))



rangeToRange :: Range -> LSP.Range
rangeToRange (Range start end) = LSP.Range (posToPosition start) (posToPosition (translate 1 end))

locToLocation :: Loc -> Location
locToLocation NoLoc = Location (Uri "") (locToRange NoLoc)
locToLocation (Loc start end) = Location (Uri $ Text.pack $ posFile start) (locToRange (Loc start end))

-- rangeToLSPRange :: Range -> LSP.Range
-- rangeToLSPRange (Range start end) = LSP.Range (posToPosition start) (posToPosition (translate 1 end))
--   where
--     translate :: Int -> Pos -> Pos
--     translate n (Pos path ln col offset) = Pos path ln ((col + n) `max` 0) ((offset + n) `max` 0)

posToPosition :: Pos -> Position
posToPosition (Pos _path ln col _offset) = Position ((ln - 1) `max` 0) ((col - 1) `max` 0)

-- rangeToLocation :: Range -> Location
-- rangeToLocation (Range start end) =
--   Location (Uri $ Text.pack $ posFile start) (rangeToLSPRange (Range start end))
