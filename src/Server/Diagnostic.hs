{-# LANGUAGE OverloadedStrings #-}

module Server.Diagnostic where

import           Data.Foldable                  ( toList )
import           Data.Loc hiding (fromLoc)
import           Data.Loc.Util                  ( translate )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Error                          ( Error(..) )
import           GCL.Predicate                  ( Origin(..)
                                                , PO(..)
                                                )
import           GCL.Type                       ( TypeError(..) )
import           GCL.WP                         ( StructError(..)
                                                , StructWarning(..)
                                                )
import           Language.LSP.Types      hiding ( Range(..)
                                                , TextDocumentSyncClientCapabilities(..)
                                                )
import           Pretty
import Data.Loc.Range
import qualified Server.Util as J

class ToDiagnostics a where
  toDiagnostics :: a -> [Diagnostic]

instance ToDiagnostics StructError where
  toDiagnostics (MissingAssertion loc) = makeError
    loc
    "Assertion Missing"
    "Assertion before the DO construct is missing"
  toDiagnostics (MissingPostcondition loc) = makeError
    loc
    "Postcondition Missing"
    "The last statement of the program should be an assertion"

instance ToDiagnostics Error where
  toDiagnostics (SyntacticError (pos, msg)) =
    makeError (Loc pos pos) "Syntax error" (Text.pack msg)
  toDiagnostics (StructError err) = toDiagnostics err
  toDiagnostics (TypeError   err) = toDiagnostics err
  toDiagnostics _                 = []

instance ToDiagnostics TypeError where
  toDiagnostics (NotInScope name loc) =
    makeError loc "Not in scope"
      $  docToText
      $  "The definition "
      <> pretty name
      <> " is not in scope"
  toDiagnostics (UnifyFailed s t loc) =
    makeError loc "Cannot unify types"
      $   docToText
      $   "Cannot unify:"
      <+> pretty s
      <>  line
      <>  "with        :"
      <+> pretty t

  toDiagnostics (RecursiveType var t loc) =
    makeError loc "Recursive type variable"
      $   docToText
      $   "Recursive type variable:"
      <+> pretty var
      <>  line
      <>  "in type             :"
      <+> pretty t

  toDiagnostics (NotFunction t loc) =
    makeError loc "Not a function"
      $   docToText
      $   "The type"
      <+> pretty t
      <+> "is not a function type"

  toDiagnostics (NotEnoughExprsInAssigment names loc) =
    makeError loc "Not Enough Expressions"
      $   docToText
      $   "Variables"
      <+> prettyList (toList names)
      <+> "do not have corresponing expressions in the assigment"

  toDiagnostics (TooManyExprsInAssigment exprs loc) =
    makeError loc "Too Many Expressions"
      $   docToText
      $   "Expressions"
      <+> prettyList (toList exprs)
      <+> "do not have corresponing variables in the assigment"


instance ToDiagnostics StructWarning where
  toDiagnostics (MissingBound range) = makeWarning
    (locOf range)
    "Bound Missing"
    "Bound missing at the end of the assertion before the DO construct \" , bnd : ... }\""
  toDiagnostics (ExcessBound range) = makeWarning
    (locOf range)
    "Excess Bound"
    "Unnecessary bound annotation at this assertion"

instance ToDiagnostics PO where
  toDiagnostics (PO _i _pre _post origin) = makeWarning loc title ""
   where
      -- we only mark the opening tokens ("do" and "if") for loops & conditionals
    first2Char :: Loc -> Loc
    first2Char NoLoc         = NoLoc
    first2Char (Loc start _) = Loc start (translate 2 start)

    loc :: Loc
    loc = case origin of
      -- we only mark the closing tokens ("od" and "fi") for loops & conditionals
      AtLoop        l -> first2Char l
      AtTermination l -> first2Char l
      AtIf          l -> first2Char l
      others          -> locOf others

    title :: Text
    title = toText origin

makeError :: Loc -> Text -> Text -> [Diagnostic]
makeError loc title body = case fromLoc loc of
  Nothing    -> []
  Just range -> [makeDiagnostic (Just DsError) range title body]

makeWarning :: Loc -> Text -> Text -> [Diagnostic]
makeWarning loc title body = case fromLoc loc of
  Nothing    -> []
  Just range -> [makeDiagnostic (Just DsWarning) range title body]

makeDiagnostic
  :: Maybe DiagnosticSeverity -> Range -> Text -> Text -> Diagnostic
makeDiagnostic severity range title body = Diagnostic
  (J.toRange range)
  severity
  Nothing
  Nothing
  title
  Nothing
  (Just $ List [DiagnosticRelatedInformation (J.toLoc range) body])