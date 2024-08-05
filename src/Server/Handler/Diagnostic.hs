{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Server.Handler.Diagnostic where

import           Data.Loc                hiding ( fromLoc )
import           Data.Loc.Range
import           Data.Loc.Util                  ( translate )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Error                          ( Error(..) )
import           GCL.Predicate                  ( Origin(..)
                                                , PO(..)
                                                )
import           GCL.Common                     ( TypeError(..) )
import           GCL.WP.Type                    ( StructError(..)
                                                , StructWarning(..)
                                                )
import           Language.LSP.Types      hiding ( ParseError
                                                , Range
                                                , TextDocumentSyncClientCapabilities(..)
                                                , line
                                                )
import           Pretty
import qualified Server.SrcLoc                 as SrcLoc
import           Syntax.Parser.Error          as Parser
                                                ( ParseError(..) )

instance Collect StructError Diagnostic where
  collect (MissingAssertion loc) = makeError
    loc
    "Assertion Missing"
    "Assertion before the DO construct is missing"
  collect (MissingPostcondition loc) = makeError
    loc
    "Postcondition Missing"
    "The last statement of the program should be an assertion"
  collect (MultiDimArrayAsgnNotImp loc) =
    makeError loc "Assignment to Multi-Dimensional Array" "Not implemented yet"
  collect (LocalVarExceedScope loc) =
    makeError loc "Local variable(s) exceeded scope"
      "Variables defined in a block must not remain in preconditions out of the block"

instance Collect Error Diagnostic where
  collect (ParseError  err) = collect err
  collect (StructError err) = collect err
  collect (TypeError   err) = collect err
  collect _                 = []

instance Collect Parser.ParseError Diagnostic where
  collect (LexicalError pos) = makeError (Loc pos pos) "Lexical error" mempty
  collect (SyntacticError pairs _) = concatMap -- the second argument is parsing log, used for debugging
    (\(loc, msg) -> makeError loc "Parse error" (Text.pack msg))
    pairs

instance Collect TypeError Diagnostic where
  collect (NotInScope name) =
    makeError (locOf name) "Not in scope"
      $  docToText
      $  "The definition "
      <> pretty name
      <> " is not in scope"

  collect (UnifyFailed s t loc) =
    makeError loc "Cannot unify types"
      $   docToText
      $   "Cannot unify:"
      <+> pretty s
      <>  line
      <>  "with        :"
      <+> pretty t

  collect (KindUnifyFailed s t loc) =
    makeError loc "Cannot unify kinds"
      $   docToText
      $   "Cannot unify:"
      <+> pretty s
      <>  line
      <>  "with        :"
      <+> pretty t

  collect (NotKFunc k loc) =
    makeError loc "Not a kind-level function"
      $   docToText
      $   "Not a kind-level function: "
      <>  pretty k
      <+> " is not a kind-level function"

  collect (RecursiveType var t loc) =
    makeError loc "Recursive type variable"
      $   docToText
      $   "Recursive type variable:"
      <+> pretty var
      <>  line
      <>  "in type             :"
      <+> pretty t

  collect (AssignToConst n) =
    makeError (locOf n) "Assigned To Const"
      $   docToText
      $   "Assigned to constant:"
      <+> pretty n

  collect (UndefinedType n) =
    makeError (locOf n) "Undefined Type"
      $   docToText
      $   "Type"
      <+> pretty n
      <+> "is undefined"

  collect (DuplicatedIdentifiers ns) =
    makeError (locOf ns) "Duplicated Identifiers"
      $   docToText
      $   "Duplicated identifiers"
      <+> pretty ns

  collect (RedundantNames ns) =
    makeError (locOf ns) "Redundant Names"
      $   docToText
      $   "Redundant names"
      <+> pretty ns

  collect (RedundantExprs exprs) =
    makeError (locOf exprs) "Redundant Exprs"
      $   docToText
      $   "Redundant exprs"
      <+> pretty exprs

  collect (MissingArguments ns) =
    makeError (locOf ns) "Missing Arguments"
      $   docToText
      $   "Missing arguments"
      <+> pretty ns
  
  collect (TooFewPatterns tys) =
    makeError (locOf tys) "Redundent Arguments"
      $   docToText
      $   "Redundent arguments"
      <+> pretty tys

  collect (TooManyPatterns pats) =
    makeError (locOf pats) "Redundent Patterns"
      $   docToText
      $   "Redundent patterns"
      <+> pretty pats

instance Collect StructWarning Diagnostic where
  collect (MissingBound range) = makeWarning
    (locOf range)
    "Bound Missing"
    "Bound missing at the end of the assertion before the DO construct \" , bnd : ... }\""
  collect (ExcessBound range) = makeWarning
    (locOf range)
    "Excess Bound"
    "Unnecessary bound annotation at this assertion"

instance Collect PO Diagnostic where
  collect (PO _pre _post _anchorHash _anchorLoc origin) = makeWarning loc
                                                                      title
                                                                      ""
   where
      -- we only mark the opening tokens ("do" and "if") for loops & conditionals
    first2Char :: Loc -> Loc
    first2Char NoLoc         = NoLoc
    first2Char (Loc start _) = Loc start (translate 2 start)

    loc :: Loc
    loc = case origin of
      AtLoop        l      -> first2Char l
      AtTermination l      -> first2Char l
      AtIf          l      -> first2Char l
      Explain _ _ _ True l -> first2Char l -- Explain with "originHighlightPartial" as True
      others               -> locOf others

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
  (SrcLoc.toLSPRange range)
  severity
  Nothing
  Nothing
  title
  Nothing
  (Just $ List [DiagnosticRelatedInformation (SrcLoc.toLSPLocation range) body])

--------------------------------------------------------------------------------

class Collect a b where
  collect :: a -> [b]

instance Collect a b => Collect (Maybe a) b where
  collect Nothing  = []
  collect (Just x) = collect x

instance (Collect a x, Collect b x) => Collect (Either a b) x where
  collect (Left  a) = collect a
  collect (Right a) = collect a
