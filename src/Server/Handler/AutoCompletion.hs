{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE DuplicateRecordFields #-}

module Server.Handler.AutoCompletion where

import           Data.Text                      ( Text )
import           Language.LSP.Types

-- To control the behaviour of autocomplete
-- see https://github.com/haskell/lsp/blob/bf95cd94f3301fe391093912e6156de7cb5c1289/lsp-types/src/Language/LSP/Types/Completion.hs
handler :: Monad m => Position -> Maybe CompletionContext -> m CompletionList
handler position completionContext
    | shouldTriggerUnicodeCompletion completionContext
        = return $ CompletionList True (List (unicodeCompletionItems position))
    | shouldTriggerDighole completionContext
        = return $ CompletionList False (List [specBracketsCompletionItem position])
    | otherwise
        = return $ CompletionList True (List [])

-- https://github.com/haskell/lsp/blob/bf95cd94f3301fe391093912e6156de7cb5c1289/lsp-types/src/Language/LSP/Types/Completion.hs#L360-L371
-- trigger Unicode symbol completion when:
--  1. a backslash "\" is being typed
--  2. current completion is incomplete
shouldTriggerUnicodeCompletion :: Maybe CompletionContext -> Bool
shouldTriggerUnicodeCompletion (Just (CompletionContext CtTriggerCharacter (Just "\\")))
    = True
shouldTriggerUnicodeCompletion (Just (CompletionContext CtTriggerForIncompleteCompletions _))
    = True
shouldTriggerUnicodeCompletion _
    = False

-- turn '?' into spec brackets '[! !]'
shouldTriggerDighole :: Maybe CompletionContext -> Bool
shouldTriggerDighole (Just (CompletionContext CtTriggerCharacter (Just "?")))
    = True
shouldTriggerDighole _
    = False

-- list of `CompletionItem`s for unicode symbols
unicodeCompletionItems :: Position -> [CompletionItem]
unicodeCompletionItems position = mconcat
    [ makeItems position
                [" "]
                (Just CiOperator)
                "\\"
                "\"\\\" Backward slash"
                "Inserting \"\\\""
    , makeItems position
                ["->", "rightarrow", "r", "to"]
                (Just CiOperator)
                "→"
                "\"→\" Rightwards Arrow"
                "The Unicode variant of \"->\""
    , makeItems position
                ["/=", "neq", "!="]
                (Just CiOperator)
                "≠"
                "\"≠\" Not Equal To"
                "The Unicode variant of \"/=\""
    , makeItems position
                [">=", "ge", "gte"]
                (Just CiOperator)
                "≥"
                "\"≥\" Greater-Than or Equal To"
                "The Unicode variant of \">=\""
    , makeItems position
                ["<=", "le", "lte"]
                (Just CiOperator)
                "≤"
                "\"≤\" Less-Than or Equal To"
                "The Unicode variant of \"<=\""
    , makeItems position
                ["==>", "Rightarrow", "implies", "R"]
                (Just CiOperator)
                "⇒"
                "\"⇒\" Rightwards Double Arrow"
                "The Unicode variant of \"=>\""
    , makeItems position
                ["<==", "Leftarrow", "ffrom", "L"]
                (Just CiOperator)
                "⇐"
                "\"⇐\" Leftwards Double Arrow"
                "The Unicode variant of \"<=\""
    , makeItems position
                ["&&", "wedge", "and"]
                (Just CiOperator)
                "∧"
                "\"∧\" Logical And"
                "The Unicode variant of \"&&\""
    , makeItems position
                ["||", "vee", "or"]
                (Just CiOperator)
                "∨"
                "\"∨\" Logical Or"
                "The Unicode variant of \"||\""
    , makeItems position
                ["~", "neg", "-"]
                (Just CiOperator)
                "¬"
                "\"¬\" Not Sign"
                "The Unicode variant of \"~\""
    , makeItems position
                ["<|", "langle", "<"]
                (Just CiValue)
                "⟨"
                "\"⟨\" Left Angle Bracket"
                "The Unicode variant of \"<|\""
    , makeItems position
                ["|>", "rangle", ">"]
                (Just CiValue)
                "⟩"
                "\"⟩\" Right Angle Bracket"
                "The Unicode variant of \"|>\""
    , makeItems position
                ["min", "downarrow", "d"]
                (Just CiValue)
                "↓"
                "\"↓\" Downwards Arrow"
                "The Unicode variant of \"min\""
    , makeItems position
                ["max", "uparrow", "u"]
                (Just CiValue)
                "↑"
                "\"↑\" Upwards Arrow"
                "The Unicode variant of \"max\""
    , makeItems position
                ["sum", "Sigma", "sigma", "Gs"]
                (Just CiValue)
                "Σ"
                "\"Σ\" Sum"
                "The Unicode variant of \"sum\""
    , makeItems position
                ["product", "Pi", "pi", "Gp"]
                (Just CiValue)
                "∏"
                "\"∏\" Product"
                "The Unicode variant of \"product\""
    , makeItems position
                ["forall", "all", "A"]
                (Just CiValue)
                "∀"
                "\"∀\" Forall"
                "The Unicode variant of \"forall\""
    , makeItems position
                ["exists", "ex", "E"]
                (Just CiValue)
                "∃"
                "\"∃\" Exists"
                "The Unicode variant of \"exists\""
    , makeItems position
                ["<=>", "equiv", "iff", "==="]
                (Just CiOperator)
                "≡"
                "\"≡\" If and only if"
                "The Unicode variant of \"<=>\""
    , makeItems position
                ["sconj"]
                (Just CiOperator)
                "٭"
                "\"٭\" SConj"
                "SConj"
    ]

-- See https://github.com/haskell/lsp/blob/bf95cd94f3301fe391093912e6156de7cb5c1289/lsp-types/src/Language/LSP/Types/Completion.hs#L288
makeItems
    :: Position
    -> [Text]
    -> Maybe CompletionItemKind
    -> Text
    -> Text
    -> Text
    -> [CompletionItem]
makeItems position labels kind symbol detail doc = flip map labels $ \label ->
  CompletionItem
    label  -- The label of this completion item.
           -- By default also the text that is inserted when selecting this completion.
    kind   -- could be CIOperator, CiValue or whatever
    Nothing -- for marking deprecated stuff
    (Just detail) -- human-readable string
    (Just $ CompletionDocString doc) -- also human-readable string
    Nothing -- deprecated
    Nothing -- select thie item when showing
    Nothing -- how to sort completion items
    Nothing -- how to filter completion items
    (Just symbol) -- the symbol we wanna insert
    (Just PlainText) -- could be a "Snippet" (with holes) or just plain text
    Nothing -- how whitespace and indentation is handled during completion
    Nothing -- TextEdit to be applied when this item has been selected (but not completed yet)
    removeSlash -- TextEdit to be applied when this item has been completed
    (Just (List [" ", "\\"])) -- commit characters
    Nothing -- command to be executed after completion
    Nothing -- ???
  where
    Position ln col = position
    removeSlash =
      Just $ List [TextEdit (Range (Position ln (col - 1)) position) ""]
    -- tempReplaceWithSymbol = Just $ CompletionEditText $ TextEdit (Range position (Position ln (col + 1 ))) "symbol"

specBracketsCompletionItem :: Position -> CompletionItem
specBracketsCompletionItem position = CompletionItem
    { _label               = "?"
    , _kind                = Just CiSnippet
    , _tags                = Nothing
    , _detail              = Nothing
    , _documentation       = Just (CompletionDocString "Type \"?\" and a space to insert a spec.")
    , _deprecated          = Nothing
    , _preselect           = Just True
    , _sortText            = Nothing
    , _filterText          = Nothing
    , _insertText          = Just "[! !]"
    , _insertTextFormat    = Just PlainText
    , _insertTextMode      = Nothing
    , _textEdit            = removeQuestionMark
    , _additionalTextEdits = Nothing
    , _commitCharacters    = Just (List [" "])
    , _command             = Nothing
    , _xdata               = Nothing
    }
    where
      Position line column = position
      removeQuestionMark =
        Just $ CompletionEditText $ TextEdit (Range (Position line (column - 1)) position) ""