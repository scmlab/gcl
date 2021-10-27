{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE TypeOperators #-}
module Server.Handler.AutoCompletion where

import           Language.LSP.Types
import           Server.Monad

handler :: Position -> Maybe CompletionContext -> ServerM (a |? CompletionList)
handler position completionContext = do
    -- only gets triggered when a backslash "\" is typed, or when the previous completion is incomplete
    let
        triggered = case completionContext of
            (Just (CompletionContext CtTriggerCharacter (Just "\\"))) -> True
            (Just (CompletionContext CtTriggerForIncompleteCompletions _)) ->
                True
            _ -> False
    if triggered
        then do
            let Position ln col = position
            let replaceRange    = Range (Position ln (col - 1)) position
            let removeSlash     = Just $ List [TextEdit replaceRange ""]

            let makeItem label kind symbol detail doc = CompletionItem
                    label
                    kind
                    Nothing
                    (Just detail)
                    (Just $ CompletionDocString doc)
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    (Just symbol)
                    (Just PlainText)
                    Nothing
                    Nothing
                    removeSlash
                    Nothing
                    Nothing
                    Nothing

            let items =
                    [ makeItem "->"
                               (Just CiOperator)
                               "→"
                               "\"→\" Rightwards Arrow"
                               "The Unicode variant of \"->\""
                    , makeItem "/="
                               (Just CiOperator)
                               "≠"
                               "\"≠\" Not Equal To"
                               "The Unicode variant of \"/=\""
                    , makeItem ">="
                               (Just CiOperator)
                               "≥"
                               "\"≥\" Greater-Than or Equal To"
                               "The Unicode variant of \">=\""
                    , makeItem "<="
                               (Just CiOperator)
                               "≤"
                               "\"≤\" Less-Than or Equal To"
                               "The Unicode variant of \"<=\""
                    , makeItem "=>"
                               (Just CiOperator)
                               "⇒"
                               "\"⇒\" Rightwards Double Arrow"
                               "The Unicode variant of \"=>\""
                    , makeItem "&&"
                               (Just CiOperator)
                               "∧"
                               "\"∧\" Logical And"
                               "The Unicode variant of \"&&\""
                    , makeItem "||"
                               (Just CiOperator)
                               "∨"
                               "\"∨\" Logical Or"
                               "The Unicode variant of \"||\""
                    , makeItem "~"
                               (Just CiOperator)
                               "¬"
                               "\"¬\" Not Sign"
                               "The Unicode variant of \"~\""
                    , makeItem "<|"
                               (Just CiValue)
                               "⟨"
                               "\"⟨\" Left Angle Bracket"
                               "The Unicode variant of \"<|\""
                    , makeItem "|>"
                               (Just CiValue)
                               "⟩"
                               "\"⟩\" Right Angle Bracket"
                               "The Unicode variant of \"|>\""
                    , makeItem "min"
                               (Just CiValue)
                               "↓"
                               "\"↓\" Downwards Arrow"
                               "The Unicode variant of \"min\""
                    , makeItem "max"
                               (Just CiValue)
                               "↑"
                               "\"↑\" Upwards Arrow"
                               "The Unicode variant of \"max\""
                    , makeItem "sum"
                               (Just CiValue)
                               "Σ"
                               "\"Σ\" Sum"
                               "The Unicode variant of \"sum\""
                    , makeItem "pi"
                               (Just CiValue)
                               "∏"
                               "\"∏\" Pi"
                               "The Unicode variant of \"pi\""
                    , makeItem "forall"
                               (Just CiValue)
                               "∀"
                               "\"∀\" Forall"
                               "The Unicode variant of \"forall\""
                    , makeItem "exitsts"
                               (Just CiValue)
                               "∃"
                               "\"∃\" Exists"
                               "The Unicode variant of \"exists\""
                    ]

            let isComplete     = True
            let completionList = CompletionList isComplete (List items)
            return $ InR completionList
        else return $ InR $ CompletionList True (List [])
