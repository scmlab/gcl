{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Syntax.Parser.TH where

import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Data.Text                      ( Text )
import           Syntax.Parser.Lexer

data Token = Token
  { name :: String
  , tok  :: Text
  , conv :: Maybe String
  }
  deriving (Eq, Show)

defaultToken :: Token
defaultToken = Token mempty mempty Nothing

defaultOpToken :: Token
defaultOpToken = Token mempty mempty (Just mempty)

newtype TokenSpec = TokenSpec [Token]


config :: TokenSpec
config = TokenSpec
  [ defaultToken { name = "LineComment", tok = "--" }
  , defaultToken { name = "BlockCommentStart", tok = "{{" }
  , defaultToken { name = "BlockCommentEnd", tok = "}}" }
  , defaultToken { name = "Skip", tok = "skip" }
  , defaultToken { name = "Abort", tok = "abort" }
  , defaultToken { name = "Do", tok = "do" }
  , defaultToken { name = "Od", tok = "od" }
  , defaultToken { name = "If", tok = "if" }
  , defaultToken { name = "Fi", tok = "fi" }
  , defaultToken { name = "Bnd", tok = "bnd" }
  , defaultToken { name = "QM", tok = "?" }
  , defaultToken { name = "Con", tok = "con" }
  , defaultToken { name = "Var", tok = "var" }
  , defaultToken { name = "Let", tok = "let" }
  , defaultToken { name = "Data", tok = "data" }
  , defaultToken { name = "Array", tok = "array" }
  , defaultToken { name = "Of", tok = "of" }
  , defaultToken { name = "New", tok = "new" }
  , defaultToken { name = "Dispose", tok = "dispose" }
  , defaultToken { name = "Range", tok = ".." }
  , defaultToken { name = "GuardBar", tok = "|" }
  , defaultToken { name = "Arrow", tok = "->" }
  , defaultToken { name = "ArrowU", tok = "→" }
  , defaultToken { name = "Star", tok = "*" }
  , defaultToken { name = "Space", tok = " " }
  , defaultToken { name = "Comma", tok = "," }
  , defaultToken { name = "Colon", tok = ":" }
  , defaultToken { name = "Semi", tok = ";" }
  , defaultToken { name = "Assign", tok = ":=" }
  , defaultToken { name = "SpecStart", tok = "[!" }
  , defaultToken { name = "SpecEnd", tok = "!]" }
  , defaultToken { name = "ParenStart", tok = "(" }
  , defaultToken { name = "ParenEnd", tok = ")" }
  , defaultToken { name = "BracketStart", tok = "[" }
  , defaultToken { name = "BracketEnd", tok = "]" }
  , defaultToken { name = "BraceStart", tok = "{" }
  , defaultToken { name = "BraceEnd", tok = "}" }
  , defaultToken { name = "QuantStarts", tok = "<|" }
  , defaultToken { name = "QuantEnds", tok = "|>" }
  , defaultToken { name = "QuantStartU", tok = "⟨" }
  , defaultToken { name = "QuantEndU", tok = "⟩" }
  , defaultToken { name = "ProofStart", tok = "{-" }
  , defaultToken { name = "ProofEnd", tok = "-}" }
  , defaultToken { name = "BackSlash", tok = "\\" }
  , defaultToken { name = "DeclStart", tok = "{:" }
  , defaultToken { name = "DeclEnd", tok = ":}" }
  , defaultOpToken { name = "EQProp", tok = "<=>" }
  , defaultOpToken { name = "EQPropU", tok = "≡" }
  , defaultOpToken { name = "EQ", tok = "=" }
  , defaultOpToken { name = "NEQ", tok = "/=" }
  , defaultOpToken { name = "NEQU", tok = "≠" }
  , defaultOpToken { name = "GT", tok = ">" }
  , defaultOpToken { name = "GTE", tok = ">=" }
  , defaultOpToken { name = "GTEU", tok = "≥" }
  , defaultOpToken { name = "LT", tok = "<" }
  , defaultOpToken { name = "LTE", tok = "<=" }
  , defaultOpToken { name = "LTEU", tok = "≤" }
  , defaultOpToken { name = "Impl", tok = "=>" }
  , defaultOpToken { name = "ImplU", tok = "⇒" }
  , defaultOpToken { name = "Conj", tok = "&&" }
  , defaultOpToken { name = "ConjU", tok = "∧" }
  , defaultOpToken { name = "Disj", tok = "||" }
  , defaultOpToken { name = "DisjU", tok = "∨" }
  , defaultOpToken { name = "Neg", tok = "~" }
  , defaultOpToken { name = "NegU", tok = "¬" }
  , defaultOpToken { name = "Add", tok = "+" }
  , defaultOpToken { name = "Sub", tok = "-" }
  , defaultOpToken { name = "Mul", tok = "*" }
  , defaultOpToken { name = "Div", tok = "/" }
  , defaultOpToken { name = "Mod", tok = "%" }
  , defaultOpToken { name = "Max", tok = "↑" }
  , defaultOpToken { name = "Min", tok = "↓" }
  , defaultOpToken { name = "PointsTo", tok = "↦" }
  , defaultOpToken { name = "SConj", tok = "٭" }
  , defaultOpToken { name = "SImp", tok = "-٭" }
  , defaultOpToken { name = "Exp", tok = "^" }
  , Token { name = "Sum", tok = "Σ", conv = Just "Add" }
  , Token { name = "Pi", tok = "∏", conv = Just "Mul" }
  , Token { name = "Forall", tok = "∀", conv = Just "Conj" }
  , Token { name = "Exists", tok = "∃", conv = Just "Disj" }
  , defaultToken { name = "Hash", tok = "#" }
  , defaultToken { name = "TypeInt", tok = "Int" }
  , defaultToken { name = "TypeBool", tok = "Bool" }
  , defaultToken { name = "TypeChar", tok = "Char" }
  , defaultToken { name = "True", tok = "True" }
  ]

mkTokTExp :: Text -> Q (TExp Text)
mkTokTExp t = [|| t ||]

mkTokDec :: String -> Text -> Q [Dec]
mkTokDec n t = do
  tokN <- varP =<< newName ("tok" ++ n)
  tokE <- normalB . unTypeQ . mkTokTExp $ t
  return [ValD tokN tokE []]

mkLexDec :: String -> Text -> Maybe String -> Q [Dec]
mkLexDec n t mc = do
  lexN  <- varP =<< newName ("lex" ++ n)
  lexE  <- unTypeQ [|| symbol $$(mkTokTExp t) ||]
  lexE' <- normalB $ case mc of
    Nothing -> pure lexE
    Just "" -> appE (varE =<< newName n) (pure lexE)
    Just c  -> appE (varE =<< newName c) (pure lexE)
  return [ValD lexN lexE' []]

mkTokDecs :: TokenSpec -> Q [Dec]
mkTokDecs (TokenSpec s) = do
  concat
    <$> mapM
          (\(Token n t _) -> do
            mkTokDec n t
          )
          s

mkLexDecs :: TokenSpec -> Q [Dec]
mkLexDecs (TokenSpec s) = do
  concat
    <$> mapM
          (\(Token n t mc) -> do
            mkLexDec n t mc
          )
          s
