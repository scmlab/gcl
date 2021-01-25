-- {-# LANGUAGE OverloadedStrings #-}

module Syntax.Parser.Lexer
  ( scan,
    LexicalError,
    Tok (..),
    TokStream,
  )
where

import Control.Monad.Except
import Control.Monad.State.Lazy
import Data.Char (isAlphaNum, isDigit, isLower, isSpace, isUpper)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Loc
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import Language.Lexer.Applicative hiding
  ( LexicalError,
  )
import qualified Language.Lexer.Applicative as Lex
import Syntax.Parser.TokenStream (PrettyToken (..))
import Text.Regex.Applicative

--------------------------------------------------------------------------------

-- | Tok & TokStream
data Tok
  = -- the following tokens will not be sent to the parser
    TokWhitespace
  | TokEOF
  | TokComment Text
  | TokNewlineAndWhitespace Int
  | TokNewlineAndWhitespaceAndBar Int
  | -- the following 3 kinds of tokens will be generated from `TokNewlineAndWhitespace`
    -- and inserted to the TokenStream instead
    TokIndent
  | TokDedent
  | TokNewline
  | -- keywords
    TokSkip
  | TokAbort
  | TokDo
  | TokOd
  | TokIf
  | TokFi
  | TokBnd
  | TokQM
  | TokVar
  | TokCon
  | TokLet
  | TokArray
  | TokOf
  | TokRange
  | TokGuardBar
  | TokArrow
  | TokArrowU
  | -- delimiters
    TokComma
  | TokColon
  | TokSemi
  | TokAssign
  | TokSpecStart
  | TokSpecEnd
  | TokParenStart
  | TokParenEnd
  | TokBracketStart
  | TokBracketEnd
  | TokBraceStart
  | TokBraceEnd
  | TokQuantStart
  | TokQuantEnd
  | TokQuantStartU
  | TokQuantEndU
  | TokProofStart
  | TokProofEnd
  | -- expression

    -- operators
    TokEQ
  | TokNEQ
  | TokNEQU
  | TokGT
  | TokGTE
  | TokGTEU
  | TokLT
  | TokLTE
  | TokLTEU
  | TokImpl
  | TokConj
  | TokDisj
  | TokNeg
  | TokImplU
  | TokConjU
  | TokDisjU
  | TokNegU
  | TokAdd
  | TokSub
  | TokMul
  | TokDiv
  | TokMod
  | TokUpperName Text
  | TokLowerName Text
  | TokInt Int
  | TokTrue
  | TokFalse
  deriving (Eq, Ord)

instance Show Tok where
  show tok = case tok of
    TokNewlineAndWhitespace n -> "newline + " ++ show n ++ " whitespaces"
    TokNewlineAndWhitespaceAndBar n -> "newline + " ++ show n ++ " whitespaces and a guard bar"
    TokIndent -> "indent"
    TokDedent -> "dedent"
    TokNewline -> "newline"
    TokWhitespace -> " "
    TokEOF -> ""
    TokComment s -> "-- " ++ Text.unpack s
    TokSkip -> "skip"
    TokAbort -> "abort"
    TokDo -> "do"
    TokOd -> "od"
    TokIf -> "if"
    TokFi -> "fi"
    TokBnd -> "bnd"
    TokQM -> "?"
    TokVar -> "var"
    TokCon -> "con"
    TokLet -> "let"
    TokArray -> "array"
    TokOf -> "of"
    TokRange -> ".."
    TokGuardBar -> "|"
    TokArrow -> "->"
    TokArrowU -> "→"
    TokComma -> ","
    TokColon -> ":"
    TokSemi -> ";"
    TokAssign -> ":="
    TokSpecStart -> "{!"
    TokSpecEnd -> "!}"
    TokParenStart -> "("
    TokParenEnd -> ")"
    TokBracketStart -> "["
    TokBracketEnd -> "]"
    TokBraceStart -> "{"
    TokBraceEnd -> "}"
    TokQuantStart -> "<|"
    TokQuantEnd -> "|>"
    TokQuantStartU -> "⟨"
    TokQuantEndU -> "⟩"
    TokProofStart -> "{-"
    TokProofEnd -> "-}"
    TokEQ -> "="
    TokNEQ -> "/="
    TokNEQU -> "≠"
    TokGT -> ">"
    TokGTE -> ">="
    TokGTEU -> "≥"
    TokLT -> "<"
    TokLTE -> "<="
    TokLTEU -> "≤"
    TokImpl -> "=>"
    TokConj -> "&&"
    TokDisj -> "||"
    TokNeg -> "~"
    TokImplU -> "⇒"
    TokConjU -> "∧"
    TokDisjU -> "∨"
    TokNegU -> "¬"
    TokAdd -> "+"
    TokSub -> "-"
    TokMul -> "*"
    TokDiv -> "/"
    TokMod -> "%"
    TokUpperName s -> Text.unpack s
    TokLowerName s -> Text.unpack s
    TokInt i -> show i
    TokTrue -> "True"
    TokFalse -> "False"

type TokStream = TokenStream (L Tok)

--------------------------------------------------------------------------------

-- | Regular expressions & the lexer
tokRE :: RE Char Tok
tokRE =
  TokSkip
    <$ string "skip"
    <|> TokAbort
    <$ string "abort"
    <|> TokDo
    <$ string "do"
    <|> TokOd
    <$ string "od"
    <|> TokIf
    <$ string "if"
    <|> TokFi
    <$ string "fi"
    <|> TokBnd
    <$ string "bnd"
    <|> TokQM
    <$ string "?"
    <|> TokCon
    <$ string "con"
    <|> TokVar
    <$ string "var"
    <|> TokLet
    <$ string "let"
    <|> TokArray
    <$ string "array"
    <|> TokOf
    <$ string "of"
    <|> TokRange
    <$ string ".."
    <|> TokGuardBar
    <$ string "|"
    <|> TokArrow
    <$ string "->"
    <|> TokArrowU
    <$ string "→"
    -- delimiters
    <|> TokComma
    <$ string ","
    <|> TokColon
    <$ string ":"
    <|> TokSemi
    <$ string ";"
    <|> TokAssign
    <$ string ":="
    <|> TokSpecStart
    <$ string "{!"
    <|> TokSpecEnd
    <$ string "!}"
    <|> TokParenStart
    <$ string "("
    <|> TokParenEnd
    <$ string ")"
    <|> TokBracketStart
    <$ string "["
    <|> TokBracketEnd
    <$ string "]"
    <|> TokBraceStart
    <$ string "{"
    <|> TokBraceEnd
    <$ string "}"
    <|> TokQuantStart
    <$ string "<|"
    <|> TokQuantEnd
    <$ string "|>"
    <|> TokQuantStartU
    <$ string "⟨"
    <|> TokQuantEndU
    <$ string "⟩"
    <|> TokProofStart
    <$ string "{-"
    <|> TokProofEnd
    <$ string "-}"
    -- literals
    <|> TokEQ
    <$ string "="
    <|> TokNEQ
    <$ string "/="
    <|> TokNEQU
    <$ string "≠"
    <|> TokGT
    <$ string ">"
    <|> TokGTE
    <$ string ">="
    <|> TokGTEU
    <$ string "≥"
    <|> TokLT
    <$ string "<"
    <|> TokLTE
    <$ string "<="
    <|> TokLTEU
    <$ string "≤"
    <|> TokImpl
    <$ string "=>"
    <|> TokConj
    <$ string "&&"
    <|> TokDisj
    <$ string "||"
    <|> TokNeg
    <$ string "~"
    <|> TokImplU
    <$ string "⇒"
    <|> TokConjU
    <$ string "∧"
    <|> TokDisjU
    <$ string "∨"
    <|> TokNegU
    <$ string "¬"
    <|> TokAdd
    <$ string "+"
    <|> TokSub
    <$ string "-"
    <|> TokMul
    <$ string "*"
    <|> TokDiv
    <$ string "/"
    <|> TokMod
    <$ string "%"
    <|> TokTrue
    <$ string "True"
    <|> TokFalse
    <$ string "False"
    <|> TokUpperName
      . Text.pack
    <$> upperNameRE
    <|> TokLowerName
      . Text.pack
    <$> lowerNameRE
    <|> TokInt
    <$> intRE

-- starts with lowercase alphabets
lowerNameRE :: RE Char String
lowerNameRE =
  (:)
    <$> psym isLower
    <*> many (psym isNameChar)

-- starts with uppercase alphabets
upperNameRE :: RE Char String
upperNameRE =
  (:)
    <$> psym isUpper
    <*> many (psym isNameChar)

intRE :: RE Char Int
intRE = read <$> some (psym isDigit)

-- predicates
isNewline :: Char -> Bool
isNewline '\n' = True
isNewline '\r' = True
isNewline _ = False

isNameChar :: Char -> Bool
isNameChar c = isAlphaNum c || c == '_' || c == '\''

isSpaceButNewline :: Char -> Bool
isSpaceButNewline c = isSpace c && not (isNewline c)

-- for ignoring white spaces
whitespaceButNewlineRE :: RE Char Tok
whitespaceButNewlineRE = msym $ \c ->
  if isSpaceButNewline c
    then Just TokWhitespace
    else Nothing

comment :: RE Char String
comment =
  -- expects spaces
  many (psym isSpace)
    -- expects "--"
    <* string "--"
    -- expects any chars but newline
    <* many (psym (not . isNewline))
    -- expects newline
    <* psym isNewline

-- for indentation bookkeeping
newlineAndWhitespace :: RE Char Tok
newlineAndWhitespace =
  TokNewlineAndWhitespace
    <$ psym isNewline -- matches a newline
    <* many comment -- skip lines of comments
    <*> reFoldl Greedy (\n _ -> succ n) 0 (psym isSpaceButNewline)

-- for indentation bookkeeping
newlineAndWhitespaceAndBar :: RE Char Tok
newlineAndWhitespaceAndBar =
  TokNewlineAndWhitespaceAndBar
    <$ psym isNewline -- matches a newline
    <* many comment -- skip lines of comments
    <*> reFoldl Greedy (\n _ -> succ n) 2 (psym isSpaceButNewline)
    <* string "| "

lexer :: Lexer Tok
lexer =
  mconcat
    [ -- meaning tokens that are sent to the parser
      token (longest tokRE),
      token (longest newlineAndWhitespaceAndBar),
      token (longest newlineAndWhitespace),
      -- meaningless tokens that are to be dumped
      whitespace (longest whitespaceButNewlineRE),
      whitespace (longest comment)
    ]

--------------------------------------------------------------------------------

-- | scan
type LexicalError = Pos

data PPState = PPState
  { -- stack of indentation levels
    ppIndentStack :: [Int],
    -- set as the number of indentation after processing tokens like `TokNewlineAndWhitespace`
    -- the second field is set to True if it's `TokNewlineAndWhitespaceAndBar`
    ppIndentation :: Maybe (Int, Bool),
    -- set to True if expected to be followed by a `TokIndent` (e.g. `TokDo`)
    ppExpectIndent :: Bool,
    -- Loc of the previous token
    ppPrevLoc :: Loc
  }

type PreprocessM = ExceptT LexicalError (State PPState)

runPreprocess :: PreprocessM a -> Either LexicalError a
runPreprocess program = evalState (runExceptT program) (PPState [0] Nothing False NoLoc)

setIdentation :: Maybe (Int, Bool) -> PreprocessM ()
setIdentation i = modify (\(PPState xs _ b l) -> PPState xs i b l)

pushStack :: Int -> PreprocessM ()
pushStack level = modify (\(PPState xs i b l) -> PPState (level : xs) i b l)

popStack :: PreprocessM ()
popStack = modify $ \(PPState xs i b l) ->
  PPState
    ( case xs of
        [] -> []
        (_ : ts) -> ts
    )
    i
    b
    l

expectingIndent :: Tok -> Bool
expectingIndent TokDo = True
expectingIndent TokIf = True
expectingIndent TokArrow = True
expectingIndent TokArrowU = True
expectingIndent _ = False

expectingDedent :: Tok -> Bool
expectingDedent TokOd = True
expectingDedent TokFi = True
expectingDedent TokGuardBar = True
expectingDedent _ = False

data Comparison = CmpNoop | CmpIndent Int | CmpNewline Bool | CmpDedent
  deriving (Show)

compareIndentation :: PreprocessM Comparison
compareIndentation = do
  indentation' <- gets ppIndentation
  case indentation' of
    Just (indentation, hasBar) -> do
      -- analyse the stack of indentation levels
      stack <- gets ppIndentStack
      let level = case stack of
            [] -> 0
            (x : _) -> x
      return $ case indentation `compare` level of
        -- the indentation is lesser than the current level
        LT -> CmpDedent
        -- the indentation is the same as the current level
        EQ -> CmpNewline hasBar
        -- the indentation is greater than the current level
        GT -> CmpIndent indentation
    Nothing -> return CmpNoop

data Override = ShouldIndent Int | ShouldDedent | DontCare
  deriving (Show)

computeOverride :: L Tok -> PreprocessM Override
computeOverride currentToken = do
  if expectingDedent (unLoc currentToken)
    then return ShouldDedent
    else do
      expectsIndent <- gets ppExpectIndent
      return $
        if expectsIndent
          then ShouldIndent $ case locOf currentToken of
            NoLoc -> 0
            Loc p _ -> posCol p - 1
          else DontCare

data Action = Noop | Indent Int | Newline | Bar | Dedent | DedentRepeat
  deriving (Show)

deviceAction :: Comparison -> Override -> Action
deviceAction (CmpIndent _) ShouldDedent = Dedent
deviceAction (CmpIndent i) (ShouldIndent _) = Indent i
deviceAction (CmpIndent _) DontCare = Noop
deviceAction (CmpNewline _) ShouldDedent = Noop
deviceAction (CmpNewline b) (ShouldIndent _) = if b then Bar else Newline
deviceAction (CmpNewline b) DontCare = if b then Bar else Newline
deviceAction CmpDedent ShouldDedent = DedentRepeat
deviceAction CmpDedent (ShouldIndent _) = DedentRepeat
deviceAction CmpDedent DontCare = DedentRepeat
deviceAction CmpNoop ShouldDedent = Dedent
deviceAction CmpNoop (ShouldIndent i) = Indent i
deviceAction CmpNoop DontCare = Noop

scan :: FilePath -> Text -> Either LexicalError TokStream
scan filepath = runPreprocess . preprocess . runLexer lexer filepath . Text.unpack
  where
    preprocess :: TokenStream (L Tok) -> PreprocessM TokStream
    preprocess TsEof = do
      stack <- gets ppIndentStack
      loc <- locEnd <$> gets ppPrevLoc
      if length stack > 1
        then do
          popStack
          TsToken (L loc TokDedent) <$> preprocess TsEof
        else return TsEof
    preprocess (TsError (Lex.LexicalError pos)) = throwError pos
    preprocess (TsToken (L _ (TokNewlineAndWhitespace n)) xs) = do
      setIdentation (Just (n, False))
      preprocess xs
    preprocess (TsToken (L _ (TokNewlineAndWhitespaceAndBar n)) xs) = do
      setIdentation (Just (n, True))
      preprocess xs
    preprocess (TsToken currentToken xs) = do
      -- devise the next Action
      comparison <- compareIndentation
      override <- computeOverride currentToken
      let action = deviceAction comparison override

      -- update state
      PPState stack i _ _ <- get
      put $ PPState stack i (expectingIndent (unLoc currentToken)) (locOf currentToken)
      case comparison of
        CmpIndent _ -> setIdentation Nothing
        CmpNewline _ -> setIdentation Nothing
        CmpDedent -> return ()
        CmpNoop -> return ()

      -- interpret the Action
      case action of
        Indent indentation -> do
          pushStack indentation
          loc <- locEnd <$> gets ppPrevLoc
          TsToken (L loc TokIndent) . TsToken currentToken <$> preprocess xs
        Newline -> do
          loc <- locEnd <$> gets ppPrevLoc
          TsToken (L loc TokNewline) . TsToken currentToken <$> preprocess xs
        Bar -> do
          loc <- locEnd <$> gets ppPrevLoc
          TsToken (L loc TokGuardBar) . TsToken currentToken <$> preprocess xs
        Dedent -> do
          popStack
          loc <- locEnd <$> gets ppPrevLoc
          TsToken (L loc TokDedent) . TsToken currentToken <$> preprocess xs
        DedentRepeat -> do
          popStack
          loc <- locEnd <$> gets ppPrevLoc
          TsToken (L loc TokDedent) <$> preprocess (TsToken currentToken xs)
        Noop -> do
          TsToken currentToken <$> preprocess xs

--------------------------------------------------------------------------------

-- | Instances of PrettyToken
instance PrettyToken Tok where
  prettyTokens (x :| []) =
    fromMaybe ("'" <> show (unLoc x) <> "'") (prettyToken' (unLoc x))
  prettyTokens xs = "\"" <> concatMap (f . unLoc) (NE.toList xs) <> "\""
    where
      f tok = case prettyToken' tok of
        Nothing -> show tok
        Just pretty -> "<" <> pretty <> ">"

-- | If the given character has a pretty representation, return that,
-- otherwise 'Nothing'. This is an internal helper.
prettyToken' :: Tok -> Maybe String
prettyToken' tok = case tok of
  TokNewlineAndWhitespace n -> Just $ "indent [" ++ show n ++ "]"
  TokWhitespace -> Just "space"
  TokEOF -> Just "end of file"
  _ -> Nothing
