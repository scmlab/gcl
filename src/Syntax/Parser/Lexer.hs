{-# LANGUAGE OverloadedStrings #-}

module Syntax.Parser.Lexer
  ( scan
  , LexicalError
  , Tok(..)
  , TokStream
  , isIndentation
  )
where

import           Syntax.Parser.TokenStream      ( PrettyToken(..) )

import           Data.Char
import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Data.List.NonEmpty            as NE
import Data.Loc (L(..), Pos, unLoc)
import           Data.Maybe                     ( fromMaybe )
import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy                as Text
import           Language.Lexer.Applicative
                                         hiding ( LexicalError )
import qualified Language.Lexer.Applicative    as Lex
import           Text.Regex.Applicative
import Control.Monad.Except
import Control.Monad.State.Lazy 
import Data.List (uncons)

--------------------------------------------------------------------------------
-- | Tok & TokStream

data Tok
    = 
    -- the following tokens will not be sent to the parser 
      TokWhitespace
    | TokEOF
    | TokComment Text
    |  TokNewlineAndWhitespace Int 
    -- the following 3 kinds of tokens will be generated from `TokNewlineAndWhitespace` 
    -- and inserted to the TokenStream instead
    | TokIndent    
    | TokDedent 
    | TokNewline

    -- keywords
    | TokSkip
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

    -- delimiters
    | TokComma
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

    -- expression

    -- operators
    | TokEQ
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
    | TokInt       Int
    | TokTrue
    | TokFalse

    deriving (Eq, Ord)

instance Show Tok where
  show tok = case tok of
    TokNewlineAndWhitespace n -> "\\n + " ++ show n ++ " \n"
    TokIndent   -> " >>\\n\n"
    TokDedent   -> " <<\\n\n"
    TokNewline   -> "\\n\n"
    TokWhitespace   -> " "
    TokEOF          -> ""
    TokComment s    -> "-- " ++ Text.unpack s
    TokSkip         -> "skip"
    TokAbort        -> "abort"
    TokDo           -> "do"
    TokOd           -> "od"
    TokIf           -> "if"
    TokFi           -> "fi"
    TokBnd          -> "bnd"
    TokQM           -> "?"
    TokVar          -> "var"
    TokCon          -> "con"
    TokLet          -> "let"
    TokArray        -> "array"
    TokOf           -> "of"
    TokRange        -> ".."
    TokGuardBar     -> "|"
    TokArrow        -> "->"
    TokArrowU       -> "→"
    TokComma        -> ","
    TokColon        -> ":"
    TokSemi         -> ";"
    TokAssign       -> ":="
    TokSpecStart    -> "{!"
    TokSpecEnd      -> "!}"
    TokParenStart   -> "("
    TokParenEnd     -> ")"
    TokBracketStart -> "["
    TokBracketEnd   -> "]"
    TokBraceStart   -> "{"
    TokBraceEnd     -> "}"
    TokQuantStart   -> "<|"
    TokQuantEnd     -> "|>"
    TokQuantStartU  -> "⟨"
    TokQuantEndU    -> "⟩"
    TokEQ           -> "="
    TokNEQ          -> "/="
    TokNEQU         -> "≠"
    TokGT           -> ">"
    TokGTE          -> ">="
    TokGTEU         -> "≥"
    TokLT           -> "<"
    TokLTE          -> "<="
    TokLTEU         -> "≤"
    TokImpl         -> "=>"
    TokConj         -> "&&"
    TokDisj         -> "||"
    TokNeg          -> "~"
    TokImplU        -> "⇒"
    TokConjU        -> "∧"
    TokDisjU        -> "∨"
    TokNegU         -> "¬"
    TokAdd          -> "+"
    TokSub          -> "-"
    TokMul          -> "*"
    TokDiv          -> "/"
    TokMod          -> "%"
    TokUpperName s  -> Text.unpack s
    TokLowerName s  -> Text.unpack s
    TokInt       i  -> show i
    TokTrue         -> "True"
    TokFalse        -> "False"


isIndentation :: Tok -> Bool
isIndentation (TokNewlineAndWhitespace _) = True 
isIndentation _ = False 


type TokStream = TokenStream (L Tok)

--------------------------------------------------------------------------------
-- | Regular expressions & the lexer

text :: Text -> RE Text Text
text raw = Text.foldr f (pure "") raw
 where
  f :: Char -> RE Text Text -> RE Text Text
  f c p = Text.cons <$ sym (Text.singleton c) <*> pure c <*> p



tokRE :: RE Text Tok
tokRE = TokSkip
    <$  text "skip"
    <|> TokAbort
    <$  text "abort"
    <|> TokDo
    <$  text "do"
    <|> TokOd
    <$  text "od"
    <|> TokIf
    <$  text "if"
    <|> TokFi
    <$  text "fi"
    <|> TokBnd
    <$  text "bnd"

    <|> TokQM
    <$  text "?"

    <|> TokCon
    <$  text "con"
    <|> TokVar
    <$  text "var"
    <|> TokLet
    <$  text "let"
    <|> TokArray
    <$  text "array"
    <|> TokOf
    <$  text "of"
    <|> TokRange
    <$  text ".."

    <|> TokGuardBar
    <$  text "|"
    <|> TokArrow
    <$  text "->"
    <|> TokArrowU
    <$  text "→"


  -- delimiters
    <|> TokComma
    <$  text ","
    <|> TokColon
    <$  text ":"
    <|> TokSemi
    <$  text ";"
    <|> TokAssign
    <$  text ":="
    <|> TokSpecStart
    <$  text "{!"
    <|> TokSpecEnd
    <$  text "!}"
    <|> TokParenStart
    <$  text "("
    <|> TokParenEnd
    <$  text ")"
    <|> TokBracketStart
    <$  text "["
    <|> TokBracketEnd
    <$  text "]"
    <|> TokBraceStart
    <$  text "{"
    <|> TokBraceEnd
    <$  text "}"
    <|> TokQuantStart
    <$  text "<|"
    <|> TokQuantEnd
    <$  text "|>"
    <|> TokQuantStartU
    <$  text "⟨"
    <|> TokQuantEndU
    <$  text "⟩"

  -- literals
    <|> TokEQ
    <$  text "="
    <|> TokNEQ
    <$  text "/="
    <|> TokNEQU
    <$  text "≠"
    <|> TokGT
    <$  text ">"
    <|> TokGTE
    <$  text ">="
    <|> TokGTEU
    <$  text "≥"
    <|> TokLT
    <$  text "<"
    <|> TokLTE
    <$  text "<="
    <|> TokLTEU
    <$  text "≤"
    <|> TokImpl
    <$  text "=>"
    <|> TokConj
    <$  text "&&"
    <|> TokDisj
    <$  text "||"
    <|> TokNeg
    <$  text "~"
    <|> TokImplU
    <$  text "⇒"
    <|> TokConjU
    <$  text "∧"
    <|> TokDisjU
    <$  text "∨"
    <|> TokNegU
    <$  text "¬"

    <|> TokAdd
    <$  text "+"
    <|> TokSub
    <$  text "-"
    <|> TokMul
    <$  text "*"
    <|> TokDiv
    <$  text "/"
    <|> TokMod
    <$  text "%"

    <|> TokTrue
    <$  text "True"
    <|> TokFalse
    <$  text "False"

    <|> TokUpperName
    <$> upperNameRE
    <|> TokLowerName
    <$> lowerNameRE
    <|> TokInt
    <$> intRE

adapt :: (Char -> Bool) -> Text -> Bool
adapt f xs | Text.null xs = False
           | otherwise    = f (Text.head xs)

-- starts with lowercase alphabets
lowerNameRE :: RE Text Text
lowerNameRE =
  Text.append
    <$> psym (adapt isLower)
    <*> (Text.concat <$> many
          (psym (adapt (\c -> isAlphaNum c || c == '_' || c == '\'')))
        )

-- starts with uppercase alphabets
upperNameRE :: RE Text Text
upperNameRE =
  Text.append
    <$> psym (adapt isUpper)
    <*> (Text.concat <$> many
          (psym $ adapt (\c -> isAlphaNum c || c == '_' || c == '\''))
        )

intRE :: RE Text Int
intRE = read <$> (Text.unpack . Text.concat <$> some (psym (adapt isDigit)))

isNewline :: Char -> Bool 
isNewline '\n' = True 
isNewline '\r' = True 
isNewline _ = False 


whitespaceButNewlineRE :: RE Text Tok
whitespaceButNewlineRE = matchWhen
  (adapt (\c -> isSpace c && not (isNewline c)))
  TokWhitespace
 where
  matchWhen :: (Text -> Bool) -> Tok -> RE Text Tok
  matchWhen p symbol = msym (\t -> if p t then Just symbol else Nothing)


commentStartRE :: RE Text Text
commentStartRE = text "--"

commentEndRE :: Text -> RE Text Tok
commentEndRE prefix =
  TokComment <$> (pure prefix +++ (Text.concat <$> many anySym) +++ text "\n")
  where (+++) = liftA2 (<>)

indentation :: RE Text Tok
indentation = TokNewlineAndWhitespace <$ psym (adapt isNewline) <*> reFoldl Greedy (\n _ -> succ n) 0 (psym $ adapt (\c -> isSpace c && not (isNewline c)))

contra :: RE Text a -> RE Char a
contra = comap Text.singleton

lexer :: Lexer Tok
lexer = mconcat
  [ 
    -- meaning tokens that are sent to the parser
    token (longest $ contra tokRE)
  , token (longest $ contra indentation)
    -- meaningless tokens that will be dumped 
  , whitespace (longest $ contra whitespaceButNewlineRE)
  , whitespace (longestShortest (contra commentStartRE) (contra . commentEndRE))
  ]

--------------------------------------------------------------------------------
-- | scan

type LexicalError = Pos
type IndentLevels = [Int]

type PreprocessM = ExceptT LexicalError (State IndentLevels)

runPreprocess :: PreprocessM a -> Either LexicalError a
runPreprocess program = evalState (runExceptT program) [0]

scan :: FilePath -> Text -> Either LexicalError TokStream
scan filepath = runPreprocess . preprocess . runLexer lexer filepath . Text.unpack

  where
    preprocess :: TokenStream (L Tok) -> PreprocessM TokStream
    preprocess TsEof = return TsEof
    preprocess (TsError (Lex.LexicalError pos)) = throwError pos
    preprocess (TsToken tok xs) = 
      case tok of 
        L loc (TokNewlineAndWhitespace n) -> do
          levels <- get 
          let (current, previous) = case uncons levels of 
                Nothing -> (0, [])
                Just pair -> pair
          case n `compare` current of 
            -- the indentation is lesser than the current level
            LT -> do 
              -- pop the indentation stack
              put previous
              -- insert a `dedent`
              TsToken (L loc TokDedent) <$> preprocess (TsToken tok xs)
            -- the indentation is the same as the current level
            EQ -> do
              -- insert a `newline` 
              TsToken (L loc TokNewline) <$> preprocess xs
            -- the indentation is greater thab the current level
            GT -> do 
              -- push it into the indentation stack
              put (n : levels)
              -- insert a `indent`
              TsToken (L loc TokIndent) <$> preprocess xs
        _ -> TsToken tok <$> preprocess xs

--------------------------------------------------------------------------------
-- | Instances of PrettyToken


instance PrettyToken Tok where
  prettyTokens (x :| []) =
    fromMaybe ("'" <> show (unLoc x) <> "'") (prettyToken' (unLoc x))
  prettyTokens xs = "\"" <> concatMap (f . unLoc) (NE.toList xs) <> "\""
   where
    f tok = case prettyToken' tok of
      Nothing     -> show tok
      Just pretty -> "<" <> pretty <> ">"

-- | If the given character has a pretty representation, return that,
-- otherwise 'Nothing'. This is an internal helper.

prettyToken' :: Tok -> Maybe String
prettyToken' tok = case tok of
  TokNewlineAndWhitespace n   -> Just $ "indent [" ++ show n ++ "]"
  TokWhitespace -> Just "space"
  TokEOF        -> Just "end of file"
  _             -> Nothing
