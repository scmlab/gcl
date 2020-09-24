{-# LANGUAGE OverloadedStrings #-}

module Syntax.Parser.Lexer
  ( scan
  , LexicalError
  , Tok(..)
  , TokStream
  )
where

import           Syntax.Parser.TokenStream      ( PrettyToken(..) )

import           Data.Char
import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Data.List.NonEmpty            as NE
import           Data.Loc 
import           Data.Maybe                     ( fromMaybe )
import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy                as Text
import           Language.Lexer.Applicative
                                         hiding ( LexicalError )
import qualified Language.Lexer.Applicative    as Lex
import           Text.Regex.Applicative
import Control.Monad.Except
import Control.Monad.State.Lazy 

--------------------------------------------------------------------------------
-- | Tok & TokStream

data Tok
    = 
    -- the following tokens will not be sent to the parser 
      TokWhitespace
    | TokEOF
    | TokComment Text
    | TokNewlineAndWhitespace Int 
    | TokNewlineAndWhitespaceAndBar Int 
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
    TokNewlineAndWhitespaceAndBar n -> "\\n + " ++ show n ++ " | \n"
    TokIndent   -> " >>\n"
    TokDedent   -> " <<\n"
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

newlineAndWhitespace :: RE Text Tok
newlineAndWhitespace = TokNewlineAndWhitespace <$ psym (adapt isNewline) <*> reFoldl Greedy (\n _ -> succ n) 0 (psym $ adapt (\c -> isSpace c && not (isNewline c)))

newlineAndWhitespaceAndBar :: RE Text Tok
newlineAndWhitespaceAndBar = TokNewlineAndWhitespaceAndBar <$ psym (adapt isNewline) <*> reFoldl Greedy (\n _ -> succ n) 2 (psym $ adapt (\c -> isSpace c && not (isNewline c))) <* text "| "

contra :: RE Text a -> RE Char a
contra = comap Text.singleton

lexer :: Lexer Tok
lexer = mconcat
  [ 
    -- meaning tokens that are sent to the parser
    token (longest $ contra tokRE)
  , token (longest $ contra newlineAndWhitespaceAndBar)
  , token (longest $ contra newlineAndWhitespace)
    -- meaningless tokens that will be dumped 
  , whitespace (longest $ contra whitespaceButNewlineRE)
  , whitespace (longestShortest (contra commentStartRE) (contra . commentEndRE))
  ]

--------------------------------------------------------------------------------
-- | scan

type LexicalError = Pos

data PPState = PPState 
  { -- stack of indentation levels
    ppIndentStack :: [Int]
    -- set as the number of indentation after processing tokens like `TokNewlineAndWhitespace`
    -- the second field is set to True if it's `TokNewlineAndWhitespaceAndBar`
  , ppIndentation :: Maybe (Int, Bool)
    -- set to True if expected to be followed by a `TokIndent` (e.g. `TokDo`)
  , ppExpectIndent :: Bool      
    -- Loc of the previous token
  , ppPrevLoc :: Loc      
  }

type PreprocessM = ExceptT LexicalError (State PPState)

runPreprocess :: PreprocessM a -> Either LexicalError a
runPreprocess program = evalState (runExceptT program) (PPState [0] Nothing False NoLoc)

setIdentation :: Maybe (Int, Bool) -> PreprocessM ()
setIdentation i = modify (\(PPState xs _ b l) -> PPState xs i b l)

pushStack :: Int -> PreprocessM ()
pushStack level = modify (\(PPState xs i b l) -> PPState (level:xs) i b l)

popStack :: PreprocessM ()
popStack = modify $ \(PPState xs i b l) -> 
  PPState (case xs of
            [] -> []
            (_:ts) -> ts) i b l

expectingIndent :: Tok -> Bool
expectingIndent TokDo = True 
expectingIndent TokIf = True 
expectingIndent TokArrow = True 
expectingIndent _ = False 

expectingDedent :: Tok -> Bool
expectingDedent TokOd = True 
expectingDedent TokFi = True 
expectingDedent TokGuardBar = True 
expectingDedent _ = False 


data Comparison = CmpNoop | CmpIndent Int | CmpNewline Bool | CmpDedent
compareIndentation :: PreprocessM Comparison
compareIndentation = do 
  indentation' <- gets ppIndentation
  case indentation' of 
    Just (indentation, hasBar) -> do
      -- analyse the stack of indentation levels
      stack <- gets ppIndentStack
      let level = case stack of 
                    [] -> 0
                    (x:_) -> x
      return $ case indentation `compare` level of 
        -- the indentation is lesser than the current level
        LT -> CmpDedent 
        -- the indentation is the same as the current level
        EQ -> CmpNewline hasBar
        -- the indentation is greater than the current level
        GT -> CmpIndent indentation
    Nothing -> return CmpNoop

data Override = ShouldIndent Int | ShouldDedent | DontCare
computeOverride :: L Tok -> PreprocessM Override
computeOverride currentToken = do 
  if expectingDedent (unLoc currentToken)  
    then return ShouldDedent
    else do 
      expectsIndent <- gets ppExpectIndent
      return $ if expectsIndent
        then ShouldIndent $ case locOf currentToken of 
                              NoLoc -> 0 
                              Loc p _ -> posCol p - 1
        else DontCare

data Action = Noop | Indent Int | Newline | Bar | Dedent | DedentRepeat

deviceAction :: Comparison -> Override -> Action 
deviceAction (CmpIndent _)    ShouldDedent      = Dedent 
deviceAction (CmpIndent i)    (ShouldIndent _)  = Indent i 
deviceAction (CmpIndent _)    DontCare          = Noop 
deviceAction (CmpNewline _)   ShouldDedent      = Noop 
deviceAction (CmpNewline b)   (ShouldIndent _)  = if b then Bar else Newline
deviceAction (CmpNewline b)   DontCare          = if b then Bar else Newline 
deviceAction CmpDedent        ShouldDedent      = DedentRepeat 
deviceAction CmpDedent        (ShouldIndent _)  = DedentRepeat
deviceAction CmpDedent        DontCare          = DedentRepeat 
deviceAction CmpNoop          ShouldDedent      = Dedent 
deviceAction CmpNoop          (ShouldIndent i)  = Indent i 
deviceAction CmpNoop          DontCare          = Noop 


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
      case comparison of 
        CmpIndent _ -> setIdentation Nothing 
        CmpNewline _ -> setIdentation Nothing 
        CmpDedent -> return ()
        CmpNoop -> do 
          PPState stack i _ _ <- get 
          let b = expectingIndent (unLoc currentToken)
          let l = locOf currentToken 
          put $ PPState stack i b l


      -- interpret the Action
      case action of 
        Indent indentation -> do 
          pushStack indentation
          loc <- locEnd <$> gets ppPrevLoc
          TsToken (L loc TokIndent) <$> TsToken currentToken <$> preprocess xs 

        Newline -> do 
          loc <- locEnd <$> gets ppPrevLoc
          TsToken (L loc TokNewline) <$> TsToken currentToken <$> preprocess xs 

        Bar -> do 
          loc <- locEnd <$> gets ppPrevLoc
          TsToken (L loc TokGuardBar) <$> TsToken currentToken <$> preprocess xs 

        Dedent -> do 
          popStack 
          loc <- locEnd <$> gets ppPrevLoc
          TsToken (L loc TokDedent) <$> TsToken currentToken <$> preprocess xs 

        DedentRepeat -> do 
          popStack
          loc <- locEnd <$> gets ppPrevLoc
          TsToken (L loc TokDedent) <$> preprocess (TsToken currentToken xs)
          
        Noop -> do 
          TsToken currentToken <$> preprocess xs 



      -- (levels, previous) <- get 


      -- case previous of 
      --   -- this is the first token of the stream
      --   Nothing -> do 
      --     -- update the state
      --     put (levels, Just currentToken)
      --     -- return the first token
      --     TsToken currentToken <$> preprocess xs

      --   Just previousToken -> do 

      --     -- see if the previous token is expecting an indent after it (e.g. TokDo)
      --     let shouldIndent = expectingIndent (unLoc previousToken)

      --     -- see if the current token is expecting an dedent before it (e.g. TokOd)
      --     let shouldDedent = expectingDedent (unLoc currentToken)

      --     -- see if the previous token is 
      --     --  `TokNewlineAndWhitespaceAndBar` or `TokNewlineAndWhitespace`
      --     let previousIndentation = indentedLevel (unLoc previousToken)


      --     case indentedLevel (unLoc currentToken) of 
      --       Just _ -> do 
      --         -- update the state but DON'T return the current token (we will deal with that in the next round)
      --         put (levels, Just currentToken)
      --         preprocess xs
      --       Nothing -> do 

      --         -- analyse the stack of indentation levels
      --         let (lvl, _) = case uncons levels of 
      --               Nothing -> (0, [])
      --               Just pair -> pair
              
      --         case previousIndentation of 
      --           Just n -> do 

      --             case n `compare` lvl of 

      --               -- the indentation is lesser than the current level
      --               LT -> do 

      --                 if shouldDedent 
      --                   then do 
      --                     tok' <- dedent previousToken 

      --                     updateToken currentToken
      --                     tok' <$> TsToken currentToken <$> preprocess xs
      --                   else do 
      --                     tok' <- dedent previousToken 
      --                     tok' <$> preprocess (TsToken currentToken xs)

      --               -- the indentation is the same as the current level
      --               EQ -> do

      --                 if shouldDedent 
      --                   then do 
      --                     tok' <- dedent previousToken 

      --                     updateToken currentToken
      --                     tok' <$> TsToken currentToken <$> preprocess xs
      --                   else do 
      --                     tok <- newline previousToken 

      --                     updateToken currentToken
      --                     tok <$> TsToken currentToken <$> preprocess xs

      --               -- the indentation is greater than the current level
      --               GT -> do 

      --                 if shouldIndent
      --                   then do  
      --                     -- error $ show (previousToken, currentToken)

      --                     tok <- indent previousToken currentToken 

      --                     updateToken currentToken
      --                     tok <$> TsToken currentToken <$> preprocess xs
      --                   else do

      --                     tok <- indent previousToken currentToken 

      --                     updateToken currentToken
      --                     tok <$> TsToken currentToken <$> preprocess xs
      --                     -- TsToken currentToken <$> preprocess xs
      --                     -- tok' <$> preprocess (TsToken currentToken xs)
      --                     -- tok' <- indent previousToken currentToken

      --           Nothing -> do 
      --             case (shouldIndent, shouldDedent) of 
      --               (True, True) -> do 
      --                 tok <- indent previousToken currentToken 
      --                 tok' <- dedent previousToken 

      --                 updateToken currentToken
      --                 tok . tok' <$> TsToken currentToken <$> preprocess xs

      --               (True, False) -> do 
      --                 tok <- indent previousToken currentToken 

      --                 updateToken currentToken
      --                 tok <$> TsToken currentToken <$> preprocess xs

      --               (False, True) -> do 
      --                 tok' <- dedent previousToken 

      --                 updateToken currentToken
      --                 tok' <$> TsToken currentToken <$> preprocess xs

      --               (False, _) -> do 

      --                 updateToken currentToken
      --                 -- return the current token and carry on
      --                 TsToken currentToken <$> preprocess xs



      -- -- see if the previous token is expecting an indent after it (e.g. TokDo)
      -- let shouldIndent = case previousToken of 
      --       Nothing -> False 
      --       Just t  -> expectingIndent t

      -- -- see if the current token is expecting an dedent before it (e.g. TokOd)
      -- let shouldDedent = case previousToken of 
      --       Nothing -> False 
      --       Just _ -> case tok of 
      --         TokOd -> True 
      --         _ -> False 

      -- let hasGuardedBar = case tok of 
      --       TokNewlineAndWhitespaceAndBar _ -> True 
      --       _ -> False

      -- let indentLevel = case tok of 
      --       TokNewlineAndWhitespaceAndBar level -> Just level
      --       TokNewlineAndWhitespace level -> Just level
      --       _ -> Nothing

      -- case indentLevel of 
      --   Just level -> do
      --     -- analyse the stack of indentation levels
      --     let (current, previous) = case uncons levels of 
      --           Nothing -> (0, [])
      --           Just pair -> pair
          
      --     case level `compare` current of 

      --       -- the indentation is lesser than the current level
      --       LT -> do 
      --         -- pop the indentation stack
      --         put (previous, Just tok)
      --         -- insert a `dedent`
      --         TsToken (L (locStart loc) TokDedent) <$> preprocess (TsToken (L loc tok) xs)
      --         -- if shouldDedent
      --         --   then 
      --         --   else TsToken (L (locStart loc) TokDedent) <$> preprocess (TsToken (L loc tok) xs)

      --       -- the indentation is the same as the current level
      --       EQ -> do
      --         -- insert a `newline` (and perhaps a `|`)
      --         if hasGuardedBar
      --           then TsToken (L (locStart loc) TokNewline) <$> TsToken (L (locEnd loc) TokGuardBar) <$> preprocess xs 
      --           else TsToken (L (locStart loc) TokNewline) <$> preprocess xs

      --       -- the indentation is greater thab the current level
      --       GT -> do 
      --         -- insert a `indent` if the previous token is expecting one, 
      --         if shouldIndent
      --           then do 
      --             -- push it into the indentation stack
      --             put (level : levels, Just tok)
      --             TsToken (L (locEnd loc) TokIndent) <$> preprocess xs
      --           else preprocess xs
      --   Nothing -> case (shouldIndent, shouldDedent) of 
      --     (True, True) -> do 
      --       -- the previous token is expecting a indent
      --       put (levels, Just tok)
      --       TsToken (L (locStart loc) TokIndent) <$> TsToken (L (locStart loc) TokDedent) <$> TsToken (L loc tok) <$> preprocess xs
      --     (True, False) -> do 
      --       -- the previous token is expecting a indent
      --       -- calculate the indentation level, and push it into the stack
      --       let level = case loc of 
      --                 NoLoc -> 0 
      --                 Loc p _ -> posCol p - 1
      --       put (level : levels, Just tok)
      --       TsToken (L (locStart loc) TokIndent) <$> TsToken (L loc tok) <$> preprocess xs
      --     (False, True) -> do 
      --       -- analyse the stack of indentation levels
      --       let (_, previous) = case uncons levels of 
      --             Nothing -> (0, [])
      --             Just pair -> pair
      --       -- pop the indentation stack
      --       put (previous, Just tok)
      --       -- insert a `dedent`
      --       TsToken (L (locStart loc) TokDedent) <$> TsToken (L loc tok) <$> preprocess xs

      --     (False, False) -> do 
      --       put (levels, Just tok)
      --       TsToken (L loc tok) <$> preprocess xs

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
