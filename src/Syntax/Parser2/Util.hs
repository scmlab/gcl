{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE RecordWildCards #-}

module Syntax.Parser2.Util
  ( M
  , Parser
  , runM
  , getLastToken
  , getLoc
  , withLoc
  , getRange
  , withRange
  , logIfSuccess
  , withLog
  , symbol
  , extract
  , ignore
  , ignoreP
  , sepByAlignmentOrSemi
  , sepByAlignmentOrSemi1
  , sepByAlignment
  , sepByAlignment1
  , simpleSepByAlign
  ) where

import           Control.Monad.State
import           Control.Monad.Writer           ( Writer, runWriter, tell)
import           Data.Loc
import           Data.Loc.Range
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Void
import           Syntax.Parser2.Lexer           ( Tok(..)
                                                , TokStream
                                                )
import           Text.Megaparsec         hiding ( Pos
                                                , State
                                                , between
                                                )
import qualified Data.List.NonEmpty as NEL
import           Data.List                      (intercalate)
import           Data.Monoid                    ( Endo(..))

--------------------------------------------------------------------------------
-- | Source location bookkeeping

type M = StateT Bookkeeping (Writer (Endo [String]))
-- About the Endo stuff, the reference: https://stackoverflow.com/questions/53785921/how-efficient-is-the-writer-monad-for-lists
--  and see the "## Logging" section.

type ID = Int
data Bookkeeping = Bookkeeping
  { currentLoc :: Loc         -- current Loc mark
  , lastToken  :: Maybe Tok   -- the last accepcted token
  , opened     :: Set ID      -- waiting to be moved to the "logged" map
                              -- when the starting position of the next token is determined
  , logged     :: Map ID Loc  -- waiting to be removed when the ending position is determined
  , index      :: Int         -- for generating fresh IDs
  , indentStack:: [L Tok]     -- Recording the tokens to indent/align to.
                                -- see the section: "## State (Bookkeeping) actions for indentation"
  }

runM :: StateT Bookkeeping (Writer (Endo [String])) a -> (a,String)
runM f = 
  let (a, pl) = runWriter $ evalStateT f (Bookkeeping NoLoc Nothing Set.empty Map.empty 0 [])
  in (a, intercalate "\n" $ appEndo pl ["\n"])


getCurrentLoc :: M Loc
getCurrentLoc = gets currentLoc

getLastToken :: M (Maybe Tok)
getLastToken = gets lastToken

-- | Returns an ID (marking the start of the range of some source location)
markStart :: M ID
markStart = do
  i <- gets index
  modify $ \st -> st { index = succ i, opened = Set.insert i (opened st) }
  return i

-- | Returns the range of some source location.
--   The range starts from where the ID is retreived, and ends from here
markEnd :: ID -> M Loc
markEnd i = do
  end       <- getCurrentLoc
  loggedPos <- gets logged
  let loc = case Map.lookup i loggedPos of
        Nothing    -> NoLoc
        Just start -> start <--> end
  modify $ \st -> st { logged = Map.delete i loggedPos }
  return loc

-- | Updates the current source location
updateLoc :: Loc -> M ()
updateLoc loc = do
  set <- gets opened
  let addedLoc = Map.fromSet (const loc) set
  modify $ \st -> st { currentLoc = loc
                     , opened     = Set.empty
                     , logged     = Map.union (logged st) addedLoc
                     }

-- | Updates the latest scanned token
updateToken :: Tok -> M ()
updateToken tok = modify $ \st -> st { lastToken = Just tok }

--------------------------------------------------------------------------------
-- | Helper functions

type Parser = ParsecT Void TokStream M

getLoc :: Parser a -> Parser (a, Loc)
getLoc parser = do
  i      <- lift markStart
  result <- parser
  loc    <- lift (markEnd i)
  return (result, loc)

getRange :: Parser a -> Parser (a, Range)
getRange parser = do
  (result, loc) <- getLoc parser
  case loc of
    NoLoc         -> error "NoLoc when getting srcloc info from a token"
    Loc start end -> return (result, Range start end)

withLoc :: Parser (Loc -> a) -> Parser a
withLoc parser = do
  (result, loc) <- getLoc parser
  return $ result loc

withRange :: Parser (Range -> a) -> Parser a
withRange parser = do
  (result, range) <- getRange parser
  return $ result range

--------------------------------------------------------------------------------
-- ## Logging

-- | Log after the parser successes.
logIfSuccess :: (a -> String) -> Parser a -> Parser a
logIfSuccess msgF p = do
  x <- p
  lift $ tell $ Endo ([msgF x]++)
  return x

-- | Log before running the parser.
withLog :: String -> Parser a -> Parser a
withLog msg p = lift (tell $ Endo ([msg]++)) >> p

--------------------------------------------------------------------------------
-- ## State (Bookkeeping) actions for indentation.

-- See the section "## Combinators for indentation" below for explanations 
-- of the concept of indentation parsing.

-- About the state actions for indentation:
-- An indentation requirement--a token--is inserted to and popped from the stack 'indentStack'
-- only by the combinator 'indentTo'.
-- When any indentation requirement being on top of the stack, the atomic token extractors: 
-- 'symbol' and 'extract', of which all parsers are based upon, would check if the incoming
-- token is indent to that requirement (through 'extractWithIndentCheck' function).

insertIndentReq :: L Tok -> Parser ()
insertIndentReq tok = do
  stack <- gets indentStack
  modify $ \st->st { indentStack = tok:stack }

popIndentReq :: Parser (Maybe (L Tok))
popIndentReq = do
  stack <- gets indentStack
  case stack of
    [] -> return Nothing
    p : ps -> do
      modify $ \st->st {indentStack = ps}
      return (Just p)

lastIndentReq :: Parser (Maybe (L Tok))
lastIndentReq = do
  stack <- gets indentStack
  case stack of
    [] -> return Nothing
    tok : _ -> return (Just tok)

colOf :: L Tok -> Int
colOf = posCol . (\(Loc s _)->s) . locOf

fitsIndentReq :: L Tok -> Maybe (L Tok) -> Bool
fitsIndentReq tokToCheck indentReq = case indentReq of
  Nothing -> True
  Just tokToAlign ->
    (colOf tokToCheck > colOf tokToAlign)
    ||
    (tokToCheck `strictEq` tokToAlign)
      --If the token is the same to the leftTip(the token to align/indent to).
      -- This could happen when backtracking happens;
      -- For example, in 'definition = choice [try funcDefnSig, typeDefn, funcDefnF]',
      -- the starts of both funcDefnSig and funcDefnF are identifiers, when funcDefnSig fails then goes to funcDefnF,
      -- the starting identifier would be checked again.
     ||
    (unLoc tokToCheck `elem`
     [ TokFi          -- 'fi'
     , TokOd          -- 'od'
     , TokBlockClose  -- ']|'
     , TokGuardBar    -- '|'
     , TokSpecClose   -- '!]'
     , TokProofClose  -- '-}'
     , TokDeclClose   -- ':}'
     ])
      -- Special cases: structural delimiters don't need to align/indent to anything
      -- or if they're not aligned, will it cause any trouble?
      -- Array's ']', quant's '|>' are considered parts of an expression, therefore not on the list.
  where
    strictEq (L l1 t1) (L l2 t2) = l1==l2 && t1==t2


-- an ideal method which doesn't work: 
-- * parse -> if success, indentCheck(without touching parser state), manually fail it when indentCheck fails
-- So the current solution is: 
-- * parse(Mega.token) with indentation check, if failed, see if the failure was caused by indentation error,
--   if so, add the indentation error message. 
extractWithIndentCheck :: (L Tok -> Maybe (L Tok,a)) -> Parser a
extractWithIndentCheck tokpred = do
  ir <- lastIndentReq
  let f (lt,a) = do
        guard $ lt `fitsIndentReq` ir
        return a
  pr <- observing $ lookAhead anySingle --later indent check needs a token
  case pr of
    Left _ -> do -- safe to assume that indentation won't be involved since there's no next token
      (_,a) <- token tokpred Set.empty
      return a
    Right ltok -> do -- now do the real extraction we need
      r <- observing $ token (tokpred >=> f) Set.empty
        -- delay the failure so we can add error messages for indentation
      case r of
        Right a -> return a -- successfully extract a will-indented token

        Left pe -> case pe of -- the extraction failed
          TrivialError _ m_ei set -> do
            if ltok `fitsIndentReq` ir
            then -- the failure was not caused by indentation error
              failure m_ei set
            else -- the failure was caused by indentation error, we need to proceed to adding error msg
              case m_ei of -- trying to utilize the original unexpected token's loc information
              Nothing -> failureWithoutLoc
              Just ei -> case ei of
                Tokens ((L loc errtok) NEL.:| tos) ->
                  failure (Just$Tokens (newErrLTok loc errtok NEL.:| tos) ) set
                Label _ -> failureWithoutLoc --might need to change in the future
                EndOfInput -> failureWithoutLoc --a case that might not going to happen, for we filtered out the case at 'observing $ lookAhead anySingle'
              where
                fromJust Nothing = error "An error that shouldn't happen here: fitsIndentReq==False implies that 'ir' is a Just."
                fromJust (Just x)= x
                irtok = fromJust ir
                lineNum = posLine $ (\(Loc s _)->s) $ locOf irtok
                newMsg = NEL.fromList $ "token '"<>show ltok<>"' not indent to '"<> show irtok<>"' of line "<>show lineNum
                newErrLTok loc errtok = L loc (ErrTokIndent errtok (unLoc irtok) lineNum)
                failureWithoutLoc = failure (Just$Label newMsg) set
          FancyError _ set -> fancyFailure set --We're not handling fancy errors yet.  

--------------------------------------------------------------------------------
-- | Combinators

-- Parsing with bookkeeping actions: symbol, extract
-- All parsers should be built upon these combinators.

-- Create a parser of some symbol (while respecting source locations)
symbol :: Tok -> Parser Loc
symbol t = do
  -- ir <- lastIndentReq
  -- loctok@(L loc tok) <- satisfy (\lt@(L _ t') -> t == t' && lt `fitsIndentReq` ir)
  (loc, tok) <- extractWithIndentCheck (\lt@(L l t') -> if t == t' then Just (lt,(l,t')) else Nothing)
  lift $ do
    updateLoc loc
    updateToken tok
  return loc

-- Useful for extracting values from a Token 
extract :: (Tok -> Maybe a) -> Parser a
extract f = do
  -- ir <- lastIndentReq
  let p loctok@(L l tok') = do
        --guard $ loctok `fitsIndentReq` ir
        (\result->(loctok, (result,l,tok'))) <$> f tok'
  -- (result, loctok@(L loc tok)) <- token p Set.empty
  (result, loc, tok) <- extractWithIndentCheck p
  lift $ do
    updateLoc loc
    updateToken tok
  return result

-- Create a parser of some symbol, that doesn't update source locations
-- effectively excluding it from source location tracking
ignore :: Tok -> Parser ()
ignore t = do
  L _ tok <- satisfy ((==) t . unLoc)
  lift $ updateToken tok
  return ()

-- The predicate version of `ignore`
ignoreP :: (Tok -> Bool) -> Parser ()
ignoreP p = do
  L _ tok <- satisfy (p . unLoc)
  lift $ updateToken tok
  return ()


-- ## Combinators for indentation
-- The design principle of indentation constraints is to reduce ambiguity of newline and alignment.
-- (if the new line is indented, then it's the continuation of the last line; if it's aligned, 
--  then it's a parallel item.)
-- This idea is fulfilled by the combinator(s) 'sepByAlignment', which tries to run the input parser
-- as many as possible, while each result is aligned one by one, and the body of each is indented to the aligned column.

-- The input Parser should be built from either 'symbol' or 'extract', for our parsers are all built on these functions, 
-- and each checks the indentation correctness of the token to be consumed.

----------------------------------------------------------------------------------------
-- ### Combinators for current internal use: indentTo, alignTo, alignAndIndentBodyTo

-- considering to export this method
indentTo :: Parser a -> L Tok -> Parser a
indentTo p tok = do
  insertIndentReq tok
  x <- observing p
  _ <- popIndentReq
  -- The requirement needs to be popped no matter the parser went successfully or not,
  -- because Bookkeeping is not back-trackable (cannot just undo insertion of IndentReq).
  case x of
    Left pe -> case pe of
      TrivialError _ m_ei set -> failure m_ei (Set.insert (Label (NEL.fromList $ "token indent to '"<>show tok<>"' of line "<>show lineNum)) set)
        -- The message here is "expecting indented token", while in 'extractWithIndentCheck', it's "unexpected unindented token".
        -- Is this message really needed?
        where
          lineNum = posLine $ (\(Loc s _)->s) $ locOf tok
      FancyError _ set -> fancyFailure set
    Right r -> return r

-- considering to export this method
alignTo :: Parser a -> L Tok -> Parser a
alignTo parser tokToAlign =
    do
      tok <- try $ lookAhead anySingle
      let lineNum = posLine $ (\(Loc s _)->s) $ locOf tokToAlign
      if colOf tok == colOf tokToAlign
      then parser
      else failure Nothing (Set.fromList [Label (NEL.fromList $ "token align to '"<>show tokToAlign<>"' of line "<>show lineNum)])
  <|>
    parser
    -- If there's no token to lookahead, let the parser decides whether this should fail and the failing message, 
    -- instead of what would be done by 'lookAhead anySingle'.
    -- If this is the case, it's implied that eof satisfies any indentation requirement.

alignAndIndentBodyTo :: Parser a -> L Tok -> Parser a
alignAndIndentBodyTo p tokToAlign = do
  let notEof = do
        tok <- try $ lookAhead anySingle
        alignTo (p `indentTo` tok) tokToAlign
  notEof <|> p
  -- NOT 'try notEof <|> p' !!!
  --   this will make the meaningful failures slip away.


----------------------------------------------------------------------------------------
-- ### Exported alignment combinators


-- | Only see if each item is aligned to the first, no indentation check in the body.
simpleSepByAlign :: Parser a -> Parser [a]
simpleSepByAlign parser = do
  let notEof = do
        tokToAlign <- try $ lookAhead anySingle
        many $ alignTo parser tokToAlign
  notEof <|> return []


sepByAlignmentOrSemi :: Parser a -> Parser [a]
sepByAlignmentOrSemi parser =
    do
      tok <- try $ lookAhead anySingle
      sepByAlignmentOrSemiHelper tok True parser
  <|>
    return []

sepByAlignmentOrSemi1 :: Parser a -> Parser [a]
sepByAlignmentOrSemi1 parser = do
  tokToAlign <- lookAhead anySingle <?> "anything to start the block"
  x <- parser `indentTo` tokToAlign
  xs <- sepByAlignmentOrSemiHelper tokToAlign True parser
  return (x:xs)

sepByAlignmentOrSemiHelper :: L Tok -> Bool -> Parser a -> Parser [a]
sepByAlignmentOrSemiHelper tokToAlign useSemi parser = do
  let oneLeadByAlign = parser `alignAndIndentBodyTo` tokToAlign
      oneLeadBySemi =  symbol TokSemi *> parser `indentTo` tokToAlign
  let semiParser = if useSemi then oneLeadBySemi else empty
  many (semiParser <|> oneLeadByAlign)

sepByAlignment :: Parser a -> Parser [a]
sepByAlignment parser = do
    do
      tok <- try $ lookAhead anySingle
      sepByAlignmentOrSemiHelper tok False parser
  <|>
    return []

sepByAlignment1 :: Parser a -> Parser [a]
sepByAlignment1 parser = do
  tokToAlign <- lookAhead anySingle <?> "anything to start the block"
  x <- parser `indentTo` tokToAlign
  xs <- sepByAlignmentOrSemiHelper tokToAlign False parser
  return (x:xs)