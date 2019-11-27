{-# LANGUAGE OverloadedStrings #-}

module Syntax.Parser
  ( parseProgram
  , parsePred
  , parseStmt
  , scan

  , toPos

  , module Syntax.Type
  ) where

import Control.Monad.Combinators.Expr
import Control.Monad.State (lift)
import Control.Monad (void)
import Data.Text.Lazy (Text)
import Data.Loc
import Data.Void
import Text.Megaparsec hiding (Pos, State, ParseError, parse)

import Syntax.Concrete
import Syntax.Parser.Lexer
import Syntax.Parser.Util hiding (withLoc)
import qualified Syntax.Parser.Util as Util
import Syntax.Type



--------------------------------------------------------------------------------
-- | States for source location bookkeeping

type Parser = ParsecT Void TokStream (PosLog Tok)

parse :: Parser a -> FilePath -> Text -> Either SyntaxError a
parse parser filepath raw = do
  let tokenStream = scan filepath raw
  case filterError tokenStream of
    Just e  -> Left (LexicalError e)
    Nothing -> case runPosLog (runParserT parser filepath tokenStream) of
      Left e -> Left (SyntacticError $ fromParseErrorBundle e)
      Right x -> Right x

parseProgram :: FilePath -> Text -> Either SyntaxError Program
parseProgram = parse program

parsePred :: Text -> Either SyntaxError Pred
parsePred = parse predicate "<predicate>"

parseStmt :: Text -> Either SyntaxError Stmt
parseStmt = parse statement "<statement>"

program :: Parser Program
program = withLoc $ do
  declarations <- many declaration
  statements <- many statement
  eof
  return $ Program declarations statements

--------------------------------------------------------------------------------
-- | Stmts

statement :: Parser Stmt
statement = choice
  [ try assign
  , abort
  , try assertWithBnd
  , spec
  , assert
  , skip
  , repetition
  , selection
  , hole
  ]


skip :: Parser Stmt
skip = withLoc' $ Skip <$ symbol TokSkip

abort :: Parser Stmt
abort = withLoc' $ Abort <$ symbol TokAbort

assert :: Parser Stmt
assert = withLoc' $ Assert <$> braces predicate

assertWithBnd :: Parser Stmt
assertWithBnd = withLoc' $ braces $ AssertWithBnd
  <$> predicate
  <*  symbol TokComma
  <*  symbol TokBnd
  <*  symbol TokSemi
  <*> expression

assign :: Parser Stmt
assign = withLoc' $ Assign <$> variableList <* symbol TokAssign <*> expressionList

repetition :: Parser Stmt
repetition = withLoc' $ Do <$  symbol TokDo
                          <*> guardedCommands
                          <*  symbol TokOd

selection :: Parser Stmt
selection = withLoc' $ If  <$  symbol TokIf
                          <*> guardedCommands
                          <*  symbol TokFi

guardedCommands :: Parser [GdCmd]
guardedCommands = sepBy1 guardedCommand (symbol TokGuardBar)

guardedCommand :: Parser GdCmd
guardedCommand = withLoc $ GdCmd  <$> predicate
                                  <*  symbol TokGuardArr
                                  <*> some statement

hole :: Parser Stmt
hole = withLoc' $ Hole <$ symbol TokQM

spec :: Parser Stmt
spec = do
  ((), start) <- getLoc $ symbol TokSpecStart
  ignoreNewlines
  stmts <- many statement
  ((), end)   <- getLoc $ symbol TokSpecEnd
  expectNewline

  return $ Spec stmts start end

--------------------------------------------------------------------------------
-- | Predicates

predicate :: Parser Pred
predicate = makeExprParser predTerm table <?> "predicate"
  where
    table :: [[Operator Parser Pred]]
    table = [ [ Prefix negation ]
            , [ InfixL conjunction ]
            , [ InfixL disjunction ]
            , [ InfixR implication ]
            ]

negation :: Parser (Pred -> Pred)
negation = do
  ((), start) <- getLoc $ do
    symbol TokNeg
  ignoreNewlines
  return $ \result -> Neg result (start <--> result)

conjunction :: Parser (Pred -> Pred -> Pred)
conjunction = do
  symbol TokConj
  ignoreNewlines
  return $ \x y -> Conj x y (x <--> y)

disjunction :: Parser (Pred -> Pred -> Pred)
disjunction = do
  symbol TokDisj
  ignoreNewlines
  return $ \x y -> Disj x y (x <--> y)

implication :: Parser (Pred -> Pred -> Pred)
implication = do
  symbol TokImpl
  ignoreNewlines
  return $ \x y -> Implies x y (x <--> y)


predTerm :: Parser Pred
predTerm =  parens predicate
        <|> (withLoc $ choice
              [ HoleP <$  symbol TokQM
              , Lit True <$ symbol TokTrue
              , Lit False <$ symbol TokFalse
              , Term <$> expression <*> binaryRelation <*> expression
              ])

binaryRelation :: Parser BinRel
binaryRelation = withLoc $ choice
  [ Eq  <$ symbol TokEQ
  , LEq <$ symbol TokLTE
  , GEq <$ symbol TokGTE
  , LTh <$ symbol TokLT
  , GTh <$ symbol TokGT
  ]

--------------------------------------------------------------------------------
-- | Expressions

expressionList :: Parser [Expr]
expressionList = sepBy1 expression (symbol TokComma)

expression :: Parser Expr
expression = parens expression
    <|> term

term :: Parser Expr
term = withLoc $ choice
  [ try (OpE <$> op <*> parens expressionList)
  , VarE    <$> variable
  , ConstE  <$> constant
  , LitE    <$> literal
  , HoleE   <$  symbol TokQM
  ]
  where
    op :: Parser Expr
    op = withLoc $ VarE <$> variable

literal :: Parser Lit
literal = choice
  [ Bol True  <$  symbol TokTrue
  , Bol False <$  symbol TokFalse
  , Num       <$> integer
  ]


--------------------------------------------------------------------------------
-- | Declarations

declaration :: Parser Declaration
declaration = choice
  [ constantDecl
  , variableDecl
  ]

constantDecl :: Parser Declaration
constantDecl = withLoc $ do
  symbol TokCon
  types <- constList
  symbol TokSemi
  t <- type'
  return $ ConstDecl types t

variableDecl :: Parser Declaration
variableDecl = withLoc $ do
  symbol TokVar
  vars <- variableList
  symbol TokSemi
  t <- type'
  return $ VarDecl vars t

--------------------------------------------------------------------------------
-- | Variables and stuff

constant :: Parser Const
constant = withLoc $ Const <$> upperName

-- seperated by commas
constList :: Parser [Const]
constList = sepBy1 constant (symbol TokComma)

variable :: Parser Var
variable = withLoc $ Var <$> lowerName

-- seperated by commas
variableList :: Parser [Var]
variableList = sepBy1 variable (symbol TokComma)

type' :: Parser Type
type' = withLoc $ Type <$> upperName

--------------------------------------------------------------------------------
-- | Combinators

ignoreNewlines :: Parser ()
ignoreNewlines = void $ many (symbol TokNewline)

expectNewline :: Parser ()
expectNewline = do
  -- see if the latest accepcted token is TokNewline
  t <- lift Util.getLatestToken
  case t of
    Just TokNewline -> return ()
    _ -> void $ some (symbol TokNewline)

-- ignores suffixing newlines
withLoc :: Parser (Loc -> a) -> Parser a
withLoc p = do
  result <- Util.withLoc p
  ignoreNewlines
  return result

-- followed by at least 1 newline
withLoc' :: Parser (Loc -> a) -> Parser a
withLoc' p = do
  result <- Util.withLoc p
  expectNewline
  return result

parens :: Parser a -> Parser a
parens = between (symbol TokParenStart) (symbol TokParenEnd)

braces :: Parser a -> Parser a
braces = between (symbol TokBraceStart) (symbol TokBraceEnd)

upperName :: Parser Text
upperName = extract p
  where
    p (TokUpperName s) = Just s
    p _ = Nothing

lowerName :: Parser Text
lowerName = extract p
  where
    p (TokLowerName s) = Just s
    p _ = Nothing

integer :: Parser Int
integer = extract p
  where
    p (TokInt s) = Just s
    p _ = Nothing
