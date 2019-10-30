{-# LANGUAGE OverloadedStrings #-}

module Syntax.Parser
  ( fromRight
  , parseProgram
  , parsePred
  , parseStmt
  , toPos
  ) where

import Control.Monad.Combinators.Expr
import Data.Text
import Data.Loc
import Data.Void
import Text.Megaparsec hiding (Pos)
import Text.Megaparsec.Error (errorBundlePretty)

import Syntax.Concrete
import Syntax.Lexer

-- import Debug.Trace

fromRight :: Either (ParseErrorBundle Text Void) b -> b
fromRight (Left e) = error $ errorBundlePretty e
fromRight (Right x) = x

parseProgram :: FilePath -> Text -> Either (ParseErrorBundle Text Void) Program
parseProgram = parse program

parsePred :: Text -> Either (ParseErrorBundle Text Void) Pred
parsePred = parse predicate "<predicate>"

parseStmt :: Text -> Either (ParseErrorBundle Text Void) Stmt
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
  , assert
  , skip
  , repetition
  , selection
  ]

skip :: Parser Stmt
skip = withLoc $ Skip <$ symbol "skip"

abort :: Parser Stmt
abort = withLoc $ Abort <$ symbol "abort"

assert :: Parser Stmt
assert = withLoc $ Assert <$> braces predicate

assertWithBnd :: Parser Stmt
assertWithBnd = withLoc $ AssertWithBnd
  <$  symbol "{"
  <*> predicate
  <*  symbol ","
  <*  symbol "bnd"
  <*  symbol ":"
  <*> expression
  <*  symbol "}"

assign :: Parser Stmt
assign = withLoc $ Assign <$> variableList <* symbol ":=" <*> expressionList

repetition :: Parser Stmt
repetition = withLoc $ Do <$  symbol "do"
                          <*> guardedCommands
                          <*  symbol "od"

selection :: Parser Stmt
selection = withLoc $ If  <$  symbol "if"
                          <*> guardedCommands
                          <*  symbol "fi"

guardedCommands :: Parser [GdCmd]
guardedCommands = sepBy1 guardedCommand (symbol "|")

guardedCommand :: Parser GdCmd
guardedCommand = withLoc $ GdCmd  <$> predicate
                                  <*  symbol "->"
                                  <*> some statement

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
  start <- getPos
  symbol "not"
  return $ \result -> Neg result (start <--> result)

conjunction :: Parser (Pred -> Pred -> Pred)
conjunction = do
  symbol "&&"
  return $ \x y -> Conj x y (x <--> y)

disjunction :: Parser (Pred -> Pred -> Pred)
disjunction = do
  symbol "||"
  return $ \x y -> Disj x y (x <--> y)

implication :: Parser (Pred -> Pred -> Pred)
implication = do
  symbol "=>"
  return $ \x y -> Implies x y (x <--> y)


predTerm :: Parser Pred
predTerm =  parens predicate
        <|> (withLoc $ choice
              [ Hole <$  symbol "?"
              , Lit True <$ symbol "true"
              , Lit False <$ symbol "false"
              , Term <$> expression <*> binaryRelation <*> expression
              ])

binaryRelation :: Parser BinRel
binaryRelation = withLoc $ choice
  [ Eq  <$ symbol "="
  , LEq <$ symbol "<="
  , GEq <$ symbol ">="
  , LTh <$ symbol "<"
  , GTh <$ symbol ">"
  ]

--------------------------------------------------------------------------------
-- | Expressions

expressionList :: Parser [Expr]
expressionList = sepBy1 expression (symbol ",")


expression :: Parser Expr
expression = parens expression
    <|> term

term :: Parser Expr
term = withLoc $ choice
  [ try (OpE <$> op <*> many expression)
  , ConstE  <$> constant
  , LitE    <$> literal
  , HoleE   <$  symbol "?"
  ]
  where
    op :: Parser Expr
    op = withLoc $ VarE <$> variable

literal :: Parser Lit
literal = choice
  [ Bol True  <$  symbol "true"
  , Bol False <$  symbol "false"
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
  symbol "con"
  types <- constList
  symbol ":"
  t <- type'
  return $ ConstDecl types t

variableDecl :: Parser Declaration
variableDecl = withLoc $ do
  symbol "var"
  vars <- variableList
  symbol ":"
  t <- type'
  return $ VarDecl vars t

--------------------------------------------------------------------------------
-- | Variables and stuff

constant :: Parser Const
constant = withLoc $ Const <$> identifierUpper

-- seperated by commas
constList :: Parser [Const]
constList = sepBy1 constant (symbol ",")

variable :: Parser Var
variable = withLoc $ Var <$> identifier

-- seperated by commas
variableList :: Parser [Var]
variableList = sepBy1 variable (symbol ",")

type' :: Parser Type
type' = withLoc $ Type <$> identifierUpper

--------------------------------------------------------------------------------
-- | Helper functions

toPos :: Stream s => PosState s -> Pos
toPos (PosState _ offset (SourcePos filepath line column) _ _) = Pos filepath (unPos line) (unPos column) offset

getPos :: Parser Pos
getPos = do
  offset <- getOffset
  SourcePos filepath line column <- getSourcePos
  return $ Pos filepath (unPos line) (unPos column) offset

withLoc :: Parser (Loc -> a) -> Parser a
withLoc parser = do
  start <- getPos
  result <- parser
  end <- getPos
  return $ result (Loc start end)

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")
