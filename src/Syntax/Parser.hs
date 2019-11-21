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
import Syntax.Parser.TokenStream


type Parser = Parsec Void TStream
-- import Debug.Trace

fromRight :: Either (ParseErrorBundle TStream Void) b -> b
fromRight (Left e) = error $ errorBundlePretty e
fromRight (Right x) = x

parseProgram :: FilePath -> Text -> Either (ParseErrorBundle TStream Void) Program
parseProgram filepath raw = parse program filepath (scan filepath raw)

parsePred :: Text -> Either (ParseErrorBundle TStream Void) Pred
parsePred = parse predicate "<predicate>" . scan "<predicate>"

parseStmt :: Text -> Either (ParseErrorBundle TStream Void) Stmt
parseStmt = parse statement "<statement>" . scan "<statement>"

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
skip = withLoc $ Skip <$ symbol TokSkip

abort :: Parser Stmt
abort = withLoc $ Abort <$ symbol TokAbort

assert :: Parser Stmt
assert = withLoc $ Assert <$> braces predicate

assertWithBnd :: Parser Stmt
assertWithBnd = withLoc $ braces $ AssertWithBnd
  <$> predicate
  <*  symbol TokComma
  <*  symbol TokBnd
  <*  symbol TokSemi
  <*> expression

assign :: Parser Stmt
assign = withLoc $ Assign <$> variableList <* symbol TokAssign <*> expressionList

repetition :: Parser Stmt
repetition = withLoc $ Do <$  symbol TokDo
                          <*> guardedCommands
                          <*  symbol TokOd

selection :: Parser Stmt
selection = withLoc $ If  <$  symbol TokIf
                          <*> guardedCommands
                          <*  symbol TokFi

guardedCommands :: Parser [GdCmd]
guardedCommands = sepBy1 guardedCommand (symbol TokGuardBar)

guardedCommand :: Parser GdCmd
guardedCommand = withLoc $ GdCmd  <$> predicate
                                  <*  symbol TokGuardArr
                                  <*> some statement

hole :: Parser Stmt
hole = withLoc $ Hole <$ symbol TokQM

spec :: Parser Stmt
spec = withLoc $ Spec <$  symbol TokSpecStart
                      <*> many statement
                      <*  symbol TokSpecEnd

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
  symbol TokNeg
  return $ \result -> Neg result (start <--> result)

conjunction :: Parser (Pred -> Pred -> Pred)
conjunction = do
  symbol TokConj
  return $ \x y -> Conj x y (x <--> y)

disjunction :: Parser (Pred -> Pred -> Pred)
disjunction = do
  symbol TokDisj
  return $ \x y -> Disj x y (x <--> y)

implication :: Parser (Pred -> Pred -> Pred)
implication = do
  symbol TokImpl
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
parens = between (symbol TokParenStart) (symbol TokParenEnd)

braces :: Parser a -> Parser a
braces = between (symbol TokBraceStart) (symbol TokBraceEnd)

--------------------------------------------------------------------------------
-- | Combinators

symbol :: Tok -> Parser (L Tok)
symbol t = single (L NoLoc t)

upperName :: Parser Text
upperName = do
  L _ (TokUpperName s) <- satisfy p
  return s
  where
    p (L _ (TokUpperName _)) = True
    p _ = False

lowerName :: Parser Text
lowerName = do
  L _ (TokLowerName s) <- satisfy p
  return s
  where
    p (L _ (TokLowerName _)) = True
    p _ = False

integer :: Parser Int
integer = do
  L _ (TokInt s) <- satisfy p
  return s
  where
    p (L _ (TokInt _)) = True
    p _ = False
