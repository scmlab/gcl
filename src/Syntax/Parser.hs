{-# LANGUAGE OverloadedStrings #-}

module Syntax.Parser where

import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Control.Monad.State (lift)
import Data.Loc
import Data.Text.Lazy (Text)
import Data.Void
import Syntax.Concrete hiding
  ( binary,
    unary,
  )
import Syntax.ConstExpr
import Syntax.Location ()
import Syntax.Parser.Lexer
import Syntax.Parser.Util
  ( PosLog,
    extract,
  )
import qualified Syntax.Parser.Util as Util
import Text.Megaparsec hiding
  ( ParseError,
    Pos,
    State,
    parse,
  )
import qualified Text.Megaparsec as Mega
import Prelude hiding (Ordering (..))

--------------------------------------------------------------------------------

-- | States for source location bookkeeping
type Parser = ParsecT Void TokStream (PosLog Tok)

type SyntacticError = (Loc, String)

parse :: Parser a -> FilePath -> TokStream -> Either [SyntacticError] a
parse parser filepath tokenStream =
  case Util.runPosLog (runParserT parser filepath tokenStream) of
    Left e -> Left (fromParseErrorBundle e)
    Right x -> Right x
  where
    fromParseErrorBundle ::
      ShowErrorComponent e => ParseErrorBundle TokStream e -> [SyntacticError]
    fromParseErrorBundle (ParseErrorBundle errors posState) =
      snd $
        foldr f (posState, []) errors
      where
        f ::
          ShowErrorComponent e =>
          Mega.ParseError TokStream e ->
          (PosState TokStream, [SyntacticError]) ->
          (PosState TokStream, [SyntacticError])
        f err (initial, accum) =
          let (_, next) = reachOffset (errorOffset err) initial
           in (next, (getLoc err, parseErrorTextPretty err) : accum)

        getLoc :: ShowErrorComponent e => Mega.ParseError TokStream e -> Loc
        -- get the Loc of all unexpected tokens
        getLoc (TrivialError _ (Just (Tokens xs)) _) = foldMap locOf xs
        getLoc _ = mempty

program :: Parser Program
program = withLoc $ do
  decls <- many (declaration <* (choice [symbol TokNewline, eof])) <?> "declarations"
  skipMany (symbol TokNewline)
  stmts <- many (statement <* (choice [symbol TokNewline, eof])) <?> "statements"
  skipMany (symbol TokNewline)

  let letBindings = pickLetBindings decls

  -- globals and precondition
  let (glob, asrts') = pickGlobals decls
  let pre = if null asrts' then [] else [Assert (conjunct asrts') NoLoc]
  return $ Program decls glob letBindings (pre ++ stmts)

specContent :: Parser [Stmt]
specContent = do
  many statement <?> "statements"

--------------------------------------------------------------------------------

-- | Declarations
declaration :: Parser Declaration
declaration = choice [constantDecl, variableDecl, letDecl] <?> "declaration"

-- declarations :: Parser [Declaration]
-- declarations = many (declaration <* symbol TokNewline) <?> "declaration seperated by newlines"

constantDecl :: Parser Declaration
constantDecl = withLoc $ do
  symbol TokCon <?> "con"
  vars <- constList
  symbol TokColon <?> "colon"
  t <- type'
  assertion <- optional (braces predicate)

  return $ ConstDecl vars t assertion

variableDecl :: Parser Declaration
variableDecl = withLoc $ do
  symbol TokVar <?> "var"
  vars <- variableList
  symbol TokColon <?> "colon"
  t <- type'
  assertion <- optional (braces predicate)
  return $ VarDecl vars t assertion

letDecl :: Parser Declaration
letDecl = withLoc $ do
  symbol TokLet <?> "let"
  name <- upper
  args <- map nameToText <$> many lower
  symbol TokEQ <?> "="
  expr <- predicate
  return $ LetDecl name args expr

--------------------------------------------------------------------------------

-- | Variables and stuff

-- separated by commas
constList :: Parser [Name]
constList = sepBy1 upper (symbol TokComma <?> "comma") <?> "a list of constants separated by commas"

-- separated by commas
variableList :: Parser [Name]
variableList = sepBy1 lower (symbol TokComma <?> "comma") <?> "a list of variables separated by commas"

--------------------------------------------------------------------------------

-- | Stmts
statement :: Parser Stmt
statement =
  choice
    [ try assign,
      abort,
      try assertWithBnd,
      spec,
      assert,
      skip,
      repetition,
      selection,
      hole
    ]
    <?> "statement"

statements :: Parser [Stmt]
statements = sepBy statement (symbol TokNewline)

statements1 :: Parser [Stmt]
statements1 = sepBy1 statement (symbol TokNewline)

skip :: Parser Stmt
skip = withLoc $ Skip <$ symbol TokSkip

abort :: Parser Stmt
abort = withLoc $ Abort <$ symbol TokAbort

assert :: Parser Stmt
assert = withLoc $ Assert <$> braces predicate

assertWithBnd :: Parser Stmt
assertWithBnd =
  withLoc $
    braces $
      LoopInvariant
        <$> predicate
        <* (symbol TokComma <?> "comma")
        <* (symbol TokBnd <?> "bnd")
        <* (symbol TokColon <?> "colon")
        <*> expression

assign :: Parser Stmt
assign =
  withLoc $
    Assign
      <$> variableList
      <* (symbol TokAssign <?> ":=")
      <*> expressionList

repetition :: Parser Stmt
repetition =
  withLoc $
    Do <$> do
      block'
        (symbol TokDo <?> "do")
        guardedCommands
        (symbol TokOd <?> "od")

selection :: Parser Stmt
selection =
  withLoc $
    If <$> do
      block'
        (symbol TokIf <?> "if")
        guardedCommands
        (symbol TokFi <?> "fi")

guardedCommands :: Parser [GdCmd]
guardedCommands = sepBy1 guardedCommand $ do
  symbol TokGuardBar <?> "|"

guardedCommand :: Parser GdCmd
guardedCommand =
  withLoc $
    GdCmd
      <$> predicate
      <* ((symbol TokArrow <?> "->") <|> (symbol TokArrowU <?> "→"))
      <*> block statements1

hole :: Parser Stmt
hole = withLoc $ SpecQM <$ (symbol TokQM <?> "?")

spec :: Parser Stmt
spec = withLoc $ do
  symbol TokSpecStart <?> "{!"
  -- expectNewline <?> "<newline> after a the start of a Spec"
  _ <- specContent
  _ <- takeWhileP (Just "anything other than '!}'") isTokSpecEnd
  symbol TokSpecEnd <?> "!}"
  -- expectNewline <?> "<newline> after a the end of a Spec"

  return $ Spec
  where
    isTokSpecEnd :: L Tok -> Bool
    isTokSpecEnd (L _ TokSpecEnd) = False
    isTokSpecEnd _ = True

--------------------------------------------------------------------------------

-- | Expressions
expressionList :: Parser [Expr]
expressionList =
  sepBy1 expression (symbol TokComma)
    <?> "a list of expressions separated by commas"

predicate :: Parser Expr
predicate = expression <?> "predicate"

expression :: Parser Expr
expression = makeExprParser term table <?> "expression"
  where
    table :: [[Operator Parser Expr]]
    table =
      [ [Postfix application],
        [InfixL $ binary Mod TokMod],
        [InfixL $ binary Mul TokMul, InfixL $ binary Div TokDiv],
        [InfixL $ binary Add TokAdd, InfixL $ binary Sub TokSub],
        [ InfixL $ binary NEQ TokNEQ,
          InfixL $ binary NEQ TokNEQU,
          InfixL $ binary LT TokLT,
          InfixL $ binary LTE TokLTE,
          InfixL $ binary LTE TokLTEU,
          InfixL $ binary GT TokGT,
          InfixL $ binary GTE TokGTE,
          InfixL $ binary GTE TokGTEU
        ],
        [InfixL $ binary EQ TokEQ],
        [Prefix $ unary Neg TokNeg, Prefix $ unary Neg TokNegU],
        [InfixL $ binary Conj TokConj, InfixL $ binary Conj TokConjU],
        [InfixL $ binary Disj TokDisj, InfixL $ binary Disj TokDisjU],
        [InfixR $ binary Implies TokImpl, InfixR $ binary Implies TokImplU]
      ]

    application :: Parser (Expr -> Expr)
    application = do
      terms <- many term
      return $ \func -> do
        let app inner t = App inner t (func <--> t)
        foldl app func terms

    unary :: Op -> Tok -> Parser (Expr -> Expr)
    unary operator' tok = do
      (op, loc) <- Util.getLoc (operator' <$ symbol tok)
      return $ \result -> App (Op op loc) result (loc <--> result)

    binary :: Op -> Tok -> Parser (Expr -> Expr -> Expr)
    binary operator' tok = do
      (op, loc) <- Util.getLoc (operator' <$ symbol tok)
      return $ \x y -> App (App (Op op loc) x (x <--> loc)) y (x <--> y)

    term :: Parser Expr
    term = try term' <|> parens expression
      where
        term' :: Parser Expr
        term' =
          withLoc
            ( choice
                [ Var <$> lower,
                  Const <$> upper,
                  Lit <$> literal,
                  Op <$ symbol TokParenStart <*> operator <* symbol TokParenEnd,
                  Quant
                    <$ symbol TokQuantStart
                    <*> term
                    <*> some lower
                    <* symbol TokColon
                    <*> expression
                    <* symbol TokColon
                    <*> expression
                    <* symbol TokQuantEnd,
                  Quant
                    <$ symbol TokQuantStartU
                    <*> term
                    <*> some lower
                    <* symbol TokColon
                    <*> expression
                    <* symbol TokColon
                    <*> expression
                    <* symbol TokQuantEndU,
                  Hole <$ symbol TokQM
                ]
            )
            <?> "term"

    literal :: Parser Lit
    literal =
      choice
        [ Bol True <$ symbol TokTrue,
          Bol False <$ symbol TokFalse,
          Num <$> integer
        ]
        <?> "literal"

    operator :: Parser Op
    operator =
      choice
        [ EQ <$ symbol TokEQ,
          NEQ <$ symbol TokNEQ,
          NEQ <$ symbol TokNEQU,
          LTE <$ symbol TokLTE,
          LTE <$ symbol TokLTEU,
          GTE <$ symbol TokGTE,
          GTE <$ symbol TokGTEU,
          LT <$ symbol TokLT,
          GT <$ symbol TokGT,
          Implies <$ symbol TokImpl,
          Implies <$ symbol TokImplU,
          Conj <$ symbol TokConj,
          Conj <$ symbol TokConjU,
          Disj <$ symbol TokDisj,
          Disj <$ symbol TokDisjU,
          Neg <$ symbol TokNeg,
          Neg <$ symbol TokNegU,
          Add <$ symbol TokAdd,
          Sub <$ symbol TokSub,
          Mul <$ symbol TokMul,
          Div <$ symbol TokDiv,
          Mod <$ symbol TokMod
        ]
        <?> "operator"

--------------------------------------------------------------------------------

-- | Type
type' :: Parser Type
type' = ignoreIndentations $ do
  makeExprParser term table <?> "type"
  where
    table :: [[Operator Parser Type]]
    table = [[InfixR function]]

    function :: Parser (Type -> Type -> Type)
    function = ignoreIndentations $ do
      choice
        [ symbol TokArrow <?> "->",
          symbol TokArrowU <?> "→"
        ]
      return $ \x y -> TFunc x y (x <--> y)

    term :: Parser Type
    term = ignoreIndentations $ do
      parens type' <|> array <|> base <?> "type term"

    base :: Parser Type
    base = do
      withLoc (TBase <$> extract isBaseType) <?> "base type"
      where
        isBaseType (TokUpperName "Int") = Just TInt
        isBaseType (TokUpperName "Bool") = Just TBool
        isBaseType (TokUpperName "Char") = Just TChar
        isBaseType _ = Nothing

    array :: Parser Type
    array = withLoc $ do
      symbol TokArray
      i <- interval
      symbol TokOf
      t <- type'
      return $ TArray i t

    interval :: Parser Interval
    interval = withLoc $ do
      start <-
        choice
          [Excluding <$ symbol TokParenStart, Including <$ symbol TokBracketStart]
      i <- expression
      symbol TokRange
      j <- expression
      end <-
        choice
          [Excluding <$ symbol TokParenEnd, Including <$ symbol TokBracketEnd]
      return $ Interval (start i) (end j)

--------------------------------------------------------------------------------

-- | Combinators
block :: Parser a -> Parser a
block parser = do
  Util.ignore TokIndent <?> "indentation"
  result <- parser
  Util.ignore TokDedent <?> "dedentation"
  return result

block' :: Parser () -> Parser a -> Parser () -> Parser a
block' open parser close = do
  open
  symbol TokIndent <?> "indentation"
  result <- parser
  choice
    [ do
        -- the ideal case
        symbol TokDedent <?> "dedentation"
        close,
      do
        -- the fucked up case:
        --  the lexer is not capable of handling cases like "if True -> skip fi"
        --  because it's not possible to determine the number of `TokDedent` before `TokFi`
        close
        symbol TokDedent <?> "dedentation"
    ]
  return result

-- consumes 0 or more newlines/indents/dedents
ignoreIndentations :: Parser a -> Parser a
ignoreIndentations parser = do
  result <- parser
  void $ many (Util.ignoreP indentationRelated)
  return result
  where
    indentationRelated TokIndent = True
    indentationRelated TokDedent = True
    indentationRelated _ = False

-- consumes 1 or more newlines
expectNewline :: Parser ()
expectNewline = do
  -- see if the latest accepcted token is TokNewline
  t <- lift Util.getLastToken
  case t of
    Just TokNewline -> return ()
    _ -> void $ some (Util.ignore TokNewline)

symbol :: Tok -> Parser ()
symbol = Util.symbol

withLoc :: Parser (Loc -> a) -> Parser a
withLoc = Util.withLoc

parens :: Relocatable a => Parser a -> Parser a
parens =
  Util.between
    (symbol TokParenStart <?> "left parenthesis")
    (symbol TokParenEnd <?> "right parenthesis")

braces :: Parser a -> Parser a
braces =
  between
    (symbol TokBraceStart <?> "left brace")
    (symbol TokBraceEnd <?> "right brace")

upperName :: Parser Text
upperName = extract p
  where
    p (TokUpperName s) = Just s
    p _ = Nothing

upper :: Parser Name
upper =
  withLoc (Name <$> upperName)
    <?> "identifier that starts with a uppercase letter"

lowerName :: Parser Text
lowerName = extract p
  where
    p (TokLowerName s) = Just s
    p _ = Nothing

lower :: Parser Name
lower =
  withLoc (Name <$> lowerName)
    <?> "identifier that starts with a lowercase letter"

integer :: Parser Int
integer = extract p <?> "integer"
  where
    p (TokInt s) = Just s
    p _ = Nothing
