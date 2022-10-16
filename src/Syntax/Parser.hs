{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Syntax.Parser where

import           Control.Monad.Combinators.Expr
import           Control.Monad.Except
import qualified Data.Either                   as Either
import           Data.List.NonEmpty             ( NonEmpty )
import           Data.Loc
import           Data.Loc.Range
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Void
import           Language.Lexer.Applicative     ( TokenStream(TsEof, TsToken) )
import           Prelude                 hiding ( EQ
                                                , GT
                                                , LT
                                                , Ordering
                                                , lookup
                                                )
import           Syntax.Common           hiding ( Fixity(..) )
import           Syntax.Concrete         hiding ( Op )
import qualified Syntax.Concrete.Types         as Expr
import           Syntax.Parser.Error
import           Syntax.Parser.Lexer
import           Syntax.Parser.Util     hiding ( Parser )
import           Text.Megaparsec         hiding ( ParseError
                                                , Pos
                                                , State
                                                , Token
                                                , parse
                                                , tokens
                                                )
import qualified Text.Megaparsec               as Mega

--------------------------------------------------------------------------------
-- | States for source location bookkeeping
type Parser = ParsecT Void TokStream M

--------------------------------------------------------------------------------

scanAndParse :: Parser a -> FilePath -> Text -> Either ParseError a
scanAndParse parser filepath source = case scan filepath source of
  Left  err    -> throwError (LexicalError err)
  Right tokens -> case parse parser filepath tokens of
    Left  (errors,logMsg) -> throwError (SyntacticError errors logMsg)
    Right val             -> return val

parse :: Parser a -> FilePath -> TokStream -> Either (NonEmpty (Loc, String), String) a
parse parser filepath tokenStream =
  case runM (runParserT (parser <* eof) filepath tokenStream) of
    (Left  e, logMsg) -> Left (fromParseErrorBundle e, logMsg)
    (Right x, _) -> Right x
 where
  fromParseErrorBundle
    :: ShowErrorComponent e
    => ParseErrorBundle TokStream e
    -> NonEmpty (Loc, String)
  fromParseErrorBundle (ParseErrorBundle errors _) = fmap toError errors
   where
    toError
      :: ShowErrorComponent e => Mega.ParseError TokStream e -> (Loc, String)
    toError err = (getLoc' err, parseErrorTextPretty err)
    -- get the Loc of all unexpected tokens
    getLoc' :: ShowErrorComponent e => Mega.ParseError TokStream e -> Loc
    getLoc' (TrivialError _ (Just (Tokens xs)) _) = foldMap locOf xs
    getLoc' _ = mempty

parseWithTokList
  :: Parser a -> FilePath -> [L Tok] -> Either (NonEmpty (Loc, String), String) a
parseWithTokList parser filepath = parse parser filepath . convert
 where
  convert :: [L Tok] -> TokStream
  convert (x : xs) = TsToken x (convert xs)
  convert []       = TsEof

declOrDefnBlock :: Parser (Either Declaration DefinitionBlock)
declOrDefnBlock = choice
  [ Left <$> declaration <?> "declaration"
  , Right <$> definitionBlock <?> "definition block"
  ]


--------------------------------------------------------------------------------

-- | Parser for SepByComma
sepBy' :: Parser (Token sep) -> Parser a -> Parser (SepBy sep a)
sepBy' delim parser = do
  x <- parser

  let f = return (Head x)
  let g = do
        sep <- delim
        xs  <- sepBy' delim parser
        return $ Delim x sep xs
  try g <|> f


sepByComma :: Parser a -> Parser (SepBy "," a)
sepByComma = sepBy' tokenComma

sepByGuardBar :: Parser a -> Parser (SepBy "|" a)
sepByGuardBar = sepBy' tokenGuardBar

-- for building parsers for tokens
adapt :: Tok -> String -> Parser (Token a)
adapt t errMsg = do
  loc <- symbol t <?> errMsg
  case loc of
    NoLoc   -> error "NoLoc when parsing token"
    Loc l r -> return $ Token l r

tokenConst :: Parser (Token "con")
tokenConst = adapt TokCon "reserved word \"con\""

tokenVar :: Parser (Token "var")
tokenVar = adapt TokVar "reserved word \"var\""

tokenData :: Parser (Token "data")
tokenData = adapt TokData "reserved word \"data\""

tokenBraceOpen :: Parser (Token "{")
tokenBraceOpen = adapt TokBraceOpen "opening curly bracket"

tokenBraceClose :: Parser (Token "}")
tokenBraceClose = adapt TokBraceClose "closing curly bracket"

tokenBracketOpen :: Parser (Token "[")
tokenBracketOpen = adapt TokBracketOpen "opening square bracket"

tokenBracketClose :: Parser (Token "]")
tokenBracketClose = adapt TokBracketClose "closing square bracket"

tokenParenOpen :: Parser (Token "(")
tokenParenOpen = adapt TokParenOpen "opening parenthesis"

tokenParenClose :: Parser (Token ")")
tokenParenClose = adapt TokParenClose "closing parenthesis"

tokenQuantOpen :: Parser (Token "<|")
tokenQuantOpen = adapt TokQuantOpen "<|"

tokenQuantOpenU :: Parser (Token "⟨")
tokenQuantOpenU = adapt TokQuantOpenU "⟨"

tokenQuantClose :: Parser (Token "|>")
tokenQuantClose = adapt TokQuantClose "|>"

tokenQuantCloseU :: Parser (Token "⟩")
tokenQuantCloseU = adapt TokQuantCloseU "⟩"

tokenSpecOpen :: Parser (Token "[!")
tokenSpecOpen = adapt TokSpecOpen "[!"

tokenSpecClose :: Parser (Token "!]")
tokenSpecClose = adapt TokSpecClose "!]"

tokenProofOpen :: Parser (Token "{-")
tokenProofOpen = adapt TokProofOpen "{-"

tokenProofClose :: Parser (Token "-}")
tokenProofClose = adapt TokProofClose "-}"

tokenBlockOpen :: Parser (Token "|[")
tokenBlockOpen = adapt TokBlockOpen "|["

tokenBlockClose :: Parser (Token "]|")
tokenBlockClose = adapt TokBlockClose "]|"

tokenDeclOpen :: Parser (Token "{:")
tokenDeclOpen = adapt TokDeclOpen "{:"

tokenDeclClose :: Parser (Token ":}")
tokenDeclClose = adapt TokDeclClose ":}"

tokenColon :: Parser (Token ":")
tokenColon = adapt TokColon "colon"

tokenSemi :: Parser (Token ";")
tokenSemi = adapt TokSemi "semi"

tokenComma :: Parser (Token ",")
tokenComma = adapt TokComma "comma"

tokenRange :: Parser (Token "..")
tokenRange = adapt TokRange ".."

tokenStar :: Parser (Token "*")
tokenStar = adapt TokMul "*"

tokenArray :: Parser (Token "array")
tokenArray = adapt TokArray "reserved word \"array\""

tokenOf :: Parser (Token "of")
tokenOf = adapt TokOf "reserved word \"of\""

tokenBnd :: Parser (Token "bnd")
tokenBnd = adapt TokBnd "reserved word \"bnd\""

tokenIf :: Parser (Token "if")
tokenIf = adapt TokIf "reserved word \"if\""

tokenFi :: Parser (Token "fi")
tokenFi = adapt TokFi "reserved word \"fi\""

tokenDo :: Parser (Token "do")
tokenDo = adapt TokDo "reserved word \"do\""

tokenOd :: Parser (Token "od")
tokenOd = adapt TokOd "reserved word \"od\""

tokenCase :: Parser (Token "case")
tokenCase = adapt TokCase "reserved word \"case\""

tokenNew :: Parser (Token "new")
tokenNew = adapt TokNew "reserved word \"new\""

tokenDispose :: Parser (Token "dispose")
tokenDispose = adapt TokDispose "reserved word \"dispose\""

tokenQuestionMark :: Parser (Token "?")
tokenQuestionMark = adapt TokQM "?"

tokenAssign :: Parser (Token ":=")
tokenAssign = adapt TokAssign ":="

tokenEQ :: Parser (Token "=")
tokenEQ = adapt TokEQ "="

tokenGuardBar :: Parser (Token "|")
tokenGuardBar = adapt TokGuardBar "|"

tokenArrow :: Parser (Either (Token "->") (Token "→"))
tokenArrow =
  choice [Left <$> adapt TokArrow "->", Right <$> adapt TokArrowU "→"]

tokenUnderscore :: Parser (Token "_")
tokenUnderscore = adapt TokUnderscore "underscore \"_\""

--------------------------------------------------------------------------------
-- Declaration 
--------------------------------------------------------------------------------

declaration :: Parser Declaration
declaration = choice [constDecl, varDecl] <?> "declaration"

constDecl :: Parser Declaration
constDecl = ConstDecl <$> tokenConst <*> declType identifier

varDecl :: Parser Declaration
varDecl = VarDecl <$> tokenVar <*> declType identifier

-- `n : type` | `n : type { expr }` | `T a1 a2 ... = C1 ai1 ai2 .. | C2 ... | ...` | `n args = expr`
definition :: Parser Definition
definition = choice [try funcDefnSig, typeDefn, funcDefnF]
 where

  funcDefnSig :: Parser Definition
  funcDefnSig = FuncDefnSig <$> declBase identifier <*> optional declProp

  funcDefnF :: Parser Definition
  funcDefnF = FuncDefn <$> identifier <*> many lower <*> tokenEQ <*> expression

  -- `data T a1 a2 ... = C1 ai1 ai2 .. | C2 ... | ...`
  typeDefn :: Parser Definition
  typeDefn = TypeDefn <$> tokenData <*> upper <*> many lower <*> tokenEQ <*> sepByGuardBar typeDefnCtor

  typeDefnCtor :: Parser TypeDefnCtor
  typeDefnCtor = TypeDefnCtor <$> upper <*> many type'

definitionBlock :: Parser DefinitionBlock
definitionBlock = DefinitionBlock <$> tokenDeclOpen <*> sepByAlignmentOrSemi definition <*> tokenDeclClose

-- `n : type`
declBase :: Parser Name -> Parser DeclBase
declBase name = DeclBase <$> sepByComma name <*> tokenColon <*> type'

-- `{ expr }`
declProp :: Parser DeclProp
declProp = DeclProp <$> tokenBraceOpen <*> expression <*> tokenBraceClose

-- `n : type` | `n : type { expr }`
declType :: Parser Name -> Parser DeclType
declType name = DeclType <$> declBase name <*> optional declProp

--------------------------------------------------------------------------------
-- Statement 
--------------------------------------------------------------------------------

statement :: Parser Stmt
statement =
  choice
      [ skip
      , proofAnchors
      , abort
      , try assertion
      , loopInvariant
      , try assignment
      , try arrayAssignment
      , try alloc
      , try lookup
      , mutate
      , dispose
      , loop
      , conditional
      , hole
      , spec
      , programBlock
      ]
    <?> "statement"

-- ZERO or more statements
statements :: Parser [Stmt]
statements = sepByAlignmentOrSemi statement

-- ONE or more statements
statements1 :: Parser [Stmt]
statements1 = sepByAlignmentOrSemi1 statement

skip :: Parser Stmt
skip = withRange $ Skip <$ symbol TokSkip

abort :: Parser Stmt
abort = withRange $ Abort <$ symbol TokAbort

assertion :: Parser Stmt
assertion = Assert <$> tokenBraceOpen <*> expression <*> tokenBraceClose

loopInvariant :: Parser Stmt
loopInvariant = do
  LoopInvariant
    <$> tokenBraceOpen
    <*> predicate
    <*> tokenComma
    <*> tokenBnd
    <*> tokenColon
    <*> expression
    <*> tokenBraceClose

assignment :: Parser Stmt
assignment =
  Assign <$> sepByComma lower <*> tokenAssign <*> sepByComma expression

arrayAssignment :: Parser Stmt
arrayAssignment =
  AAssign
    <$> lower
    <*> tokenBracketOpen
    <*> expression
    <*> tokenBracketClose
    <*> tokenAssign
    <*> expression


loop :: Parser Stmt
loop = Do <$> tokenDo <* optional tokenGuardBar <*> sepByGuardBar guardedCommand <*> tokenOd

conditional :: Parser Stmt
conditional = If <$> tokenIf <* optional tokenGuardBar <*> sepByGuardBar guardedCommand <*> tokenFi

guardedCommand :: Parser GdCmd
guardedCommand = GdCmd <$> predicate <*> tokenArrow <*> sepByAlignmentOrSemi1 statement --blockOf statement

hole :: Parser Stmt
hole = SpecQM <$> (rangeOf <$> tokenQuestionMark)

spec :: Parser Stmt
spec =
  Spec
    <$> tokenSpecOpen
    <*> takeWhileP (Just "anything other than '!]'") notTokSpecClose
      -- Although here we directly use mega's method instead of our 'symbol' and 'extract',
      -- it is enclosed by parsers built with 'symbol'.
    <*> tokenSpecClose
 where
  notTokSpecClose :: L Tok -> Bool
  notTokSpecClose (L _ TokSpecClose) = False
  notTokSpecClose _                  = True

proofAnchors :: Parser Stmt
proofAnchors =
  Proof
    <$> tokenProofOpen
    <*> many proofAnchor
    <*> tokenProofClose
 where
  proofAnchor :: Parser ProofAnchor
  proofAnchor = do
    (hash, range) <- getRange $ extract extractHash
    skipProof
    return $ ProofAnchor hash range

  skipProof :: Parser ()
  skipProof = void $ takeWhileP
    (Just "anything other than '-]' or another proof anchor")
    notTokProofCloseOrProofAnchor

  notTokProofCloseOrProofAnchor :: L Tok -> Bool
  notTokProofCloseOrProofAnchor (L _ TokProofClose     ) = False
  notTokProofCloseOrProofAnchor (L _ (TokProofAnchor _)) = False
  notTokProofCloseOrProofAnchor _                        = True

  extractHash (TokProofAnchor s) = Just (Text.pack s)
  extractHash _                  = Nothing

alloc :: Parser Stmt
alloc =
  Alloc
    <$> lower
    <*> tokenAssign
    <*> tokenNew
    <*> tokenParenOpen
    <*> sepByComma expression
    <*> tokenParenClose


lookup :: Parser Stmt
lookup = HLookup <$> lower <*> tokenAssign <*> tokenStar <*> expression

mutate :: Parser Stmt
mutate = HMutate <$> tokenStar <*> expression <*> tokenAssign <*> expression

dispose :: Parser Stmt
dispose = Dispose <$> tokenDispose <*> expression

programBlock :: Parser Stmt
programBlock =
  Block
    <$> tokenBlockOpen
    <*> program
    <*> tokenBlockClose

program :: Parser Program
program = do

  mixed <- sepByAlignmentOrSemi (choice [Left <$> declOrDefnBlock, Right <$> statement])

  let (decls, stmts) = Either.partitionEithers mixed

  return $ Program decls stmts

--------------------------------------------------------------------------------
-- Expression 
--------------------------------------------------------------------------------

predicate :: Parser Expr
predicate = expression <?> "predicate"

expression :: Parser Expr
expression = makeExprParser (term <|> caseOf) opTable <?> "expression"
  -- An idea of handling the parsing of minus: 
  --  If the first symbol is "-", and the rest of the expression isn't wrapped inside parentheses, 
  --  then the first "-" belongs to the first symbol after "-".
 where
  opTable :: [[Operator Parser Expr]]
  opTable =
    [ 
      [InfixL (return App)] 
    , [ InfixL $ binary (ArithOp . Mod) TokMod
      , InfixL $ binary (ArithOp . Mul) TokMul
      , InfixL $ binary (ArithOp . Div) TokDiv
      , InfixN $ binary (ArithOp . Exp) TokExp
      ]

    , [ InfixL $ binary (ArithOp . Add) TokAdd
      , InfixL $ binary (ArithOp . Sub) TokSub
      ]

    , [ InfixN $ binary (ArithOp . Max) TokMax
      , InfixN $ binary (ArithOp . Min) TokMin
      ]

      -- =
    , [ InfixL $ binary (ChainOp . EQ) TokEQ
      -- ~, <, <=, >, >=
      , InfixL $ binary (ChainOp . NEQ) TokNEQ
      , InfixL $ binary (ChainOp . NEQU) TokNEQU
      , InfixL $ binary (ChainOp . LT) TokLT
      , InfixL $ binary (ChainOp . LTE) TokLTE
      , InfixL $ binary (ChainOp . LTEU) TokLTEU
      , InfixL $ binary (ChainOp . GT) TokGT
      , InfixL $ binary (ChainOp . GTE) TokGTE
      , InfixL $ binary (ChainOp . GTEU) TokGTEU
      ]

      --- &&
    , [ InfixL $ binary (ArithOp . Conj) TokConj
      , InfixL $ binary (ArithOp . ConjU) TokConjU
      --- ||
      , InfixL $ binary (ArithOp . Disj) TokDisj
      , InfixL $ binary (ArithOp . DisjU) TokDisjU
    ]
      
    
      -- =>
    , [ InfixR $ binary (ArithOp . Implies) TokImpl
      , InfixR $ binary (ArithOp . ImpliesU) TokImplU
      -- <=>
      , InfixL $ binary (ChainOp . EQProp) TokEQProp
      , InfixL $ binary (ChainOp . EQPropU) TokEQPropU
      ]

    , [ Prefix $ foldr1 (.) <$> some (unary (ArithOp . Neg) TokNeg
                                       <|> unary (ArithOp . NegU) TokNegU)
      , Prefix $ unary (ArithOp . NegNum) TokSub
      ]
    ]


  unary :: (Loc -> Op) -> Tok -> Parser (Expr -> Expr)
  unary operator' tok = do
    loc <- symbol tok
    return $ \result -> App (Expr.Op (operator' loc)) result

  binary :: (Loc -> Op) -> Tok -> Parser (Expr -> Expr -> Expr)
  binary operator' tok = do
    (op, loc) <- getLoc (operator' <$ symbol tok)
    return $ \x y -> App (App (Expr.Op (op loc)) x) y

  parensExpr :: Parser Expr
  parensExpr = Paren <$> tokenParenOpen <*> expression <*> tokenParenClose

  caseOf :: Parser Expr
  caseOf = Case <$> tokenCase <*> expression <*> tokenOf <*> sepByAlignmentOrSemi1 caseClause --blockOf caseClause


  caseClause :: Parser CaseClause
  caseClause = CaseClause <$> pattern' <*> tokenArrow <*> expression --block expression

  term :: Parser Expr
  term =
    choice
        [ Lit <$> literal
        , try array
        , parensExpr
        , Var <$> lower
        , Const <$> upper
        , Quant
        <$> choice [Left <$> tokenQuantOpen, Right <$> tokenQuantOpenU]
        <*> choice [Left <$> operator, Right <$> identifier]
        <*> some lower
        <*> tokenColon
        <*> expression
        <*> tokenColon
        <*> expression
        <*> choice [Left <$> tokenQuantClose, Right <$> tokenQuantCloseU]
        ]
      <?> "term"

  -- shoule parse A[A[i]], A[i1][i2]...[in]
  array :: Parser Expr
  array = do
    arr     <- choice [parensExpr, Var <$> lower, Const <$> upper]
    indices <- some $ do
      open  <- tokenBracketOpen
      xs    <- term
      close <- tokenBracketClose
      return (open, xs, close)
    return $ helper arr indices
    where
    helper :: Expr -> [(Token "[", Expr, Token "]")] -> Expr
    helper a []               = a
    helper a ((o, x, c) : xs) = helper (Arr a o x c) xs

  operator :: Parser Op
  operator = choice [ChainOp <$> chainOp, ArithOp <$> arithOp] <?> "operator"
   where
    chainOp :: Parser ChainOp
    chainOp = choice
      [ EQProp <$> symbol TokEQProp
      , EQPropU <$> symbol TokEQPropU
      , EQ <$> symbol TokEQ
      , NEQ <$> symbol TokNEQ
      , NEQU <$> symbol TokNEQU
      , LTE <$> symbol TokLTE
      , LTEU <$> symbol TokLTEU
      , GTE <$> symbol TokGTE
      , GTEU <$> symbol TokGTEU
      , LT <$> symbol TokLT
      , GT <$> symbol TokGT
      ]

    arithOp :: Parser ArithOp
    arithOp = choice
      [ Implies <$> symbol TokImpl
      , ImpliesU <$> symbol TokImplU
      , Conj <$> symbol TokConj
      , ConjU <$> symbol TokConjU
      , Disj <$> symbol TokDisj
      , DisjU <$> symbol TokDisjU
      , Neg <$> symbol TokNeg
      , NegU <$> symbol TokNegU
      , Add <$> symbol TokAdd
      , Sub <$> symbol TokSub
      , Mul <$> symbol TokMul
      , Div <$> symbol TokDiv
      , Mod <$> symbol TokMod
      , Max <$> symbol TokMax
      , Min <$> symbol TokMin
      , Exp <$> symbol TokExp
      , Add <$> symbol TokSum
      , Mul <$> symbol TokProd
      , Conj <$> symbol TokForall
      , Disj <$> symbol TokExist
      , Hash <$> symbol TokHash
      ]

-- TODO: LitChar 
literal :: Parser Lit
literal =
  withRange
      (choice
        [ LitBool True <$ symbol TokTrue
        , LitBool False <$ symbol TokFalse
        , LitInt <$> integer
        , LitChar <$> character
        ]
      )
    <?> "literal"

pattern' :: Parser Pattern
pattern' = choice
  [ PattLit <$> literal
  , PattParen <$> tokenParenOpen <*> pattern' <*> tokenParenClose
  , PattWildcard <$> tokenUnderscore
  , PattBinder <$> lower
  , PattConstructor <$> upper <*> many pattern'
  ]

--------------------------------------------------------------------------------
-- Type 
--------------------------------------------------------------------------------

type' :: Parser Type
type' = do
  makeExprParser term table <?> "type"
 where
  table :: [[Operator Parser Type]]
  table = [[InfixR function]]

  function :: Parser (Type -> Type -> Type)
  function = do
    arrow <- tokenArrow
    return $ \x y -> TFunc x arrow y

  term :: Parser Type
  term = parensType <|> array <|> try typeVar <|> typeName <?> "type term"

  parensType :: Parser Type
  parensType = TParen <$> tokenParenOpen <*> type' <*> tokenParenClose


  typeVar :: Parser Type
  typeVar = TVar <$> lower

  typeName :: Parser Type
  typeName = TCon <$> upper <*> many lower

  array :: Parser Type
  array = TArray <$> tokenArray <*> interval <*> tokenOf <*> type'

  interval :: Parser Interval
  interval = Interval <$> endpointOpening <*> tokenRange <*> endpointClosing

  endpointOpening :: Parser EndpointOpen
  endpointOpening = choice
    [ IncludingOpening <$> tokenBracketOpen <*> expression
    , ExcludingOpening <$> tokenParenOpen <*> expression
    ]

  endpointClosing :: Parser EndpointClose
  endpointClosing = do
    expr <- expression
    choice
      [ IncludingClosing expr <$> tokenBracketClose
      , ExcludingClosing expr <$> tokenParenClose
      ]

--------------------------------------------------------------------------------

upperName :: Parser Text
upperName = extract p
 where
  p (TokUpperName s) = Just s
  p _                = Nothing

upper :: Parser Name
upper =
  withLoc (Name <$> upperName)
    <?> "identifier that starts with a uppercase letter"

lowerName :: Parser Text
lowerName = extract p
 where
  p (TokLowerName s) = Just s
  p _                = Nothing

lower :: Parser Name
lower =
  withLoc (Name <$> lowerName)
    <?> "identifier that starts with a lowercase letter"

identifier :: Parser Name
identifier =
  withLoc (choice [Name <$> lowerName, Name <$> upperName]) <?> "identifier"

integer :: Parser Int
integer = extract p <?> "integer"
 where
  p (TokInt s) = Just s
  p _          = Nothing

character :: Parser Char
character = extract p <?> "character"
 where
  p (TokChar c) = Just c
  p _           = Nothing
