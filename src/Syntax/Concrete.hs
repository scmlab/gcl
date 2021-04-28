{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}

module Syntax.Concrete where

-- import Syntax.Parser.Lexer (Tok (..))

import Control.Monad.Except
import Data.Loc (Loc (..), Located (locOf), Pos, (<-->))
import Data.Text (Text)
import GHC.Base (Symbol)
import GHC.Generics (Generic)
import qualified Syntax.Abstract as A
import Syntax.Common
import qualified Syntax.ConstExpr as ConstExpr
import Prelude hiding (Ordering (..))

--------------------------------------------------------------------------------

-- | Typeclass for converting Syntax.Concrete to Syntax.Abstract
class ToAbstract a b | a -> b where
  toAbstract :: a -> Except Loc b

--------------------------------------------------------------------------------

-- | A Token with start & ending Pos
data Token (a :: Symbol) = Token Pos Pos
  deriving (Eq, Show)

instance Located (Token a) where
  locOf (Token l r) = l <--> r

instance (Located a, Located b) => Located (Either a b) where
  locOf (Left x) = locOf x
  locOf (Right x) = locOf x

-- | A non-empty list of stuff seperated by commas
data SepBy (sep :: Symbol) a = Head a | Delim a (Token sep) (SepBy sep a)
  deriving (Eq, Show)

fromSepBy :: SepBy sep a -> [a]
fromSepBy (Head a) = [a]
fromSepBy (Delim a _ as) = a : fromSepBy as

instance Located a => Located (SepBy sep a) where
  locOf (Head a) = locOf a
  locOf (Delim a _ as) = a <--> locOf as

--------------------------------------------------------------------------------

-- | Program / Declaration / Statement
data Program
  = Program
      [Either Declaration BlockDeclaration] -- constant and variable declarations
      [Stmt] -- main program
  deriving (Eq, Show)

instance ToAbstract Program A.Program where
  toAbstract (Program decls' stmts') = do
    declss <- forM decls' $ \decl -> case decl of
      Left d -> (: []) <$> toAbstract d
      Right d -> toAbstract d
    let decls = concat declss
    let letBindings = ConstExpr.pickLetBindings decls
    let (globProps, assertions) = ConstExpr.pickGlobals decls
    let pre = [A.Assert (A.conjunct assertions) NoLoc | not (null assertions)]
    stmts <- mapM toAbstract stmts'

    return $ A.Program decls globProps letBindings (pre ++ stmts) (decls' <--> stmts)

instance Located Program where
  locOf (Program a b) = a <--> b

data Decl = Decl (SepBy "," Name) (Token ":") Type deriving (Eq, Show)

instance ToAbstract Decl ([Name], A.Type) where
  toAbstract (Decl a _ b) = do
    b' <- toAbstract b
    return (fromSepBy a, b')

instance Located Decl where
  locOf (Decl l _ r) = l <--> r

data DeclProp = DeclProp (Token "{") Expr (Token "}") deriving (Eq, Show)

instance ToAbstract DeclProp A.Expr where
  toAbstract (DeclProp _ e _) = toAbstract e

instance Located DeclProp where
  locOf (DeclProp l _ r) = l <--> r

data DeclBody = DeclBody Name [Name] (Token "=") Expr deriving (Eq, Show)

instance ToAbstract DeclBody (Name, [Name], A.Expr) where
  toAbstract (DeclBody n args _ b) = do
    b' <- toAbstract b
    return (n, args, b')

instance Located DeclBody where
  locOf (DeclBody l _ _ r) = l <--> r

data Declaration
  = ConstDecl (Token "con") Decl
  | ConstDeclWithProp (Token "con") Decl DeclProp
  | VarDecl (Token "var") Decl
  | VarDeclWithProp (Token "var") Decl DeclProp
  | LetDecl (Token "let") DeclBody
  deriving (Eq, Show)

instance ToAbstract Declaration A.Declaration where
  toAbstract declaration = case declaration of
    ConstDecl _ decl -> do
      (name, body) <- toAbstract decl
      return $ A.ConstDecl name body Nothing (locOf decl)
    ConstDeclWithProp _ decl prop -> do
      (name, body) <- toAbstract decl
      prop' <- toAbstract prop
      return $ A.ConstDecl name body (Just prop') (locOf decl)
    VarDecl _ decl -> do
      (name, body) <- toAbstract decl
      return $ A.VarDecl name body Nothing (locOf decl)
    VarDeclWithProp _ decl prop -> do
      (name, body) <- toAbstract decl
      prop' <- toAbstract prop
      return $ A.VarDecl name body (Just prop') (locOf decl)
    LetDecl _ decl -> do
      (name, args, body) <- toAbstract decl
      return $ A.LetDecl name args body (locOf decl)

instance Located Declaration where
  locOf (ConstDecl l r) = l <--> r
  locOf (ConstDeclWithProp l _ r) = l <--> r
  locOf (VarDecl l r) = l <--> r
  locOf (VarDeclWithProp l _ r) = l <--> r
  locOf (LetDecl l r) = l <--> r

data BlockDecl = BlockDecl Decl (Maybe (Either DeclProp Expr)) (Maybe DeclBody) deriving (Eq, Show)

-- One BlockDecl can be parse into a ConstDecl or a ConstDecl and a LetDecl
instance ToAbstract BlockDecl [A.Declaration] where
  toAbstract declaration = case declaration of
    BlockDecl decl Nothing Nothing -> do
      (names, type') <- toAbstract decl
      return [A.ConstDecl names type' Nothing (locOf declaration)]
    BlockDecl decl Nothing (Just declBody) -> do
      (names, type') <- toAbstract decl
      (declBodyName, declBodyArgs, declBody') <- toAbstract declBody
      return [A.ConstDecl names type' Nothing (locOf declaration), A.LetDecl declBodyName declBodyArgs declBody' (locOf declBody)]
    BlockDecl decl (Just (Left declProp)) Nothing -> do
      (names, type') <- toAbstract decl
      prop <- toAbstract declProp
      return [A.ConstDecl names type' (Just prop) (locOf declaration)]
    BlockDecl decl (Just (Right prop)) Nothing -> do
      (names, type') <- toAbstract decl
      prop' <- toAbstract prop
      return [A.ConstDecl names type' (Just prop') (locOf declaration)]
    BlockDecl decl (Just (Left declProp)) (Just declBody) -> do
      (names, type') <- toAbstract decl
      prop <- toAbstract declProp
      (declBodyName, declBodyArgs, declBody') <- toAbstract declBody
      return [A.ConstDecl names type' (Just prop) (locOf declaration), A.LetDecl declBodyName declBodyArgs declBody' (locOf declBody)]
    BlockDecl decl (Just (Right prop)) (Just declBody) -> do
      (names, type') <- toAbstract decl
      prop' <- toAbstract prop
      (declBodyName, declBodyArgs, declBody') <- toAbstract declBody
      return [A.ConstDecl names type' (Just prop') (locOf declaration), A.LetDecl declBodyName declBodyArgs declBody' (locOf declBody)]

instance Located BlockDecl where
  locOf (BlockDecl l _ r) = l <--> r

data BlockDeclaration = BlockDeclaration (Token "{:") [BlockDecl] (Token ":}") deriving (Eq, Show)

instance ToAbstract BlockDeclaration [A.Declaration] where
  toAbstract (BlockDeclaration _ decls _) = concat <$> mapM toAbstract decls

instance Located BlockDeclaration where
  locOf (BlockDeclaration l _ r) = l <--> r

data Stmt
  = Skip Loc
  | Abort Loc
  | Assign (SepBy "," Name) (Token ":=") (SepBy "," Expr)
  | Assert (Token "{") Expr (Token "}")
  | LoopInvariant (Token "{") Expr (Token ",") (Token "bnd") (Token ":") Expr (Token "}")
  | Do (Token "do") (SepBy "|" GdCmd) (Token "od")
  | If (Token "if") (SepBy "|" GdCmd) (Token "fi")
  | SpecQM Loc -- ? to be rewritten as {!!} by the frontend
  | Spec (Token "[!") Text (Token "!]")
  | Proof (Token "{-") (Token "-}")
  deriving (Eq, Show)

instance ToAbstract Stmt A.Stmt where
  toAbstract stmt = case stmt of
    Skip l -> pure (A.Skip l)
    Abort l -> pure (A.Abort l)
    Assign a _ b -> A.Assign (fromSepBy a) <$> mapM toAbstract (fromSepBy b) <*> pure (a <--> b)
    Assert l a r -> A.Assert <$> toAbstract a <*> pure (l <--> r)
    LoopInvariant l a _ _ _ b r -> A.LoopInvariant <$> toAbstract a <*> toAbstract b <*> pure (l <--> r)
    Do l a r -> A.Do <$> mapM toAbstract (fromSepBy a) <*> pure (l <--> r)
    If l a r -> A.If <$> mapM toAbstract (fromSepBy a) <*> pure (l <--> r)
    SpecQM l -> throwError l
    Spec l t r -> pure (A.Spec t (l <--> r))
    Proof l r -> pure (A.Proof (l <--> r))

instance Located Stmt where
  locOf (Skip l) = l
  locOf (Abort l) = l
  locOf (Assign l _ r) = l <--> r
  locOf (Assert l _ r) = l <--> r
  locOf (LoopInvariant l _ _ _ _ _ r) = l <--> r
  locOf (Do l _ r) = l <--> r
  locOf (If l _ r) = l <--> r
  locOf (SpecQM l) = l
  locOf (Spec l _ r) = l <--> r
  locOf (Proof l r) = l <--> r

data GdCmd = GdCmd Expr (Either (Token "->") (Token "→")) [Stmt] deriving (Eq, Show)

instance ToAbstract GdCmd A.GdCmd where
  toAbstract (GdCmd a _ b) = A.GdCmd <$> toAbstract a <*> mapM toAbstract b <*> pure (a <--> b)

--------------------------------------------------------------------------------

-- | Interval
data EndpointOpen
  = IncludingOpening (Token "[") Expr
  | ExcludingOpening (Token "(") Expr
  deriving (Eq, Show)

data EndpointClose
  = IncludingClosing Expr (Token "]")
  | ExcludingClosing Expr (Token ")")
  deriving (Eq, Show)

instance ToAbstract EndpointOpen A.Endpoint where
  toAbstract (IncludingOpening _ a) = A.Including <$> toAbstract a
  toAbstract (ExcludingOpening _ a) = A.Excluding <$> toAbstract a

instance ToAbstract EndpointClose A.Endpoint where
  toAbstract (IncludingClosing a _) = A.Including <$> toAbstract a
  toAbstract (ExcludingClosing a _) = A.Excluding <$> toAbstract a

instance Located EndpointOpen where
  locOf (IncludingOpening l e) = l <--> e
  locOf (ExcludingOpening l e) = l <--> e

instance Located EndpointClose where
  locOf (IncludingClosing e l) = e <--> l
  locOf (ExcludingClosing e l) = e <--> l

-- | Interval
data Interval = Interval EndpointOpen (Token "..") EndpointClose deriving (Eq, Show)

instance ToAbstract Interval A.Interval where
  toAbstract (Interval a _ b) = A.Interval <$> toAbstract a <*> toAbstract b <*> pure (a <--> b)

instance Located Interval where
  locOf (Interval l _ r) = l <--> r

-- | Base Type
data TBase
  = TInt Loc
  | TBool Loc
  | TChar Loc
  deriving (Eq, Show)

instance Located TBase where
  locOf (TInt l) = l
  locOf (TBool l) = l
  locOf (TChar l) = l

instance ToAbstract TBase A.TBase where
  toAbstract (TInt _) = pure A.TInt
  toAbstract (TBool _) = pure A.TBool
  toAbstract (TChar _) = pure A.TChar

-- | Type
data Type
  = TParen (Token "(") Type (Token ")")
  | TBase TBase
  | TArray (Token "array") Interval (Token "of") Type
  | TFunc Type (Either (Token "->") (Token "→")) Type
  | TVar Name
  deriving (Eq, Show)

instance ToAbstract Type A.Type where
  toAbstract (TParen _ a _) = toAbstract a
  toAbstract (TBase a) = A.TBase <$> toAbstract a <*> pure (locOf a)
  toAbstract (TArray l a _ b) = A.TArray <$> toAbstract a <*> toAbstract b <*> pure (l <--> b)
  toAbstract (TFunc a _ b) = A.TFunc <$> toAbstract a <*> toAbstract b <*> pure (a <--> b)
  toAbstract (TVar a) = pure $ A.TVar a (locOf a)

instance Located Type where
  locOf (TParen l _ r) = l <--> r
  locOf (TBase a) = locOf a
  locOf (TArray l _ _ r) = l <--> r
  locOf (TFunc l _ r) = l <--> r
  locOf (TVar x) = locOf x

--------------------------------------------------------------------------------

-- | Expressions
data Expr
  = Paren (Token "(") Expr (Token ")")
  | Lit Lit
  | Var Name
  | Const Name
  | Op Op
  | Chain Expr Op Expr -- Left Associative
  | Arr Expr (Token "[") Expr (Token "]")
  | App Expr Expr
  | Quant
      (Either (Token "<|") (Token "⟨"))
      (Either Op Expr)
      [Name]
      (Token ":")
      Expr
      (Token ":")
      Expr
      (Either (Token "|>") (Token "⟩"))
  deriving (Eq, Show, Generic)

instance Located Expr where
  locOf (Paren l _ r) = l <--> r
  locOf (Lit x) = locOf x
  locOf (Var x) = locOf x
  locOf (Const x) = locOf x
  locOf (Op x) = locOf x
  locOf (Chain e1 op e2) = e1 <--> op <--> e2
  locOf (Arr l _ _ r) = l <--> r
  locOf (App x y) = x <--> y
  locOf (Quant l _ _ _ _ _ _ r) = l <--> r

instance ToAbstract Expr A.Expr where
  toAbstract x = case x of
    Paren _ a _ -> toAbstract a
    Lit a -> A.Lit <$> toAbstract a <*> pure (locOf x)
    Var a -> pure $ A.Var a (locOf x)
    Const a -> pure $ A.Const a (locOf x)
    Op a -> A.Op <$> toAbstract a <*> pure (locOf x)
    Chain a op b ->
      case a of
        Chain {} -> do
          let ar = chainRightmost a
          A.App
            <$> ( A.App (A.Op A.Conj NoLoc) <$> toAbstract a
                    <*> pure (locOf a)
                )
            -- ar op b
            <*> ( A.App <$> (A.App <$> toAbstract (Op op) <*> toAbstract ar <*> pure (locOf ar))
                    <*> toAbstract b
                    <*> pure (ar <--> b)
                )
            <*> pure (locOf x)
        _ ->
          A.App <$> (A.App <$> toAbstract (Op op) <*> toAbstract a <*> pure (a <--> op)) <*> toAbstract b <*> pure (locOf x)
    Arr arr _ i _ -> A.App <$> toAbstract arr <*> toAbstract i <*> pure (locOf x)
    App a b -> A.App <$> toAbstract a <*> toAbstract b <*> pure (locOf x)
    Quant _ a b _ c _ d _ -> A.Quant <$> either (toAbstract . Op) toAbstract a <*> pure b <*> toAbstract c <*> toAbstract d <*> pure (locOf x)

chainRightmost :: Expr -> Expr
chainRightmost (Chain _ _ b) = chainRightmost b
chainRightmost expr = expr

--------------------------------------------------------------------------------

-- | Literals (Integer / Boolean / Character)
data Lit = LitInt Int Loc | LitBool Bool Loc | LitChar Char Loc
  deriving (Show, Eq, Generic)

instance ToAbstract Lit A.Lit where
  toAbstract (LitInt a _) = pure $ A.Num a
  toAbstract (LitBool a _) = pure $ A.Bol a
  toAbstract (LitChar a _) = pure $ A.Chr a

instance Located Lit where
  locOf (LitInt _ l) = l
  locOf (LitBool _ l) = l
  locOf (LitChar _ l) = l

--------------------------------------------------------------------------------

-- | Operators
data Op
  = -- binary relations
    EQ Loc
  | NEQ Loc
  | NEQU Loc
  | LTE Loc
  | LTEU Loc
  | GTE Loc
  | GTEU Loc
  | LT Loc
  | GT Loc
  | -- logic operators
    Implies Loc
  | ImpliesU Loc
  | Conj Loc
  | ConjU Loc
  | Disj Loc
  | DisjU Loc
  | -- arithmetics
    Neg Loc
  | NegU Loc
  | Add Loc
  | Sub Loc
  | Mul Loc
  | Div Loc
  | Mod Loc
  | -- For Quant
    Sum Loc
  | Forall Loc
  | Exists Loc
  deriving (Show, Eq, Generic)

instance ToAbstract Op A.Op where
  toAbstract (EQ _) = pure A.EQ
  toAbstract (NEQ _) = pure A.NEQ
  toAbstract (NEQU _) = pure A.NEQ
  toAbstract (LTE _) = pure A.LTE
  toAbstract (LTEU _) = pure A.LTE
  toAbstract (GTE _) = pure A.GTE
  toAbstract (GTEU _) = pure A.GTE
  toAbstract (LT _) = pure A.LT
  toAbstract (GT _) = pure A.GT
  toAbstract (Implies _) = pure A.Implies
  toAbstract (ImpliesU _) = pure A.Implies
  toAbstract (Conj _) = pure A.Conj
  toAbstract (ConjU _) = pure A.Conj
  toAbstract (Disj _) = pure A.Disj
  toAbstract (DisjU _) = pure A.Disj
  toAbstract (Neg _) = pure A.Neg
  toAbstract (NegU _) = pure A.Neg
  toAbstract (Add _) = pure A.Add
  toAbstract (Sub _) = pure A.Sub
  toAbstract (Mul _) = pure A.Mul
  toAbstract (Div _) = pure A.Div
  toAbstract (Mod _) = pure A.Mod
  toAbstract (Sum _) = pure A.Sum
  toAbstract (Forall _) = pure A.Forall
  toAbstract (Exists _) = pure A.Exists

instance Located Op where
  locOf (Implies l) = l
  locOf (ImpliesU l) = l
  locOf (Disj l) = l
  locOf (DisjU l) = l
  locOf (Conj l) = l
  locOf (ConjU l) = l
  locOf (Neg l) = l
  locOf (NegU l) = l
  locOf (EQ l) = l
  locOf (NEQ l) = l
  locOf (NEQU l) = l
  locOf (LTE l) = l
  locOf (LTEU l) = l
  locOf (GTE l) = l
  locOf (GTEU l) = l
  locOf (LT l) = l
  locOf (GT l) = l
  locOf (Add l) = l
  locOf (Sub l) = l
  locOf (Mul l) = l
  locOf (Div l) = l
  locOf (Mod l) = l
  locOf (Sum l) = l
  locOf (Exists l) = l
  locOf (Forall l) = l

classify :: Op -> Fixity
classify (Implies _) = InfixR 1
classify (ImpliesU _) = InfixR 1
classify (Disj _) = InfixL 2
classify (DisjU _) = InfixL 2
classify (Conj _) = InfixL 3
classify (ConjU _) = InfixL 3
classify (Neg _) = Prefix 4
classify (NegU _) = Prefix 4
classify (EQ _) = Infix 5
classify (NEQ _) = Infix 6
classify (NEQU _) = Infix 6
classify (LTE _) = Infix 6
classify (LTEU _) = Infix 6
classify (GTE _) = Infix 6
classify (GTEU _) = Infix 6
classify (LT _) = Infix 6
classify (GT _) = Infix 6
classify (Add _) = InfixL 7
classify (Sub _) = InfixL 7
classify (Mul _) = InfixL 8
classify (Div _) = InfixL 8
classify (Mod _) = InfixL 9
classify (Sum _) = Prefix 5
classify (Exists _) = Prefix 6
classify (Forall _) = Prefix 7
