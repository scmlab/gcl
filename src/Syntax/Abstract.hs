{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Syntax.Abstract where

import Control.Monad.State
import Data.Text (Text)
import Data.Map (Map)

import qualified Syntax.Concrete as C

type Index = Int

data Program = Program [Declaration] Stmt
  deriving (Show)

data Declaration
  = ConstDecl [Const] Type
  | VarDecl [Var] Type
  deriving (Show)

data Stmt
  = Skip
  | Abort
  | Seq     Stmt Stmt
  | Assign  [Var] [Expr]
  | Assert  Pred
  | Do      (Maybe Pred) Expr [GdCmd]
  | If      (Maybe Pred)      [GdCmd]
  deriving (Show)

data GdCmd = GdCmd Pred [Stmt] deriving (Show)

--------------------------------------------------------------------------------
-- | Predicates

data BinRel = Eq | LEq | GEq | LTh | GTh
  deriving (Show, Eq)

data Pred = Term    Expr BinRel Expr
          | Implies Pred Pred
          | Conj    Pred Pred
          | Disj    Pred Pred
          | Neg     Pred
          | Hole    Index
          deriving (Show)

--------------------------------------------------------------------------------
-- | Expressions

data Lit  = Num Int
          | Bol Bool
          deriving Show

type OpName = Text
data Expr = VarE    Var
          | ConstE  Const
          | LitE    Lit
          | OpE     OpName [Expr]
          | HoleE   Index  [Subst]
          deriving Show

type Subst = Map String Expr

--------------------------------------------------------------------------------
-- | Variables and stuff

type Const = Text
type Var = Text
type Type = Text

--------------------------------------------------------------------------------
-- Converting from Concrete Syntax Tree

type AbstractM = State Index

abstractProgram :: C.Program -> Program
abstractProgram = runAbstractM . fromConcrete

runAbstractM :: AbstractM a -> a
runAbstractM f = evalState f 0

-- returns the current index and increment it in the state
index :: AbstractM Index
index = do
  i <- get
  put (succ i)
  return i


class FromConcrete a b | a -> b where
  fromConcrete :: a -> AbstractM b

instance FromConcrete C.Lit Lit where
  fromConcrete (C.Num x) = Num <$> pure x
  fromConcrete (C.Bol x) = Bol <$> pure x

instance FromConcrete C.Const Const where
  fromConcrete (C.Const x _) = pure x

instance FromConcrete C.Var Var where
  fromConcrete (C.Var x _) = pure x

instance FromConcrete C.Type Type where
  fromConcrete (C.Type x _) = pure x

instance FromConcrete C.Expr Expr where
  fromConcrete (C.VarE x    _) = VarE   <$> fromConcrete x
  fromConcrete (C.ConstE x  _) = ConstE <$> fromConcrete x
  fromConcrete (C.LitE x    _) = LitE   <$> fromConcrete x
  fromConcrete (C.OpE x xs  _) = OpE    <$> pure x <*> mapM fromConcrete xs
  fromConcrete (C.HoleE     _) = HoleE  <$> index <*> pure []

instance FromConcrete C.BinRel BinRel where
  fromConcrete (C.Eq  _) = pure Eq
  fromConcrete (C.LEq _) = pure LEq
  fromConcrete (C.GEq _) = pure GEq
  fromConcrete (C.LTh _) = pure LTh
  fromConcrete (C.GTh _) = pure GTh

instance FromConcrete C.Pred Pred where
  fromConcrete (C.Term p r q  _) = Term     <$> fromConcrete p
                                            <*> fromConcrete r
                                            <*> fromConcrete q
  fromConcrete (C.Implies p q _) = Implies  <$> fromConcrete p
                                            <*> fromConcrete q
  fromConcrete (C.Conj p q    _) = Conj     <$> fromConcrete p
                                            <*> fromConcrete q
  fromConcrete (C.Disj p q    _) = Disj     <$> fromConcrete p
                                            <*> fromConcrete q
  fromConcrete (C.Neg p       _) = Neg      <$> fromConcrete p
  fromConcrete (C.Hole        _) = Hole     <$> index

instance FromConcrete C.GdCmd GdCmd where
  fromConcrete (C.GdCmd p q _) = GdCmd  <$> fromConcrete p
                                        <*> mapM fromConcrete q

instance FromConcrete C.Stmt Stmt where
  fromConcrete (C.Skip       _) = pure Skip
  fromConcrete (C.Abort      _) = pure Abort
  fromConcrete (C.Assign p q _) = Assign  <$> mapM fromConcrete p
                                          <*> mapM fromConcrete q
  fromConcrete (C.Assert p   _) = Assert  <$> fromConcrete p
  fromConcrete (C.Do     p q _) = Do      <$> pure Nothing
                                          <*> fromConcrete p
                                          <*> mapM fromConcrete q
  fromConcrete (C.If     p   _) = If      <$> pure Nothing
                                          <*> mapM fromConcrete p

instance FromConcrete C.Declaration Declaration where
  fromConcrete (C.ConstDecl p q _) = ConstDecl  <$> mapM fromConcrete p
                                                <*> fromConcrete q
  fromConcrete (C.VarDecl   p q _) = VarDecl    <$> mapM fromConcrete p
                                                <*> fromConcrete q

instance FromConcrete C.Program Program where
  fromConcrete (C.Program p q _) = Program  <$> mapM fromConcrete p
                                            <*> (seqAll <$> mapM fromConcrete q)
    where
      seqAll :: [Stmt] -> Stmt
      seqAll [] = Skip
      seqAll (x:xs) = foldl Seq x xs
