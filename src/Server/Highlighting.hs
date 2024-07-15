{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module Server.Highlighting
  ( Highlighting
  , collectHighlighting
  ) where

import           Control.Monad.RWS
import           Data.Foldable                  ( toList )
import           Data.Loc                       ( Located(locOf)
                                                , posCol
                                                , posLine
                                                )
import           Data.Loc.Range
import qualified Language.LSP.Types               as J
import           Server.IntervalMap                ( Collect(..)
                                                , M
                                                , runM
                                                )
import qualified Server.IntervalMap               as IntervalMap
import           Syntax.Common
import           Syntax.Concrete

type Highlighting = J.SemanticTokenAbsolute

collectHighlighting :: Program -> [Highlighting]
collectHighlighting program =
  toList $ runM mempty (collect program :: M () Highlighting ())

--------------------------------------------------------------------------------
-- helper function for converting some syntax node to Highlighting

addHighlighting
  :: Located a
  => J.SemanticTokenTypes
  -> [J.SemanticTokenModifiers]
  -> a
  -> M () Highlighting ()
addHighlighting types modifiers node = case fromLoc (locOf node) of
  Nothing    -> return ()
  Just range -> tell $ IntervalMap.singleton range $ J.SemanticTokenAbsolute
    (posLine (rangeStart range) - 1)
    (posCol (rangeStart range) - 1)
    (rangeSpan range)
    types
    modifiers

--------------------------------------------------------------------------------
-- newtypes for giving common datatype like `Name` different interpretations

newtype AsConstructor = AsConstructor Name

instance Collect () Highlighting AsConstructor where
  collect (AsConstructor a) = addHighlighting J.SttEnumMember [] a

newtype AsVariable = AsVariable Name

instance Collect () Highlighting AsVariable where
  collect (AsVariable a) = addHighlighting J.SttVariable [] a

newtype AsName = AsName Name

instance Collect () Highlighting AsName where
  collect (AsName a) = addHighlighting J.SttFunction [] a

--------------------------------------------------------------------------------
-- Program

instance Collect () Highlighting Program where
  collect (Program as bs) = do
    collect as
    collect bs

--------------------------------------------------------------------------------
-- Definition

instance Collect () Highlighting DefinitionBlock where
  collect (DefinitionBlock _tokA as _tokB) = collect as

instance Collect () Highlighting Definition where
  collect (TypeDefn tokData name binders _tokDef bs) = do
    addHighlighting J.SttKeyword   [] tokData
    addHighlighting J.SttType      [] name
    addHighlighting J.SttParameter [] binders
    collect bs

  collect (FuncDefnSig a b) = do
    collect a
    collect b
  collect (FuncDefn a bs _tok c) = do
    addHighlighting J.SttFunction [J.StmDeclaration] a
    collect (fmap AsVariable bs)
    collect c

--------------------------------------------------------------------------------
-- Declaration

instance Collect () Highlighting Declaration where
  collect (ConstDecl tok a) = do
    addHighlighting J.SttKeyword [] tok
    collect a
  collect (VarDecl tok a) = do
    addHighlighting J.SttKeyword [] tok
    collect a

instance Collect () Highlighting TypeDefnCtor where
  collect (TypeDefnCtor name _) = do
    collect (AsName name)

instance Collect () Highlighting DeclBase where
  collect (DeclBase as _ b) = do
    collect (fmap AsVariable as)
    collect b

instance Collect () Highlighting DeclProp where
  collect (DeclProp _tokA a _tokB) = collect a
instance Collect () Highlighting DeclType where
  collect (DeclType a b) = do
    collect a
    collect b

--------------------------------------------------------------------------------
-- Stmt

instance Collect () Highlighting Stmt where
  collect :: Stmt -> M () Highlighting ()
  collect = \case
    Skip  x          -> addHighlighting J.SttKeyword [] x
    Abort x          -> addHighlighting J.SttKeyword [] x
    Assign as tok bs -> do
      collect (fmap AsVariable as)
      addHighlighting J.SttKeyword [] tok
      collect bs
    AAssign a _ b _ tok c -> do
      collect (AsVariable a)
      addHighlighting J.SttKeyword [] tok
      collect b
      collect c
    Assert _ a _                  -> collect a
    LoopInvariant _ a _ tok _ b _ -> do
      collect a
      addHighlighting J.SttKeyword [] tok
      collect b
    Do tokA as tokB -> do
      addHighlighting J.SttKeyword [] tokA
      collect as
      addHighlighting J.SttKeyword [] tokB
    If tokA as tokB -> do
      addHighlighting J.SttKeyword [] tokA
      collect as
      addHighlighting J.SttKeyword [] tokB
    SpecQM _         -> return ()
    Spec tokA _ tokB -> do
      addHighlighting J.SttKeyword [] tokA
      addHighlighting J.SttKeyword [] tokB
    Proof _ _ _ range -> do
      addHighlighting J.SttKeyword [] range
      -- addHighlighting J.SttKeyword [] tokA
      -- addHighlighting J.SttKeyword [] tokB
    Alloc a tok tokNew _ bs _ -> do
      collect (AsVariable a)
      addHighlighting J.SttKeyword []                  tok
      addHighlighting J.SttKeyword [J.StmModification] tokNew
      collect bs
    HLookup a tok tokStar b -> do
      collect (AsVariable a)
      addHighlighting J.SttKeyword []                  tok
      addHighlighting J.SttKeyword [J.StmModification] tokStar
      collect b
    HMutate tokStar a tok b -> do
      addHighlighting J.SttKeyword [J.StmModification] tokStar
      collect a
      addHighlighting J.SttKeyword [] tok
      collect b
    Dispose tok a -> do
      addHighlighting J.SttKeyword [] tok
      collect a
    -- TODO:
    Block{} -> return ()

instance Collect () Highlighting GdCmd where
  collect (GdCmd a tok bs) = do
    collect a
    addHighlighting J.SttMacro [] tok
    collect bs

--------------------------------------------------------------------------------

instance Collect () Highlighting Expr where
  collect = \case
    Paren _ a _ -> collect a
    Lit   a     -> collect a
    Var   a     -> collect (AsVariable a)
    Const a     -> collect (AsVariable a)
    Op    a     -> addHighlighting J.SttOperator [] a
    Chain ch    -> case ch of
      Pure expr -> collect expr
      More ch' op expr -> do
        collect (Chain ch')
        addHighlighting J.SttOperator [] op
        collect expr
    Arr a _ b _ -> do
      collect a
      collect b
    App a@App{} b -> do
      collect a
      collect b
    App (Const a) b -> do
      collect (AsName a)
      collect b
    App a b -> do
      collect a
      collect b
    Quant tokA op names tokB a tokC b tokD -> do
      addHighlighting J.SttKeyword  [] tokA
      addHighlighting J.SttOperator [] op
      collect (map AsVariable names)
      addHighlighting J.SttKeyword [] tokB
      collect a
      addHighlighting J.SttKeyword [] tokC
      collect b
      addHighlighting J.SttKeyword [] tokD
    Case tokA expr tokB cases -> do
      addHighlighting J.SttKeyword [] tokA
      collect expr
      addHighlighting J.SttKeyword [] tokB
      collect cases

instance Collect () Highlighting CaseClause where
  collect (CaseClause _ arrow body) = do
    addHighlighting J.SttMacro [] arrow
    collect body

instance Collect () Highlighting Lit where
  collect x = addHighlighting J.SttNumber [] x

--------------------------------------------------------------------------------

instance Collect () Highlighting EndpointOpen where
  collect (IncludingOpening tok a) = do
    addHighlighting J.SttKeyword [] tok
    collect a
  collect (ExcludingOpening tok a) = do
    addHighlighting J.SttKeyword [] tok
    collect a

instance Collect () Highlighting EndpointClose where
  collect (IncludingClosing a tok) = do
    collect a
    addHighlighting J.SttKeyword [] tok
  collect (ExcludingClosing a tok) = do
    collect a
    addHighlighting J.SttKeyword [] tok

instance Collect () Highlighting Interval where
  collect (Interval a tok b) = do
    collect a
    addHighlighting J.SttKeyword [] tok
    collect b

instance Collect () Highlighting TBase where
  collect = addHighlighting J.SttType []

instance Collect () Highlighting Type where
  collect (TParen _ a _             ) = collect a
  collect (TBase a                  ) = collect a
  collect (TArray tokArray a tokOf b) = do
    addHighlighting J.SttKeyword [] tokArray
    collect a
    addHighlighting J.SttKeyword [] tokOf
    collect b
  collect (TOp op) = addHighlighting J.SttOperator [] op
  collect (TData name _) = addHighlighting J.SttType [] name
  collect (TApp a b)  = do
    collect a
    collect b
  collect (TMetaVar name _)  = addHighlighting J.SttType [] name
