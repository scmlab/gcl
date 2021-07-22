{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Server.Handler.Highlighting where

import           Data.Loc.Range
import qualified Language.LSP.Types            as J


import           Server.Stab
import           Syntax.Concrete

import           Data.Loc                hiding ( fromLoc )

import           Data.Foldable                  ( toList )
import           Error                          ( Error )
import           Server.DSL
import           Server.Interpreter.RealWorld
import           Syntax.Common                  ( Name )
import           Syntax.Parser                  ( pProgram )

ignoreErrors
  :: Either [Error] (Either J.ResponseError (Maybe J.SemanticTokens))
  -> Either J.ResponseError (Maybe J.SemanticTokens)
ignoreErrors (Left  _errors) = Right Nothing
ignoreErrors (Right xs     ) = xs

handler
  :: J.Uri
  -> (Either J.ResponseError (Maybe J.SemanticTokens) -> ServerM ())
  -> ServerM ()
handler uri responder = do
  case J.uriToFilePath uri of
    Nothing       -> return ()
    Just filepath -> do
      interpret filepath (responder . ignoreErrors) $ do
        source  <- getSource
        program <- parse pProgram source
        let legend = J.SemanticTokensLegend
              (J.List J.knownSemanticTokenTypes)
              (J.List J.knownSemanticTokenModifiers)
        let tokens = J.makeSemanticTokens legend (collect program)
        case tokens of
          Left t -> return $ Left $ J.ResponseError J.InternalError t Nothing
          Right tokens' -> return $ Right $ Just tokens'

--------------------------------------------------------------------------------
-- Helper functions for converting stuff to SemanticTokenAbsolute

toToken
  :: Ranged a
  => J.SemanticTokenTypes
  -> [J.SemanticTokenModifiers]
  -> a
  -> [J.SemanticTokenAbsolute]
toToken types modifiers x =
  let range = rangeOf x
  in  [ J.SemanticTokenAbsolute (posLine (rangeStart range) - 1)
                                (posCol (rangeStart range) - 1)
                                (rangeSpan range)
                                types
                                modifiers
      ]

toToken'
  :: Located a
  => J.SemanticTokenTypes
  -> [J.SemanticTokenModifiers]
  -> a
  -> [J.SemanticTokenAbsolute]
toToken' types modifiers x = case fromLoc (locOf x) of
  Nothing -> []
  Just range ->
    [ J.SemanticTokenAbsolute (posLine (rangeStart range) - 1)
                              (posCol (rangeStart range) - 1)
                              (rangeSpan range)
                              types
                              modifiers
    ]

--------------------------------------------------------------------------------
-- newtypes for giving common datatype like `Name` different interpretations

newtype AsVariable = AsVariable Name

instance Collect AsVariable J.SemanticTokenAbsolute where
  collect (AsVariable a) = toToken' J.SttVariable [] a

--------------------------------------------------------------------------------

instance Collect Program J.SemanticTokenAbsolute where
  collect (Program as _bs) = as >>= collect

instance Collect Declaration' J.SemanticTokenAbsolute where
  collect (Left  a ) = collect a
  collect (Right _a) = []-- collect a

instance Collect Declaration J.SemanticTokenAbsolute where
  collect (ConstDecl _tokCon a) = collect a
  collect (VarDecl   _tokVar a) = collect a

instance Collect DeclType J.SemanticTokenAbsolute where
  collect (DeclType a _b) = collect a

instance Collect DeclBase J.SemanticTokenAbsolute where
  collect (DeclBase as _ b) =
    (map AsVariable (toList as) >>= collect) <> collect b

--------------------------------------------------------------------------------
-- Expr 

instance Collect Expr J.SemanticTokenAbsolute where
  collect _ = []

--------------------------------------------------------------------------------
-- Type 

instance Collect EndpointOpen J.SemanticTokenAbsolute where
  collect (IncludingOpening tok a) = toToken J.SttKeyword [] tok <> collect a
  collect (ExcludingOpening tok a) = toToken J.SttKeyword [] tok <> collect a

instance Collect EndpointClose J.SemanticTokenAbsolute where
  collect (IncludingClosing a tok) = collect a <> toToken J.SttKeyword [] tok
  collect (ExcludingClosing a tok) = collect a <> toToken J.SttKeyword [] tok

instance Collect Interval J.SemanticTokenAbsolute where
  collect (Interval a tok b) =
    collect a <> toToken J.SttKeyword [] tok <> collect b

instance Collect TBase J.SemanticTokenAbsolute where
  collect = toToken' J.SttType []

instance Collect Type J.SemanticTokenAbsolute where
  collect (TParen _ a _) = collect a
  collect (TBase a     ) = collect a
  collect (TArray tokArray a tokOf b) =
    toToken J.SttKeyword [] tokArray
      <> collect a
      <> toToken J.SttKeyword [] tokOf
      <> collect b
  collect (TFunc a tok b) =
    collect a
      <> toToken J.SttOperator  [] tok
      <> collect b
  collect (TVar name) = toToken' J.SttType [] name