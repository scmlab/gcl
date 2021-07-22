{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Server.Handler.Highlighting where

import           Data.Loc.Range
import qualified Language.LSP.Types            as J


import           Server.Stab
import           Syntax.Concrete

import           Data.Loc hiding (fromLoc)

import           Error                          ( Error )
import           Server.DSL
import           Server.Interpreter.RealWorld
import           Syntax.Parser                  ( pProgram )
import Data.Foldable (toList)

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

toToken
  :: Ranged a
  => J.SemanticTokenTypes
  -> [J.SemanticTokenModifiers]
  -> a
  -> J.SemanticTokenAbsolute
toToken types modifiers x =
  let range = rangeOf x
  in  J.SemanticTokenAbsolute (posLine (rangeStart range) - 1)
                              (posCol (rangeStart range) - 1)
                              (rangeSpan range)
                              types
                              modifiers

toToken'
  :: Located a
  => J.SemanticTokenTypes
  -> [J.SemanticTokenModifiers]
  -> a
  -> [J.SemanticTokenAbsolute]
toToken' types modifiers x = case fromLoc (locOf x) of
  Nothing -> []
  Just range ->
    [J.SemanticTokenAbsolute (posLine (rangeStart range) - 1)
                              (posCol (rangeStart range) - 1)
                              (rangeSpan range)
                              types
                              modifiers]

instance Collect Program J.SemanticTokenAbsolute where
  collect (Program as _bs) = as >>= collect

instance Collect Declaration' J.SemanticTokenAbsolute where
  collect (Left a) = collect a
  collect (Right _a) = []-- collect a

instance Collect Declaration J.SemanticTokenAbsolute where
  collect (ConstDecl _tokCon a) = collect a
  collect (VarDecl _tokVar a) = collect a

instance Collect DeclType J.SemanticTokenAbsolute where
  collect (DeclType a _b) = collect a

instance Collect DeclBase J.SemanticTokenAbsolute where
  collect (DeclBase as _ _ ) = toList as >>= toToken' J.SttVariable []
