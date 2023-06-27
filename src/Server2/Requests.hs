{-# LANGUAGE FlexibleContexts #-}

module Server2.Requests where

import           Control.Monad.Except
import           Control.Monad.Trans.State.Strict


import qualified Data.List                     as List
-- :import qualified Data.IntMap                   as IntMap
import           Data.IntMap                    ( IntMap )

import           Data.Text                      ( Text )
import           Data.Loc                hiding ( fromLoc )
import           Data.Loc.Range

import qualified GCL.Type                      as TypeChecking
import qualified GCL.WP                        as WP
import           GCL.WP.Type                    ( StructWarning )
import           GCL.Predicate

import           Error

import qualified Syntax.Concrete               as C
import qualified Syntax.Abstract               as A
-- import           Syntax.Abstract                ( Type )

import qualified Syntax.Parser                 as Parser
import           Syntax.Parser                 ( Parser )

data Request = Load FilePath Text 

data Response = PSG [PO] [Spec] [A.Expr]
-- data Response = 
--        EditText Range -- ^ Range to replace
--                 Text -- ^ Text to replace with
--      | GetSource -- ^ Read the content of a file from the LSP filesystem

data Session = Session
     { filePath  :: Maybe FilePath
     , srcTree   :: Maybe A.Program 
     , proofObs  :: Maybe [PO]
     , specs     :: Maybe [(Int, TypeEnv, Spec)]
     , globals   :: Maybe [A.Expr]
     }

emptySession = Session Nothing Nothing Nothing Nothing

type SessionId = Int
type ServerM = StateT [(SessionId, Session)] (Except [Error])

handleLoad :: Int -> FilePath -> Text -> ServerM Response
handleLoad sId path src = do
    concrete <- parseWithParser Parser.program path src
    abstract <- toAbs concrete
    typeCheck abstract
    (pos, specs, globals, _, _, _) <- sweep abstract
    updSession sId (Session (Just path) (Just abstract)
                      (Just pos) (Just specs) (Just globals))
    return (PSG pos specs globals)


handleRefine :: Int -> Int -> Text -> ServerM Response
handleRefine sId spId src = do
    path     <- getFilePath sId ""
    concrete <- parseWithParser Parser.statements1 path src
    abstract <- toAbs concrete
    -- SCM: gotta type check this! needs type environment

    return undefined

-- stages 

parseWithParser :: MonadError [Error] m => Parser a -> FilePath -> Text -> m a
parseWithParser parser path src =
  case Parser.scanAndParse parser path src of
    Left  err -> throwError [ParseError err]
    Right val -> return val

toAbs :: (Monad m, C.ToAbstract c a) => c -> m a
toAbs concrete = 
 case runExcept (C.toAbstract concrete) of
    Left (Range _start _end) -> -- should dig hole!
      undefined -- SCM: gotta deal with this
                -- digHole (Range start end) >>= parse >>= convert
    Right program -> return program


typeCheck :: MonadError [Error] m => A.Program -> m ()
typeCheck abstract = case TypeChecking.runTypeCheck abstract of
    Left  e -> throwError [TypeError e]
    Right (_, _scopeTree) -> return () 
    -- currently the typechecker returns a scope tree.
    -- not sure how to use that yet.


sweep :: MonadError [Error] m => A.Program -> 
         m ([PO], [Spec], [A.Expr], [StructWarning],
            IntMap (Int, A.Expr), Int)
sweep abstract@(A.Program _ _ globalProps _ _) = do
  case WP.sweep abstract of
    Left  e -> throwError [StructError e]
    Right (pos, specs, warnings, redexes, counter) -> 
        return (List.sort pos, List.sortOn locOf specs, 
                globalProps, warnings, redexes, counter)

-- state management helpers

modSessionWith :: SessionId -> (Session -> Session) -> ServerM ()
modSessionWith sId f = modify (modSWith f sId)
   where modSWith f i [] = []
         modSWith f i ((j,s):ss) | i == j = (i, f s) : ss
                                 | otherwise = (j,s) : modSWith f i ss

updSession :: SessionId -> Session -> ServerM ()
updSession i s = modSessionWith i (upd s)
  where upd (Session fp' sr' po' sp' gl')
            (Session fp  sr  po  sp  gl ) =
          Session (fp' `mplus` fp) (sr' `mplus` sr) 
                  (po' `mplus` po) (sp' `mplus` sp) (gl' `mplus` gl)


getFilePath :: SessionId -> FilePath -> ServerM FilePath
getFilePath sId def = do 
    ss <- get
    case lookup sId ss of
        Just session -> maybe (return def) return (filePath session) 
        Nothing -> error "internal error: wrong session id"
                   -- SCM: deal with this later.