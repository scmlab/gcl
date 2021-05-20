{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.DSL where

import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Trans.Free
import Control.Monad.Writer
import Data.List (find, sortOn)
import Data.Loc
import Data.Loc.Range
import Data.Text (Text)
import qualified Data.Text as Text
import Error
import qualified GCL.Type as TypeChecking
import GCL.WP (StructWarning)
import qualified GCL.WP as WP
import Language.LSP.Types ( Diagnostic )
import Server.CustomMethod
import qualified Syntax.Abstract as A
import Syntax.Concrete.ToAbstract
import Syntax.Parser (Parser, pProgram, pStmts, runParse)
import Syntax.Predicate (PO, Spec (specLoc), specPayload)
import Prelude hiding (span)

--------------------------------------------------------------------------------

-- The "Syntax" of the DSL for handling LSP requests and responses
data Cmd next
  = EditText Range Text (Text -> next)
  | GetFilePath (FilePath -> next)
  | GetSource (Text -> next)
  | PutLastSelection (Int, Int) next
  | GetLastSelection (Maybe (Int, Int) -> next)
  | BumpResponseVersion (Int -> next)
  | Log Text next
  | Terminate [ResKind] [Diagnostic]
  deriving (Functor)

type CmdM = FreeT Cmd (Except [Error])

runCmdM :: CmdM a -> Either [Error] (FreeF Cmd a (CmdM a))
runCmdM = runExcept . runFreeT

editText :: Range -> Text -> CmdM Text
editText range text = liftF (EditText range text id)

getFilePath :: CmdM FilePath
getFilePath = liftF (GetFilePath id)

getSource :: CmdM Text
getSource = liftF (GetSource id)

setLastSelection :: (Int, Int) -> CmdM ()
setLastSelection selection = liftF (PutLastSelection selection ())

getLastSelection :: CmdM (Maybe (Int, Int))
getLastSelection = liftF (GetLastSelection id)

logM :: Text -> CmdM ()
logM text = liftF (Log text ())

bumpVersion :: CmdM Int
bumpVersion = liftF (BumpResponseVersion id)

terminate :: [ResKind] -> [Diagnostic] -> CmdM ()
terminate x y = liftF (Terminate x y)

------------------------------------------------------------------------------

-- converts the "?" at a given location to "[!   !]"
-- and returns the modified source and the difference of source length
digHole :: Pos -> CmdM Text
digHole pos = do
  let indent = Text.replicate (posCol pos - 1) " "
  let holeText = "[!\n" <> indent <> "\n" <> indent <> "!]"
  editText (Range pos pos) holeText

-- | Try to parse a piece of text in a Spec
refine :: Text -> (Int, Int) -> CmdM (Spec, Text)
refine source (start, end) = do
  result <- findPointedSpec
  case result of
    Nothing -> throwError [Others "Please place the cursor in side a Spec to refine it"]
    Just spec -> do
      source' <- getSource
      let payload = Text.unlines $ specPayload source' spec
      -- HACK, `pStmts` will kaput if we feed empty strings into it
      let payloadIsEmpty = Text.null (Text.strip payload)
      if payloadIsEmpty
        then return ()
        else void $ parse pStmts payload
      return (spec, payload)
  where
    findPointedSpec :: CmdM (Maybe Spec)
    findPointedSpec = do
      program <- parseProgram source
      (_, specs, _, _) <- sweep program
      return $ find (pointed (start, end)) specs
      where
        pointed :: (Int, Int) -> Spec -> Bool
        pointed (x, y) spec = case specLoc spec of
          NoLoc -> False
          Loc open close ->
            (posCoff open <= x && x <= posCoff close + 1)
              || (posCoff open <= y && y <= posCoff close + 1)

typeCheck :: A.Program -> CmdM ()
typeCheck p = case runExcept (TypeChecking.checkProg p) of
  Left e -> throwError [TypeError e]
  Right v -> return v

sweep :: A.Program -> CmdM ([PO], [Spec], [A.Expr], [StructWarning])
sweep program@(A.Program _ globalProps _ _ _) =
  case WP.sweep program of
    Left e -> throwError [StructError e]
    Right (pos, specs, warings) -> do
      return (sortOn locOf pos, sortOn locOf specs, globalProps, warings)

--------------------------------------------------------------------------------

-- | Parse with a parser
parse :: Parser a -> Text -> CmdM a
parse p source = do
  filepath <- getFilePath
  case runParse p filepath source of
    Left errors -> throwError $ map SyntacticError errors
    Right val -> return val

parseProgram :: Text -> CmdM A.Program
parseProgram source = do
  concrete <- parse pProgram source
  case runExcept (toAbstract concrete) of
    Left NoLoc -> throwError [Others "NoLoc in parseProgram"]
    Left (Loc start _) -> digHole start >>= parseProgram
    Right program -> return program

-- | Compare the cursor position with something
--  EQ: the cursor is placed within that thing
--  LT: the cursor is placed BEFORE (but not touching) that thing
--  GT: the cursor is placed AFTER (but not touching) that thing
compareWithPosition :: Located a => Int -> a -> Ordering
compareWithPosition offset x = case locOf x of
  NoLoc -> EQ
  Loc start end ->
    if offset < posCoff start
      then LT
      else
        if offset - 1 > posCoff end
          then GT
          else EQ

-- | See if something is within the selection
withinSelection :: Located a => (Int, Int) -> a -> Bool
withinSelection (left, right) x =
  compareWithPosition left x == EQ
    || compareWithPosition right x == EQ
    || (compareWithPosition left x == LT && compareWithPosition right x == GT)

--------------------------------------------------------------------------------

