{-# LANGUAGE OverloadedStrings, FlexibleContexts, DeriveGeneric,
             TypeSynonymInstances, FlexibleInstances #-}

module GCL.WP where

import Control.Monad.State hiding (guard)
import Control.Monad.Writer hiding (guard)
import Control.Monad.Except hiding (guard)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Loc (Loc(..), Located(..), posLine, posCol)
import Data.Aeson
import GHC.Generics

import Syntax.Concrete
-- import Syntax.Abstract (Fresh(..))
import qualified Syntax.Abstract as A
import Syntax.Abstract.Location

-- type Expr = A.Expr
type Index = A.Index

data Obligation = Obligation Index Expr Expr [ObliOrigin]
       deriving (Show, Generic)

data Specification = Specification
  { specID       :: Int
  , specPreCond  :: Expr
  , specPostCond :: Expr
  , specLoc      :: Loc
  } deriving (Show, Generic)

data ObliOrigin = AroundAbort Loc
                | AroundSkip Loc
                | AssertGuaranteed Loc
                | AssertSufficient Loc
                | Assignment Loc
                | IfTotal Loc
                | IfBranch Loc
                | LoopBase Loc
                | LoopInd Loc
                | LoopTermBase Loc
                | LoopTermDec Loc
                | LoopInitialize Loc
      deriving Generic

type SM = WriterT [Obligation] (WriterT [Specification]
                (StateT (Int, Int, Int)
                  (Either StructError)))

-- create a proof obligation

obligate :: Expr -> Expr -> ObliOrigin -> SM ()
obligate p q l = do
  -- NOTE: this could use some love
  unless (predEq p q) $ do
    (i, j, k) <- get
    put (succ i, j, k)
    tell [Obligation i p q [l]]

-- inform existence of a spec hole

tellSpec :: Expr -> Expr -> Loc -> SM ()
tellSpec p q loc = do
  (i, j, k) <- get
  put (i, succ j, k)
  lift $ tell [Specification j p q loc]

--------------------------------------------------------------------------------
-- | Structure, and Weakest-Precondition

struct :: Bool -> Expr -> Maybe Expr -> Stmt -> Expr -> SM ()

struct _ pre _ (Abort l) _ = obligate pre false (AroundAbort l)

struct _ pre _ (Skip l) post = obligate pre post (AroundSkip l)

struct _ pre _ (Assert p l) post =
   obligate pre p (AssertGuaranteed l) >>
   obligate p post (AssertSufficient l)

struct _ _ _ (AssertWithBnd _ _ l) _ =
   throwError (ExcessBound l)

struct _ pre _ (Assign xs es l) post = do
  post' <- subst (assignmentEnv xs es) post
  obligate pre post' (Assignment l)

struct b pre _ (If gcmds l) post = do
  let guards = getGuards gcmds
  obligate pre (disjunct guards) (IfTotal l)
  forM_ gcmds $ \(GdCmd guard body l') ->
    addObliOrigin (IfBranch l')
     (structStmts b (pre `conj` guard) Nothing body post)

struct _ _ Nothing (Do _ l) _ =
  throwError (MissingBound l)
 {- Or if we want to tolerate the user and carry on ---
 do -- warn that bnd is missing
  let gcmds' = map (\(GdCmd x y _) -> (depart x, y)) gcmds
  let guards = map fst gcmds'
  obligate (inv `A.conj` (A.conjunct (map A.neg guards))) post
  --
  forM_ gcmds' $ \(guard, body) -> do
    structStmts b (inv `A.conj` guard) Nothing body inv
 -}

struct b inv (Just bnd) (Do gcmds l) post = do
  -- base case
  let guards = getGuards gcmds
  obligate (inv `conj` (conjunct (map neg guards))) post (LoopBase l)
  -- inductive cases
  forM_ gcmds $ \(GdCmd guard body l') ->
    addObliOrigin (LoopInd l')
      (structStmts b (inv `conj` guard) Nothing body inv)
  -- termination
  obligate (inv `conj` disjunct guards)
       (bnd `gte` (Lit (Num 0) NoLoc)) (LoopTermBase l)
  -- bound decrementation
  oldbnd <- freshVar "bnd"
  let invB = inv `conj` (bnd `eqq` Var oldbnd NoLoc)
  forM_ gcmds $ \(GdCmd guard body l') ->
    addObliOrigin (LoopTermDec l')
      (structStmts False (invB `conj` guard) Nothing body
             (bnd `lte` Var oldbnd NoLoc))

struct _ _ _ (SpecQM l) _    = throwError $ DigHole l
struct b pre _ (Spec l) post = when b (tellSpec pre post l)


structStmts :: Bool -> Expr -> Maybe Expr -> [Stmt] -> Expr -> SM ()

structStmts _ _ _ [] _ = return ()

structStmts b pre _ (Assert p l : stmts) post =
  obligate pre p (AssertGuaranteed l) >>
  structStmts b p Nothing stmts post

structStmts b pre _ (AssertWithBnd p bnd l : stmts) post =
  let origin = if startsWithDo stmts then LoopInitialize l
                   else AssertGuaranteed l
  in obligate pre p origin >>
     structStmts b p (Just bnd) stmts post
 where startsWithDo (Do _ _ : _) = True
       startsWithDo _ = False

structStmts b pre bnd (stmt : stmts) post = do
  post' <- wpStmts b stmts post
  struct b pre bnd stmt post'

structProg :: [Stmt] -> SM ()
structProg [] = return ()
structProg (Assert pre _ : stmts) =
  case (init stmts, last stmts) of
    (stmts', Assert post _) ->
       structStmts True pre Nothing stmts' post
    (_, stmt) -> throwError (MissingPostcondition (locOf stmt))
structProg stmts =
  case (init stmts, last stmts) of
    (stmts', Assert post _) ->
       structStmts True true Nothing stmts' post
    (_, stmt) -> throwError (MissingPostcondition (locOf stmt))


wpStmts :: Bool -> [Stmt] -> Expr -> SM Expr

wpStmts _ [] post = return post

wpStmts b (Assert pre _ : stmts) post =
  structStmts b pre Nothing stmts post >>
  return pre
 -- where pre' = depart pre

wpStmts b (AssertWithBnd pre bnd _ : stmts) post =
  structStmts b pre (Just bnd) stmts post >>
  return pre
 -- where (pre', bnd') = (depart pre, depart bnd)

wpStmts b (stmt : stmts) post = do
  post' <- wpStmts b stmts post
  wp b stmt post'

wp :: Bool -> Stmt -> Expr -> SM Expr

wp _ (Abort _) _ = return false

wp _ (Skip _) post = return post

wp _ (Assert p l) post =
   obligate p post (AssertSufficient l) >> return p
 where p' = depart p

wp _ (AssertWithBnd p _ l) post =
   obligate p post (AssertSufficient l) >> return p
 where p' = depart p

wp _ (Assign xs es _) post =
  subst (assignmentEnv xs es) post

wp b (If gcmds _) post = do
    forM_ gcmds $ \(GdCmd guard body _) ->
      structStmts b guard Nothing body post
    let guards = getGuards gcmds
    return (disjunct guards) -- is this enough?

wp _ (Do _ l) _ = throwError (MissingAssertion l)

wp _ (SpecQM l) _ = throwError $ DigHole l

wp b (Spec l) post =
  when b (tellSpec post post l) >> return post  -- not quite right

wpProg :: [Stmt] -> SM Expr
wpProg [] = return $ hydrate A.tt
wpProg stmts =
  case (init stmts, last stmts) of
    (stmts', Assert p _) -> wpStmts True stmts' p
    (_, stmt) -> throwError (MissingPostcondition (locOf stmt))

assignmentEnv :: [Lower] -> [Expr] -> Subst
assignmentEnv xs es = Map.fromList (zip (map Left xs) es)

--------------------------------------------------------------------------------
-- | The monad, and other supportive operations

instance Fresh SM where
  fresh = do (i, j, k) <- get
             put (i, j, succ k)
             return k

runSM :: SM a -> (Int, Int, Int) -> Either StructError
            (((a, [Obligation]), [Specification]), (Int, Int, Int))
runSM p s = runStateT (runWriterT . runWriterT $ p) s

runWP :: SM a -> Either StructError ((a, [Obligation]), [Specification])
runWP p = fmap fst $ runSM p (0,0,0)

instance Located ObliOrigin where
  locOf (AroundAbort      l) = l
  locOf (AroundSkip       l) = l
  locOf (AssertGuaranteed l) = l
  locOf (AssertSufficient l) = l
  locOf (Assignment       l) = l
  locOf (IfTotal          l) = l
  locOf (IfBranch         l) = l
  locOf (LoopBase         l) = l
  locOf (LoopInd          l) = l
  locOf (LoopTermBase     l) = l
  locOf (LoopTermDec      l) = l
  locOf (LoopInitialize   l) = l

-- instance Located Obligation where
--   locOf (Obligation _ _ _ o) = locOf o

censorObli :: ([Obligation] -> [Obligation]) -> SM a -> SM a
censorObli = censor

addObliOrigin :: ObliOrigin -> SM a -> SM a
addObliOrigin ori =
  censorObli (map (\(Obligation i p q os) -> Obligation i p q (ori:os)))

censorSpec :: ([Specification] -> [Specification]) -> SM a -> SM a
censorSpec f = mapWriterT (censor f)

-- the following is temporary

showLoc :: Loc -> String
showLoc NoLoc     = ""
showLoc (Loc l _) = "(" ++ show (posLine l) ++ "," ++ show (posCol l) ++ ")"

instance Show ObliOrigin where
  show (AroundAbort l) = "AroundAbort " ++ showLoc l
  show (AroundSkip l) = "AroundSkip " ++ showLoc l
  show (AssertGuaranteed l) = "AssertGuaranteed " ++ showLoc l
  show (AssertSufficient l) = "AssertSufficient " ++ showLoc l
  show (Assignment l) = "Assignment " ++ showLoc l
  show (IfTotal l) = "IfTotal " ++ showLoc l
  show (IfBranch l) = "IfBranch " ++ showLoc l
  show (LoopBase l) = "LoopBase " ++ showLoc l
  show (LoopInd l) = "LoopInd " ++ showLoc l
  show (LoopTermBase l) = "LoopTermBase " ++ showLoc l
  show (LoopTermDec l) = "LoopTermDec " ++ showLoc l
  show (LoopInitialize l) = "LoopInitialize " ++ showLoc l



data StructError = MissingAssertion Loc
                 | MissingBound Loc
                 | ExcessBound  Loc
                 | MissingPostcondition Loc
                 | DigHole Loc
                deriving (Eq, Show, Generic)

instance Located StructError where
  locOf (MissingAssertion loc) = loc
  locOf (MissingBound     loc) = loc
  locOf (ExcessBound      loc) = loc
  locOf (MissingPostcondition loc) = loc
  locOf (DigHole loc) = loc

instance ToJSON StructError where
