{-# LANGUAGE LambdaCase, MultiParamTypeClasses, FlexibleContexts,
             TypeSynonymInstances, FlexibleInstances #-}

module GCL.Exec where

import Prelude hiding (Ordering(..))
import Data.Loc
import Data.Text.Lazy (pack)
import Control.Arrow ((***))
import Control.Monad.Except
import Control.Monad.State hiding (guard)
import GHC.Base (Alternative(..))

import Syntax.Abstract

type Store = [(Var, Val)]

data Val = VNum Int | VBol Bool | VChr Char
         | VFun (Val -> Either ExecError Val)
         | VArr Int [Val]
         | Undef

class (MonadPlus m, MonadError ExecError m, MonadState Store m)
           => ExecMonad m where
--  lookupStore :: Loc -> Var -> m Val
--  updateStore :: Loc -> Var -> Val -> m ()

lookupStore :: MonadState Store m => Loc -> Var -> m Val
lookupStore l x =
  (lookup x <$> get) >>= \case
   Nothing -> error "shouldn't happen"
   Just (VArr n xs) -> return (VFun (arrToFun l n xs))
   Just v -> return v

updateStore :: MonadState Store m => Loc -> Var -> Val -> m ()
updateStore _ x v = do
   store <- get
   put ((x,v) : filter (not . (==x) . fst) store)

  -- make evalExpr monadic for now,
  --   since it may want to throw errors (e.g. div by zero).

evalExpr :: ExecMonad m => Loc -> Expr -> m Val
evalExpr _ (Lit v) = return (litToVal v)
evalExpr l (Const x) = lookupStore l x
evalExpr l (Var x) = lookupStore l x
evalExpr l (Op op) = evalOp l op
evalExpr l (App e1 e2) =
   evalExpr l e1 >>= \case
    VFun f -> evalExpr l e2 >>= \v -> liftEither (f v)
    _ -> error "type error, shouldn't happen"
evalExpr _ (Quant _ _ _ _) = error "not supported"
evalExpr _ (Hole _ _) = error "shouldn't happen"

litToVal :: Lit -> Val
litToVal (Num n) = VNum n
litToVal (Bol a) = VBol a
litToVal (Chr c) = VChr c

---

data ExecError = Aborted Loc
               | AllFailedInIf Loc
               | DivByZero Loc
               | ArrayOutOfBound Int Int Loc
 deriving (Show, Eq)


execStmt :: ExecMonad m => Stmt -> m ()
execStmt (Skip _)  = return ()
execStmt (Abort l) = throwError (Aborted l)
execStmt (Assign xs es l) = execAsgn xs es l
execStmt (Assert _ _) = return ()
execStmt (Spec _) = error "spec cannot be executed"
execStmt (If _ gcmds l) =
  pickGCmds l (return ()) (throwError (AllFailedInIf l)) gcmds
execStmt (Do pre bnd gcmds l) =
  pickGCmds l (execStmt (Do pre bnd gcmds l)) (return ()) gcmds

execStmts :: ExecMonad m => [Stmt] -> m ()
execStmts [] = return ()
execStmts (s:ss) = execStmt s >> execStmts ss

execAsgn :: ExecMonad m => [Var] -> [Expr] -> Loc -> m ()
execAsgn xs es l = do
  vs <- mapM (evalExpr l) es
  mapM_ (uncurry (updateStore l)) (zip xs vs)

 -- SCM: Not sure whether it is more complicated than necessary,
 --      but I need a way to distinguish between "all choices failed"
 --      and "end of choices after some succssful executions".

pickGCmds :: ExecMonad m =>
             Loc -> m () -> m () -> [GdCmd] -> m ()
pickGCmds _ _    ex [] = ex
pickGCmds l cont ex (GdCmd g cmds : gs) =
  evalExpr l g >>= \case
    VBol False -> pickGCmds l cont ex gs
    VBol True ->  (execStmts cmds >> cont) `mplus`
                  pickGCmds' gs
    _ -> error "type error, shouldn't happen"
 where -- pickGCmds -- some choices have succeeded
       pickGCmds' [] = mzero
       pickGCmds' (GdCmd g' cmds' : gs') =
         evalExpr l g' >>= \case
           VBol False -> pickGCmds' gs'
           VBol True ->  (execStmts cmds' >> cont) `mplus`
                         pickGCmds' gs'
           _ -> error "type error, shouldn't happen"

execProg :: ExecMonad m => Program -> m ()
execProg (Program _ Nothing) = return ()
execProg (Program decls (Just (stmts, _, _))) = do
  mapM_ declare decls
  execStmts stmts

declare :: ExecMonad m => Declaration -> m ()
declare (ConstDecl cs _) =
  mapM_ (\x -> updateStore NoLoc x Undef) cs
declare (VarDecl xs _) =
  mapM_ (\x -> updateStore NoLoc x Undef) xs

-- Lifting primitive operators.
-- Should these be written with dependent type, or type family?

evalOp :: ExecMonad m => Loc -> Op -> m Val
evalOp _ EQ  = return (liftOp2IRel (==))
evalOp _ NEQ = return (liftOp2IRel (/=))
evalOp _ LT  = return (liftOp2IRel (<))
evalOp _ LTE = return (liftOp2IRel (<=))
evalOp _ GTE = return (liftOp2IRel (>=))
evalOp _ GT  = return (liftOp2IRel (>))
evalOp _ Implies  = return (liftOp2Bool (\p q -> not p || q))
evalOp _ Conj     = return (liftOp2Bool (&&))
evalOp _ Disj     = return (liftOp2Bool (||))
evalOp _ Neg      = return (liftOpBool not)
evalOp _ Add = return (liftOp2Int (+))
evalOp _ Sub = return (liftOp2Int (-))
evalOp _ Mul = return (liftOp2Int (*))
evalOp l Div = return $
  VFun (\case (VNum v1) ->
               (Right (VFun (\case (VNum 0)  -> Left (DivByZero l)
                                   (VNum v2) -> Right (VNum (v1 `div` v2))
                                   _ -> error "type error, shouldn't happen")))
              _ -> error "type error, shouldn't happen" )
-- evalOp l Mod = return $ modVFun l

modVFun :: Loc -> Val
modVFun l =
  VFun (\case (VNum v1) ->
               (Right (VFun (\case (VNum 0)  -> Left (DivByZero l)
                                   (VNum v2) -> Right (VNum (v1 `mod` v2))
                                   _ -> error "type error, shouldn't happen")))
              _ -> error "type error, shouldn't happen" )

liftOpInt :: (Int -> Int) -> Val
liftOpInt f = VFun (\case (VNum v) -> Right . VNum . f $ v
                          _ -> error "type error, shouldn't happen" )

liftOpBool :: (Bool -> Bool) -> Val
liftOpBool f = VFun (\case VBol v -> Right . VBol . f $ v
                           _ -> error "type error, shouldn't happen" )

liftOp2Int :: (Int -> Int -> Int) -> Val
liftOp2Int f =
  VFun (\case VNum v -> Right . liftOpInt . f $ v
              _ -> error "type error, shouldn't happen" )

liftOp2Bool :: (Bool -> Bool -> Bool) -> Val
liftOp2Bool f =
  VFun (\case VBol v -> Right . liftOpBool . f $ v
              _ -> error "type error, shouldn't happen" )

liftOp2IRel :: (Int -> Int -> Bool) -> Val
liftOp2IRel f =
  VFun (\case VNum v1 ->
                Right (VFun (\case VNum v2 -> (Right (VBol (f v1 v2)))
                                   _ -> error "type error, shouldn't happen"))
              _ -> error "type error, shouldn't happen" )

arrToFun :: Loc -> Int -> [a] -> Val -> Either ExecError a
arrToFun l n xs (VNum i)
  | i < n     = Right (xs !! i)
  | otherwise = Left (ArrayOutOfBound i n l)
arrToFun _ _ _ _ = error "type error, shouldn't hapen"

-- misc.

instance Show Val where
  showsPrec p (VNum i) = showsPrec p i
  showsPrec p (VBol b) = showsPrec p b
  showsPrec p (VChr c) = showsPrec p c
  showsPrec _ (VFun _) = ("<Fun>" ++)
  showsPrec p (VArr _ xs) = showsPrec p xs
  showsPrec _ Undef = ("undef" ++)

-- Some choices for ExecMonad

prelude :: Store
prelude = map (pack *** id)
 [("mod", modVFun NoLoc)]

-- tests
--  try runExNondet (execStmts stmts) store0

store0 :: Store
store0 = map (pack *** id) [("x", VNum 24), ("y", VNum 56)]

stmts0 :: [Stmt]
stmts0 = [Assign [pack "y"] [var "y" `divi` litN 0] NoLoc,
          Assign [pack "y"] [var "y" `minus` var "x"] NoLoc]

-- My own monad

newtype ExNondet e s a = ExNd {runExNondet :: s -> [(Either e a, s)]}

instance Functor (ExNondet e s) where
  fmap f (ExNd m) = ExNd (map (either Left (Right . f) *** id) . m)

instance Applicative (ExNondet e s) where
  pure = return
  fs <*> xs = do {f <- fs; x <- xs; return (f x)}

instance Monad (ExNondet e s) where
  return x = ExNd (\s -> [(Right x, s)])
  (ExNd m) >>= f = ExNd (concat . map (bindW f) . m)
    where bindW _ (Left e,  s) = [(Left e, s)]
          bindW g (Right x, s) = runExNondet (g x) s

instance MonadState s (ExNondet e s) where
  get   = ExNd (\s -> [(Right s,  s)])
  put s = ExNd (\_ -> [(Right (), s)])

instance MonadError e (ExNondet e s) where
  throwError e = ExNd (\s -> [(Left e, s)])
  catchError = undefined -- later?

instance Alternative (ExNondet e s) where
  empty = mzero
  (<|>) = mplus

instance MonadPlus (ExNondet e s) where
  mzero = ExNd (const [])
  m1 `mplus` m2 = ExNd (\s -> runExNondet m1 s ++ runExNondet m2 s)

instance ExecMonad (ExNondet ExecError Store) where
  
{-
instance MonadState Store m => ExecMonad m where
-- instance ExecMonad (ExNondet ExecError Store) where
  lookupStore l x =
    (lookup x <$> get) >>= \case
     Nothing -> error "shouldn't happen"
     Just (VArr n xs) -> return (VFun (arrToFun l n xs))
     Just v -> return v
  updateStore _ x v = do
     store <- get
     put ((x,v) : filter (not . (==x) . fst) store)
-}

{-
SCM : I tried using monad transformers but both combinations
      turn out to behave awkward. So I defined my own.

type ExNondet = ExceptT ExecError (StateT Store [])

-- StateT s (ExceptT e m) =  s -> m (Either e (a,s))
-- ExceptT e (StateT s m) =  s -> m (Either e a, ss)
--  which do we want?

  -- do we need these? why?
instance Semigroup ExecError where
   (<>) = undefined
instance Monoid ExecError where
   mempty = undefined
   mappend = undefined

runExNondet :: ExNondet a -> [(Either ExecError a, Store)]
runExNondet m = runExNondetWith m []

runExNondetWith :: ExNondet a -> Store -> [(Either ExecError a, Store)]
runExNondetWith m = runStateT (runExceptT m)

{- Neither seemed right.

type ExNondet = StateT Store (ExceptT ExecError [])

runExNondet :: ExNondet a -> [Either ExecError (a, Store)]
runExNondet m = runExNondetWith m []

runExNondetWith :: ExNondet a -> Store -> [Either ExecError (a, Store)]
runExNondetWith m s = runExceptT (runStateT m s)
-}

instance ExecMonad ExNondet where
  lookupStore l x =
    (lookup x <$> get) >>= \case
     Nothing -> error "shouldn't happen"
     Just (VArr n xs) -> return (VFun (arrToFun l n xs))
     Just v -> return v
  updateStore _ x v = do
     store <- get
     put ((x,v) : filter (not . (==x) . fst) store)
-}
