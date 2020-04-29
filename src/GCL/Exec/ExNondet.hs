{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module GCL.Exec.ExNondet where

import           Control.Arrow                  ( (***) )
import           Control.Monad.Except
import           Control.Monad.State     hiding ( guard )
import           GHC.Base                       ( Alternative(..) )

import           GCL.Exec.ExecMonad

-- run a program by
--    runExNondet (execProg program) prelude

newtype ExNondet e s a = ExNd {runExNondet :: s -> [(Either e a, s)]}

instance Functor (ExNondet e s) where
  fmap f (ExNd m) = ExNd (map (either Left (Right . f) *** id) . m)

instance Applicative (ExNondet e s) where
  pure = return
  fs <*> xs = do
    f <- fs
    x <- xs
    return (f x)

instance Monad (ExNondet e s) where
  return x = ExNd (\s -> [(Right x, s)])
  (ExNd m) >>= f = ExNd (concat . map (bindW f) . m)
   where
    bindW _ (Left  e, s) = [(Left e, s)]
    bindW g (Right x, s) = runExNondet (g x) s

instance MonadState s (ExNondet e s) where
  get = ExNd (\s -> [(Right s, s)])
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
