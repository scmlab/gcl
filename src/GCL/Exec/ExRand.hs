{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module GCL.Exec.ExRand where

import           System.Random
import           Control.Monad.Except
import           Control.Monad.State     hiding ( guard )
import           GHC.Base                       ( Alternative(..) )

import           GCL.Exec.ExecMonad

-- run a program by
--    runExRand (execProg program) prelude (mkStdGen 813)

newtype ExRand g e s a =
    ExRd {runExRand :: s -> g -> (Either e a, s, g)}

instance Functor (ExRand g e s) where
  fmap f (ExRd m) =
    ExRd (\s g -> (\(x, s', g') -> (either Left (Right . f) x, s', g')) (m s g))

instance Applicative (ExRand g e s) where
  pure = return
  fs <*> xs = do
    f <- fs
    x <- xs
    return (f x)

instance Monad (ExRand g e s) where
  return x = ExRd (\s g -> (Right x, s, g))
  (ExRd m) >>= f = ExRd (\s g -> bindW f (m s g))
   where
    bindW _ (Left  e, s', g') = (Left e, s', g')
    bindW k (Right x, s', g') = runExRand (k x) s' g'

instance MonadState s (ExRand g e s) where
  get = ExRd (\s g -> (Right s, s, g))
  put s = ExRd (\_ g -> (Right (), s, g))

instance MonadError e (ExRand g e s) where
  throwError e = ExRd (\s g -> (Left e, s, g))
  catchError = undefined -- later?

instance Alternative (ExRand g e s) where
  empty = mzero
  (<|>) = mplus

instance MonadPlus (ExRand g e s) where
  mzero = undefined -- not needed?
  m1 `mplus` _ = m1

instance RandomGen g => ExecMonad (ExRand g ExecError Store) where
  shuffle xs =
    ExRd (\s g -> let (xs', g') = rPermute g xs in (Right xs', s, g'))

  -- randomly permute a list

rPermute :: RandomGen g => g -> [a] -> ([a], g)
rPermute g []  = ([], g)    -- isolate the most common base cases,
rPermute g [x] = ([x], g)   -- for efficiency.
rPermute g [x, y] =
  let (b, g') = random g in (if b then [x, y] else [y, x], g')
rPermute g xs = rPermute' g (length xs) xs
 where
  rPermute' g _ [] = ([], g)
  rPermute' g n (x : xs) =
    let (xs', g' ) = rPermute' g (n - 1) xs
        (i  , g'') = randomR (0, n - 1) g'
    in  (swapHd i x xs', g'')

-- swap the first element (x) of a list with the ith element.

swapHd :: Int -> a -> [a] -> [a]
swapHd 0 x xs = x : xs
swapHd i x xs = let (ys, y, zs) = dissect i xs in y : ys ++ [x] ++ zs
 where
  dissect _ []       = error "cannot happen"
  dissect 1 (y : ys) = ([], y, ys)
  dissect n (y : ys) = let (ws, w, zs) = dissect (n - 1) ys in (y : ws, w, zs)
