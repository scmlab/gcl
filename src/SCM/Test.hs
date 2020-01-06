module SCM.Test where

import qualified Data.Text.Lazy.IO as Text
-- import System.Console.GetOpt
-- import System.Environment

import REPL
import Prelude
import Pretty ()
import System.Random

import GCL.Type

import GCL.Exec
import GCL.Exec.ExNondet
import GCL.Exec.ExRand

runtst :: IO ()
runtst = do
      let filepath = "SCM/tst1.gcl" -- "SCM/tst.gcl" --

      raw <- Text.readFile filepath

      let parsed = scan filepath raw >>=
                    parseProgram filepath >>=
                     abstract

      case parsed of
        Right program -> do
           print program
           putStr "\n"
           case runTM' (checkProg program) of
             (Right _, tstate) -> do
                print "typechecked"
                putStr "\n"
                print tstate
                putStr "\n"
                print (runExRand (execProg program) prelude (mkStdGen 813))
             (Left terr, tstate) -> do
                print terr
                putStr "\n"
                print tstate
        Left errors -> print errors
