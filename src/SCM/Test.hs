module SCM.Test where

import qualified Data.Text.Lazy.IO as Text
-- import System.Console.GetOpt
-- import System.Environment

import REPL
import Syntax.Abstract hiding (abstract)
import GCL.Exec
import Prelude
import Pretty ()

runtst :: IO ()
runtst = do
      let filepath = "SCM/tst.gcl" -- "../examples/factor.gcl"

      raw <- Text.readFile filepath

      let parsed = scan filepath raw >>=
                    parseProgram filepath >>=
                     abstract

      case parsed of
        Right program -> do
           print program
           -- print (runExNondetWith (execProg program) prelude)
        Left errors -> print errors
