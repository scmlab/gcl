{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances,
             FlexibleContexts, MonoLocalBinds #-}
module Syntax.Typed.Instances.Substitution where

import           Data.Loc
import           Prelude                 hiding ( Ordering(..) )
import           Syntax.Typed.Types
import           Syntax.Substitution
import           GCL.Common
import           Syntax.Common

instance Fresh m => Substitutable m Expr Expr where

instance Fresh m => Substitutable m Stmt Expr where
