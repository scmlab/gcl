{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances,
             FlexibleContexts #-}
module Syntax.Substitution where

import           Control.Monad                  ( forM )
import           Prelude hiding                 ( lookup )
import           Data.Text                      ( Text )
import           Data.Map hiding                ( map )
import           Data.Set                       ( Set )
import qualified Data.Set as Set
import           Data.Loc                       ( Loc )
import           GCL.Common
import           GCL.WP.Util                    ( declaredNames )
import           Syntax.Abstract.Types
import           Syntax.Common

-- a substitution Subst b is a mapping from names to b.
-- 

type Subst b = Map Text b

-- subs :: Variableous e => Subst b -> Text -> 

class Substitutable m a b where
  subst :: Subst b -> a -> m a

-- types that has a concept of a "variable"
class Variableous e where
  isVar :: e -> Maybe Name
  mkVar :: Name -> e

instance Variableous Expr where
  isVar (Var   x) = Just x
  isVar _         = Nothing
  mkVar = Var

instance Fresh m => Substitutable m Expr Expr where
  subst _ e@(Lit _ _) = return e
  subst sb (Var x) =
    return . maybe (Var x) id $ lookup (nameToText x) sb
  subst _ e@(Op _) = return e
  subst sb (App e1 e2 l) =
    App <$> subst sb e1 <*> subst sb e2 <*> pure l
  subst sb (Lam x e l)
     | nameToText x `elem` keys sb = return (Lam x e l)
     | otherwise = do
         (xs', e', _) <- substBinder sb [x] e
         return (Lam (head xs') e' l)
  subst sb (Func f clauses l) =
    Func f <$> mapM (subst sb) clauses <*> pure l
  subst sb (Tuple es) = Tuple <$> mapM (subst sb) es
  subst sb (Quant op xs ran body l) = do
       (xs', (ran', body'), _) <- substBinder sb xs (ran, body)
       return $ Quant op xs' ran' body' l
  subst _ (RedexKernel _ _ _ _) = error "not knowing what is going on here"
  subst _ (RedexShell _ _) = error "not knowing what is going on here"
  subst sb (ArrIdx a i l) =
    ArrIdx <$> subst sb a <*> subst sb i <*> pure l
  subst sb (ArrUpd a i v l) =
    ArrUpd <$> subst sb a <*> subst sb i <*> subst sb v <*> pure l
  subst sb (Case e cases l) = do
    cases' <- forM cases
      $ \(CaseClause patt body) -> CaseClause patt <$> subst sb body
    return $ Case e cases' l


instance Fresh m => Substitutable m FuncClause Expr where
  subst _ = return
   -- SCM: deal with this later.

instance Fresh m => Substitutable m Stmt Expr where
  subst _ s@(Skip _) = return s
  subst _ s@(Abort _) = return s
  subst sb (Assign ns es l) = do
    let ns' = renameVars sb ns   --- this could fail!
    Assign ns' <$> subst sb es <*> pure l
  subst sb (AAssign a i v l) =
    AAssign <$> subst sb a <*> subst sb i <*> subst sb v <*> pure l
  subst sb (Assert e l) = Assert <$> subst sb e <*> pure l
  subst sb (LoopInvariant e b l) =
    LoopInvariant <$> subst sb e <*> subst sb b <*> pure l
  subst sb (Do gdcmds l) =
    Do <$> mapM (subst sb) gdcmds <*> pure l
  subst sb (If gdcmds l) =
    If <$> mapM (subst sb) gdcmds <*> pure l
  subst _ s@(Spec _ _) = return s
  subst _ s@(Proof _ _ _) = return s
  subst sb (Alloc x es l) =
    Alloc (renameVar sb x) <$> mapM (subst sb) es <*> pure l
  subst sb (HLookup x i l) =
    HLookup (renameVar sb x) <$> subst sb i <*> pure l
  subst sb (HMutate e v l) =
    HMutate <$> subst sb e <*> subst sb v <*> pure l
  subst sb (Dispose e l) =
    Dispose <$> subst sb e <*> pure l
  subst sb (Block prog l) = Block <$> subst sb prog <*> pure l

instance Fresh m => Substitutable m GdCmd Expr where
  subst sb (GdCmd e stmts l) =
     GdCmd <$> subst sb e
           <*> mapM (subst sb) stmts <*> pure l

instance Fresh m => Substitutable m Program Expr where
  subst sb (Program defns decls props stmts l) = do
     (_, (decls', props', stmts'), _) <-
       substBinder sb locals (decls, props, stmts)
     return $ Program defns decls' props' stmts' l
     -- SCM: TODO: deal with defns
    where locals = declaredNames decls

instance Fresh m => Substitutable m Declaration Expr where
  subst sb (ConstDecl ns t expr l) =
     ConstDecl (renameVars sb ns) t <$> subst sb expr <*> pure l
  subst sb (VarDecl ns t expr l) =
     VarDecl (renameVars sb ns) t <$> subst sb expr <*> pure l

-- structual instance declarations

instance (Monad m, Substitutable m a b) => Substitutable m [a] b where
  subst sb = mapM (subst sb)

instance (Monad m, Substitutable m a c, Substitutable m b c) =>
          Substitutable m (a, b) c where
  subst sb (x, y) = (,) <$> subst sb x <*> subst sb y

instance (Monad m, Substitutable m a d,
                   Substitutable m b d,
                   Substitutable m c d) =>
          Substitutable m (a, b, c) d where
  subst sb (x, y, z) = do x' <- subst sb x
                          y' <- subst sb y
                          z' <- subst sb z
                          return (x', y', z')

instance (Monad m, Substitutable m a b) =>
         Substitutable m (Maybe a) b where
  subst sb (Just x) = Just <$> subst sb x
  subst _  Nothing  = return Nothing

-- A common pattern: performing substitution on an
-- expression with binders, e.g
--       (\ xs -> e) sb = (\ xs' -> e')
--   where (xs', e') <- substBinder sb xs e.
-- It performs substitution on e, while renaming xs if necessary.

substBinder :: (Fresh m, Variableous e,
                Substitutable m a e, Free e, Free a) =>
                Subst e -> [Name] -> a -> m ([Name], a, Subst e)
substBinder sb binders body = do
     sb'' <- genBinderRenaming fvsb binders
     let binders' = renameVars sb'' binders
     let sbnew = sb'' <> sb'
     body' <- subst sbnew body
     return $ (binders', body', sbnew)
  where sb'  = shrinkSubst binders (freeVarsT body) sb
        fvsb = Set.unions . map freeVarsT . elems $ sb'

-- Utilities

shrinkSubst :: [Name] -> Set Text -> Subst b -> Subst b
shrinkSubst binders ns subs =
  restrictKeys (substractKeys subs (map nameToText binders)) ns
 where substractKeys sb bs =
         filterWithKey (\k _ -> not (k `elem` bs)) sb

{- composeSubst s1 s2

-}

-- composeSubst :: Substitutable m b b => Subst b -> Subst b -> Subst b
-- composeSubst

{-  genBinderRenaming fvs xs 
    produces a substitution that renames those 
    variables in xs that occur in fvs.
-}

genBinderRenaming :: (Fresh m, Variableous e) =>
                  Set Text -> [Name] -> m (Subst e)
genBinderRenaming _ [] = return empty
genBinderRenaming fvs (Name x l : xs)
   | x `Set.member` fvs = do
       x' <- freshName x l
       insert x (mkVar x') <$> genBinderRenaming fvs xs
   | otherwise = genBinderRenaming fvs xs

{- renameVars renames a list of Names.
   renameVar renames a Name.
   Defined for convenience: while a substiution maps a Name to
   a (general) expression, we define renameVars for occassions
   that a substition maps a Name to a Name.
-}

renameVars :: Variableous e => Subst e -> [Name] -> [Name]
renameVars sb = map (renameVar sb)

renameVar :: Variableous e => Subst e -> Name -> Name
renameVar sb x = case lookup (nameToText x) sb of
            Nothing -> x
            Just e  -> case isVar e of
                         Just y -> y
                         Nothing -> error "variable should be substituted for a variable"
        --- SCM: we assume that renameVars always succeed.
        --       Do we need to raise a catchable error?


-- Type

instance Fresh m => Substitutable m Type Type where
  subst _ (TBase b l) = return $ TBase b l
  subst sb (TVar x  ) =
    return . maybe (TVar x) id $ lookup (nameToText x) sb
  subst sb (TMetaVar x) = 
    return . maybe (TMetaVar x) id $ lookup (nameToText x) sb
  subst sb (TArray ran t l) = 
    TArray ran <$> subst sb t <*> pure l
  subst sb (TTuple ts) =
    TTuple <$> mapM (subst sb) ts
  subst sb (TFunc t1 t2 l) =
    TFunc <$> subst sb t1 <*> subst sb t2 <*> pure l
  subst _ tcon@(TCon _ _ _) = return tcon