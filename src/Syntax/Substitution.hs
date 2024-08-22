{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances,
             FlexibleContexts, MonoLocalBinds, FunctionalDependencies #-}
module Syntax.Substitution where

import           Control.Monad                  ( forM )
import           Prelude hiding                 ( lookup )
import           Data.Text                      ( Text )
import           Data.Map hiding                ( map )
import           Data.Set                       ( Set )
import qualified Data.Set as Set
import           Data.Loc                       ( Loc )
import           GCL.Common hiding              ( Substitutable ( subst ) )
import           Syntax.Abstract.Types
import           Syntax.Abstract.Util           ( declaredNames )
import           Syntax.Common

type Subst b = Map Text b

class Substitutable m a b where
  subst :: Subst b -> a -> m a

-- types that has a concept of a "variable"
class Variableous e t | e -> t where  -- t denotes the type of the variable
  isVar :: e -> Maybe (Name, t)
  mkVar :: Name -> t -> Loc -> e

instance Variableous Expr () where
  isVar (Var   x _) = Just (x, ())
  isVar (Const x _) = Just (x, ())
  isVar _           = Nothing
  mkVar x () l      = Var x l

instance Fresh m => Substitutable m Expr Expr where
  subst _ e@(Lit _ _) = return e
  subst sb (Var x l) =
    return . maybe (Var x l) id $ lookup (nameToText x) sb
  subst sb (Const x l) =
    return . maybe (Var x l) id $ lookup (nameToText x) sb
  subst _ e@(Op _) = return e
  subst sb (Chain ch) = Chain <$> subst sb ch
  subst sb (App e1 e2 l) =
    App <$> subst sb e1 <*> subst sb e2 <*> pure l
  subst sb (Lam x e l)
     | nameToText x `elem` keys sb = return (Lam x e l)
     | otherwise = do
         (xs', e', _) <- substBinderTypeless sb [x] e
         return (Lam (head xs') e' l)
  subst sb (Func f clauses l) =
    Func f <$> mapM (subst sb) clauses <*> pure l
  subst sb (Tuple es) = Tuple <$> mapM (subst sb) es
  subst sb (Quant op xs ran body l) = do
       (xs', (ran', body'), _) <- substBinderTypeless sb xs (ran, body)
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

-- just a wrapper calling substBinder, when e is typeless Expr
substBinderTypeless :: (Fresh m, Substitutable m a Expr, Free a) =>
  Subst Expr -> [Name] -> a -> m ([Name], a, Subst Expr)
substBinderTypeless sb binders body =
  (\(binders', body', sb') -> (map fst binders', body', sb'))
   <$> (substBinder sb [(b,()) | b <- binders] body)

instance Fresh m => Substitutable m Chain Expr where
  subst sb (Pure expr loc) = Pure <$> subst sb expr <*> pure loc
  subst sb (More ch' op expr loc) = More <$> subst sb ch' <*> pure op <*> subst sb expr <*> pure loc

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
       substBinderTypeless sb locals (decls, props, stmts)
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

substBinder :: (Fresh m, Variableous e t,
                Substitutable m a e, Free e, Free a) =>
                Subst e -> [(Name, t)] -> a -> m ([(Name, t)], a, Subst e)
substBinder sb binders body = do
     sb'' <- genBinderRenaming fvsb binders
     let binders' = zip (renameVars sb'' (map fst binders)) (map snd binders)
     let sbnew = sb'' <> sb'
     body' <- subst sbnew body
     return $ (binders', body', sbnew)
  where sb'  = shrinkSubst (map fst binders) (freeVarsT body) sb
        fvsb = Set.unions . map freeVarsT . elems $ sb'

-- Utilities

shrinkSubst :: [Name] -> Set Text -> Subst b -> Subst b
shrinkSubst binders ns subs =
  restrictKeys (substractKeys subs (map nameToText binders)) ns
 where substractKeys sb bs =
         filterWithKey (\k _ -> not (k `elem` bs)) sb

genBinderRenaming :: (Fresh m, Variableous e t) =>
                  Set Text -> [(Name, t)] -> m (Subst e)
genBinderRenaming _ [] = return empty
genBinderRenaming fvs ((Name x l, t) : xs)
   | x `Set.member` fvs = do
       x' <- freshName x l
       insert x (mkVar x' t l) <$> genBinderRenaming fvs xs
   | otherwise = genBinderRenaming fvs xs

renameVars :: Variableous e t => Subst e -> [Name] -> [Name]
renameVars sb = map (renameVar sb)

renameVar :: Variableous e t => Subst e -> Name -> Name
renameVar sb x = case lookup (nameToText x) sb of
            Nothing -> x
            Just e  -> case isVar e of
                         Just (y, _) -> y
                         Nothing -> error "variable should be substituted for a variable"
        --- SCM: we assume that renameVars always succeed.
        --       Do we need to raise a catchable error?
