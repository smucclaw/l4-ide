{-# LANGUAGE DataKinds #-}
module L4.TypeCheck where

{-
import Control.Applicative
import Control.Monad
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import Optics.Core

import qualified Simala.Expr.Type as Simala
import L4.Syntax

type KEnv = Set Name
type TEnv = Map Name (Type' Resolved)

data TypedExpr =
  MkTypedExpr
    (Expr Resolved)
    (Type' Resolved)

data Check a

instance Functor Check
instance Applicative Check
instance Monad Check
instance Alternative Check

data Resolved

instance Eq Resolved

decls :: Traversal' (Program n) (Decl n)
decls = gposition @2 % traversed % gposition @4 % traversed

decls' :: forall n. Traversal' (Program n) (Decl n)
decls' = gplate @(Decl n)

names :: Traversal' (Program Name) Name
names = gplate @Name

exprs :: forall n. Traversal' (Program n) (Expr n)
exprs = gplate @(Expr n)

lookupVar :: Name -> Check Resolved
lookupVar = undefined

extractType :: Resolved -> Check (Type' Resolved)
extractType = undefined

withExtendedEnvironment :: KEnv -> TEnv -> Check a -> Check a
withExtendedEnvironment = undefined

checkProgram :: Program Name -> Check (Program Resolved)
checkProgram program@(MkProgram ann sections) = do
  kenv <- collectProgram program
  withExtendedEnvironment kenv undefined (MkProgram ann <$> traverse checkSection sections)

checkSection = undefined

collectProgram :: Program Name -> Check KEnv
collectProgram (MkProgram ann sections) =
  go Set.empty sections
  where
    go :: KEnv -> [Section Name] -> Check KEnv
    go !acc []                             = pure acc
    go !acc (MkSection ann _lvl _n decls : ss) =
      undefined

checkTypeSig :: TypeSig Name -> Check (KEnv, TEnv, TypeSig Resolved, Type' Resolved)
checkTypeSig (MkTypeSig ann givens giveth) = undefined

checkDecide :: Decide Name -> Check (Decide Resolved)
checkDecide (MkDecide ann tsig clauses) = do
  (kenv, tenv, tsig', t) <- checkTypeSig tsig
  clauses' <- withExtendedEnvironment kenv tenv $ traverse (\ clause -> checkClause clause t) clauses
  pure (MkDecide ann tsig' clauses')

checkClause :: Clause Name -> Type' Resolved -> Check (Clause Resolved)
checkClause (GuardedClause ann e g) t =
  GuardedClause ann <$> checkExpr e t <*> checkGuard g

checkGuard :: Guard Name -> Check (Guard Resolved)
checkGuard (PlainGuard ann e) = PlainGuard ann <$> checkExpr e (Boolean mempty)
checkGuard (Otherwise ann)    = pure $ Otherwise ann

inferExpr :: Expr Name -> Check TypedExpr
inferExpr (And ann e1 e2) = do
  e1' <- checkExpr e1 (Boolean mempty)
  e2' <- checkExpr e2 (Boolean mempty)
  pure (MkTypedExpr (And ann e1' e2') (Boolean mempty))
inferExpr (Or ann e1 e2) = do
  e1' <- checkExpr e1 (Boolean mempty)
  e2' <- checkExpr e2 (Boolean mempty)
  pure (MkTypedExpr (Or ann e1' e2') (Boolean mempty))
inferExpr (Not ann e) = do
  e' <- checkExpr e (Boolean mempty)
  pure (MkTypedExpr (Not ann e') (Boolean mempty))
inferExpr (Var ann n) = do
  r <- lookupVar n
  t <- extractType r
  pure (MkTypedExpr (Var ann r) t)
inferExpr (Is ann e1 e2) = do
  MkTypedExpr e1' t <- inferExpr e1
  e2' <- checkExpr e2 t -- TODO: not any type is equality-compatible!
  pure (MkTypedExpr (Is ann e1' e2') (Boolean mempty))
inferExpr (Proj ann e l) = do
  MkTypedExpr e' t <- inferExpr e
  case t of
    Record ann rs -> do
      tl <- inferLabel rs l
      pure (MkTypedExpr (Proj ann e' l) tl)
    _ -> recordExpectedError

inferLabel :: [TypedName Resolved] -> Label -> Check (Type' Resolved)
inferLabel rs l =
  case go rs of
    Just t  -> pure t
    Nothing -> recordProjectionError rs l
  where
    go [] = Nothing
    go (MkTypedName ann r t : rs')
      | resolvedName r == l = Just t
      | otherwise           = go rs'

resolvedName :: Resolved -> Name
resolvedName = undefined

recordExpectedError :: Check a
recordExpectedError =
  undefined

recordProjectionError :: [TypedName Resolved] -> Label -> Check a
recordProjectionError =
  undefined
checkExpr :: Expr Name -> Type' Resolved -> Check (Expr Resolved)
checkExpr e t = do
  MkTypedExpr e' t' <- inferExpr e
  guard (t == t')
  pure e'

-- traversals:
--
-- 1. build map of user-defined types
-- 2. check well-scopedness / well-kindedness of user-defined types
-- 3. check well-scopedness / well-typedness of terms
-- 4. translate terms to Simala

-- for the translation, what do we really need?
--
-- in a program, only the "Decide" decls are being translated
-- into functions

-- data SimalaExpr =
--     Undefined
--   | IfThenElse SimalaExpr SimalaExpr SimalaExpr
--   | Builtin SimalaBuiltin [SimalaExpr]
--   | Fun [Name] SimalaExpr

translateDecide :: Decide Name -> Simala.Expr
translateDecide (MkDecide _ann tsig clauses) =
  translateTypeSig tsig (translateClauses clauses)

translateTypeSig :: TypeSig Name -> Simala.Expr -> Simala.Expr
translateTypeSig (MkTypeSig _ann (MkGivenSig _ tns) _) e =
  Simala.Fun Simala.Transparent (extractNames tns) e

extractNames :: [TypedName Name] -> [Simala.Name]
extractNames = map (\ (MkTypedName _ann (Name _  n) _) -> n)

translateClauses :: [Clause Name] -> Simala.Expr
translateClauses [] = Simala.Undefined
translateClauses (GuardedClause _ann e (Otherwise _) : _) = translateExpr e
translateClauses (GuardedClause _ann e (PlainGuard _ c) : clauses) =
  Simala.Builtin Simala.IfThenElse [translateExpr c, translateExpr e, translateClauses clauses]

translateExpr :: Expr Name -> Simala.Expr
translateExpr (And _ann e1 e2) = Simala.Builtin Simala.And [translateExpr e1, translateExpr e2]
translateExpr (Or _ann e1 e2)  = Simala.Builtin Simala.Or  [translateExpr e1, translateExpr e2]
translateExpr (Is _ann e1 e2)  = Simala.Builtin Simala.Eq  [translateExpr e1, translateExpr e2]
translateExpr (Not _ann e)     = Simala.Builtin Simala.Not [translateExpr e]
translateExpr (Proj _ann e (Name _ l))  = Simala.Project (translateExpr e) l
translateExpr (Var _ann (Name _ n))     = Simala.Var n
-}
