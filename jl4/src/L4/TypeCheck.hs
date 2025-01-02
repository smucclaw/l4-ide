{-# LANGUAGE DataKinds #-}
module L4.TypeCheck where

-- We perform scope checking and type checking in one go.
-- The reason is that we employ type-directed name resolution:
-- we allow the same identifier to be in scope several times,
-- with different types, and we allow references to be resolved
-- if only one of the available definitions fits. [We could
-- theoretically even allow ambiguity, and treat a definition
-- as being a multi-definition with several different valid
-- instantiations.]
--
-- Within scope and type checking, we perform several passes of
-- the program / module:
--
-- 1a. collect all user-defined types
--
--     Types cannot depend on terms, only on other types.
--     Our kind system is simple: we have a kind TYPE of
--     inhabited, fully instantiated types, and we allow
--     abstraction only over TYPE. In other words, the kinds
--     of types are uniquely determined by their arity.
--
--     We allow forward references (and thus in principle
--     mutually recursive types).
--
-- 1b. scope and kind-check the bodies of user-defined types

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.Logic
import Control.Monad.Identity
import Control.Monad.State
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import Optics.Core

import L4.Syntax

type KEnv = Set Name
type TEnv = Map Name (Type' Resolved)

data Typed a =
  MkTyped
    (a Resolved)
    (Type' Resolved)

newtype Check a =
  MkCheck (LogicT (StateT CheckState (ExceptT CheckError Identity)) a)
  deriving newtype
    ( Functor, Applicative, Monad
    , Alternative
    , MonadState CheckState
    , MonadError CheckError
    , MonadLogic
    )

data CheckError
data CheckState =
  MkCheckState
    { environment  :: !(Map Name [CheckEntity])
    , substitution :: !(Map Int (Type' Resolved))
    , supply       :: !Int
    }
  deriving stock (Eq, Show)

data CheckEntity =
    KnownType (TypeDecl Resolved)
  | KnownTerm (Type' Resolved) TermKind
  | QuantifiedType
  deriving stock (Eq, Show)

data TermKind =
    Computable
  | Constructor
  | Selector
  deriving stock (Eq, Show)

abort :: CheckError -> Check a
abort = throwError

choose :: [Check a] -> Check a
choose = asum

-- Functionality needed for the Check monad:
--
-- error messages (possibly soft and hard errors for non-determinism)
--
-- abort :: CheckError -> Check a      -- hard error
-- throw :: CheckException -> Check a  -- soft error
-- -- unclear whether we need to be able to catch these
--
-- non-determinism (TDNR, multiple types per identifier)
--
-- choose :: [Check a] -> Check a
--
-- environments (known types and their definitions, known in-scope variables and types)
--
-- effectively:
--
-- Map Name TypeDecl   -- possibly an extended form of TypeDecl allowing for unknown types
-- Map Name Type       -- possibly with an extension allowing to identify record labels and constructors
--
-- unification variable supply
--
-- fresh :: Text -> Check Type
--
-- current substitution
--
-- effectively:
--
-- Map Int Type
--
-- potentially logging

data Resolved =
    Def Name
  | Ref Name Name
  deriving stock (Eq, Show)

{-
decls :: Traversal' (Program n) (Decl n)
decls = gposition @2 % traversed % gposition @4 % traversed

decls' :: forall n. Traversal' (Program n) (Decl n)
decls' = gplate @(Decl n)

names :: Traversal' (Program Name) Name
names = gplate @Name

exprs :: forall n. Traversal' (Program n) (Expr n)
exprs = gplate @(Expr n)
-}

lookupVar :: Name -> Check Resolved
lookupVar = undefined

extractType :: Resolved -> Check (Type' Resolved)
extractType = undefined

{-
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
checkGuard (PlainGuard ann e) = PlainGuard ann <$> checkExpr e boolean
checkGuard (Otherwise ann)    = pure $ Otherwise ann
-}

boolean :: Type' Resolved
boolean = TyApp mempty (Def (PreDef mempty "BOOLEAN")) []

number :: Type' Resolved
number = TyApp mempty (Def (PreDef mempty "NUMBER")) []

checkBinOp ::
     Type' Resolved
  -> Type' Resolved
  -> Type' Resolved
  -> (Anno -> Expr Resolved -> Expr Resolved -> Expr Resolved)
  -> Anno
  -> Expr Name
  -> Expr Name
  -> Check (Typed Expr)
checkBinOp t1 t2 tr op ann e1 e2 = do
  e1' <- checkExpr e1 t1
  e2' <- checkExpr e2 t2
  pure (MkTyped (op ann e1' e2') tr)

inferExpr :: Expr Name -> Check (Typed Expr)
inferExpr (And ann e1 e2) = 
  checkBinOp boolean boolean boolean And ann e1 e2
inferExpr (Or ann e1 e2) =
  checkBinOp boolean boolean boolean Or ann e1 e2
inferExpr (Implies ann e1 e2) =
  checkBinOp boolean boolean boolean Implies ann e1 e2
inferExpr (Equals ann e1 e2) =
      checkBinOp boolean boolean boolean Equals ann e1 e2
  <|> checkBinOp number  number  boolean Equals ann e1 e2
inferExpr (Not ann e) = do
  e' <- checkExpr e boolean
  pure (MkTyped (Not ann e') boolean)
inferExpr (Plus ann e1 e2) =
  checkBinOp number number number Plus ann e1 e2
inferExpr (Minus ann e1 e2) =
  checkBinOp number number number Minus ann e1 e2
inferExpr (Times ann e1 e2) =
  checkBinOp number number number Times ann e1 e2
inferExpr (DividedBy ann e1 e2) =
  checkBinOp number number number DividedBy ann e1 e2
inferExpr (Cons _ann _e1 _e2) =
  undefined -- TODO: polymorphism
inferExpr (Proj ann e l) = do
  MkTyped e' te <- inferExpr e
  (l', tl) <- checkRecordLabel te l
  pure (MkTyped (Proj ann e' l') tl)
inferExpr (Var ann n) = do
  r <- lookupVar n
  t <- extractType r
  pure (MkTyped (Var ann r) t)
inferExpr (Lam _ann _givens _e) =
  undefined -- TODO: fresh variables
inferExpr (App _ann _n _es) =
  undefined
inferExpr (IfThenElse ann e1 e2 e3) = do
  e1' <- checkExpr e1 boolean
  MkTyped e2' t2 <- inferExpr e2
  e3' <- checkExpr e3 t2
  pure (MkTyped (IfThenElse ann e1' e2' e3') t2)
inferExpr (Consider _ann _e _branches) =
  undefined
inferExpr (ParenExpr ann e) = do
  MkTyped e' t <- inferExpr e
  pure (MkTyped (ParenExpr ann e') t)

checkRecordLabel :: Type' Resolved -> Name -> Check (Resolved, Type' Resolved)
checkRecordLabel = undefined



{-
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
-}

checkExpr :: Expr Name -> Type' Resolved -> Check (Expr Resolved)
checkExpr e t = do
  MkTyped e' t' <- inferExpr e
  guard (t == t')
  pure e'

{-
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
