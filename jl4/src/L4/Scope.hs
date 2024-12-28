module L4.Scope where

{-
import Control.Monad.State
import Data.Functor.Identity
import Data.Function
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Optics.State

import L4.Syntax

newtype Scope a = MkScope (ScopeState -> (a, ScopeState))
  deriving (Functor, Applicative, Monad, MonadState ScopeState) via State ScopeState

data ScopeState =
  MkScopeState
    { currentScope :: [Env]
    , nextUnique   :: Unique
    , scopeErrors  :: [ScopeError]
    }
  deriving stock Generic

type Unique = Int

data Resolved =
  MkResolved
    { unique :: Unique
    , name   :: Name
    , kind   :: NameKind
    }

data ScopeError =
  MkScopeError Name

data NameKind =
    RecordName [Resolved]
  | NormalName
  | Defined
  | Unresolved

unresolved :: Name -> Resolved
unresolved n =
  MkResolved (-1) n Unresolved

instance Eq Resolved where
  (==) = (==) `on` (.unique)

instance Ord Resolved where
  compare = compare `on` (.unique)

type Env = Map Name Resolved

data Path

-- | Assigns a new unique to the name.
--
declare :: Name -> Scope Resolved
declare n = do
  u <- newUnique
  pure (MkResolved u n Defined)

resolve :: Name -> Scope Resolved
resolve n = do
  envs <- use #currentScope
  case Map.lookup n (Map.unions envs) of
    Nothing -> do
      scopeError n
      pure (unresolved n)
    Just r  -> pure r

scopeError :: Name -> Scope ()
scopeError n =
  modifying #scopeErrors (MkScopeError n :)

newUnique :: Scope Unique
newUnique = do
  u <- use #nextUnique
  assign #nextUnique (u + 1)
  pure u

-- | Adds a resolved name to the current scope.
--
addToScope :: Resolved -> Scope ()
addToScope = undefined

addRecordToScope :: Resolved -> Env -> Scope ()
addRecordToScope = undefined

pushEnv :: Env -> Scope ()
pushEnv env = do
  modifying #currentScope (env :)

popEnv :: Scope Env
popEnv = do
  envs <- use #currentScope
  case envs of
    [] -> error "internal error: empty scope stack"
    (e : es) -> do
      assign #currentScope es
      pure e

captureScope :: Scope a -> Scope (Env, a)
captureScope m = do
  pushEnv Map.empty
  x <- m
  env <- popEnv
  pure (env, x)

withExtendedScope :: Env -> Scope a -> Scope a
withExtendedScope e m = do
  pushEnv e
  x <- m
  _ <- popEnv
  pure x

scopeCheckProgram :: Program Name -> Scope (Program Resolved)
scopeCheckProgram (MkProgram ann sections) =
  MkProgram ann <$> traverse scopeCheckSection sections

scopeCheckSection :: Section Name -> Scope (Section Resolved)
scopeCheckSection (MkSection ann lvl n decls) = do
  r <- declare n -- we do not currently allow to refer to section names
  decls' <- traverse scopeCheckDecl decls
  pure (MkSection ann lvl r decls')

scopeCheckDecl :: Decl Name -> Scope (Decl Resolved)
scopeCheckDecl (Declare ann d) = Declare ann <$> scopeCheckDeclare d
scopeCheckDecl (Decide ann d)  = Decide ann <$> scopeCheckDecide d

scopeCheckDeclare :: Declare Name -> Scope (Declare Resolved)
scopeCheckDeclare (MkDeclare ann n t) = do
  r <- declare n
  t' <- scopeCheckType r t
  addToScope r
  pure (MkDeclare ann r t')

scopeCheckType :: Resolved -> Type' Name -> Scope (Type' Resolved)
scopeCheckType r (NamedType ann n) = do
  addToScope r
  NamedType ann <$> resolve n
scopeCheckType r (Enum ann ns) = do
  addToScope r
  Enum ann <$> traverse declare ns
scopeCheckType r (Record ann tns) = do
  (env, t') <- captureScope (Record ann <$> traverse scopeCheckTypedName tns)
  addRecordToScope r env
  pure t'
scopeCheckType r (Boolean ann) = do
  addToScope r
  pure $ Boolean ann

scopeCheckTypedName :: TypedName Name -> Scope (TypedName Resolved)
scopeCheckTypedName (MkTypedName ann n t) = do
  r <- declare n
  t' <- scopeCheckType r t
  addToScope r
  pure (MkTypedName ann r t')

scopeCheckDecide :: Decide Name -> Scope (Decide Resolved)
scopeCheckDecide (MkDecide ann tsig clauses) = do
  (env, tsig') <- captureScope (scopeCheckTypeSig tsig)
  clauses' <- withExtendedScope env $
    traverse scopeCheckClause clauses
  pure (MkDecide ann tsig' clauses')

scopeCheckClause :: Clause Name -> Scope (Clause Resolved)
scopeCheckClause (GuardedClause ann e g) =
  GuardedClause ann <$> scopeCheckExpr e <*> scopeCheckGuard g

scopeCheckTypeSig :: TypeSig Name -> Scope (TypeSig Resolved)
scopeCheckTypeSig (MkTypeSig ann (MkGivenSig annGiven tns) tn) = do
  tns' <- traverse scopeCheckTypedName tns
  (_env, tn') <- captureScope (do
    traverse (\(MkGivethSig annGiveth names) -> MkGivethSig annGiveth <$> scopeCheckTypedName names) tn) -- TODO
  pure (MkTypeSig ann (MkGivenSig annGiven tns') tn')

scopeCheckExpr :: Expr Name -> Scope (Expr Resolved)
scopeCheckExpr (And ann e1 e2) = And ann <$> scopeCheckExpr e1 <*> scopeCheckExpr e2
scopeCheckExpr (Or ann e1 e2) = Or ann <$> scopeCheckExpr e1 <*> scopeCheckExpr e2
scopeCheckExpr (Is ann e1 e2) = Is ann <$> scopeCheckExpr e1 <*> scopeCheckExpr e2
scopeCheckExpr (Not ann e) = Not ann <$> scopeCheckExpr e
scopeCheckExpr (Proj ann e label) = Proj ann <$> scopeCheckExpr e <*> pure label
scopeCheckExpr (Var ann n) = Var ann <$> resolve n

scopeCheckGuard :: Guard Name -> Scope (Guard Resolved)
scopeCheckGuard (PlainGuard ann e) = PlainGuard ann <$> scopeCheckExpr e
scopeCheckGuard (Otherwise ann) = pure (Otherwise ann)
-}
