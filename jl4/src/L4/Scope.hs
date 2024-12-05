module L4.Scope where

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
scopeCheckProgram (MkProgram sections) =
  MkProgram <$> traverse scopeCheckSection sections

scopeCheckSection :: Section Name -> Scope (Section Resolved)
scopeCheckSection (MkSection lvl n decls) = do
  r <- declare n -- we do not currently allow to refer to section names
  decls' <- traverse scopeCheckDecl decls
  pure (MkSection lvl r decls')

scopeCheckDecl :: Decl Name -> Scope (Decl Resolved)
scopeCheckDecl (Declare d) = Declare <$> scopeCheckDeclare d
scopeCheckDecl (Decide d)  = Decide <$> scopeCheckDecide d

scopeCheckDeclare :: Declare Name -> Scope (Declare Resolved)
scopeCheckDeclare (MkDeclare n t) = do
  r <- declare n
  t' <- scopeCheckType r t
  addToScope r
  pure (MkDeclare r t')

scopeCheckType :: Resolved -> Type' Name -> Scope (Type' Resolved)
scopeCheckType r (NamedType n) = do
  addToScope r
  NamedType <$> resolve n
scopeCheckType r (Enum ns) = do
  addToScope r
  Enum <$> traverse declare ns
scopeCheckType r (Record tns) = do
  (env, t') <- captureScope (Record <$> traverse scopeCheckTypedName tns)
  addRecordToScope r env
  pure t'
scopeCheckType r Boolean = do
  addToScope r
  pure Boolean
  
scopeCheckTypedName :: TypedName Name -> Scope (TypedName Resolved)
scopeCheckTypedName (MkTypedName n t) = do
  r <- declare n
  t' <- scopeCheckType r t
  addToScope r
  pure (MkTypedName r t')
  
scopeCheckDecide :: Decide Name -> Scope (Decide Resolved)
scopeCheckDecide (MkDecide tsig clauses) = do
  (env, tsig') <- captureScope (scopeCheckTypeSig tsig)
  clauses' <- withExtendedScope env $
    traverse scopeCheckClause clauses
  pure (MkDecide tsig' clauses')

scopeCheckClause :: Clause Name -> Scope (Clause Resolved)
scopeCheckClause (GuardedClause e g) =
  GuardedClause <$> scopeCheckExpr e <*> scopeCheckGuard g

scopeCheckTypeSig :: TypeSig Name -> Scope (TypeSig Resolved)
scopeCheckTypeSig (MkTypeSig tns tn) = do
  tns' <- traverse scopeCheckTypedName tns
  (_env, tn') <- captureScope (traverse scopeCheckTypedName tn) -- TODO
  pure (MkTypeSig tns' tn')

scopeCheckExpr :: Expr Name -> Scope (Expr Resolved)
scopeCheckExpr (And e1 e2) = And <$> scopeCheckExpr e1 <*> scopeCheckExpr e2
scopeCheckExpr (Or e1 e2) = Or <$> scopeCheckExpr e1 <*> scopeCheckExpr e2
scopeCheckExpr (Is e1 e2) = Is <$> scopeCheckExpr e1 <*> scopeCheckExpr e2
scopeCheckExpr (Not e) = Not <$> scopeCheckExpr e
scopeCheckExpr (Proj e label) = Proj <$> scopeCheckExpr e <*> pure label
scopeCheckExpr (Var n) = Var <$> resolve n

scopeCheckGuard :: Guard Name -> Scope (Guard Resolved)
scopeCheckGuard (PlainGuard e) = PlainGuard <$> scopeCheckExpr e
scopeCheckGuard Otherwise = pure Otherwise
