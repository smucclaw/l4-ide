-- | Types needed during the scope and type checking phase.
module L4.TypeCheck.Types where

import Base
import L4.Annotation (HasSrcRange(..))
import L4.Parser.SrcSpan (SrcRange(..))
import L4.Syntax
import L4.TypeCheck.With

import Control.Applicative

type Environment  = Map RawName [Unique]
type EntityInfo   = Map Unique (Name, CheckEntity)
type Substitution = Map Int (Type' Resolved)

-- | Note that 'KnownType' does not imply this is a new generative type on its own,
-- because it includes type synonyms now. For type synonyms primarily, we also store
-- the arguments, so that we can properly substitute when instantiated.
--
data CheckEntity =
    KnownType Kind [Resolved] (TypeDecl Resolved)
  | KnownTerm (Type' Resolved) TermKind
  | KnownTypeVariable
  deriving stock (Eq, Generic, Show)
  deriving anyclass NFData

data TermKind =
    Computable -- ^ a variable with known definition (let or global)
  | Assumed
  | Local -- ^ a local variable (introduced by a lambda or pattern)
  | Constructor
  | Selector
  deriving stock (Eq, Generic, Show)
  deriving anyclass NFData

data CheckState =
  MkCheckState
    { environment  :: !Environment
    , entityInfo   :: !EntityInfo
    , errorContext :: !CheckErrorContext
    , substitution :: !Substitution
    , supply       :: !Int
    }
  deriving stock (Eq, Generic, Show)

data CheckErrorWithContext =
  MkCheckErrorWithContext
    { kind    :: !CheckError
    , context :: !CheckErrorContext
    }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data CheckError =
    OutOfScopeError Name (Type' Resolved)
  | KindError Kind [Type' Name]
  | TypeMismatch ExpectationContext (Type' Resolved) (Type' Resolved) -- expected, given
  | InconsistentNameInSignature Name (Maybe Name)
  | InconsistentNameInAppForm Name (Maybe Name)
  | NonDistinctError NonDistinctContext [[Name]]
  | AmbiguousTermError Name [(Resolved, Type' Resolved)]
  | AmbiguousTypeError Name [(Resolved, Kind)]
  | InternalAmbiguityError
  | IncorrectArgsNumberApp Resolved Int Int -- expected, given
  | IllegalApp Resolved (Type' Resolved) Int
  | IllegalAppNamed Resolved (Type' Resolved)
  | IncompleteAppNamed Resolved [OptionallyNamedType Resolved]
  | CheckInfo (Type' Resolved)
  | IllegalTypeInKindSignature (Type' Resolved)
  | MissingEntityInfo Resolved
  deriving stock (Eq, Generic, Show)
  deriving anyclass NFData

data ExpectationContext =
    ExpectAssumeSignatureContext (Maybe SrcRange) -- actual result range from the signature, if it exists
  -- | ExpectProjectionSelectorContext
  | ExpectIfConditionContext -- condition of if-then-else
  | ExpectPatternScrutineeContext (Expr Resolved) -- pattern type must match type of scrutinee
  | ExpectNotArgumentContext -- arg of NOT
  | ExpectConsArgument2Context -- second arg of cons
  | ExpectIfBranchesContext -- all branches of an if-then-else must have the same type
  | ExpectConsiderBranchesContext -- all branches of a consider must have the same type
  | ExpectHomogeneousListContext -- all elements of a list literal must have the same type
  | ExpectNamedArgContext Resolved Resolved -- function, name of arg
  | ExpectAppArgContext Bool Resolved Int -- projection?, function, number of arg
  | ExpectBinOpArgContext Text Int -- opname, number of arg (TODO: it would be better to have the token and its range here!)
  | ExpectDecideSignatureContext (Maybe SrcRange) -- actual result type range from the signature, if it exists
  deriving stock (Eq, Generic, Show)
  deriving anyclass NFData

data NonDistinctContext =
    NonDistinctConstructors
  | NonDistinctSelectors
  | NonDistinctQuantifiers
  | NonDistinctTypeAppForm
  | NonDistinctTermAppForm
  deriving stock (Eq, Generic, Show)
  deriving anyclass NFData

data CheckErrorContext =
    WhileCheckingDeclare Name CheckErrorContext
  | WhileCheckingDecide Name CheckErrorContext
  | WhileCheckingAssume Name CheckErrorContext
  | WhileCheckingExpression (Expr Name) CheckErrorContext
  | WhileCheckingPattern (Pattern Name) CheckErrorContext
  | WhileCheckingType (Type' Name) CheckErrorContext
  | None
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data Severity = SWarn | SError | SInfo
  deriving stock (Eq, Show)

instance HasSrcRange CheckErrorWithContext where
  rangeOf (MkCheckErrorWithContext e ctx) = rangeOf e <|> rangeOf ctx

instance HasSrcRange CheckErrorContext where
  rangeOf None = Nothing
  rangeOf (WhileCheckingDeclare n _)    = rangeOf n
  rangeOf (WhileCheckingDecide n _)     = rangeOf n
  rangeOf (WhileCheckingAssume n _)     = rangeOf n
  rangeOf (WhileCheckingExpression e _) = rangeOf e
  rangeOf (WhileCheckingPattern p _)    = rangeOf p
  rangeOf (WhileCheckingType t _)       = rangeOf t

instance HasSrcRange CheckError where
  rangeOf (OutOfScopeError n _)             = rangeOf n
  rangeOf (InconsistentNameInSignature n _) = rangeOf n
  rangeOf (InconsistentNameInAppForm n _)   = rangeOf n
  rangeOf _                                 = Nothing

newtype CheckEnv = MkCheckEnv { moduleUri :: NormalizedUri }

newtype Check a =
  MkCheck (CheckEnv -> CheckState -> [(With CheckErrorWithContext a, CheckState)])

runCheck :: Check a -> CheckEnv -> CheckState -> [(With CheckErrorWithContext a, CheckState)]
runCheck (MkCheck f) = f

runCheck' :: CheckEnv -> CheckState -> Check a -> [(With CheckErrorWithContext a, CheckState)]
runCheck' e s (MkCheck f) = f e s


-- NOTES on the Check monad:
--
-- Contrary to my first belief, we cannot
-- afford making the complete CheckState global, because the substitution
-- must be branch-dependent. Unification can create bindings for a
-- previously introduced unification variable (prior to branches) that
-- become invalid if the branch is not selected. So we have to backtrack
-- the current substitution.
--
-- I'm not sure if the environment and supply should be backtracked.
-- The supply would be better / safer to be global? Why? If we ever want
-- to accept / merge the results of several branches, then we'd have to
-- be careful that we don't get inconsistencies. On the other hand, we
-- probably only want to do this if unification variables introduced in
-- the different branches have been unambiguously resolved, so it should
-- probably be fine.

data CheckResult =
  MkCheckResult
    { program      :: Module  Resolved
    , errors       :: [CheckErrorWithContext]
    , substitution :: Substitution
    , environment  :: Environment
    , entityInfo   :: EntityInfo
    }
  deriving stock (Eq, Show)

-- -------------------
-- Instances for Check
-- -------------------

instance Functor Check where
  fmap = liftM

instance Applicative Check where
  pure x =
    MkCheck $ \_ s -> [(Plain x, s)]
  (<*>) = ap

instance Monad Check where
  (>>=) :: forall a b. Check a -> (a -> Check b) -> Check b
  m >>= f =
    MkCheck $ \e s ->
      let
        results = runCheck m e s
      in
        concatMap (\ (wea, s') -> distributeWith (fmap (runCheck' e s' . f) wea)) results

instance MonadState CheckState Check where
  get :: Check CheckState
  get = MkCheck $ \_ s -> [(Plain s, s)]

  put :: CheckState -> Check ()
  put s = MkCheck $ \_ _ -> [(Plain (), s)]

instance MonadReader CheckEnv Check where
  ask = MkCheck \e s -> [(Plain e, s)]
  local f c = MkCheck $ runCheck c . f

instance MonadWith CheckErrorWithContext Check where
  with :: CheckErrorWithContext -> Check ()
  with e = MkCheck $ \_ s -> [(With e (Plain ()), s)]

instance Alternative Check where
  empty :: Check a
  empty = MkCheck $ \_ _ -> []

  (<|>) :: Check a -> Check a -> Check a
  (<|>) m1 m2 = MkCheck $ \e s -> runCheck m1 e s ++ runCheck m2 e s
