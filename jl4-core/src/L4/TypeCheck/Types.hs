-- | Types needed during the scope and type checking phase.
module L4.TypeCheck.Types where

import Base
import L4.Annotation (HasSrcRange(..), HasAnno(..), AnnoExtra, AnnoToken, emptyAnno)
import L4.Parser.SrcSpan (SrcRange(..))
import L4.Syntax
import L4.TypeCheck.With

import Control.Applicative
import L4.Lexer (PosToken)
import qualified Data.Map.Strict as Map
import qualified Data.List.NonEmpty as NE

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
  | KnownSection (Section Resolved)
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
    { substitution :: !Substitution
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

data CheckEnv =
  MkCheckEnv
    { moduleUri    :: !NormalizedUri
    , environment  :: !Environment
    , entityInfo   :: !EntityInfo
    , errorContext :: !CheckErrorContext
    }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

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
    { program      :: !(Module  Resolved)
    , errors       :: ![CheckErrorWithContext]
    , substitution :: !Substitution
    , environment  :: !Environment
    , entityInfo   :: !EntityInfo
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

-- ----------------------------------------------------------------------------
-- Primitives for Uniques, Errors and similar.
-- ----------------------------------------------------------------------------

step :: Check Int
step = do
  current <- use #supply
  let next = current + 1
  assign #supply next
  pure current

newUnique :: Check Unique
newUnique = do
  i <- step
  u <- asks (.moduleUri)
  pure (MkUnique 'c' i u)

addError :: CheckError -> Check ()
addError e = do
  ctx <- asks (.errorContext)
  with (MkCheckErrorWithContext e ctx)

choose :: [Check a] -> Check a
choose = asum

anyOf :: [a] -> Check a
anyOf = asum . fmap pure

-- ----------------------------------------------------------------------------
-- JL4 specific primitives
-- ----------------------------------------------------------------------------

fresh :: RawName -> Check (Type' Resolved)
fresh prefix = do
  i <- step
  pure (InfVar emptyAnno prefix i)

outOfScope :: Name -> Type' Resolved -> Check Resolved
outOfScope n t = do
  addError (OutOfScopeError n t)
  u <- newUnique
  pure (OutOfScope u n)

ambiguousTerm :: Name -> [(Resolved, Type' Resolved)] -> Check Resolved
ambiguousTerm n xs = do
  addError (AmbiguousTermError n xs)
  u <- newUnique
  pure (OutOfScope u n)

ambiguousType :: Name -> [(Resolved, Kind)] -> Check Resolved
ambiguousType n xs = do
  addError (AmbiguousTypeError n xs)
  u <- newUnique
  pure (OutOfScope u n)

-- ----------------------------------------------------------------------------
-- JL4 specific primitives for resolving names
-- ----------------------------------------------------------------------------

resolvedType :: Resolved -> Check Resolved
resolvedType n = do
  ei <- asks (.entityInfo)
  case Map.lookup (getUnique n) ei of
    Nothing -> do
      -- TODO: there are cases where this situation is a clear bug.
      -- Sometimes, it is fine, e.g. when the 'Resolved' is 'OutOfScope'.
      -- Tricky what to do here.
      -- addError $ MissingEntityInfo n
      pure n
    Just (_, checkEntity) ->
      pure case checkEntity of
        KnownType kind _resolved _tyDecl -> setAnnResolvedKindOfResolved kind n
        KnownTerm ty _term -> setAnnResolvedTypeOfResolved ty n
        KnownTypeVariable -> setAnnResolvedKindOfResolved 0 n
        KnownSection _ -> n -- TODO: this is probably not what we want, maybe some internal error?

lookupRawNameInEnvironment :: RawName -> Check [(Unique, Name, CheckEntity)]
lookupRawNameInEnvironment rn = do
  -- NOTE: split in a list of qualifiers and the actual name
  let (tn, qs)  = case rn of
        QualifiedName n' qs' -> (NormalName n', NE.toList qs')
        n -> (n, [])

  env <- asks (.environment)
  ei  <- asks (.entityInfo)

  let -- NOTE: from a list of qualifiers, try to build all paths through sections that may be
      -- meant with the qualified name
      -- The outer list describes the path, the inner list describes all possibilities at a
      -- section level.
      mkCandidateSections :: [Text] -> [[Section Resolved]]
      mkCandidateSections =
        foldl
          (\acc q -> case Map.lookup (NormalName q) env of
            Just us ->  mapMaybe isKnownSection us : acc

            -- NOTE: this case is important because it cuts off options as soon as we run into unknown sections,
            -- i.e. Foo.Bar.baz where Foo does exist (and contains baz) but Bar doesn't
            Nothing -> [[]]
          )
          []

      isKnownSection :: Unique -> Maybe (Section Resolved)
      isKnownSection u = do
        (_n, KnownSection s) <- Map.lookup u ei
        pure s

      -- NOTE: this is used to filter the paths through sections down to the ones that are valid.
      -- We return the sections that are contained in existing sections
      sectionInSection :: [[Section Resolved]] -> [Section Resolved]
      -- NOTE: we have to treat this specially otherwise the recursive call will always
      -- bottom out at an empty list of sections which makes the entire algorithm fail
      sectionInSection [secs] = secs
      sectionInSection (secs : secss) =
        mapMaybe
          (\sec@(MkSection _ mn _ _) -> do
             u <- getUnique <$> mn
             guard $ any (isTopLevelBindingInSection u) $ sectionInSection secss
             pure sec
          )
          secs
      sectionInSection [] = []

      reversedSections = sectionInSection $ mkCandidateSections qs

      proc :: Unique -> Maybe (Unique, Name, CheckEntity)
      proc u = do
        (o, ce) <- Map.lookup u ei
        -- NOTE: if there are no qualifiers, we don't have to even try to sort out
        -- by qualifiers (i.e. nothing changes if no qualifiers are used)
        guard $ null qs || any (isTopLevelBindingInSection u) reversedSections

        pure (u, o, ce)

      candidates :: [(Unique, Name, CheckEntity)]
      candidates = mapMaybe proc (Map.findWithDefault [] tn env)

  pure candidates

isTopLevelBindingInSection :: Unique -> Section Resolved -> Bool
isTopLevelBindingInSection u (MkSection _a  _mn _maka decls) = any (elem u . matchesUnq) decls
  where
  matchesUnq = \case
    Declare _ (MkDeclare _ _ af _) -> afUnq af
    Decide _ (MkDecide _ _ af _) -> afUnq af
    Assume _ (MkAssume _ _ af _) -> afUnq af
    Directive _ _ -> []
    Import _ _ -> []
    -- NOTE: Sections are a toplevel binding in the current section but can also contain further
    -- toplevel bindings
    Section _ (MkSection _ mr _ decls') -> foldMap (\r -> [getUnique r]) mr <> foldMap matchesUnq decls'

  afUnq = map getUnique . appFormHeads

resolveTerm' :: (TermKind -> Bool) -> Name -> Check (Resolved, Type' Resolved)
resolveTerm' p n = do
  options <- lookupRawNameInEnvironment (rawName n)
  case mapMaybe proc options of
    [] -> do
      v <- fresh (rawName n)
      rn <- outOfScope (setAnnResolvedType v n) v
      pure (rn, v)
    [x] -> pure x
    xs -> anyOf xs <|> do
      v <- fresh (rawName n)
      rn <- ambiguousTerm (setAnnResolvedType v n) xs
      pure (rn, v)
  where
    proc :: (Unique, Name, CheckEntity) -> Maybe (Resolved, Type' Resolved)
    proc (u, o, KnownTerm t tk) | p tk = Just (Ref (setAnnResolvedType t n) u o, t)
    proc _                             = Nothing

resolveTerm :: Name -> Check (Resolved, Type' Resolved)
resolveTerm = resolveTerm' (const True)

_resolveSelector :: Name -> Check (Resolved, Type' Resolved)
_resolveSelector = resolveTerm' (== Selector)

resolveConstructor :: Name -> Check (Resolved, Type' Resolved)
resolveConstructor = resolveTerm' (== Constructor)

setAnnResolvedType ::
     (HasAnno a, AnnoToken a ~ PosToken, AnnoExtra a ~ Extension)
  => Type' Resolved -> a -> a
setAnnResolvedType t x =
  setAnno (set annInfo (Just (TypeInfo t Nothing)) (getAnno x)) x

setAnnResolvedKind ::
     (HasAnno a, AnnoToken a ~ PosToken, AnnoExtra a ~ Extension)
  => Kind -> a -> a
setAnnResolvedKind k x =
  setAnno (set annInfo (Just (KindInfo k)) (getAnno x)) x

setAnnResolvedTypeOfResolved :: Type' Resolved -> Resolved -> Resolved
setAnnResolvedTypeOfResolved t = \case
  Def u n -> Def u (setAnnResolvedType t n)
  Ref r u o -> Ref (setAnnResolvedType t r) u o
  OutOfScope u n -> OutOfScope u (setAnnResolvedType t n)

setAnnResolvedKindOfResolved :: Kind -> Resolved -> Resolved
setAnnResolvedKindOfResolved k = \case
  Def u n -> Def u (setAnnResolvedKind k n)
  Ref r u o -> Ref (setAnnResolvedKind k r) u o
  OutOfScope u n -> OutOfScope u (setAnnResolvedKind k n)
