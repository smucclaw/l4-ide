-- | Types needed during the scope and type checking phase.
{-# LANGUAGE UndecidableInstances #-}
module L4.TypeCheck.Types where

import Base
import qualified Optics
import L4.Annotation (HasSrcRange(..), HasAnno(..), AnnoExtra, AnnoToken, emptyAnno)
import L4.Lexer (PosToken)
import L4.Parser.SrcSpan (SrcRange(..), SrcPos)
import L4.Syntax
import L4.TypeCheck.With
import qualified L4.Utils.IntervalMap as IV

import Control.Applicative
import qualified Data.Map.Strict as Map
import qualified Generics.SOP as SOP
import Optics.Core (gplate, traverseOf, (%), (?~))
import Control.Exception (throw, Exception)

type Environment  = Map RawName [Unique]
type EntityInfo   = Map Unique (Name, CheckEntity)
type Substitution = Map Int (Type' Resolved)
type RangeMap     = IV.IntervalMap SrcPos
type InfoMap      = RangeMap Info
type ScopeMap     = RangeMap (Environment, EntityInfo)
type NlgMap       = RangeMap Nlg

-- | Note that 'KnownType' does not imply this is a new generative type on its own,
-- because it includes type synonyms now. For type synonyms primarily, we also store
-- the arguments, so that we can properly substitute when instantiated.
--
data CheckEntity =
    KnownType Kind [Resolved] (Maybe (Type' Resolved))
  | KnownTerm (Type' Resolved) TermKind
  | KnownSection (Section Resolved)
  | KnownTypeVariable
  deriving stock (Eq, Generic, Show)
  deriving anyclass NFData

data CheckState =
  MkCheckState
    { substitution :: !Substitution
    , supply       :: !Int
    , infoMap      :: !InfoMap
    , scopeMap     :: !ScopeMap
    , nlgMap       :: !NlgMap
    }
  deriving stock (Generic)

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
  | AmbiguousOperatorError Text
  | AmbiguousTypeError Name [(Resolved, Kind)]
  | InternalAmbiguityError
  | IncorrectArgsNumberApp Resolved Int Int -- expected, given
  | IllegalApp Resolved (Type' Resolved) Int
  | IllegalAppNamed Resolved (Type' Resolved)
  | IncompleteAppNamed Resolved [OptionallyNamedType Resolved]
  | CheckInfo (Type' Resolved)
  | CheckWarning CheckWarning
  | IllegalTypeInKindSignature (Type' Resolved)
  | MissingEntityInfo Resolved
  | DesugarAnnoRewritingError (Expr Name) HoleInfo
  deriving stock (Eq, Generic, Show)
  deriving anyclass NFData

data CheckWarning
  = PatternMatchRedundant [Branch Resolved]
  | PatternMatchesMissing [BranchLhs Resolved]
  deriving stock (Eq, Generic, Show)
  deriving anyclass NFData

-- | When rewriting the 'Anno' of a syntax node, we encountered an internal error.
-- We expected a certain number of 'AnnoHole's, e.g., in a binary operation we expect 2 'AnnoHole's,
-- but we got a different number.
--
-- This is an internal error that shouldn't occur.
data HoleInfo = HoleInfo
  { expected :: !Int
  , got :: !Int
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass NFData

data InternalCheckError =
    MissingSrcRangeForDeclaration Anno
  | MissingDeclForSrcRange Anno
  deriving stock Show
  deriving anyclass Exception

data ExpectationContext =
    ExpectAssumeSignatureContext (Maybe SrcRange) -- actual result range from the signature, if it exists
  -- | ExpectProjectionSelectorContext
  | ExpectIfConditionContext -- condition of if-then-else
  | ExpectPatternScrutineeContext (Expr Resolved) -- pattern type must match type of scrutinee
  | ExpectNotArgumentContext -- arg of NOT
  | ExpectPercentArgumentContext -- arg of '%'
  | ExpectConsArgument2Context -- second arg of cons
  | ExpectIfBranchesContext -- all branches of an if-then-else must have the same type
  | ExpectConsiderBranchesContext -- all branches of a consider must have the same type
  | ExpectHomogeneousListContext -- all elements of a list literal must have the same type
  | ExpectNamedArgContext Resolved Resolved -- function, name of arg
  | ExpectAppArgContext Bool Resolved Int -- projection?, function, number of arg
  | ExpectBinOpArgContext Text Int -- opname, number of arg (TODO: it would be better to have the token and its range here!)
  | ExpectDecideSignatureContext (Maybe SrcRange) -- actual result type range from the signature, if it exists
  | ExpectRegulativePartyContext -- party clause of obligation
  | ExpectRegulativeActionContext -- action clause of obligation
  | ExpectRegulativeDeadlineContext -- within clause of obligation
  | ExpectRegulativeTimestampContext -- timestamp of a regulative event
  | ExpectRegulativeFollowupContext -- hence clause of regulative rule
  | ExpectRegulativeContractContext -- when invoking a contract directive
  | ExpectRegulativeEventContext -- check an event expr
  | ExpectRegulativeProvidedContext -- the provided clauses are predicates on the variables bound by the pattern
  | ExpectAssertContext -- assertions should be Boolean
  | ExpectPostUrlContext -- URL argument of POST
  | ExpectPostHeadersContext -- headers argument of POST
  | ExpectPostBodyContext -- body argument of POST
  | ExpectConcatArgumentContext -- argument of CONCAT
  | ExpectAsStringArgumentContext -- argument of AS STRING
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

-- | A checked function signature.
data FunTypeSig = MkFunTypeSig
  { anno :: Anno
  , rtysig :: TypeSig Resolved
  -- ^ Already checked 'TypeSig'
  , rappForm :: AppForm Resolved
  -- ^ Already checked 'AppForm'
  , resultType :: Type' Resolved
  -- ^ Result type of the function
  , name :: CheckInfo
  -- ^ Name of this function
  , arguments :: [CheckInfo]
  -- ^ Arguments to the function.
  -- Includes type variables.
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (SOP.Generic, NFData)

-- | A checked declaration signature.
--
-- Contains in particular kind info for the type-level declaration.
--
data DeclTypeSig = MkDeclTypeSig
  { anno :: Anno
  , rtysig :: TypeSig Resolved
  -- ^ Already checked 'TypeSig'
  , rappForm :: AppForm Resolved
  -- ^ Already checked 'AppForm'
  , typeSynonym :: Maybe (Type' Name)
  -- ^ Whether this 'DeclaredHeadCache' is a type synonym.
  -- If yes, what is its *unchecked* type?
  , name :: CheckInfo
  -- ^ Name of this data type.
  , tyVars :: [CheckInfo]
  -- ^ Type variable arguments of this data type.
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (SOP.Generic, NFData)

data DeclChecked a = MkDeclChecked
  { payload :: a
  , publicNames :: [CheckInfo]
  }
  deriving stock (Eq, Functor, Generic, Show)
  deriving anyclass (SOP.Generic, NFData)

type DeclareOrAssume = Either (Declare Resolved) (Assume Resolved)

data CheckEnv =
  MkCheckEnv
    { moduleUri            :: !NormalizedUri
    , environment          :: !Environment
    , entityInfo           :: !EntityInfo
    , functionTypeSigs     :: !(Map SrcRange FunTypeSig)
    , declTypeSigs         :: !(Map SrcRange DeclTypeSig)
    , declareDeclarations  :: !(Map SrcRange (DeclChecked (Declare Resolved)))
    , assumeDeclarations   :: !(Map SrcRange (DeclChecked (Assume Resolved)))
    , errorContext         :: !CheckErrorContext
    , sectionStack         :: ![NonEmpty Text]
    }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

newtype SectionNames =
  MkSectionNames
    { sectionNames :: [Text]
    }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data CheckInfo = MkCheckInfo
  { names :: [Resolved]
  , checkEntity :: CheckEntity
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
    , infoMap      :: !InfoMap
    , nlgMap       :: !NlgMap
    , scopeMap     :: !ScopeMap
    }

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

addWarning :: CheckWarning -> Check ()
addWarning = addError . CheckWarning

fatalInternalError :: InternalCheckError -> Check a
fatalInternalError e = throw e

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
-- Info Map
-- ----------------------------------------------------------------------------


addInfoForSrcRange :: SrcRange -> Info -> Check ()
addInfoForSrcRange srcRange i = do
  env <- asks $ view #environment
  ei <- asks $ view #entityInfo
  modifying' #scopeMap $
    IV.insert (IV.srcRangeToInterval srcRange) (env, ei)
  modifying' #infoMap $
    IV.insert (IV.srcRangeToInterval srcRange) i

addNlgForSrcRange :: SrcRange -> Nlg -> Check ()
addNlgForSrcRange srcRange i =
  modifying' #nlgMap $
    IV.insert (IV.srcRangeToInterval srcRange) i

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
      case checkEntity of
        KnownType kind _resolved _ -> do
          setAnnResolvedKindOfResolved kind n
        KnownTerm ty term -> do
          setAnnResolvedTypeOfResolved ty (Just term) n
        KnownTypeVariable -> do
          setAnnResolvedAsTypeVar n
        KnownSection _ -> pure n -- TODO: this is probably not what we want, maybe some internal error?

lookupRawNameInEnvironment :: RawName -> Check [(Unique, Name, CheckEntity)]
lookupRawNameInEnvironment rn = do
  env <- asks (.environment)
  ei  <- asks (.entityInfo)
  let
      proc :: Unique -> Maybe (Unique, Name, CheckEntity)
      proc u = do
        (o, ce) <- Map.lookup u ei
        pure (u, o, ce)

      candidates :: [(Unique, Name, CheckEntity)]
      candidates = mapMaybe proc (Map.findWithDefault [] rn env)

  pure candidates

isTopLevelBindingInSection :: Unique -> Section Resolved -> Bool
isTopLevelBindingInSection u (MkSection _a  _mn _maka decls) = any (elem u . map getUnique . relevantResolveds) decls
  where
  relevantResolveds = \ case
    Declare _ (MkDeclare _ _ af _) -> appFormHeads af
    Decide _ (MkDecide _ _ af _) -> appFormHeads af
    Assume _ (MkAssume _ _ af _) -> appFormHeads af
    Directive _ _ -> []
    Import _ _ -> []
    -- NOTE: Sections are a toplevel binding in the current section but can also contain further
    -- toplevel bindings
    Section _ (MkSection _ mr maka decls') -> toResolved mr <> toResolved maka <> foldMap relevantResolveds decls'

resolveTerm' :: (TermKind -> Bool) -> Name -> Check (Resolved, Type' Resolved)
resolveTerm' p n = do
  options <- lookupRawNameInEnvironment (rawName n)
  case mapMaybe proc options of
    [] -> do
      v <- fresh (rawName n)
      n' <- setAnnResolvedType v Nothing n
      rn <- outOfScope n' v
      pure (rn, v)
    [x] -> x
    xs -> choose xs <|> do
      v <- fresh (rawName n)
      n' <- setAnnResolvedType v Nothing n
      xs' <- sequenceA xs
      rn <- ambiguousTerm n' xs'
      pure (rn, v)
  where
    proc :: (Unique, Name, CheckEntity) -> Maybe (Check (Resolved, Type' Resolved))
    proc (u, o, KnownTerm t tk) | p tk = Just $ do
      n' <-  setAnnResolvedType t (Just tk) n
      pure (Ref n' u o, t)
    proc _                             = Nothing

resolveTerm :: Name -> Check (Resolved, Type' Resolved)
resolveTerm = resolveTerm' (const True)

_resolveSelector :: Name -> Check (Resolved, Type' Resolved)
_resolveSelector = resolveTerm' (== Selector)

resolveConstructor :: Name -> Check (Resolved, Type' Resolved)
resolveConstructor = resolveTerm' (== Constructor)

setAnnNlg ::
     (HasAnno a, AnnoToken a ~ PosToken, AnnoExtra a ~ Extension)
  => Nlg -> a -> Check a
setAnnNlg nlg a = withRange a $ \r -> do
  addNlgForSrcRange r nlg
  pure $ a & annoOf % annNlg ?~ nlg

setAnnResolvedType ::
     (HasAnno a, AnnoToken a ~ PosToken, AnnoExtra a ~ Extension)
  => Type' Resolved -> Maybe TermKind -> a -> Check a
setAnnResolvedType t k a = withRange a $ \r -> do
  let info = TypeInfo t k
  addInfoForSrcRange r info
  pure $ a & annoOf % annInfo ?~ info

setAnnResolvedKind ::
     (HasAnno a, AnnoToken a ~ PosToken, AnnoExtra a ~ Extension)
  => Kind -> a -> Check a
setAnnResolvedKind k a = withRange a $ \r -> do
  let info = KindInfo k
  addInfoForSrcRange r info
  pure $ a & annoOf % annInfo ?~ info

setAnnTypeVar ::
     (HasAnno a, AnnoToken a ~ PosToken, AnnoExtra a ~ Extension)
  => a -> Check a
setAnnTypeVar a = withRange a $ \r -> do
  let info = TypeVariable
  addInfoForSrcRange r info
  pure $ a & annoOf % annInfo ?~ info

withRange :: (HasAnno a, Applicative f) => a -> (SrcRange -> f a) -> f a
withRange a f = case rangeOf (getAnno a) of
  Nothing -> pure a
  Just r -> f r

setAnnNlgOfResolved :: Nlg -> Resolved -> Check Resolved
setAnnNlgOfResolved nlg = traverseResolved (setAnnNlg nlg)

setAnnResolvedTypeOfResolved :: Type' Resolved -> Maybe TermKind -> Resolved -> Check Resolved
setAnnResolvedTypeOfResolved t k = traverseResolved (setAnnResolvedType t k)

setAnnResolvedKindOfResolved :: Kind -> Resolved -> Check Resolved
setAnnResolvedKindOfResolved k = traverseResolved (setAnnResolvedKind k)

setAnnResolvedAsTypeVar :: Resolved -> Check Resolved
setAnnResolvedAsTypeVar = traverseResolved setAnnTypeVar

type ToResolved = Optics.GPlate Resolved

toResolved :: ToResolved a => a -> [Resolved]
toResolved = Optics.toListOf Optics.gplate

-- | Should never return 'Nothing' if our system is OK.
getEntityInfo :: Resolved -> Check (Maybe CheckEntity)
getEntityInfo r = do
  ei <- asks (.entityInfo)
  case Map.lookup (getUnique r) ei of
    Nothing       -> do
      addError (MissingEntityInfo r)
      pure Nothing
    Just (_n, ce) -> pure (Just ce)

-- | Given a substitution from uniques to types, substitute
-- all occurrences of the given uniques. There's no scoping
-- going on in this, because uniques are unique!
--
substituteType :: Map Unique (Type' Resolved) -> Type' Resolved -> Type' Resolved
substituteType _ (Type ann)               = Type ann
substituteType s t@(TyApp _ r []) =
  case Map.lookup (getUnique r) s of
    Nothing -> t
    Just t' -> t'
substituteType s (TyApp ann n ts)         =
  TyApp ann n (substituteType s <$> ts)
substituteType s (Fun ann onts t)         =
  Fun ann (substituteOptionallyNamedType s <$> onts) (substituteType s t)
substituteType _ (Forall ann ns t)        =
  Forall ann ns t -- TODO!! Inner Forall needs some form of alpha renaming.
substituteType _ (InfVar ann prefix i)    = InfVar ann prefix i
-- substituteType s (ParenType ann t)        = ParenType ann (substituteType s t)

substituteOptionallyNamedType :: Map Unique (Type' Resolved) -> OptionallyNamedType Resolved -> OptionallyNamedType Resolved
substituteOptionallyNamedType s (MkOptionallyNamedType ann mn t) =
  MkOptionallyNamedType ann mn (substituteType s t)

-- | A class for applying the subsitution on inference variables exhaustively.
--
-- Note that we currently are applying the substitution late, which means we have
-- to recursively apply it.
--
class ApplySubst a where
  applySubst :: a -> Check a

instance ApplySubst (Type' Resolved) where
  applySubst (Type  ann)       = pure (Type ann)
  applySubst (TyApp ann n ts)  = TyApp ann n <$> traverse applySubst ts
  applySubst (Fun ann onts t)  = Fun ann <$> traverse applySubst onts <*> applySubst t
  applySubst (Forall ann ns t) = Forall ann ns <$> applySubst t
  applySubst (InfVar ann rn i) = do
    s <- use #substitution
    case Map.lookup i s of
      Nothing -> pure (InfVar ann rn i)
      Just t  -> do
        -- we actually modify the substitution so that we don't do the same work many times if the same variable occurs often
        -- we are still traversing every time though; we could do better
        t' <- applySubst t
        modifying #substitution (Map.insert i t')
        pure t'

instance ApplySubst (OptionallyNamedType Resolved) where
  applySubst (MkOptionallyNamedType ann mn t) = MkOptionallyNamedType ann mn <$> applySubst t

instance ApplySubst CheckError where
  applySubst = traverseOf (gplate @(Type' Resolved) @CheckError) applySubst

instance ApplySubst CheckErrorContext where
  applySubst = traverseOf (gplate @(Type' Resolved) @CheckErrorContext) applySubst

instance ApplySubst CheckErrorWithContext where
  applySubst = traverseOf (gplate @(Type' Resolved) @CheckErrorWithContext) applySubst

instance ApplySubst EntityInfo where
  applySubst = traverse (\(n, entity) -> (n, ) <$> applySubst entity)

instance ApplySubst CheckEntity where
  applySubst = traverseOf (gplate @(Type' Resolved)) applySubst

-- | Checks that two resolved names refer to the same unique.
ensureSameRef :: Resolved -> Resolved -> Check Bool
ensureSameRef r1 r2 =
  pure (getUnique r1 == getUnique r2)

optionallyNamedTypeType :: OptionallyNamedType n -> Type' n
optionallyNamedTypeType (MkOptionallyNamedType _ _ t) = t

-- | Biased choice. Only takes the second option if the first fails.
--
orElse :: Check a -> Check a -> Check a
orElse m1 m2 = do
  MkCheck $ \ e s ->
    let
      candidates = runCheck m1 e s

      isSuccess (Plain _, _)  = True
      isSuccess (With _ _, _) = False
    in
      case filter isSuccess candidates of
        [] -> runCheck m2 e s
        xs -> xs

-- | Allow the subcomputation to have at most one result.
--
prune :: forall a. Check a -> Check a
prune m = do
  ctx <- asks (.errorContext)
  MkCheck $ \ s env ->
    let
      candidates :: [(With CheckErrorWithContext a, CheckState)]
      candidates = runCheck m s env

      proc []                    = [] -- should never occur
      proc [a]                   = [a]
      proc ((Plain a, s')  : cs) = procPlain (Plain a, s') cs
      proc ((With e x, s') : cs) = procWith (With e x, s') cs

      -- We have a success, we don't want a second one
      procPlain a []                   = [a]
      procPlain a ((With _ _, _) : cs) = procPlain a cs
      procPlain a cs
        | plain = [first (With (MkCheckErrorWithContext InternalAmbiguityError ctx)) a]
        -- NOTE: in the case of ambiguity errors, the ambiguity error will always occurs as
        -- the last element in the list
        | otherwise = [last cs]
        where
          plain = allPlain $ map fst cs

      -- We have a failure, we're still looking for a success, and prefer the last failure
      procWith a []                    = [a]
      procWith _ ((Plain a, s')  : cs) = procPlain (Plain a, s') cs
      procWith _ ((With e x, s') : cs) = procWith (With e x, s') cs

    in
      proc candidates

allPlain :: [With e a] -> Bool
allPlain = all \case
  Plain{} -> True
  With {} -> False

-- | Prune to one result if there's a clearly best one at this point,
-- but don't force it.
--
softprune :: forall a. Check a -> Check a
softprune m = do
  MkCheck $ \ s env ->
    let
      candidates :: [(With CheckErrorWithContext a, CheckState)]
      candidates = runCheck m s env

      proc []                    = [] -- should never occur
      proc [a]                   = [a]
      proc ((Plain a, s')  : cs) = procPlain (Plain a, s') cs
      proc ((With e x, s') : cs) = procWith (With e x, s') cs

      -- We have a success, we don't want a second one
      procPlain a []                    = [a]
      procPlain _ ((Plain _, _)  : _cs) = candidates
      procPlain a ((With _ _, _) :  cs) = procPlain a cs

      -- We have a failure, we're still looking for a success, and prefer the last failure
      procWith a []                     = [a]
      procWith _ ((Plain a, s')  : cs)  = procPlain (Plain a, s') cs
      procWith _ ((With e x, s') : cs)  = procWith (With e x, s') cs

    in
      proc candidates

-- | Set the 'CheckErrorContext' for the given scope.
errorContext :: (CheckErrorContext -> CheckErrorContext) -> Check a -> Check a
errorContext errorCtx =
  local (\env -> env { errorContext = errorCtx env.errorContext })

-- | Push a new 'Section' Name onto the stack.
-- This is used for exposing qualified names during type checking.
pushSection :: NonEmpty Text -> Check a -> Check a
pushSection rawSectionName =
  local (\env -> env { sectionStack = env.sectionStack <> [rawSectionName] })

-- ------------------------------------
-- Scope Manipulation
-- ------------------------------------

-- | Make the given 'CheckInfo' known in the given scope.
extendKnown :: CheckInfo -> Check a -> Check a
extendKnown ci =
  extendKnownMany [ci]

-- | Make many 'CheckInfo's known for the given scope.
extendKnownMany :: [CheckInfo] -> Check a -> Check a
extendKnownMany cis = do
  local (extendEnv cis)

-- | Extend the scope of the 'CheckEnv' with all '[CheckInfo]'.
extendEnv :: [CheckInfo] -> CheckEnv -> CheckEnv
extendEnv cis env =
  foldl' goCheckInfo env cis
 where
  goCheckInfo :: CheckEnv -> CheckInfo -> CheckEnv
  goCheckInfo e ci =
    foldl' (goResolved ci.checkEntity) e ci.names

  goResolved :: CheckEntity -> CheckEnv -> Resolved -> CheckEnv
  goResolved ce e a = MkCheckEnv
    { environment = Map.alter proc (rawName n) e.environment
    , entityInfo = Map.insert u (n, ce) e.entityInfo
    , moduleUri = e.moduleUri
    , errorContext = e.errorContext
    , functionTypeSigs = e.functionTypeSigs
    , declTypeSigs = e.declTypeSigs
    , declareDeclarations = e.declareDeclarations
    , assumeDeclarations = e.assumeDeclarations
    , sectionStack = e.sectionStack
    }
    where
      u :: Unique
      n :: Name
      (u, n) = getUniqueName a

      proc :: Maybe [Unique] -> Maybe [Unique]
      proc Nothing   = Just [u]
      proc (Just xs) = Just (u : xs)

makeKnown :: Resolved -> CheckEntity -> CheckInfo
makeKnown a =
  makeKnownMany [a]

makeKnownMany :: [Resolved] -> CheckEntity -> CheckInfo
makeKnownMany rs ce =
  MkCheckInfo
    { names = rs
    , checkEntity = ce
    }

-- -------------------------------------
-- Smart constructors for Resolved names
-- -------------------------------------

def :: Name -> Check Resolved
def n = do
  u <- newUnique
  pure (Def u n)

-- | Introduce the new name as a defining occurrence of an alias of an already existing name.
defAka :: Resolved -> Name -> Check Resolved
defAka r n = do
  let u = getUnique r
  pure (Def u n)

ref :: Name -> Resolved -> Check Resolved
ref n a =
  let
    (u, o) = getUniqueName a
  in
    pure (Ref n u o)

