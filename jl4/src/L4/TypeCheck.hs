{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances, DuplicateRecordFields #-}
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
-- As of now, scope and type-checking is essentially still a
-- one-pass process. We traverse the AST and resolve all information
-- as we go. This means that currently, forward references are not
-- possible. We are planning to change this by making two passes
-- and some dependency analysis. (TODO)
--
-- The result of scope- and type checking is a new program with
-- resolved names everywhere. So after this phase, the unique
-- numbers used in resolved names should be used for determining
-- what points where.
--
-- The resulting program also contains type annotations for
-- the inferred types of expressions (to be e.g. used in LSP hovering),
-- but note that these types can still contain inference variables.
--
-- Currently, one therefore also has to extract the final substitution
-- after type checking and apply this in order to get a properly
-- expanded type.
--
-- Notes on Type-Directed Name Resolution (TDNR):
--
-- The basic idea is that we perform name resolution based on types.
-- In principle, multiple entities of the same name can coexist in scope.
-- In a case where such a multi-entity is being referred to, we do not
-- immediately complain. Instead, we try all options, and decide later
-- which one to select.
--
-- There are a number of problems that arise due to this:
--
-- 1. There is a tension between the desire to continue after an error,
--    and the notion of multiple results. If we pursue an "abort on error"
--    strategy, we have an erroneous computation abort and automatically
--    try the next branch. If there's no successful result, we might still
--    have to decide how to combine / prefer between the several possible
--    errors, but it's otherwise fine.
--
--    However, if in principle we can (and want to be able to) continue
--    on every error, then how do we ever figure out which branch is best,
--    and also how do we make sure we do not get errors from branches not
--    selected?
--
--    The correct solution seems to be here to use some form of partiality
--    monad to tag results with errors, and then to prune based on errors.
--
-- 2. Binding groups and mutually recursive definitions. It is preferable
--    to check groups of mutually recursive bindings together, because it
--    leads to more possibilities with performing correct type inference.
--    However, how do we determine whether a group is mutually recursive
--    if name resolution depends on type checking? I still have to think
--    about this more, but it seems clear to me that we will have to
--    compromise somewhere.


import Base
import qualified Base.Map as Map
import qualified Base.Text as Text
import Control.Applicative
import Control.DeepSeq (NFData)
import Control.Monad.Logic
import Data.Bifunctor
import Data.Containers.ListUtils (nubOrd)
import Data.Either (partitionEithers)
import Data.Proxy
import qualified Generics.SOP as SOP
import Optics.Core hiding (anyOf, re)

import L4.ExactPrint
import L4.Lexer (PosToken (..), TokenType (..), SrcRange (..), SrcPos (..))
import L4.Syntax
import L4.Annotation

newtype Check a =
  MkCheck (CheckState -> [(With CheckErrorWithContext a, CheckState)])

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

data CheckState =
  MkCheckState
    { environment  :: !Environment
    , errorContext :: !CheckErrorContext
    , substitution :: !Substitution
    , supply       :: !Int
    }
  deriving stock (Eq, Generic, Show)

type Environment = Map RawName [(Unique, Name, CheckEntity)]
type Substitution = Map Int (Type' Resolved)

type Kind = Int -- arity of the type

runCheck :: Check a -> CheckState -> [(With CheckErrorWithContext a, CheckState)]
runCheck (MkCheck f) = f

initialCheckState :: Environment -> Substitution -> CheckState
initialCheckState environment substitution =
  MkCheckState
    { environment
    , substitution
    , errorContext = None
    , supply       = 0
    }

-- | Main entry point for scope- and type-checking.
--
-- Takes the program as input.
--
-- Produces:
--
-- - a list of errors or informational messages (for @#CHECK@ directives)
-- - a resolved and type-annotated version of the program
-- - the final substitution (for resolving type annotations in the program)
--
doCheckProgram :: Program Name -> CheckResult
doCheckProgram program =
  case runCheckUnique (inferProgram program) (initialCheckState initialEnvironment Map.empty) of
    (w, s) ->
      let
        (errs, rprog) = runWith w
      in
        -- might be nicer to be able to do this within the inferProgram call / at the end of it
        case runCheckUnique (traverse applySubst errs) s of
          (w', s') ->
            let (moreErrs, substErrs) = runWith w'
            in CheckResult
              { program = rprog
              , errors = substErrs ++ moreErrs
              , substitution = s'.substitution
              , environment = s'.environment
              }

data CheckResult
  = CheckResult
  { program :: Program Resolved
  , errors :: [CheckErrorWithContext]
  , substitution :: Substitution
  , environment :: Environment
  }
  deriving stock (Eq, Show)

-- | Can be used to apply the final substitution after type-checking, expanding
-- inference variables whenever possible.
--
applyFinalSubstitution :: ApplySubst a => Substitution -> a -> a
applyFinalSubstitution subst t =
  let
    cs = initialCheckState Map.empty subst
  in
    case runCheckUnique (applySubst t) cs of
      (w, _cs') ->
        let
          (_errs, r) = runWith w
        in
          r

-- | Helper function to run the check monad an expect a unique result.
runCheckUnique :: Check a -> CheckState -> (With CheckErrorWithContext a, CheckState)
runCheckUnique c s =
  case runCheck c s of
    [] -> error "internal error: expected unique result, got none"
    [(w, s')] -> (w, s')
    _ -> error "internal error: expected unique result, got several"

-- --------------------------------
-- Building the initial environment
-- --------------------------------

-- | Helper function to create a predefined name.
preDef :: Text -> Name
preDef t =
  MkName
    (mkAnno
      [mkCluster
        (CsnCluster
          (ConcreteSyntaxNode [MkPosToken (MkSrcRange (MkSrcPos 0 0) (MkSrcPos 0 0) 0) (TIdentifier t)] Nothing Visible)
          (ConcreteSyntaxNode [] Nothing Hidden)
        )
      ]
    )
    (PreDef t)

-- uniques of built-in / predefs are having the 'b' marker:
--
-- 10  BOOLEAN
-- 11  NUMBER
-- 12  STRING
-- 13  LIST
-- 30  FALSE
-- 31  TRUE
-- 32  EMPTY
-- 40  A (type variable in EMPTY)

-- BOOLEAN

booleanUnique :: Unique
booleanUnique = MkUnique 'b' 10

booleanName :: Name
booleanName = preDef "BOOLEAN"

booleanRef :: Resolved
booleanRef = Ref booleanName booleanUnique booleanName

boolean :: Type' Resolved
boolean = TyApp emptyAnno booleanRef []

falseUnique :: Unique
falseUnique = MkUnique 'b' 30

falseName :: Name
falseName = preDef "FALSE"

falseDef :: Resolved
falseDef = Def falseUnique falseName

falseRef :: Resolved
falseRef = Ref falseName falseUnique falseName

trueUnique :: Unique
trueUnique = MkUnique 'b' 31

trueName :: Name
trueName = preDef "TRUE"

trueDef :: Resolved
trueDef = Def trueUnique trueName

trueRef :: Resolved
trueRef = Ref trueName trueUnique trueName

-- NUMBER

numberUnique :: Unique
numberUnique = MkUnique 'b' 11

numberName :: Name
numberName = preDef "NUMBER"

numberRef :: Resolved
numberRef = Ref numberName numberUnique numberName

number :: Type' Resolved
number = TyApp emptyAnno numberRef []

-- STRING

stringUnique :: Unique
stringUnique = MkUnique 'b' 12

stringName :: Name
stringName = preDef "STRING"

stringRef :: Resolved
stringRef = Ref stringName stringUnique stringName

string :: Type' Resolved
string = TyApp emptyAnno stringRef []

-- LIST

listUnique :: Unique
listUnique = MkUnique 'b' 13

listName :: Name
listName = preDef "LIST"

listRef :: Resolved
listRef = Ref listName listUnique listName

list :: Type' Resolved -> Type' Resolved
list a = TyApp emptyAnno listRef [a]

emptyUnique :: Unique
emptyUnique = MkUnique 'b' 32

emptyName :: Name
emptyName = preDef "EMPTY"

emptyDef :: Resolved
emptyDef = Def emptyUnique emptyName

emptyRef :: Resolved
emptyRef = Ref emptyName emptyUnique emptyName

aUnique :: Unique
aUnique = MkUnique 'b' 40

aName :: Name
aName = MkName emptyAnno (NormalName "A")

aDef :: Resolved
aDef = Def aUnique aName

aRef :: Resolved
aRef = Ref aName aUnique aName

initialEnvironment :: Environment
initialEnvironment =
  Map.fromList
    [ (NormalName "BOOLEAN", [ (booleanUnique, preDef "BOOLEAN", KnownType 0 (EnumDecl emptyAnno [MkConDecl emptyAnno falseDef [], MkConDecl emptyAnno trueDef []])) ])
    , (NormalName "FALSE",   [ (falseUnique,   preDef "FALSE",   KnownTerm boolean Constructor) ])
    , (NormalName "TRUE",    [ (trueUnique,    preDef "TRUE",    KnownTerm boolean Constructor) ])
    , (NormalName "NUMBER",  [ (numberUnique,  preDef "NUMBER",  KnownType 0 (EnumDecl emptyAnno [])) ])
    , (NormalName "STRING",  [ (stringUnique,  preDef "STRING",  KnownType 0 (EnumDecl emptyAnno [])) ])
    , (NormalName "LIST",    [ (listUnique,    preDef "LIST",    KnownType 1 (EnumDecl emptyAnno [MkConDecl emptyAnno emptyDef []])) ])
    , (NormalName "EMPTY",   [ (emptyUnique,   preDef "EMPTY",   KnownTerm (Forall emptyAnno [aDef] (list (TyApp emptyAnno aRef []))) Constructor) ])
      -- NOTE: we currently do not include the Cons constructor because it has special syntax
    ]

instance Functor Check where
  fmap = liftM

instance Applicative Check where
  pure x =
    MkCheck $ \ s -> [(Plain x, s)]
  (<*>) = ap

instance Monad Check where
  (>>=) :: forall a b. Check a -> (a -> Check b) -> Check b
  m >>= f =
    MkCheck $ \ s ->
      let
        results = runCheck m s
      in
        concatMap (\ (wea, s') -> distributeWith (fmap (flip runCheck s' . f) wea)) results

distributeWith :: With e [(With e a, b)] -> [(With e a, b)]
distributeWith (Plain xs) = xs
distributeWith (With e w) = fmap (first (With e)) (distributeWith w)

runWith :: With e a -> ([e], a)
runWith (Plain a)  = ([], a)
runWith (With e w) = first (e :) (runWith w)

instance MonadState CheckState Check where
  get :: Check CheckState
  get = MkCheck $ \ s -> [(Plain s, s)]

  put :: CheckState -> Check ()
  put s = MkCheck $ \ _ -> [(Plain (), s)]

instance MonadWith CheckErrorWithContext Check where
  with :: CheckErrorWithContext -> Check ()
  with e = MkCheck $ \ s -> [(With e (Plain ()), s)]

instance Alternative Check where
  empty :: Check a
  empty = MkCheck $ \ _ -> []

  (<|>) :: Check a -> Check a -> Check a
  (<|>) m1 m2 = MkCheck $ \ s -> runCheck m1 s ++ runCheck m2 s

data With e a =
    With e (With e a)
  | Plain a

instance Functor (With e) where
  fmap = liftM

instance Applicative (With e) where
  pure = Plain
  (<*>) = ap

instance Monad (With e) where
  Plain a  >>= f = f a
  With e m >>= f = With e (m >>= f)

class MonadWith e m | m -> e where
  with :: e -> m ()

instance MonadWith e (With e) where
  with e = With e (Plain ())

instance (Monad m, MonadWith e m) => MonadWith e (LogicT m) where
  with e = lift (with e)

instance (Monad m, MonadWith e m) => MonadWith e (StateT s m) where
  with e = lift (with e)

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
  | UnificationErrorRef (Unique, Name) (Unique, Name)
  -- | InternalUnificationErrorDef Resolved Resolved -- ^ if called on defining occurrences of names
  | UnificationError (Type' Resolved) (Type' Resolved)
  | OccursCheck (Type' Resolved) (Type' Resolved)
  | InconsistentNameInSignature Name (Maybe Name)
  | InconsistentNameInAppForm Name (Maybe Name)
  -- | InternalUnificationErrorBind Int (Type' Resolved) (Type' Resolved) -- ^ if trying to substitute inconsistently
  | NonDistinctError [Name]
  | AmbiguousTermError Name [(Resolved, Type' Resolved)]
  | AmbiguousTypeError Name [(Resolved, Kind)]
  | InternalAmbiguityError
  | IllegalAppNamed (Type' Resolved)
  | IncompleteAppNamed [OptionallyNamedType Resolved]
  | CheckInfo (Type' Resolved)
  | IllegalTypeInKindSignature (Type' Resolved)
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

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

data CheckEntity =
    KnownType Kind (TypeDecl Resolved)
  | KnownTerm (Type' Resolved) TermKind
  | KnownTypeVariable
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data TermKind =
    Computable -- ^ a variable with known definition (let or global)
  | Assumed
  | Local -- ^ a local variable (introduced by a lambda or pattern)
  | Constructor
  | Selector
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

choose :: [Check a] -> Check a
choose = asum

anyOf :: [a] -> Check a
anyOf = asum . fmap pure

-- | Biased choice. Only takes the second option if the first fails.
--
orElse :: Check a -> Check a -> Check a
orElse m1 m2 = do
  MkCheck $ \ s ->
    let
      candidates = runCheck m1 s

      isSuccess (Plain _, _)  = True
      isSuccess (With _ _, _) = False
    in
      case filter isSuccess candidates of
        [] -> runCheck m2 s
        xs -> xs

-- | Allow the subcomputation to have at most one result.
--
prune :: forall a. Check a -> Check a
prune m = do
  ctx <- use #errorContext
  MkCheck $ \ s ->
    let
      candidates :: [(With CheckErrorWithContext a, CheckState)]
      candidates = runCheck m s

      proc []                    = [] -- should never occur
      proc [a]                   = [a]
      proc ((Plain a, s')  : cs) = procPlain (Plain a, s') cs
      proc ((With e x, s') : cs) = procWith (With e x, s') cs

      -- We have a success, we don't want a second one
      procPlain a []                   = [a]
      procPlain a ((Plain _, _)  : []) = [first (With (MkCheckErrorWithContext InternalAmbiguityError ctx)) a]
      procPlain _ ((Plain _, _)  : cs) = [last cs]
      procPlain a ((With _ _, _) : cs) = procPlain a cs

      -- We have a failure, we're still looking for a success, and prefer the last failure
      procWith a []                     = [a]
      procWith _ ((Plain a, s')  : cs)  = procPlain (Plain a, s') cs
      procWith _ ((With e x, s') : cs)  = procWith (With e x, s') cs

    in
      proc candidates

-- | Prune to one result if there's a clearly best one at this point,
-- but don't force it.
--
softprune :: forall a. Check a -> Check a
softprune m = do
  MkCheck $ \ s ->
    let
      candidates :: [(With CheckErrorWithContext a, CheckState)]
      candidates = runCheck m s

      proc []                    = [] -- should never occur
      proc [a]                   = [a]
      proc ((Plain a, s')  : cs) = procPlain (Plain a, s') cs
      proc ((With e x, s') : cs) = procWith (With e x, s') cs

      -- We have a success, we don't want a second one
      procPlain a []                    = [a]
      procPlain _ ((Plain _, _)  : [])  = candidates
      procPlain _ ((Plain _, _)  : _cs) = candidates
      procPlain a ((With _ _, _) :  cs) = procPlain a cs

      -- We have a failure, we're still looking for a success, and prefer the last failure
      procWith a []                     = [a]
      procWith _ ((Plain a, s')  : cs)  = procPlain (Plain a, s') cs
      procWith _ ((With e x, s') : cs)  = procWith (With e x, s') cs

    in
      proc candidates

step :: Check Int
step = do
  current <- use #supply
  let next = current + 1
  assign #supply next
  pure current

newUnique :: Check Unique
newUnique = do
  i <- step
  pure (MkUnique 'c' i)

fresh :: RawName -> Check (Type' Resolved)
fresh prefix = do
  i <- step
  pure (InfVar emptyAnno prefix i)

addError :: CheckError -> Check ()
addError e = do
  ctx <- use #errorContext
  with (MkCheckErrorWithContext e ctx)

resolveTerm' :: (TermKind -> Bool) -> Name -> Check (Resolved, Type' Resolved)
resolveTerm' p n = do
  env <- use #environment
  case mapMaybe proc (Map.findWithDefault [] (rawName n) env) of
    [] -> do
      v <- fresh (rawName n)
      rn <- outOfScope (setAnno (set annResolveType (Just v) (getAnno n)) n) v
      pure (rn, v)
    [x] -> pure x
    xs -> anyOf xs <|> do
      v <- fresh (rawName n)
      rn <- ambiguousTerm (setAnno (set annResolveType (Just v) (getAnno n)) n) xs
      pure (rn, v)
  where
    proc :: (Unique, Name, CheckEntity) -> Maybe (Resolved, Type' Resolved)
    proc (u, o, KnownTerm t tk) | p tk = Just (Ref (setAnno (set annResolveType (Just t) (getAnno n)) n) u o, t)
    proc _                             = Nothing

resolveTerm :: Name -> Check (Resolved, Type' Resolved)
resolveTerm = resolveTerm' (const True)

resolveSelector :: Name -> Check (Resolved, Type' Resolved)
resolveSelector = resolveTerm' (== Selector)

resolveConstructor :: Name -> Check (Resolved, Type' Resolved)
resolveConstructor = resolveTerm' (== Constructor)

checkSelector :: Type' Resolved -> Name -> Check (Resolved, Type' Resolved)
checkSelector recordt n = do
  v <- fresh (NormalName "sel") -- introduce a variable for the result / field type
  let selType = fun_ [recordt] v
  (rn, prt) <- resolveSelector n
  rt <- instantiate prt
  unify selType rt
  pure (rn, v) -- TODO: apply the substitution

instantiate :: Type' Resolved -> Check (Type' Resolved)
instantiate (Forall _ann ns t) = do
  substitution <- Map.fromList <$> traverse (\ n -> let (u, o) = getUniqueName n; r = rawName o in fresh r >>= \ v -> pure (u, v)) ns
  pure (substituteType substitution t)
instantiate t             = pure t

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

forall' :: [Resolved] -> Type' Resolved -> Type' Resolved
forall' [] t = t
forall' ns t = Forall emptyAnno ns t

fun_ :: [Type' Resolved] -> Type' Resolved -> Type' Resolved
fun_ [] t = t
fun_ ts t = fun (MkOptionallyNamedType emptyAnno Nothing <$> ts) t

fun :: [OptionallyNamedType Resolved] -> Type' Resolved -> Type' Resolved
fun [] t = t
fun ts t = Fun emptyAnno ts t

app :: n -> [Type' n] -> Type' n
app = TyApp emptyAnno

-- | Make a type application from a type variable. Also change defining
-- occurrences into references.
--
-- This is because this function is used to build the types of constructors
-- and selectors, where we build a type from the original type declaration
-- which has defining occurrences of the type variables.
--
tyvar :: Resolved -> Type' Resolved
tyvar (Def u n) = TyApp emptyAnno (Ref n u n) []
tyvar r         = TyApp emptyAnno r           []

checkBinOp ::
     Type' Resolved
  -> Type' Resolved
  -> Type' Resolved
  -> (Anno -> Expr Resolved -> Expr Resolved -> Expr Resolved)
  -> Anno
  -> Expr Name
  -> Expr Name
  -> Check (Expr Resolved, Type' Resolved)
checkBinOp t1 t2 tr op ann e1 e2 = do
  e1' <- checkExpr e1 t1
  e2' <- checkExpr e2 t2
  pure (op ann e1' e2', tr)

def :: Name -> Check Resolved
def n = do
  u <- newUnique
  pure (Def u n)

ref :: Name -> Resolved -> Check Resolved
ref n a =
  let
    (u, o) = getUniqueName a
  in
    pure (Ref n u o)

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

inferDeclare :: Declare Name -> Check (Declare Resolved)
inferDeclare (MkDeclare ann tysig appForm t) = do
  (rd, extend) <- scope $ do
    setErrorContext (WhileCheckingDeclare (getName appForm))
    (rappForm, rtysig) <- checkTypeAppFormTypeSigConsistency appForm tysig
    ce <- inferTypeAppForm' rappForm rtysig
    -- (rappForm, ce) <- inferTypeAppForm appForm
    (rt, extend) <- inferTypeDecl rappForm t
    pure (MkDeclare ann rtysig rappForm rt, makeKnown (appFormHead rappForm) (ce rt) >> extend)
  extend
  pure rd

-- | We allow assumptions for types, but we could potentially be more
-- sophisticated here.
--
-- In particular, we currently treat assumed types as enumeration types with no known
-- constructors (empty types). It would be better to have a dedicated case for these.
--
-- TODO: I think the checking whether we have a type declaration or a term
-- declaration is off, because we can have a type declaration of the form
--
-- GIVETH A TYPE
-- ASSUME T
--
-- which would currently not match the first case.
--
inferAssume :: Assume Name -> Check (Assume Resolved)
inferAssume (MkAssume ann tysig appForm (Just (Type tann))) = do
  -- declaration of a type
  (rd, extend) <- scope $ do
    setErrorContext (WhileCheckingAssume (getName appForm))
    (rappForm, rtysig) <- checkTypeAppFormTypeSigConsistency appForm tysig
    ce <- inferTypeAppForm' rappForm rtysig
    -- TODO: do we ever check the result kind?
    pure (MkAssume ann rtysig rappForm (Just (Type tann)), makeKnown (appFormHead rappForm) (ce (EnumDecl emptyAnno [])))
  extend
  pure rd
inferAssume (MkAssume ann tysig appForm mt) = do
  -- declaration of a term
  (rd, extend) <- scope $ do
    setErrorContext (WhileCheckingAssume (getName appForm))
    (rappForm, rtysig) <- checkTermAppFormTypeSigConsistency appForm tysig -- (MkTypeSig mempty (MkGivenSig mempty []) (Just (MkGivethSig mempty t)))
    (ce, _rt, result) <- inferTermAppForm rappForm rtysig
    -- check that the given result type matches the result type in the type signature
    rmt <- case mt of
      Nothing -> pure Nothing
      Just t  -> do
        rt <- inferType t
        unify result rt
        pure (Just rt)
    pure (MkAssume ann rtysig rappForm rmt, makeKnown (appFormHead rappForm) ce)
  extend
  pure rd

inferDirective :: Directive Name -> Check (Directive Resolved)
inferDirective (Eval ann e) = do
  setErrorContext (WhileCheckingExpression e)
  (re, _) <- prune $ inferExpr e
  pure (Eval ann re)
inferDirective (Check ann e) = scope $ do
  setErrorContext (WhileCheckingExpression e)
  (re, te) <- prune $ inferExpr e
  addError (CheckInfo te)
  pure (Check ann re)

inferSection :: Section Name -> Check (Section Resolved)
inferSection (MkSection ann lvl mn topdecls) = do
  rmn <- traverse def mn -- we currently treat section names as defining occurrences, but they play no further role
  rtopdecls <- traverse inferTopDecl topdecls
  pure (MkSection ann lvl rmn rtopdecls)

inferLocalDecl :: LocalDecl Name -> Check (LocalDecl Resolved)
inferLocalDecl (LocalDecide ann decide) = do
  rdecide <- softprune $ inferDecide decide
  pure (LocalDecide ann rdecide)
inferLocalDecl (LocalAssume ann assume) = do
  rassume <- softprune $ inferAssume assume
  pure (LocalAssume ann rassume)

inferTopDecl :: TopDecl Name -> Check (TopDecl Resolved)
inferTopDecl (Declare ann declare) = do
  rdeclare <- prune $ inferDeclare declare
  pure (Declare ann rdeclare)
inferTopDecl (Decide ann decide) = do
  rdecide <- prune $ inferDecide decide
  pure (Decide ann rdecide)
inferTopDecl (Assume ann assume) = do
  rassume <- prune $ inferAssume assume
  pure (Assume ann rassume)
inferTopDecl (Directive ann directive) = do
  rdirective <- inferDirective directive
  pure (Directive ann rdirective)

-- TODO: Somewhere near the top we should do dependency analysis. Note that
-- there is a potential problem. If we use type-directed name resolution but
-- also allow forward references, then how are we going to determine mutual
-- recursion? Optimistically, pessimistically, something in between?
--
inferProgram :: Program Name -> Check (Program Resolved)
inferProgram (MkProgram ann sections) = do
  rsections <- traverse inferSection sections
  pure (MkProgram ann rsections)

-- | This covers constants and functions being defined.
--
-- These have an optional type signature. We require some compatibility between
-- the formal arguments given in the definition itself, and the formal arguments
-- given in the type signature.
--
-- TODO: This is more complicated due to potential polymorphism.
--
inferDecide :: Decide Name -> Check (Decide Resolved)
inferDecide (MkDecide ann tysig appForm expr) = do
  (rd, extend) <- scope $ do
    setErrorContext (WhileCheckingDecide (getName appForm))
    (rappForm, rtysig) <- checkTermAppFormTypeSigConsistency appForm tysig
    (ce, rt, result) <- inferTermAppForm rappForm rtysig
    rexpr <- checkExpr expr result
    let ann' = set annResolveType (Just rt) ann
    pure (MkDecide ann' rtysig rappForm rexpr, makeKnown (appFormHead rappForm) ce)
  extend
  pure rd

-- | We allow the following cases:
--
-- 1. Arguments specified in the appform, but not in GIVEN. This is the same as providing
--    a compatible GIVEN without types for the arguments.
--
-- 2. Arguments specified in the GIVEN, but not in the appform. This is the same as
--    providing compatible arguments in the appform.
--
-- 3. Arguments specified both in the GIVEN and the appform. In this case, the names
--    and order of the arguments must be consistent.
--
-- We do so by reducing the first two cases to the third case and then proceeding.
--
-- TODO: There's something potentially strange here for now.
-- We have a "double binding" occurrence of the argument names. We currently treat
-- the occurrence in the appform as the true binding occurrence, and the other one
-- as a reference to that binding occurrence. But in the second case above, the
-- appform occurrences aren't truly there. This means we'll have the binding occurrence
-- not truly existing in the source program.
--
checkTermAppFormTypeSigConsistency :: AppForm Name -> TypeSig Name -> Check (AppForm Resolved, TypeSig Resolved)
checkTermAppFormTypeSigConsistency appForm@(MkAppForm _ _ ns) (MkTypeSig tann (MkGivenSig gann []) mgiveth) =
  checkTermAppFormTypeSigConsistency'
    appForm
    (MkTypeSig tann (MkGivenSig gann ((\ n -> MkOptionallyTypedName emptyAnno n Nothing) <$> ns)) mgiveth)
checkTermAppFormTypeSigConsistency (MkAppForm aann n []) tysig@(MkTypeSig _ (MkGivenSig _ otns) _) =
  checkTermAppFormTypeSigConsistency'
    (MkAppForm aann n (getName <$> filter isTerm otns))
    tysig
checkTermAppFormTypeSigConsistency appForm tysig =
  checkTermAppFormTypeSigConsistency' appForm tysig

isTerm :: OptionallyTypedName Name -> Bool
isTerm (MkOptionallyTypedName _ _ (Just (Type _))) = False
isTerm _                                           = True

-- | Handles the third case described in 'checkTermAppFormTypeSigConsistency'.
checkTermAppFormTypeSigConsistency' :: AppForm Name -> TypeSig Name -> Check (AppForm Resolved, TypeSig Resolved)
checkTermAppFormTypeSigConsistency' (MkAppForm aann n ns) (MkTypeSig tann (MkGivenSig gann otns) mgiveth) = do
  rn <- def n
  (rns, rotns) <- ensureNameConsistency ns otns
  rmgiveth <- traverse inferGiveth mgiveth
  pure (MkAppForm aann rn rns, MkTypeSig tann (MkGivenSig gann rotns) rmgiveth)

-- | This is like 'checkTermAppFormTypeSigConsistency', but for definitions that are a part of types.
--
-- For types, we generally allow only type arguments to appear in a type signature.
--
-- We still allow the following cases:
--
-- 1. Arguments specified in the appform, but not in GIVEN.
--
-- 2. Arguments specified in the GIVEN, but not in the appform.
--
-- 3. Arguments specified both in the GIVEN and the appform. In this case, the names
--    and order of the arguments must be consistent.
--
-- We do so by reducing the first two cases to the third case and then proceeding.
--
checkTypeAppFormTypeSigConsistency :: AppForm Name -> TypeSig Name -> Check (AppForm Resolved, TypeSig Resolved)
checkTypeAppFormTypeSigConsistency appForm@(MkAppForm _ _ ns) (MkTypeSig tann (MkGivenSig gann []) mgiveth) =
  checkTypeAppFormTypeSigConsistency'
    appForm
    (MkTypeSig tann (MkGivenSig gann ((\ n -> MkOptionallyTypedName emptyAnno n (Just (Type emptyAnno))) <$> ns)) mgiveth)
checkTypeAppFormTypeSigConsistency (MkAppForm aann n []) tysig@(MkTypeSig _ (MkGivenSig _ otns) _) =
  checkTypeAppFormTypeSigConsistency'
    (MkAppForm aann n (getName <$> otns))
    tysig
checkTypeAppFormTypeSigConsistency appForm tysig =
  checkTypeAppFormTypeSigConsistency' appForm tysig

-- | Handles the third case described in 'checkTypeAppFormTypeSigConsistency'.
checkTypeAppFormTypeSigConsistency' :: AppForm Name -> TypeSig Name -> Check (AppForm Resolved, TypeSig Resolved)
checkTypeAppFormTypeSigConsistency' (MkAppForm aann n ns) (MkTypeSig tann (MkGivenSig gann otns) mgiveth) = do
  rn <- def n
  (rns, rotns) <- ensureTypeNameConsistency ns otns
  rmgiveth <- traverse inferTypeGiveth mgiveth
  pure (MkAppForm aann rn rns, MkTypeSig tann (MkGivenSig gann rotns) rmgiveth)

inferGiveth :: GivethSig Name -> Check (GivethSig Resolved)
inferGiveth (MkGivethSig ann t) = do
  rt <- inferType t
  pure (MkGivethSig ann rt)

inferTypeGiveth :: GivethSig Name -> Check (GivethSig Resolved)
inferTypeGiveth (MkGivethSig ann (Type tann)) = do
  pure (MkGivethSig ann (Type tann))
inferTypeGiveth (MkGivethSig ann t) = do
  setErrorContext (WhileCheckingType t)
  rt <- inferType t
  addError (IllegalTypeInKindSignature rt)
  pure (MkGivethSig ann rt)

-- | Checks that the names are consistent, and resolve the 'OptionallyTypedName's.
--
-- Note that the list of 'OptionallyTypedName's can contain both type variables and
-- term variables. The consistency here is only with respect to term variables.
-- The type variables are considered defining occurrences that can be used in the
-- types of subsequent names.
--
ensureNameConsistency :: [Name] -> [OptionallyTypedName Name] -> Check ([Resolved], [OptionallyTypedName Resolved])
ensureNameConsistency [] [] = pure ([], [])
ensureNameConsistency ns (MkOptionallyTypedName ann n (Just (Type tann)) : otns) = do
  rn <- def n
  makeKnown rn KnownTypeVariable
  (rns, rotns) <- ensureNameConsistency ns otns
  pure (rns, MkOptionallyTypedName ann rn (Just (Type tann)) : rotns)
ensureNameConsistency (n : ns) (otn : otns)
  | rawName n == rawName (getName otn) = do
  rn <- def n
  rotn <- mkref rn otn
  (rns, rotns) <- ensureNameConsistency ns otns
  pure (rn : rns, rotn : rotns)
  | otherwise = do
  addError (InconsistentNameInAppForm n (Just (getName otn)))
  addError (InconsistentNameInSignature (getName otn) (Just n))
  rn <- def n
  rotn <- mkref rn otn -- questionable, will point to wrong name!
  (rns, rotns) <- ensureNameConsistency ns otns
  pure (rn : rns, rotn : rotns)
ensureNameConsistency (n : ns) [] = do
  addError (InconsistentNameInAppForm n Nothing)
  rn <- def n
  (rns, _) <- ensureNameConsistency ns []
  pure (rn : rns, [])
ensureNameConsistency [] (MkOptionallyTypedName ann n mt : otns) = do
  addError (InconsistentNameInSignature n Nothing)
  rn <- def n
  rmt <- traverse inferType mt
  (_, rotns) <- ensureNameConsistency [] otns
  pure ([], MkOptionallyTypedName ann rn rmt : rotns)

-- | Checks that the names are consistent, and resolve the 'OptionallyTypedName's.
--
-- Note that the list of 'OptionallyTypedName's can in principle contain both
-- type variables and term variables, but term variables are not permitted here.
--
ensureTypeNameConsistency :: [Name] -> [OptionallyTypedName Name] -> Check ([Resolved], [OptionallyTypedName Resolved])
ensureTypeNameConsistency [] [] = pure ([], [])
ensureTypeNameConsistency (n : ns) (MkOptionallyTypedName ann n' (Just (Type tann)) : otns)
  | rawName n == rawName n' = do
  rn <- def n
  rn' <- ref n' rn
  (rns, rotns) <- ensureTypeNameConsistency ns otns
  pure (rn : rns, MkOptionallyTypedName ann rn' (Just (Type tann)) : rotns)
ensureTypeNameConsistency (n : ns) (MkOptionallyTypedName ann n' Nothing : otns)
  | rawName n == rawName n' = do
  rn <- def n
  rn' <- ref n' rn
  (rns, rotns) <- ensureTypeNameConsistency ns otns
  pure (rn : rns, MkOptionallyTypedName ann rn' Nothing : rotns)
ensureTypeNameConsistency (n : ns) (otn : otns) = do
  addError (InconsistentNameInAppForm n (Just (getName otn)))
  addError (InconsistentNameInSignature (getName otn) (Just n))
  rn <- def n
  rotn <- mkref rn otn -- questionable, will point to wrong name!
  (rns, rotns) <- ensureTypeNameConsistency ns otns
  pure (rn : rns, rotn : rotns)
ensureTypeNameConsistency (n : ns) [] = do
  addError (InconsistentNameInAppForm n Nothing)
  rn <- def n
  (rns, _) <- ensureNameConsistency ns []
  pure (rn : rns, [])
ensureTypeNameConsistency [] (MkOptionallyTypedName ann n mt : otns) = do
  addError (InconsistentNameInSignature n Nothing)
  rn <- def n
  rmt <- traverse inferType mt
  (_, rotns) <- ensureNameConsistency [] otns
  pure ([], MkOptionallyTypedName ann rn rmt : rotns)

mkref :: Resolved -> OptionallyTypedName Name -> Check (OptionallyTypedName Resolved)
mkref r (MkOptionallyTypedName ann n mt) = do
  rn <- ref n r
  rmt <- traverse inferType mt
  pure (MkOptionallyTypedName ann rn rmt)

appFormHead :: AppForm n -> n
appFormHead (MkAppForm _ n _) = n

appFormArgs :: AppForm n -> [n]
appFormArgs (MkAppForm _ _ ns) = ns

appFormType :: AppForm Resolved -> Type' Resolved
appFormType (MkAppForm _ann n args) = app n (tyvar <$> args)

inferTypeDecl :: AppForm Resolved -> TypeDecl Name -> Check (TypeDecl Resolved, Check ())
inferTypeDecl rappForm (EnumDecl ann conDecls) = do
  ensureDistinct (getName <$> conDecls)
  (rconDecls, extends) <- unzip <$> traverse (inferConDecl rappForm) conDecls
  pure (EnumDecl ann rconDecls, sequence_ extends)
inferTypeDecl rappForm (RecordDecl ann tns) = do
  -- a record declaration is just a special case of an enum declaration
  (MkConDecl rann _ rtns, extend) <- inferConDeclResolved rappForm (appFormHead rappForm) (MkConDecl ann (getName rappForm) tns)
  pure (RecordDecl rann rtns, extend)

-- TODO: merge with inferConDecl
inferConDeclResolved :: AppForm Resolved -> Resolved -> ConDecl Name -> Check (ConDecl Resolved, Check ())
inferConDeclResolved rappForm n (MkConDecl ann _ tns) = do
  ensureDistinct (getName <$> tns)
  (rtns, extends) <- unzip <$> traverse (inferSelector rappForm) tns
  let
    conType = forall' (appFormArgs rappForm) (fun (typedNameOptionallyNamedType <$> rtns) (appFormType rappForm))
    conInfo = KnownTerm conType Constructor
  -- instantiated <- instantiate conType
  -- trace (Text.unpack $ simpleprint conType) (pure ())
  -- trace (Text.unpack $ simpleprint instantiated) (pure ())
  pure (MkConDecl ann n rtns, makeKnown n conInfo >> sequence_ extends)

inferConDecl :: AppForm Resolved -> ConDecl Name -> Check (ConDecl Resolved, Check ())
inferConDecl rappForm (MkConDecl ann n tns) = do
  ensureDistinct (getName <$> tns)
  dn <- def n
  (rtns, extends) <- unzip <$> traverse (inferSelector rappForm) tns
  let
    conType = forall' (appFormArgs rappForm) (fun (typedNameOptionallyNamedType <$> rtns) (appFormType rappForm))
    conInfo = KnownTerm conType Constructor
  -- instantiated <- instantiate conType
  -- trace (Text.unpack $ simpleprint conType) (pure ())
  -- trace (Text.unpack $ simpleprint instantiated) (pure ())
  pure (MkConDecl ann dn rtns, makeKnown dn conInfo >> sequence_ extends)

typedNameOptionallyNamedType :: TypedName n -> OptionallyNamedType n
typedNameOptionallyNamedType (MkTypedName _ n t) = MkOptionallyNamedType emptyAnno (Just n) t

optionallyNamedTypeType :: OptionallyNamedType n -> Type' n
optionallyNamedTypeType (MkOptionallyNamedType _ _ t) = t

inferSelector :: AppForm Resolved -> TypedName Name -> Check (TypedName Resolved, Check ())
inferSelector rappForm (MkTypedName ann n t) = do
  rt <- inferType t
  dn <- def n
  let selectorInfo = KnownTerm (forall' (appFormArgs rappForm) (fun_ [appFormType rappForm] rt)) Selector
  pure (MkTypedName ann dn rt, makeKnown dn selectorInfo)

-- | Infers / checks a type to be of kind TYPE.
inferType :: Type' Name -> Check (Type' Resolved)
inferType g = scope $ do
  setErrorContext (WhileCheckingType g)
  case g of
    Type ann -> pure (Type ann)
    TyApp ann n ts -> do
      (rn, kind) <- resolveType n
      checkKind kind ts
      rts <- traverse inferType ts
      pure (TyApp ann rn rts)
    Fun ann onts t -> do
      ronts <- traverse inferFunArg onts
      rt <- inferType t
      pure (Fun ann ronts rt)
    Forall ann ns t -> do
      ensureDistinct ns
      dns <- traverse def ns
      rt <- scope $ do
        traverse_ (flip makeKnown KnownTypeVariable) dns
        inferType t
      pure (Forall ann dns rt)
    InfVar ann prefix i -> pure (InfVar ann prefix i)
--    ParenType ann t -> do
--      rt <- inferType t
--      pure (ParenType ann rt)

inferFunArg :: OptionallyNamedType Name -> Check (OptionallyNamedType Resolved)
inferFunArg (MkOptionallyNamedType ann mn t) = do
  rmn <- traverse def mn
  rt <- inferType t
  pure (MkOptionallyNamedType ann rmn rt)

checkKind :: Kind -> [Type' Name] -> Check ()
checkKind kind xs
  | length xs == kind = pure ()
  | otherwise         = addError (KindError kind xs)

resolveType :: Name -> Check (Resolved, Kind)
resolveType n = do
  env <- use #environment
  case mapMaybe proc (Map.findWithDefault [] (rawName n) env) of
    [] -> do
      rn <- outOfScope n (Type emptyAnno)
      pure (rn, 0)
    [x] -> pure x
    xs -> anyOf xs <|> do
      rn <- ambiguousType n xs
      pure (rn, 0)
  where
    proc :: (Unique, Name, CheckEntity) -> Maybe (Resolved, Kind)
    proc (u, o, KnownTypeVariable) = Just (Ref n u o, 0)
    proc (u, o, KnownType kind _)  = Just (Ref n u o, kind)
    proc _                         = Nothing

rawName :: Name -> RawName
rawName (MkName _ raw) = raw

getUniqueName :: Resolved -> (Unique, Name)
getUniqueName r = (getUnique r, getOriginal r)

class HasName a where
  getName :: a -> Name

instance HasName Name where
  getName n = n

instance HasName Resolved where
  getName = getActual

instance HasName a => HasName (AppForm a) where
  getName (MkAppForm _ n _) = getName n

instance HasName a => HasName (ConDecl a) where
  getName (MkConDecl _ n _) = getName n

instance HasName a => HasName (TypedName a) where
  getName (MkTypedName _ann n _t) = getName n

instance HasName a => HasName (OptionallyTypedName a) where
  getName (MkOptionallyTypedName _ann n _mt) = getName n

kindOfAppForm :: AppForm n -> Kind
kindOfAppForm (MkAppForm _ann _ args) =
  length args

-- | Infer / check an "appform" as it appears for datatypes.
inferTypeAppForm :: AppForm Name -> Check (AppForm Resolved, TypeDecl Resolved -> CheckEntity)
inferTypeAppForm appForm@(MkAppForm ann n args) = do
  ensureDistinct (n : args)
  dn <- def n
  let kind = kindOfAppForm appForm
  makeKnown dn (KnownType kind (EnumDecl emptyAnno [])) -- preliminary, for recursive types
  dargs <- traverse def args
  traverse_ (flip makeKnown KnownTypeVariable) dargs
  pure (MkAppForm ann dn dargs, KnownType kind)

inferTypeAppForm' :: AppForm Resolved -> TypeSig Resolved -> Check (TypeDecl Resolved -> CheckEntity)
inferTypeAppForm' appForm@(MkAppForm _ n args) _tysig = do
  ensureDistinct (getName <$> (n : args)) -- should we do this earlier?
  let kind = kindOfAppForm appForm
  let typeInfo = KnownType kind (EnumDecl emptyAnno []) -- prelminary info about type, used for recursive types
  makeKnown n typeInfo -- this makes the name known for recursive uses
  traverse_ (flip makeKnown KnownTypeVariable) args
  pure (KnownType kind)

-- | This happens after consistency checking which is in turn already doing part of
-- name resolution, so this takes a resolved appform. We do the environment handling
-- here.
inferTermAppForm :: AppForm Resolved -> TypeSig Resolved -> Check (CheckEntity, Type' Resolved, Type' Resolved)
inferTermAppForm (MkAppForm _ n args) tysig = do
  ensureDistinct (getName <$> (n : args)) -- should we do this earlier?
  (rt, result, extend) <- typeSigType tysig
  let termInfo = KnownTerm rt Computable
  makeKnown n termInfo -- this makes the name known for recursive uses
  extend
  pure (termInfo, rt, result)

inferLamGivens :: GivenSig Name -> Check (GivenSig Resolved, [Type' Resolved])
inferLamGivens (MkGivenSig ann otns) = do
  (rotns, rargts) <- unzip <$> traverse inferOptionallyTypedName otns
  pure (MkGivenSig ann rotns, rargts)

-- TODO: there is unfortunate overlap between this and optionallyTypedNameType,
-- but perhaps it's ok ...
inferOptionallyTypedName :: OptionallyTypedName Name -> Check (OptionallyTypedName Resolved, Type' Resolved)
inferOptionallyTypedName (MkOptionallyTypedName ann n Nothing) = do
  rn <- def n
  v <- fresh (rawName n)
  makeKnown rn (KnownTerm v Local)
  pure (MkOptionallyTypedName ann rn (Just v), v)
inferOptionallyTypedName (MkOptionallyTypedName ann n (Just t)) = do
  rn <- def n
  rt <- inferType t
  makeKnown rn (KnownTerm rt Local)
  pure (MkOptionallyTypedName ann rn (Just rt), rt)

-- | Turn a type signature into a type, introducing inference variables for
-- unknown types. Also returns the result type.
-- Also returns terms to be brought into scope locally for the arguments.
--
-- Returns a quantified type if type arguments are involved.
--
-- TODO: It's possibly weird that we add the names to the scope in here.
--
typeSigType :: TypeSig Resolved -> Check (Type' Resolved, Type' Resolved, Check ())
typeSigType (MkTypeSig _ (MkGivenSig _ otns) mgiveth) = do
  let (tyvars, others) = partitionEithers (isQuantifier <$> otns)
  ronts <- traverse mkOptionallyNamedType others
  rt <- maybeGivethType mgiveth
  pure (forall' tyvars (fun ronts rt), rt, traverse_ proc ronts)
  where
    mkOptionallyNamedType :: (Resolved, Maybe (Type' Resolved)) -> Check (OptionallyNamedType Resolved)
    mkOptionallyNamedType (n, Nothing) = do
      v <- fresh (rawName (getName n))
      pure (MkOptionallyNamedType emptyAnno (Just n) v)
    mkOptionallyNamedType (n, Just t)  = do
      pure (MkOptionallyNamedType emptyAnno (Just n) t)

    proc :: OptionallyNamedType Resolved -> Check ()
    proc (MkOptionallyNamedType _ Nothing  _) = pure () -- should not happen
    proc (MkOptionallyNamedType _ (Just n) t) =
      makeKnown n (KnownTerm t Local)

isQuantifier :: OptionallyTypedName Resolved -> Either Resolved (Resolved, Maybe (Type' Resolved))
isQuantifier (MkOptionallyTypedName _ n (Just (Type _))) = Left n
isQuantifier (MkOptionallyTypedName _ n mt             ) = Right (n, mt)

maybeGivethType :: Maybe (GivethSig Resolved) -> Check (Type' Resolved)
maybeGivethType Nothing                  = fresh (NormalName "r") -- we have no obvious prefix?
maybeGivethType (Just (MkGivethSig _ t)) = pure t -- type is already resolved

-- | Ensure that changes to the environment remain local to the passed computation.
-- Also scopes the error context.
--
scope :: Check a -> Check a
scope m = do
  savedCtx <- use #errorContext
  savedEnv <- use #environment
  a <- m
  assign #errorContext savedCtx
  assign #environment savedEnv
  pure a

setErrorContext :: (CheckErrorContext -> CheckErrorContext) -> Check ()
setErrorContext f =
  modifying #errorContext f

ensureDistinct :: [Name] -> Check ()
ensureDistinct ns
  | nubOrd raws == raws = pure ()
  | otherwise           = addError (NonDistinctError ns)
  where
    raws = rawName <$> ns

-- | Makes the given named item known in the current scope,
-- with the given specification.
--
makeKnown :: Resolved -> CheckEntity -> Check ()
makeKnown a ce =
  modifying #environment
    (Map.alter proc (rawName n))
  where
    u :: Unique
    n :: Name
    (u, n) = getUniqueName a

    new :: (Unique, Name, CheckEntity)
    new = (u, n, ce)

    proc :: Maybe [(Unique, Name, CheckEntity)] -> Maybe [(Unique, Name, CheckEntity)]
    proc Nothing   = Just [new]
    proc (Just xs) = Just (new : xs)

checkExpr :: Expr Name -> Type' Resolved -> Check (Expr Resolved)
checkExpr (IfThenElse ann e1 e2 e3) t = softprune $ scope $ do
  re <- checkIfThenElse ann e1 e2 e3 t
  let re' = setAnno (set annResolveType (Just t) (getAnno re)) re
  pure re'
checkExpr (Consider ann e branches) t = softprune $ scope $ do
  re <- checkConsider ann e branches t
  let re' = setAnno (set annResolveType (Just t) (getAnno re)) re
  pure re'
-- checkExpr (ParenExpr ann e) t = do
--   re <- checkExpr e t
--   pure (ParenExpr ann re)
checkExpr (Where ann e ds) t = softprune $ scope $ do
  rds <- traverse inferLocalDecl ds
  re <- checkExpr e t
  pure (Where ann re rds)
checkExpr e t = softprune $ scope $ do
  setErrorContext (WhileCheckingExpression e)
  (re, rt) <- inferExpr e
  unify t rt
  pure re

checkIfThenElse :: Anno -> Expr Name -> Expr Name -> Expr Name -> Type' Resolved -> Check (Expr Resolved)
checkIfThenElse ann e1 e2 e3 t = do
  e1' <- checkExpr e1 boolean
  e2' <- checkExpr e2 t
  e3' <- checkExpr e3 t
  pure (IfThenElse ann e1' e2' e3')

checkConsider :: Anno -> Expr Name -> [Branch Name] -> Type' Resolved -> Check (Expr Resolved)
checkConsider ann e branches t = do
  (re, te) <- inferExpr e
  rbranches <- traverse (checkBranch te t) branches
  pure (Consider ann re rbranches)

inferExpr :: Expr Name -> Check (Expr Resolved, Type' Resolved)
inferExpr g = softprune $ scope $ do
  setErrorContext (WhileCheckingExpression g)
  (re, te) <- inferExpr' g
  let re' = setAnno (set annResolveType (Just te) (getAnno re)) re
  pure (re', te)

inferExpr' :: Expr Name -> Check (Expr Resolved, Type' Resolved)
inferExpr' g =
  case g of
    And ann e1 e2 ->
      checkBinOp boolean boolean boolean And ann e1 e2
    Or ann e1 e2 ->
      checkBinOp boolean boolean boolean Or ann e1 e2
    Implies ann e1 e2 ->
      checkBinOp boolean boolean boolean Implies ann e1 e2
    Equals ann e1 e2 -> -- TODO: probably better to actually treat it as polymorphic
      choose
        [ checkBinOp boolean boolean boolean Equals ann e1 e2
        , checkBinOp number  number  boolean Equals ann e1 e2
        , checkBinOp string  string  boolean Equals ann e1 e2
        ]
    Leq ann e1 e2 ->
      choose
        [ checkBinOp boolean boolean boolean Leq ann e1 e2
        , checkBinOp number  number  boolean Leq ann e1 e2
        , checkBinOp string  string  boolean Leq ann e1 e2
        ]
    Geq ann e1 e2 ->
      choose
        [ checkBinOp boolean boolean boolean Geq ann e1 e2
        , checkBinOp number  number  boolean Geq ann e1 e2
        , checkBinOp string  string  boolean Geq ann e1 e2
        ]
    Lt ann e1 e2 ->
      choose
        [ checkBinOp boolean boolean boolean Lt ann e1 e2
        , checkBinOp number  number  boolean Lt ann e1 e2
        , checkBinOp string  string  boolean Lt ann e1 e2
        ]
    Gt ann e1 e2 ->
      choose
        [ checkBinOp boolean boolean boolean Gt ann e1 e2
        , checkBinOp number  number  boolean Gt ann e1 e2
        , checkBinOp string  string  boolean Gt ann e1 e2
        ]
    Not ann e -> do
      e' <- checkExpr e boolean
      pure (Not ann e', boolean)
    Plus ann e1 e2 ->
      checkBinOp number number number Plus ann e1 e2
    Minus ann e1 e2 ->
      checkBinOp number number number Minus ann e1 e2
    Times ann e1 e2 ->
      checkBinOp number number number Times ann e1 e2
    DividedBy ann e1 e2 ->
      checkBinOp number number number DividedBy ann e1 e2
    Modulo ann e1 e2 ->
      checkBinOp number number number Modulo ann e1 e2
    Cons ann e1 e2 -> do
      (re1, rt1) <- inferExpr e1
      let listType = list rt1
      re2 <- checkExpr e2 listType
      pure (Cons ann re1 re2, listType)
    Proj ann e l -> do
      (e', te) <- inferExpr e
      (l', tl) <- checkSelector te l
      pure (Proj ann e' l', tl)
    Var ann n -> do
      (r, pt) <- resolveTerm n
      t <- instantiate pt
      pure (Var ann r, t)
    Lam ann givens e -> do
      (rgivens, rargts) <- inferLamGivens givens
      (re, te) <- inferExpr e
      pure (Lam ann rgivens re, fun_ rargts te)
    App ann n es -> do
      (rn, pt) <- resolveTerm n
      t <- instantiate pt
      (res, ts) <- unzip <$> traverse inferExpr es
      rt <- fresh (NormalName "v")
      let tf = fun_ ts rt
      unify t tf
      pure (App ann rn res, rt)
    AppNamed ann n nes _morder -> do
      (rn, pt) <- resolveTerm n
      t <- instantiate pt
      (ornes, rt) <- inferAppNamed t nes
      let (order, rnes) = unzip ornes
      pure (AppNamed ann rn rnes (Just order), rt)
    IfThenElse ann e1 e2 e3 -> do
      v <- fresh (NormalName "ifthenelse")
      re <- checkIfThenElse ann e1 e2 e3 v
      pure (re, v)
    Consider ann e branches -> do
      v <- fresh (NormalName "consider")
      re <- checkConsider ann e branches v
      pure (re, v)
--    ParenExpr ann e -> do
--      (e', t) <- inferExpr e
--      pure (ParenExpr ann e', t)
    Lit ann l -> do
      t <- inferLit l
      pure (Lit ann l, t)
    List ann es -> do
      v <- fresh (NormalName "list")
      res <- traverse (flip checkExpr v) es
      pure (List ann res, list v)
    Where ann e ds -> scope $ do
      rds <- traverse inferLocalDecl ds
      (re, t) <- inferExpr e
      pure (Where ann re rds, t)

inferAppNamed :: Type' Resolved -> [NamedExpr Name] -> Check ([(Int, NamedExpr Resolved)], Type' Resolved)
inferAppNamed (Fun _ onts t) nes = do
  ornes <- supplyAppNamed (zip [0 ..] onts) nes
  pure (ornes, t)
inferAppNamed t _nes = do
  addError (IllegalAppNamed t)
  v <- fresh (NormalName "v")
  pure ([], v) -- TODO: This is unnecessarily lossy. We could still check the expressions and treat all names as out of scope.

supplyAppNamed :: [(Int, OptionallyNamedType Resolved)] -> [NamedExpr Name] -> Check [(Int, NamedExpr Resolved)]
supplyAppNamed []   [] = pure []
supplyAppNamed onts [] = do
  addError (IncompleteAppNamed (snd <$> onts))
  pure []
supplyAppNamed onts (MkNamedExpr ann n e : nes) = do
  (i, rn, t, onts') <- findOptionallyNamedType n onts
  re <- checkExpr e t
  rnes <- supplyAppNamed onts' nes
  pure ((i, MkNamedExpr ann rn re) : rnes)

findOptionallyNamedType :: Name -> [(Int, OptionallyNamedType Resolved)] -> Check (Int, Resolved, Type' Resolved, [(Int, OptionallyNamedType Resolved)])
findOptionallyNamedType n [] = do
  v <- fresh (NormalName "v")
  rn <- outOfScope n v
  pure (0, rn, v, [])
findOptionallyNamedType n ((i, MkOptionallyNamedType _ (Just n') t) : onts)
  | rawName n == rawName (getOriginal n') = do
    rn <- ref n n'
    pure (i, rn, t, onts)
findOptionallyNamedType n (ont : onts) = do
    (i, rn, t, onts') <- findOptionallyNamedType n onts
    pure (i, rn, t, ont : onts')

checkBranch :: Type' Resolved -> Type' Resolved -> Branch Name -> Check (Branch Resolved)
checkBranch tscrutinee tresult (When ann pat e)  = scope $ do
  (rpat, extend) <- checkPattern pat tscrutinee
  extend
  re <- checkExpr e tresult
  pure (When ann rpat re)
checkBranch _tscrutinee tresult (Otherwise ann e) = do
  re <- checkExpr e tresult
  pure (Otherwise ann re)

checkPattern :: Pattern Name -> Type' Resolved -> Check (Pattern Resolved, Check ())
checkPattern p t = do
  (rp, rt, extend) <- inferPattern p
  unify t rt
  pure (rp, extend)

-- Note: PatVar doesn't really get produced by the parser. We replace
-- PatApps that are not in scope with PatVar applications here in the
-- scope and type checker.
inferPattern :: Pattern Name -> Check (Pattern Resolved, Type' Resolved, Check ())
inferPattern g@(PatVar ann n)      = scope $ do
  setErrorContext (WhileCheckingPattern g)
  inferPatternVar ann n
inferPattern g@(PatApp ann n [])   = scope $ do
  setErrorContext (WhileCheckingPattern g)
  inferPatternApp ann n [] `orElse` inferPatternVar ann n
inferPattern g@(PatApp ann n ps)   = scope $ do
  setErrorContext (WhileCheckingPattern g)
  inferPatternApp ann n ps
inferPattern g@(PatCons ann p1 p2) = scope $ do
  setErrorContext (WhileCheckingPattern g)
  (rp1, rt1, extend1) <- inferPattern p1
  let listType = list rt1
  (rp2, extend2) <- checkPattern p2 listType
  pure (PatCons ann rp1 rp2, listType, extend1 >> extend2)

inferPatternVar :: Anno -> Name -> Check (Pattern Resolved, Type' Resolved, Check ())
inferPatternVar ann n = do
  rn <- def n
  rt <- fresh (NormalName "p")
  pure (PatVar ann rn, rt, makeKnown rn (KnownTerm rt Local))

inferPatternApp :: Anno -> Name -> [Pattern Name] -> Check (Pattern Resolved, Type' Resolved, Check ())
inferPatternApp ann n ps = do
  (rn, pt) <- resolveConstructor n
  t <- instantiate pt
  (rps, ts, extend) <- unzip3 <$> traverse inferPattern ps
  rt <- fresh (NormalName "v")
  let tf = fun_ ts rt
  unify t tf
  -- TODO: apply substitution
  pure (PatApp ann rn rps, rt, sequence_ extend)

inferLit :: Lit -> Check (Type' Resolved)
inferLit (NumericLit _ _) =
  pure number
inferLit (StringLit _ _) =
  pure string

ensureSameRef :: Resolved -> Resolved -> Check ()
ensureSameRef r1 r2
  | getUnique r1 == getUnique r2 = pure ()
  | otherwise = addError (UnificationErrorRef (getUniqueName r1) (getUniqueName r2))

-- We leave it somewhat vague how unify treats forall-types and TYPE.
-- In general, types should be instantiated prior to unification, and
-- kind-checking should not involve unification.
--
unify :: Type' Resolved -> Type' Resolved -> Check ()
unify (TyApp _ann1 n1 ts1) (TyApp _ann2 n2 ts2) = do
  ensureSameRef n1 n2
  -- We should not need to check the same length because we've done kind checking.
  traverse_ (uncurry unify) (zip ts1 ts2)
unify (Fun _ann1 onts1 t1) (Fun _ann2 onts2 t2)
  | length onts1 == length onts2 = do
    traverse_ (uncurry unify) (zip (optionallyNamedTypeType <$> onts1) (optionallyNamedTypeType <$> onts2))
    unify t1 t2
unify (Type _ann1) (Type _ann2) = pure ()
unify t1@(InfVar _ann1 _pre1 i1) t2
  | i1 `elem` infVars t2 = addError (OccursCheck t1 t2)
  | otherwise            = bind i1 t2
unify t1 t2@(InfVar _ann2 _pre2 i2)
  | i2 `elem` infVars t1 = addError (OccursCheck t1 t2)
  | otherwise            = bind i2 t1
unify t1 t2 = addError (UnificationError t1 t2)

infVars :: Type' Resolved -> [Int]
infVars (Type _)        = []
infVars (TyApp _ _ ts)  = concatMap infVars ts
infVars (Fun _ onts t)  = concatMap (infVars . optionallyNamedTypeType) onts ++ infVars t
infVars (Forall _ _ t)  = infVars t
infVars (InfVar _ _ i)  = [i]
-- infVars (ParenType _ t) = infVars t

bind :: Int -> Type' Resolved -> Check ()
bind i t = do
  subst <- use #substitution
  case Map.lookup i subst of
    Nothing -> assign #substitution (Map.insert i t subst)
    Just t' -> unify t' t -- addError (InternalUnificationErrorBind i t t')

prettySrcRange :: FilePath -> Maybe SrcRange -> Text
prettySrcRange fp Nothing = Text.pack fp <> ":<unknown range>"
prettySrcRange fp (Just (MkSrcRange p1 p2 _)) = Text.pack fp <> ":" <> prettySrcPos p1 <> prettyPartialSrcPos p1 p2

prettySrcPos :: SrcPos -> Text
prettySrcPos (MkSrcPos l c) = Text.pack (show l) <> ":" <> Text.pack (show c)

prettyPartialSrcPos :: SrcPos -> SrcPos -> Text
prettyPartialSrcPos (MkSrcPos rl rc) p@(MkSrcPos l c)
  | rl == l && rc == c = ""
  | rl == l            = "-" <> Text.pack (show c)
  | otherwise          = "-" <> prettySrcPos p

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

data Severity = SWarn | SError | SInfo
  deriving stock (Eq, Show)

severity :: CheckErrorWithContext -> Severity
severity (MkCheckErrorWithContext e _) =
  case e of
    CheckInfo {} -> SInfo
    _            -> SError

prettyCheckErrorWithContext :: CheckErrorWithContext -> Text
prettyCheckErrorWithContext (MkCheckErrorWithContext e ctx) =
  Text.unlines (prettyCheckErrorContext ctx) <> prettyCheckError e

prettyCheckErrorContext :: CheckErrorContext -> [Text]
prettyCheckErrorContext None                             = []
prettyCheckErrorContext (WhileCheckingDeclare n ctx)     = "while checking DECLARE of " <> simpleprint n <> ":" : prettyCheckErrorContext ctx
prettyCheckErrorContext (WhileCheckingDecide n ctx)      = "while checking DECIDE/MEANS of " <> simpleprint n <> ":" : prettyCheckErrorContext ctx
prettyCheckErrorContext (WhileCheckingAssume n ctx)      = "while checking ASSUME of " <> simpleprint n <> ":" : prettyCheckErrorContext ctx
prettyCheckErrorContext (WhileCheckingExpression _e ctx) = prettyCheckErrorContext ctx
prettyCheckErrorContext (WhileCheckingPattern _p ctx)    = prettyCheckErrorContext ctx
prettyCheckErrorContext (WhileCheckingType _t ctx)       = prettyCheckErrorContext ctx

prettyCheckError :: CheckError -> Text
prettyCheckError (OutOfScopeError n t)                     = "out of scope: " <> simpleprint n <> ", of type: " <> simpleprint t
prettyCheckError (KindError k ts)                          = "kind error: expected " <> Text.pack (show k) <> ", received " <> Text.pack (show (length ts))
prettyCheckError (UnificationErrorRef n1 n2)               = "cannot unify: " <> simpleprint n1 <> ", " <> simpleprint n2
prettyCheckError (UnificationError t1 t2)                  = "cannot unify: " <> simpleprint t1 <> ", " <> simpleprint t2
prettyCheckError (OccursCheck t1 t2)                       = "occurs check: " <> simpleprint t1 <> ", " <> simpleprint t2
prettyCheckError (InconsistentNameInSignature n Nothing)   = "inconsistent name (listed in signature, but not in the definition): " <> simpleprint n
prettyCheckError (InconsistentNameInSignature n (Just n')) = "inconsistent name: " <> simpleprint n <> ", definition has: " <> simpleprint n'
prettyCheckError (InconsistentNameInAppForm n Nothing)     = "inconsistent name (listed in definition, but not in the signature): " <> simpleprint n
prettyCheckError (InconsistentNameInAppForm n (Just n'))   = "inconsistent name: " <> simpleprint n <> ", signature has: " <> simpleprint n'
prettyCheckError (AmbiguousTermError n rs)                 = "ambiguous: " <> simpleprint n <> ", options:\n" <> Text.intercalate "\n" (simpleprint . snd <$> rs)
prettyCheckError (AmbiguousTypeError n _rs)                = "ambiguous: " <> simpleprint n
prettyCheckError InternalAmbiguityError                    = "ambiguous [internal]"
prettyCheckError (NonDistinctError _)                      = "non-distinct names"
prettyCheckError (IllegalAppNamed t)                       = "named application to a non-function: " <> simpleprint t
prettyCheckError (IncompleteAppNamed _onts)                = "missing arguments in named application"
prettyCheckError (CheckInfo t)                             = simpleprint t
prettyCheckError (IllegalTypeInKindSignature t)            = "only TYPE is allowed in signatures for type definitions, but found: " <> simpleprint t

class SimplePrint a where
  simpleprint :: a -> Text

instance SimplePrint a => SimplePrint (Type' a) where
  simpleprint :: Type' a -> Text
  simpleprint (Type _)        = "TYPE"
  simpleprint (TyApp _ n [])  = simpleprint n
  simpleprint (TyApp _ n ts)  = "(" <> simpleprint n <> " OF " <> Text.intercalate ", " (map simpleprint ts) <> ")"
  simpleprint (Fun _ onts t)  = "(FUNCTION FROM " <> Text.intercalate " AND " (map simpleprint onts) <> " TO " <> simpleprint t <> ")"
  simpleprint (Forall _ ns t) = "(FORALL " <> Text.intercalate ", " (map simpleprint ns) <> " " <> simpleprint t <> ")"
  simpleprint (InfVar _ n i)  = "_" <> simpleprint n <> Text.pack (show i)
--  simpleprint (ParenType _ t) = "(" <> simpleprint t <> ")"

instance SimplePrint a => SimplePrint (OptionallyNamedType a) where
  simpleprint (MkOptionallyNamedType _ _ t) = simpleprint t

instance SimplePrint RawName where
  simpleprint rn = rawNameToText rn

instance SimplePrint (Unique, Name) where
  simpleprint (_u, n) = simpleprint n

rawNameToText :: RawName -> Text
rawNameToText (NormalName n) = n
rawNameToText (PreDef n)     = n

instance SimplePrint Resolved where
  simpleprint (Def _ n)        = simpleprint n
  simpleprint (Ref n _ _)      = simpleprint n
  simpleprint (OutOfScope _ n) = simpleprint n

instance SimplePrint Name where
  simpleprint (MkName _ n) = simpleprint n

exactprint' :: ToConcreteNodes PosToken a => a -> Text
exactprint' x =
  case exactprint x of
    Left _  -> "<exactprint-error>"
    Right t -> t

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

-------------------------------------
-- Computing info for goto definition
-------------------------------------

-- | It would be better to have a tree structure so that we can exclude
-- large parts of the tree easily. However, right now we don't have range
-- information cheaply in the tree, so I thought I could as well build a
-- list.
--
-- Note that we are building a tree for type information, and possibly we
-- could do something similar here. It's just probably not worth it.
--
class ToResolved a where
  toResolved :: a -> [Resolved]
  default toResolved :: (SOP.Generic a, SOP.All (AnnoFirst a ToResolved) (SOP.Code a)) => a -> [Resolved]
  toResolved = genericToResolved

deriving anyclass instance ToResolved (Program Resolved)
deriving anyclass instance ToResolved (Section Resolved)
deriving anyclass instance ToResolved (TopDecl Resolved)
deriving anyclass instance ToResolved (LocalDecl Resolved)
deriving anyclass instance ToResolved (Assume Resolved)
deriving anyclass instance ToResolved (Declare Resolved)
deriving anyclass instance ToResolved (TypeDecl Resolved)
deriving anyclass instance ToResolved (ConDecl Resolved)
deriving anyclass instance ToResolved (Type' Resolved)
deriving anyclass instance ToResolved (TypedName Resolved)
deriving anyclass instance ToResolved (OptionallyTypedName Resolved)
deriving anyclass instance ToResolved (OptionallyNamedType Resolved)
deriving anyclass instance ToResolved (Decide Resolved)
deriving anyclass instance ToResolved (AppForm Resolved)
deriving anyclass instance ToResolved (Expr Resolved)
deriving anyclass instance ToResolved (NamedExpr Resolved)
deriving anyclass instance ToResolved (Branch Resolved)
deriving anyclass instance ToResolved (Pattern Resolved)
deriving anyclass instance ToResolved (TypeSig Resolved)
deriving anyclass instance ToResolved (GivethSig Resolved)
deriving anyclass instance ToResolved (GivenSig Resolved)
deriving anyclass instance ToResolved (Directive Resolved)

instance ToResolved Lit where
  toResolved = const []

instance ToResolved Int where
  toResolved = const []

instance ToResolved RawName where
  toResolved = const []

instance ToResolved Resolved where
  toResolved = pure

instance ToResolved a => ToResolved (Maybe a) where
  toResolved = concatMap toResolved

instance ToResolved a => ToResolved [a] where
  toResolved = concatMap toResolved

genericToResolved :: forall a. (SOP.Generic a, SOP.All (AnnoFirst a ToResolved) (SOP.Code a)) => a -> [Resolved]
genericToResolved =
  genericToNodes
    (Proxy @ToResolved)
    toResolved
    (const concat :: Anno' a -> [[Resolved]] -> [Resolved])

findDefinition :: ToResolved a => SrcPos -> a -> Maybe SrcRange
findDefinition pos a = do
  r <- find matches (toResolved a)
  rangeOf (getOriginal r)
  where
    matches :: Resolved -> Bool
    matches r =
      case rangeOf (getName r) of
        Just range -> inRange pos range
        Nothing -> False

-- | We ignore the file name, because we assume this has already been checked.
inRange :: SrcPos -> SrcRange -> Bool
inRange (MkSrcPos l c) (MkSrcRange (MkSrcPos l1 c1) (MkSrcPos l2 c2) _) =
     (l, c) >= (l1, c1)
  && (l, c) <= (l2, c2)

--------------------------------------------------
-- Computing info for hover (types of expressions)
--------------------------------------------------

data TypesTree =
  TypesNode
    { range    :: Maybe SrcRange
    , type'    :: Maybe (Type' Resolved)
    , children :: [TypesTree]
    }

class ToTypesTree a where
  toTypesTree :: a -> [TypesTree]
  default toTypesTree :: (SOP.Generic a, AnnoToken a ~ PosToken, AnnoExtra a ~ Extension, SOP.All (AnnoFirst a ToTypesTree) (SOP.Code a)) => a -> [TypesTree]
  toTypesTree = genericToTypesTree

instance HasSrcRange TypesTree where
  rangeOf = (.range)

genericToTypesTree :: forall a. (SOP.Generic a, AnnoToken a ~ PosToken, AnnoExtra a ~ Extension, SOP.All (AnnoFirst a ToTypesTree) (SOP.Code a)) => a -> [TypesTree]
genericToTypesTree =
  genericToNodes
    (Proxy @ToTypesTree)
    toTypesTree
    (mergeTypesTrees (Proxy @a))

mergeTypesTrees :: (AnnoToken a ~ PosToken, AnnoExtra a ~ Extension) => Proxy a -> Anno' a -> [[TypesTree]] -> [TypesTree]
mergeTypesTrees _ anno children =
  [TypesNode (rangeOf (concat children)) (anno ^. annResolveType) (concat children)]

findType :: ToTypesTree a => SrcPos -> a -> Maybe (SrcRange, Type' Resolved)
findType pos a =
  asum (go <$> toTypesTree a)
  where
    go :: TypesTree -> Maybe (SrcRange, Type' Resolved)
    go (TypesNode Nothing _t children)     =
        asum (go <$> children) -- <|> if t == Nothing then Just (range, Type mempty) else (range,) <$> t
    go (TypesNode (Just range) t children)
      | pos `inRange` range                =
        asum (go <$> children) <|> {- if t == Nothing then Just (range, Type mempty) else -} (range,) <$> t
      | otherwise                          = Nothing

deriving anyclass instance ToTypesTree (Program Resolved)
deriving anyclass instance ToTypesTree (Section Resolved)
deriving anyclass instance ToTypesTree (TopDecl Resolved)
deriving anyclass instance ToTypesTree (LocalDecl Resolved)
deriving anyclass instance ToTypesTree (Assume Resolved)
deriving anyclass instance ToTypesTree (Declare Resolved)
deriving anyclass instance ToTypesTree (TypeDecl Resolved)
deriving anyclass instance ToTypesTree (ConDecl Resolved)
deriving anyclass instance ToTypesTree (Type' Resolved)
deriving anyclass instance ToTypesTree (TypedName Resolved)
deriving anyclass instance ToTypesTree (OptionallyTypedName Resolved)
deriving anyclass instance ToTypesTree (OptionallyNamedType Resolved)
deriving anyclass instance ToTypesTree (Decide Resolved)
deriving anyclass instance ToTypesTree (AppForm Resolved)
deriving anyclass instance ToTypesTree (Expr Resolved)
deriving anyclass instance ToTypesTree (NamedExpr Resolved)
deriving anyclass instance ToTypesTree (Branch Resolved)
deriving anyclass instance ToTypesTree (Pattern Resolved)
deriving anyclass instance ToTypesTree (TypeSig Resolved)
deriving anyclass instance ToTypesTree (GivethSig Resolved)
deriving anyclass instance ToTypesTree (GivenSig Resolved)
deriving anyclass instance ToTypesTree (Directive Resolved)

instance ToTypesTree Lit where
  toTypesTree l =
    [TypesNode (rangeFromAnno (getAnno l)) ((getAnno l) ^. annResolveType) []]

-- | Try to extract the range from an anno which we assume to be
-- without holes.
rangeFromAnno :: Anno -> Maybe SrcRange
rangeFromAnno (Anno _ _ csns) = rangeOf (go csns)
  where
    go []              = []
    go (AnnoHole _  : cs) = go cs -- should not happen
    go (AnnoCsn _ m : cs) = m : go cs

instance ToTypesTree Int where
  toTypesTree _ =
    [TypesNode Nothing Nothing []]

instance ToTypesTree Name where
  toTypesTree n =
    [TypesNode (rangeFromAnno (getAnno n)) ((getAnno n) ^. annResolveType) []]

instance ToTypesTree RawName where
  toTypesTree _ =
    [TypesNode Nothing Nothing []]

instance ToTypesTree Resolved where
  toTypesTree n =
    toTypesTree (getName n)

instance ToTypesTree a => ToTypesTree (Maybe a) where
  toTypesTree =
    concatMap toTypesTree

instance ToTypesTree a => ToTypesTree [a] where
  toTypesTree =
    concatMap toTypesTree

