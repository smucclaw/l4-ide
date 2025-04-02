module L4.TypeCheck
  ( module X
  , HasName(..)
  , applyFinalSubstitution
  , combineEnvironmentEntityInfo
  , doCheckProgram
  , doCheckProgramWithDependencies
  , initialCheckState
  , isQuantifier
  , prettyCheckError
  , prettyCheckErrorWithContext
  , severity
  )
  where

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
import L4.Annotation
import L4.Parser.SrcSpan (prettySrcRange, prettySrcRangeM)
import L4.Print (prettyLayout, quotedName)
import L4.Syntax
import L4.TypeCheck.Annotation
import L4.TypeCheck.Environment as X
import L4.TypeCheck.Types as X
import L4.TypeCheck.With as X

import Control.Applicative
import Data.Bifunctor
import Data.Either (partitionEithers)
import Optics.Core hiding (anyOf, re)

mkInitialCheckState :: Environment -> EntityInfo -> Substitution -> CheckState
mkInitialCheckState environment entityInfo substitution =
  MkCheckState
    { environment
    , entityInfo
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
doCheckProgram :: NormalizedUri -> Module  Name -> CheckResult
doCheckProgram = doCheckProgramWithDependencies initialCheckState

initialCheckState :: CheckState
initialCheckState = mkInitialCheckState initialEnvironment initialEntityInfo Map.empty

doCheckProgramWithDependencies :: CheckState -> NormalizedUri -> Module  Name -> CheckResult
doCheckProgramWithDependencies checkState moduleUri program =
  case runCheckUnique (inferProgram program) MkCheckEnv {moduleUri} checkState of
    (w, s) ->
      let
        (errs, rprog) = runWith w
      in
        -- might be nicer to be able to do this within the inferProgram call / at the end of it
        case runCheckUnique (traverse applySubst errs) MkCheckEnv {moduleUri} s of
          (w', s') ->
            let (moreErrs, substErrs) = runWith w'
            in MkCheckResult
              { program = rprog
              , errors = substErrs ++ moreErrs
              , substitution = s'.substitution
              , environment = s'.environment
              , entityInfo = s'.entityInfo
              }

-- | Combines environment and entityInfo into one single list
--
-- This is currently used to generate top-level completions.
--
combineEnvironmentEntityInfo :: Environment -> EntityInfo -> [(Name, CheckEntity)]
combineEnvironmentEntityInfo env ei =
    foldMap (uncurry lookupUniques) $ Map.toList env
    where
      lookupUniques rn = mapMaybe \unique -> do
        (n, ce) <- ei Map.!? unique
        pure (replaceRawName rn n, ce)

      -- NOTE: the reason why we do this is because the CheckEntity doesn't contain the original name
      -- e.g. if you have `foo AKA bar`, the CheckEntity will always contain `bar`. However, the environment
      -- still has the correct name.
      replaceRawName rn (MkName a _) = MkName a rn

-- | Can be used to apply the final substitution after type-checking, expanding
-- inference variables whenever possible.
--
applyFinalSubstitution :: ApplySubst a => Substitution -> NormalizedUri -> a -> a
applyFinalSubstitution subst moduleUri t =
  let
    cs = mkInitialCheckState Map.empty Map.empty subst
  in
    case runCheckUnique (applySubst t) MkCheckEnv {moduleUri} cs of
      (w, _cs') ->
        let
          (_errs, r) = runWith w
        in
          r

-- | Helper function to run the check monad an expect a unique result.
runCheckUnique :: Check a -> CheckEnv -> CheckState  -> (With CheckErrorWithContext a, CheckState)
runCheckUnique c e s =
  case runCheck c e s of
    [] -> error "internal error: expected unique result, got none"
    [(w, s')] -> (w, s')
    _ -> error "internal error: expected unique result, got several"

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
  ctx <- use #errorContext
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
      procPlain _ ((Plain _, _)  : [])  = candidates
      procPlain _ ((Plain _, _)  : _cs) = candidates
      procPlain a ((With _ _, _) :  cs) = procPlain a cs

      -- We have a failure, we're still looking for a success, and prefer the last failure
      procWith a []                     = [a]
      procWith _ ((Plain a, s')  : cs)  = procPlain (Plain a, s') cs
      procWith _ ((With e x, s') : cs)  = procWith (With e x, s') cs

    in
      proc candidates

-- | Should never return 'Nothing' if our system is OK.
getEntityInfo :: Resolved -> Check (Maybe CheckEntity)
getEntityInfo r = do
  ei <- use #entityInfo
  case Map.lookup (getUnique r) ei of
    Nothing       -> do
      addError (MissingEntityInfo r)
      pure Nothing
    Just (_n, ce) -> pure (Just ce)

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
  -> Text
  -> (Anno -> Expr Resolved -> Expr Resolved -> Expr Resolved)
  -> Anno
  -> Expr Name
  -> Expr Name
  -> Check (Expr Resolved, Type' Resolved)
checkBinOp t1 t2 tr opname op ann e1 e2 = do
  e1' <- checkExpr (ExpectBinOpArgContext opname 1) e1 t1
  e2' <- checkExpr (ExpectBinOpArgContext opname 2) e2 t2
  pure (op ann e1' e2', tr)

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

inferDeclare :: Declare Name -> Check (Declare Resolved)
inferDeclare (MkDeclare ann tysig appForm t) = do
  (rd, extend) <- scope $ do
    setErrorContext (WhileCheckingDeclare (getName appForm))
    (rappForm, rtysig) <- checkTypeAppFormTypeSigConsistency appForm tysig
    inferTypeAppForm' rappForm rtysig
    (rt, extend) <- inferTypeDecl rappForm t
    pure (MkDeclare ann rtysig rappForm rt, extend)
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
    inferTypeAppForm' rappForm rtysig
    -- TODO: do we ever check the result kind?
    let
      extend =
        makeKnownMany
          (appFormHeads rappForm)
          (KnownType (kindOfAppForm rappForm)
            (view appFormArgs rappForm)
            (EnumDecl emptyAnno [])
          )
    pure (MkAssume ann rtysig rappForm (Just (Type tann)), extend)
  extend
  pure rd
inferAssume (MkAssume ann tysig appForm mt) = do
  -- declaration of a term
  (rd, extend) <- scope $ do
    setErrorContext (WhileCheckingAssume (getName appForm))
    (rappForm, rtysig) <- checkTermAppFormTypeSigConsistency appForm tysig -- (MkTypeSig mempty (MkGivenSig mempty []) (Just (MkGivethSig mempty t)))
    (ce, rt, result) <- inferTermAppForm rappForm rtysig
    -- check that the given result type matches the result type in the type signature
    rmt <- case mt of
      Nothing -> pure Nothing
      Just t  -> do
        rt' <- inferType t
        expect (ExpectAssumeSignatureContext (rangeOf result)) result rt'
        pure (Just rt')
    let rd = setAnnResolvedType rt (MkAssume ann rtysig rappForm rmt)
    pure (rd, makeKnownMany (appFormHeads rappForm) ce)
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

-- We process imports prior to normal scope- and type-checking. Therefore, this is trivial.
inferImport :: Import Name -> Check (Import Resolved)
inferImport (MkImport ann n) = do
  rn <- def n
  pure (MkImport ann rn)

inferSection :: Section Name -> Check (Section Resolved)
inferSection (MkSection ann lvl mn maka topdecls) = do
  rmn <- traverse def mn -- we currently treat section names as defining occurrences, but they play no further role
  rmaka <-
    case rmn of
      Nothing -> pure Nothing -- we do not support anonymous sections with AKAs
      Just rn -> traverse (inferAka rn) maka
  rtopdecls <- traverse inferTopDecl topdecls
  pure (MkSection ann lvl rmn rmaka rtopdecls)

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
inferTopDecl (Import ann import_) = do
  rimport_ <- inferImport import_
  pure (Import ann rimport_)

-- TODO: Somewhere near the top we should do dependency analysis. Note that
-- there is a potential problem. If we use type-directed name resolution but
-- also allow forward references, then how are we going to determine mutual
-- recursion? Optimistically, pessimistically, something in between?
--
inferProgram :: Module  Name -> Check (Module  Resolved)
inferProgram (MkModule ann uri sections) = do
  rsections <- traverse inferSection sections
  pure (MkModule ann uri rsections)

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
    rexpr <- checkExpr (ExpectDecideSignatureContext (rangeOf result)) expr result
    let ann' = set annInfo (Just (TypeInfo rt)) ann
    decide <- nlgDecide $ MkDecide ann' rtysig rappForm rexpr
    pure (decide, makeKnownMany (appFormHeads rappForm) ce)
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
checkTermAppFormTypeSigConsistency appForm@(MkAppForm _ _ ns _) (MkTypeSig tann (MkGivenSig gann []) mgiveth) =
  checkTermAppFormTypeSigConsistency'
    appForm
    (MkTypeSig tann (MkGivenSig gann ((\ n -> MkOptionallyTypedName emptyAnno n Nothing) <$> ns)) mgiveth)
checkTermAppFormTypeSigConsistency (MkAppForm aann n [] maka) tysig@(MkTypeSig _ (MkGivenSig _ otns) _) =
  checkTermAppFormTypeSigConsistency'
    (MkAppForm aann n (getName <$> filter isTerm otns) maka)
    tysig
checkTermAppFormTypeSigConsistency appForm tysig =
  checkTermAppFormTypeSigConsistency' appForm tysig

isTerm :: OptionallyTypedName Name -> Bool
isTerm (MkOptionallyTypedName _ _ (Just (Type _))) = False
isTerm _                                           = True

-- | Handles the third case described in 'checkTermAppFormTypeSigConsistency'.
checkTermAppFormTypeSigConsistency' :: AppForm Name -> TypeSig Name -> Check (AppForm Resolved, TypeSig Resolved)
checkTermAppFormTypeSigConsistency' (MkAppForm aann n ns maka) (MkTypeSig tann (MkGivenSig gann otns) mgiveth) = do
  rn <- def n
  (rns, rotns) <- ensureNameConsistency ns otns
  rmgiveth <- traverse inferGiveth mgiveth
  rmaka <- traverse (inferAka rn) maka
  pure (MkAppForm aann rn rns rmaka, MkTypeSig tann (MkGivenSig gann rotns) rmgiveth)

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
checkTypeAppFormTypeSigConsistency appForm@(MkAppForm _ _ ns _) (MkTypeSig tann (MkGivenSig gann []) mgiveth) =
  checkTypeAppFormTypeSigConsistency'
    appForm
    (MkTypeSig tann (MkGivenSig gann ((\ n -> MkOptionallyTypedName emptyAnno n (Just (Type emptyAnno))) <$> ns)) mgiveth)
checkTypeAppFormTypeSigConsistency (MkAppForm aann n [] maka) tysig@(MkTypeSig _ (MkGivenSig _ otns) _) =
  checkTypeAppFormTypeSigConsistency'
    (MkAppForm aann n (getName <$> otns) maka)
    tysig
checkTypeAppFormTypeSigConsistency appForm tysig =
  checkTypeAppFormTypeSigConsistency' appForm tysig

-- | Handles the third case described in 'checkTypeAppFormTypeSigConsistency'.
checkTypeAppFormTypeSigConsistency' :: AppForm Name -> TypeSig Name -> Check (AppForm Resolved, TypeSig Resolved)
checkTypeAppFormTypeSigConsistency' (MkAppForm aann n ns maka) (MkTypeSig tann (MkGivenSig gann otns) mgiveth) = do
  rn <- def n
  (rns, rotns) <- ensureTypeNameConsistency ns otns
  rmgiveth <- traverse inferTypeGiveth mgiveth
  rmaka <- traverse (inferAka rn) maka
  pure (MkAppForm aann rn rns rmaka, MkTypeSig tann (MkGivenSig gann rotns) rmgiveth)

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

appFormType :: AppForm Resolved -> Type' Resolved
appFormType (MkAppForm _ann n args _maka) = app n (tyvar <$> args)

-- | We do not make the names known, even though they are defining occurrences,
-- because we do not have any information about what the names are supposed to be.
--
inferAka :: Resolved -> Aka Name -> Check (Aka Resolved)
inferAka r (MkAka ann ns) = do
  rns <- traverse (defAka r) ns
  pure (MkAka ann rns)

inferTypeDecl :: AppForm Resolved -> TypeDecl Name -> Check (TypeDecl Resolved, Check ())
inferTypeDecl rappForm (EnumDecl ann conDecls) = do
  let
    rs     = appFormHeads rappForm
    td rcs = EnumDecl ann rcs
    kt     = KnownType (kindOfAppForm rappForm) (view appFormArgs rappForm)
  makeKnownMany rs (kt (td []))
  ensureDistinct NonDistinctConstructors (getName <$> conDecls)
  (rconDecls, extends) <- unzip <$> traverse (inferConDecl rappForm) conDecls
  pure (td rconDecls, makeKnownMany rs (kt (td rconDecls)) >> sequence_ extends)
inferTypeDecl rappForm (RecordDecl ann _mcon tns) = do
  -- we currently do not allow the user to specify their own constructor name
  -- a record declaration is just a special case of an enum declaration
  let
    rs = appFormHeads rappForm
    kt = KnownType (kindOfAppForm rappForm) (view appFormArgs rappForm)
  makeKnownMany rs (kt (EnumDecl emptyAnno []))
  (MkConDecl _ mrcon rtns, extend) <- inferConDecl rappForm (MkConDecl ann (getOriginal (view appFormHead rappForm)) tns)
  let
    td = RecordDecl ann (Just mrcon) rtns
  pure (td, makeKnownMany rs (kt td) >> extend)
inferTypeDecl rappForm (SynonymDecl ann t) = do
  let
    rs = appFormHeads rappForm
    kt = KnownType (kindOfAppForm rappForm) (view appFormArgs rappForm)
  rt <- inferType t
  let
    td = SynonymDecl ann rt
  pure (td, makeKnownMany rs (kt td))

inferConDecl :: AppForm Resolved -> ConDecl Name -> Check (ConDecl Resolved, Check ())
inferConDecl rappForm (MkConDecl ann n tns) = do
  ensureDistinct NonDistinctSelectors (getName <$> tns)
  dn <- def n
  (rtns, extends) <- unzip <$> traverse (inferSelector rappForm) tns
  let
    conType = forall' (view appFormArgs rappForm) (fun (typedNameOptionallyNamedType <$> rtns) (appFormType rappForm))
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
  let selectorInfo = KnownTerm (forall' (view appFormArgs rappForm) (fun_ [appFormType rappForm] rt)) Selector
  pure (MkTypedName ann dn rt, makeKnown dn selectorInfo)

-- | Infers / checks a type to be of kind TYPE.
inferType :: Type' Name -> Check (Type' Resolved)
inferType g = softprune $ scope $ do
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
      ensureDistinct NonDistinctQuantifiers ns
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
  options <- lookupRawNameInEnvironment (rawName n)
  case mapMaybe proc options of
    [] -> do
      let kind = 0
      rn <- outOfScope (setAnnResolvedKind kind n) (Type emptyAnno)
      pure (rn, kind)
    [x] -> pure x
    xs -> anyOf xs <|> do
      let kind = 0
      rn <- ambiguousType (setAnnResolvedKind kind n) xs
      pure (rn, kind)
  where
    proc :: (Unique, Name, CheckEntity) -> Maybe (Resolved, Kind)
    proc (u, o, KnownTypeVariable)       = let kind = 0 in Just (Ref (setAnnResolvedKind kind n) u o, kind)
    proc (u, o, KnownType kind _ _)      = Just (Ref (setAnnResolvedKind kind n) u o, kind)
    proc _                               = Nothing

class HasName a where
  getName :: a -> Name

instance HasName Name where
  getName n = n

instance HasName Resolved where
  getName = getActual

instance HasName a => HasName (AppForm a) where
  getName (MkAppForm _ n _ _) = getName n

instance HasName a => HasName (ConDecl a) where
  getName (MkConDecl _ n _) = getName n

instance HasName a => HasName (TypedName a) where
  getName (MkTypedName _ann n _t) = getName n

instance HasName a => HasName (OptionallyTypedName a) where
  getName (MkOptionallyTypedName _ann n _mt) = getName n

kindOfAppForm :: AppForm n -> Kind
kindOfAppForm (MkAppForm _ann _n args _maka) =
  length args

-- | We bring the args into scope, but not the entity itself, not even for the purpose
-- of recursive types. The reason is that e.g. type synonyms cannot be recursive, and
-- that we therefore do not have sufficient info yet.
--
inferTypeAppForm' :: AppForm Resolved -> TypeSig Resolved -> Check ()
inferTypeAppForm' rappForm _tysig = do
  let rs = appFormHeads rappForm
  let args = view appFormArgs rappForm
  ensureDistinct NonDistinctTypeAppForm (getName <$> (rs <> args)) -- should we do this earlier?
  makeKnownMany args KnownTypeVariable

-- | This happens after consistency checking which is in turn already doing part of
-- name resolution, so this takes a resolved appform. We do the environment handling
-- here.
inferTermAppForm :: AppForm Resolved -> TypeSig Resolved -> Check (CheckEntity, Type' Resolved, Type' Resolved)
inferTermAppForm rappForm tysig = do
  let rs = appFormHeads rappForm
  let args = view appFormArgs rappForm
  ensureDistinct NonDistinctTermAppForm (getName <$> (rs <> args)) -- should we do this earlier?
  (rt, result, extend) <- typeSigType tysig
  let termInfo = KnownTerm rt Computable
  makeKnownMany rs termInfo -- this makes the name known for recursive uses
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
  savedEi  <- use #entityInfo -- possibly not necessary, but also not harmful
  a <- m
  assign #errorContext savedCtx
  assign #environment savedEnv
  assign #entityInfo savedEi
  pure a

setErrorContext :: (CheckErrorContext -> CheckErrorContext) -> Check ()
setErrorContext f =
  modifying #errorContext f

ensureDistinct :: NonDistinctContext -> [Name] -> Check ()
ensureDistinct ndc ns = do
  let
    dups = filter ((>= 2) . length) (groupOn rawName (sortOn rawName ns))
  unless (null dups) $
    addError (NonDistinctError ndc dups)

-- | Makes the given named item known in the current scope,
-- with the given specification.
--
makeKnown :: Resolved -> CheckEntity -> Check ()
makeKnown a ce = do
  -- traceM $ "Trying to make known " <> Text.unpack (prettyLayout a) <> ": " <> debugCandidate (u, n, ce)
  modifying' #environment (Map.alter proc (rawName n))
  modifying' #entityInfo  (Map.insert u (n, ce))
  where
    u :: Unique
    n :: Name
    (u, n) = getUniqueName a

    proc :: Maybe [Unique] -> Maybe [Unique]
    proc Nothing   = Just [u]
    proc (Just xs) = Just (u : xs)

makeKnownMany :: [Resolved] -> CheckEntity -> Check ()
makeKnownMany rs ce =
  traverse_ (flip makeKnown ce) rs

checkExpr :: ExpectationContext -> Expr Name -> Type' Resolved -> Check (Expr Resolved)
checkExpr ec (IfThenElse ann e1 e2 e3) t = softprune $ scope $ do
  re <- checkIfThenElse ec ann e1 e2 e3 t
  let re' = setAnnResolvedType t re
  pure re'
checkExpr ec (Consider ann e branches) t = softprune $ scope $ do
  re <- checkConsider ec ann e branches t
  let re' = setAnnResolvedType t re
  pure re'
-- checkExpr (ParenExpr ann e) t = do
--   re <- checkExpr e t
--   pure (ParenExpr ann re)
checkExpr ec (Where ann e ds) t = softprune $ scope $ do
  rds <- traverse (nlgLocalDecl <=< inferLocalDecl) ds
  re <- checkExpr ec e t
  -- We have to immediately resolve 'Nlg' annotations, as 'ds'
  -- brings new bindings into scope.
  re2 <- nlgExpr re
  let re' = setAnnResolvedType t (Where ann re2 rds)
  pure re'
checkExpr ec e t = softprune $ scope $ do
  setErrorContext (WhileCheckingExpression e)
  (re, rt) <- inferExpr e
  expect ec t rt
  pure re

checkIfThenElse :: ExpectationContext -> Anno -> Expr Name -> Expr Name -> Expr Name -> Type' Resolved -> Check (Expr Resolved)
checkIfThenElse ec ann e1 e2 e3 t = do
  e1' <- checkExpr ExpectIfConditionContext e1 boolean
  e2' <- checkExpr ec e2 t
  e3' <- checkExpr ec e3 t
  pure (IfThenElse ann e1' e2' e3')

checkConsider :: ExpectationContext -> Anno -> Expr Name -> [Branch Name] -> Type' Resolved -> Check (Expr Resolved)
checkConsider ec ann e branches t = do
  (re, te) <- inferExpr e
  rbranches <- traverse (checkBranch ec re te t) branches
  pure (Consider ann re rbranches)

inferExpr :: Expr Name -> Check (Expr Resolved, Type' Resolved)
inferExpr g = softprune $ scope $ do
  setErrorContext (WhileCheckingExpression g)
  (re, te) <- inferExpr' g
  let re' = setAnnResolvedType te re
  pure (re', te)

inferExpr' :: Expr Name -> Check (Expr Resolved, Type' Resolved)
inferExpr' g =
  case g of
    And ann e1 e2 ->
      checkBinOp boolean boolean boolean "AND" And ann e1 e2
    Or ann e1 e2 ->
      checkBinOp boolean boolean boolean "OR"  Or ann e1 e2
    Implies ann e1 e2 ->
      checkBinOp boolean boolean boolean "IMPLIES" Implies ann e1 e2
    Equals ann e1 e2 -> do
      (re1, rt1) <- inferExpr e1
      re2 <- checkExpr (ExpectBinOpArgContext "EQUALS" 2) e2 rt1 -- TODO: it would be better to have a designated expectation context for EQUALS
      pure (Equals ann re1 re2, boolean)
    Leq ann e1 e2 -> -- TODO: consider making all the comparison operators polymorphic as well
      choose
        [ checkBinOp boolean boolean boolean "AT MOST" Leq ann e1 e2
        , checkBinOp number  number  boolean "AT MOST" Leq ann e1 e2
        , checkBinOp string  string  boolean "AT MOST" Leq ann e1 e2
        ]
    Geq ann e1 e2 ->
      choose
        [ checkBinOp boolean boolean boolean "AT LEAST" Geq ann e1 e2
        , checkBinOp number  number  boolean "AT LEAST" Geq ann e1 e2
        , checkBinOp string  string  boolean "AT LEAST" Geq ann e1 e2
        ]
    Lt ann e1 e2 ->
      choose
        [ checkBinOp boolean boolean boolean "LESS THAN" Lt ann e1 e2
        , checkBinOp number  number  boolean "LESS THAN" Lt ann e1 e2
        , checkBinOp string  string  boolean "LESS THAN" Lt ann e1 e2
        ]
    Gt ann e1 e2 ->
      choose
        [ checkBinOp boolean boolean boolean "GREATER THAN" Gt ann e1 e2
        , checkBinOp number  number  boolean "GREATER THAN" Gt ann e1 e2
        , checkBinOp string  string  boolean "GREATER THAN" Gt ann e1 e2
        ]
    Not ann e -> do
      e' <- checkExpr ExpectNotArgumentContext e boolean
      pure (Not ann e', boolean)
    Plus ann e1 e2 ->
      checkBinOp number number number "PLUS" Plus ann e1 e2
    Minus ann e1 e2 ->
      checkBinOp number number number "MINUS" Minus ann e1 e2
    Times ann e1 e2 ->
      checkBinOp number number number "TIMES" Times ann e1 e2
    DividedBy ann e1 e2 ->
      checkBinOp number number number "DIVIDED BY" DividedBy ann e1 e2
    Modulo ann e1 e2 ->
      checkBinOp number number number "MODULO" Modulo ann e1 e2
    Cons ann e1 e2 -> do
      (re1, rt1) <- inferExpr e1
      let listType = list rt1
      re2 <- checkExpr ExpectConsArgument2Context e2 listType
      pure (Cons ann re1 re2, listType)
    Proj ann e l -> do
      -- Handling this similar to App.
      --
      -- 1.
      (rl, pt) <- resolveTerm l
      t <- instantiate pt

      -- 2. - 5.
      (res, rt) <- matchFunTy True rl t [e]

      re <-
        case res of
          [re] -> pure re
          _ -> pure $ error "internal error in matchFunTy"

      pure (Proj ann re rl, rt)
    Var ann n -> do
      (r, pt) <- resolveTerm n
      t <- instantiate pt
      pure (Var ann r, t)
    Lam ann givens e -> do
      (rgivens, rargts) <- inferLamGivens givens
      (re, te) <- inferExpr e
      -- We have to resolve NLG annotations now, as the 'Lam' brings new
      -- variables into scope.
      re2 <- nlgExpr re
      pure (Lam ann rgivens re2, fun_ rargts te)
    App ann n es -> do
      -- We want good type error messages. Therefore, we pursue the
      -- following strategy:
      --
      -- 1. We infer the type of the function and instantiate it.
      --
      -- 2. Then, we introduce fresh variables for the arguments and the result.
      --
      -- 3. Then, we unify the type of the function against a constructed
      -- function type from the generated variables.
      --
      -- 4. At this point, if unification fails, we know that the function
      -- is either not a function at all or expects a different number of
      -- arguments than have been supplied.
      --
      -- 5. If unification succeeds, we can now proceed by *checking* all
      -- arguments of the function against their expected result types.

      -- 1.
      (rn, pt) <- resolveTerm n
      t <- instantiate pt

      -- 2. - 5.
      -- Note that if there are no arguments, then matchFunTy does not
      -- actually insist on the type t being a function.
      (res, rt) <- matchFunTy False rn t es

      pure (App ann rn res, rt)
    AppNamed ann n nes _morder -> do
      (rn, pt) <- resolveTerm n
      t <- instantiate pt
      (ornes, rt) <- inferAppNamed rn t nes
      let (order, rnes) = unzip ornes
      pure (AppNamed ann rn rnes (Just order), rt)
    IfThenElse ann e1 e2 e3 -> do
      v <- fresh (NormalName "ifthenelse")
      re <- checkIfThenElse ExpectIfBranchesContext ann e1 e2 e3 v
      pure (re, v)
    Consider ann e branches -> do
      v <- fresh (NormalName "consider")
      re <- checkConsider ExpectConsiderBranchesContext ann e branches v
      pure (re, v)
--    ParenExpr ann e -> do
--      (e', t) <- inferExpr e
--      pure (ParenExpr ann e', t)
    Lit ann l -> do
      t <- inferLit l
      pure (Lit ann l, t)
    List ann es -> do
      v <- fresh (NormalName "list")
      res <- traverse (\ e -> checkExpr ExpectHomogeneousListContext e v) es
      pure (List ann res, list v)
    Where ann e ds -> scope $ do
      rds <- traverse inferLocalDecl ds
      (re, t) <- inferExpr e
      pure (Where ann re rds, t)

inferAppNamed :: Resolved -> Type' Resolved -> [NamedExpr Name] -> Check ([(Int, NamedExpr Resolved)], Type' Resolved)
inferAppNamed r (Fun _ onts t) nes = do
  ornes <- supplyAppNamed r (zip [0 ..] onts) nes
  pure (ornes, t)
inferAppNamed r t _nes = do
  addError (IllegalAppNamed r t)
  v <- fresh (NormalName "v")
  pure ([], v) -- TODO: This is unnecessarily lossy. We could still check the expressions and treat all names as out of scope.

supplyAppNamed :: Resolved -> [(Int, OptionallyNamedType Resolved)] -> [NamedExpr Name] -> Check [(Int, NamedExpr Resolved)]
supplyAppNamed _r []   [] = pure []
supplyAppNamed  r onts [] = do
  addError (IncompleteAppNamed r (snd <$> onts))
  pure []
supplyAppNamed  r onts (MkNamedExpr ann n e : nes) = do
  (i, rn, t, onts') <- findOptionallyNamedType n onts
  re <- checkExpr (ExpectNamedArgContext r rn) e t
  rnes <- supplyAppNamed r onts' nes
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

checkBranch :: ExpectationContext -> Expr Resolved -> Type' Resolved -> Type' Resolved -> Branch Name -> Check (Branch Resolved)
checkBranch ec scrutinee tscrutinee tresult (When ann pat e)  = scope $ do
  (rpat, extend) <- checkPattern (ExpectPatternScrutineeContext scrutinee) pat tscrutinee
  extend
  re <- checkExpr ec e tresult
  When ann
    -- We have to resolve NLG annotations now because
    -- bound variables are brought into scope.
    <$> nlgPattern rpat
    <*> nlgExpr re
checkBranch ec _scrutinee _tscrutinee tresult (Otherwise ann e) = do
  re <- checkExpr ec e tresult
  Otherwise ann
    -- We have to resolve NLG annotations now because
    -- bound variables are brought into scope.
    -- In the 'Otherwise' case, there are no new variables, but
    -- for consistency, we still resolve the NLG annotations now.
    <$> nlgExpr re

checkPattern :: ExpectationContext -> Pattern Name -> Type' Resolved -> Check (Pattern Resolved, Check ())
checkPattern ec p t = do
  setErrorContext (WhileCheckingPattern p)
  (rp, rt, extend) <- inferPattern p
  expect ec t rt
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
  (rp2, extend2) <- checkPattern ExpectConsArgument2Context p2 listType
  pure (PatCons ann rp1 rp2, listType, extend1 >> extend2)

inferPatternVar :: Anno -> Name -> Check (Pattern Resolved, Type' Resolved, Check ())
inferPatternVar ann n = do
  rn <- def n
  rt <- fresh (NormalName "p")
  pure (PatVar ann rn, rt, makeKnown rn (KnownTerm rt Local))

inferPatternApp :: Anno -> Name -> [Pattern Name] -> Check (Pattern Resolved, Type' Resolved, Check ())
inferPatternApp ann n ps = do
  -- We are employing a similar strategy as in the App case for expressions.
  --
  -- 1.
  (rn, pt) <- resolveConstructor n
  t <- instantiate pt

  -- 2. - 5.
  (rps, rt, extend) <- matchPatFunTy rn t ps

  pure (PatApp ann rn rps, rt, extend)

inferLit :: Lit -> Check (Type' Resolved)
inferLit (NumericLit _ _) =
  pure number
inferLit (StringLit _ _) =
  pure string

ensureSameRef :: Resolved -> Resolved -> Check Bool
ensureSameRef r1 r2 =
  pure (getUnique r1 == getUnique r2)

-- | A special case of unification where we know the given type must
-- be a function and we know its arguments. We special-case it because
-- this allows us to generate substantially better error messages.
--
-- NOTE that if there are *no* arguments, we are not expecting a function,
-- and are trivially done.
--
matchFunTy :: Bool -> Resolved -> Type' Resolved -> [Expr Name] -> Check ([Expr Resolved], Type' Resolved)
matchFunTy _isProjection _r t []   =
  pure ([], t)
matchFunTy  isProjection r t args =
  case t of
    InfVar _ann _pre i -> do
      subst <- use #substitution
      case Map.lookup i subst of
        Nothing -> do
          -- We know nothing about the type of the thing we're applying.
          -- So we construct a new function type of the right shape by
          -- generating variables and bind the variable to that type.
          --
          -- Then we can proceed to checking the arguments.
          argts <- traverse (const (fresh (NormalName "arg"))) args
          rt <- fresh (NormalName "res")
          let tf = fun_ argts rt
          assign #substitution (Map.insert i tf subst)

          rargs <- traverse (\ (j, e, t') -> checkExpr (ExpectAppArgContext isProjection r j) e t') (zip3 [1 ..] args argts)
          pure (rargs, rt)

        Just t' -> matchFunTy isProjection r t' args
    Fun _ann onts rt
      -- We know already that the type of the thing we're applying
      -- is a function, good. So we can check the number of arguments
      -- and then check the arguments against their expected result
      -- types.
      | nonts == nargs -> do
        rargs <- traverse (\ (j, e, t') -> checkExpr (ExpectAppArgContext isProjection r j) e t') (zip3 [1 ..] args (optionallyNamedTypeType <$> onts))
        pure (rargs, rt)

      | otherwise -> do
        addError (IncorrectArgsNumberApp r nonts nargs)
        rargs <- fst . unzip <$> traverse inferExpr args
        pure (rargs, rt)
      where
        nonts = length onts
        nargs = length args
    TyApp _ann n ts -> do
      mt' <- tryExpandTypeSynonym n ts
      maybe illegalAppError (\ t' -> matchFunTy isProjection r t' args) mt'
    _ -> illegalAppError
  where
    illegalAppError = do
      -- We are trying to apply a non-function.
      addError (IllegalApp r t (length args))
      rargs <- fst . unzip <$> traverse inferExpr args
      pure (rargs, t)

-- | A variant of matchFunTy for expressions. The main difference is that patterns introduce
-- names, and we need to collect these. With a little bit of work, we could probably unify
-- this with matchFunTy.
--
matchPatFunTy :: Resolved -> Type' Resolved -> [Pattern Name] -> Check ([Pattern Resolved], Type' Resolved, Check ())
matchPatFunTy _r t []   =
  pure ([], t, pure ())
matchPatFunTy  r t args =
  case t of
    InfVar _ann _pre i -> do
      subst <- use #substitution
      case Map.lookup i subst of
        Nothing -> do
          -- We know nothing about the type of the thing we're applying.
          -- So we construct a new function type of the right shape by
          -- generating variables and bind the variable to that type.
          --
          -- Then we can proceed to checking the arguments.
          argts <- traverse (const (fresh (NormalName "arg"))) args
          rt <- fresh (NormalName "res")
          let tf = fun_ argts rt
          assign #substitution (Map.insert i tf subst)

          (rargs, extends) <- unzip <$> traverse (\ (j, e, t') -> checkPattern (ExpectAppArgContext False r j) e t') (zip3 [1 ..] args argts)
          pure (rargs, rt, sequence_ extends)

        Just t' -> matchPatFunTy r t' args
    Fun _ann onts rt
      -- We know already that the type of the thing we're applying
      -- is a function, good. So we can check the number of arguments
      -- and then check the arguments against their expected result
      -- types.
      | nonts == nargs -> do
        (rargs, extends) <- unzip <$> traverse (\ (j, e, t') -> checkPattern (ExpectAppArgContext False r j) e t') (zip3 [1 ..] args (optionallyNamedTypeType <$> onts))
        pure (rargs, rt, sequence_ extends)

      | otherwise -> do
        addError (IncorrectArgsNumberApp r nonts nargs)
        (rargs, _, extends) <- unzip3 <$> traverse inferPattern args
        pure (rargs, rt, sequence_ extends)
      where
        nonts = length onts
        nargs = length args
    _ -> do
      -- We are trying to apply a non-function.
      addError (IllegalApp r t (length args))
      (rargs, _, extends) <- unzip3 <$> traverse inferPattern args
      pure (rargs, t, sequence_ extends)

-- Wrapper for unify that fails at this point.
-- First argument is the expected type (pushed in), the second argument is the
-- given type (pulled out).
expect :: ExpectationContext -> Type' Resolved -> Type' Resolved -> Check ()
expect ec expected given = do
  b <- unify expected given
  unless b $ addError (TypeMismatch ec expected given)

tryExpandTypeSynonym :: Resolved -> [Type' Resolved] -> Check (Maybe (Type' Resolved))
tryExpandTypeSynonym r args = do
  ce <- getEntityInfo r
  case ce of
    Just (KnownType _kind params (SynonymDecl _ t)) -> do
      let substitution = Map.fromList (zipWith (\ n t' -> (getUnique n, t')) params args)
      pure (Just (substituteType substitution t))
    _ -> pure Nothing

-- We leave it somewhat vague how unify treats forall-types and TYPE.
-- In general, types should be instantiated prior to unification, and
-- kind-checking should not involve unification.
--
-- Unify proceeds in multiple layers.
--
-- First, we have to substitute. (TODO: We should probably just apply
-- the substitution eagerly all the time. There is a reason other
-- implementations do this as well.) Why? Because we have to prevent
-- infinite types from arising (which are usually unwanted, but more
-- importantly even, can make the system loop very easily). And in
-- order to detect cycles, we need to know the full set of inference
-- variables that occur in the target type once we try to bind an
-- inference variable. The easiest way to achieve this is to
-- substitute first.
--
-- Next, we have to expand type synonyms.
-- When do we want to expand a type synonym?
--
-- Basically whenever we have ruled out the inference variable cases,
-- because if we have one inference variable against a type synonym,
-- we can just bind directly.
--
-- So we check the inference variable cases first, and then try to
-- expand in 'expandAndUnify', and once we've established we cannot
-- expand, we handle the remaining cases in 'unifyBase'.
--
unify :: Type' Resolved -> Type' Resolved -> Check Bool
unify t1 t2 = do
  t1' <- applySubst t1
  t2' <- applySubst t2
  unify' t1' t2'

-- | Unify cases for inference variables, after substitution. Prevent
-- infinite types by performing the so-called "occurs check".
--
unify' :: Type' Resolved -> Type' Resolved -> Check Bool
unify' (InfVar _ann1 _pre1 i1) t2@(InfVar _ann2 _pre2 i2)
  | i1 == i2             = pure True
  | otherwise            = bind i1 t2
unify' (InfVar _ann1 _pre1 i1) t2
  | i1 `elem` infVars t2 = pure False -- addError (OccursCheck t1 t2)
  | otherwise            = bind i1 t2
unify' t1 (InfVar _ann2 _pre2 i2)
  | i2 `elem` infVars t1 = pure False -- addError (OccursCheck t1 t2)
  | otherwise            = bind i2 t1
unify' t1 t2             = expandAndUnify t1 t2

-- | Handles the cases where we've established we have no top-level
-- inference variables.
--
expandAndUnify :: Type' Resolved -> Type' Resolved -> Check Bool
expandAndUnify t1 t2 =
  tryExpand t1 (\ t1' -> unify' t1' t2) $
  tryExpand t2 (\ t2' -> unify' t1 t2') $
  unifyBase t1 t2
  where
    -- Tries to expand the given type synonym. If expansion succeeds,
    -- applies the success continuation, otherwise the failure
    -- continuation.
    --
    tryExpand :: Type' Resolved -> (Type' Resolved -> Check r) -> Check r -> Check r
    tryExpand (TyApp _ann n ts)  kSuccess kFail = do
      mt' <- tryExpandTypeSynonym n ts
      maybe kFail kSuccess mt'
    tryExpand _                 _kSuccess kFail = kFail

-- | Handles the cases where we've established we have no top-level
-- type synonym application and no inference variables.
--
unifyBase :: Type' Resolved -> Type' Resolved -> Check Bool
unifyBase (TyApp _ann1 n1 ts1) (TyApp _ann2 n2 ts2) = do
  -- both are type constructors or type variables
  r <- ensureSameRef n1 n2
  -- We should not need to check the same length because we've done kind checking.
  rs <- traverse (uncurry unify') (zip ts1 ts2)
  pure (and (r : rs))
unifyBase (Fun _ann1 onts1 t1) (Fun _ann2 onts2 t2)
  | length onts1 == length onts2 = do
    rs <- traverse (uncurry unify') (zip (optionallyNamedTypeType <$> onts1) (optionallyNamedTypeType <$> onts2))
    r <- unify' t1 t2
    pure (and (r : rs))
unifyBase (Type _ann1) (Type _ann2) = pure True
unifyBase _t1 _t2 = pure False -- addError (UnificationError t1 t2)

infVars :: Type' Resolved -> [Int]
infVars (Type _)        = []
infVars (TyApp _ _ ts)  = concatMap infVars ts
infVars (Fun _ onts t)  = concatMap (infVars . optionallyNamedTypeType) onts ++ infVars t
infVars (Forall _ _ t)  = infVars t
infVars (InfVar _ _ i)  = [i]
-- infVars (ParenType _ t) = infVars t

bind :: Int -> Type' Resolved -> Check Bool
bind i t = do
  modifying' #substitution (Map.insert i t)
  pure True

severity :: CheckErrorWithContext -> Severity
severity (MkCheckErrorWithContext e _) =
  case e of
    CheckInfo {} -> SInfo
    _            -> SError

prettyCheckErrorWithContext :: CheckErrorWithContext -> [Text]
prettyCheckErrorWithContext (MkCheckErrorWithContext e ctx) =
  prettyCheckErrorContext ctx (prettyCheckError e)

prettyCheckErrorContext :: CheckErrorContext -> [Text] -> [Text]
prettyCheckErrorContext None                             e = e
prettyCheckErrorContext (WhileCheckingDeclare n ctx)     e =
  [ "I found a problem while checking the type declaration of " <> quotedName n <> ":" ]
  <> map ("  " <>) (prettyCheckErrorContext ctx e)
prettyCheckErrorContext (WhileCheckingDecide n ctx)      e =
  [ "I found a problem while checking the definition of " <> quotedName n <> ":" ]
  <> map ("  " <>) (prettyCheckErrorContext ctx e)
prettyCheckErrorContext (WhileCheckingAssume n ctx)      e =
  [ "I found a problem while checking the assumption of " <> quotedName n <> ":" ]
  <> map ("  " <>) (prettyCheckErrorContext ctx e)
prettyCheckErrorContext (WhileCheckingExpression _e ctx) e = prettyCheckErrorContext ctx e
prettyCheckErrorContext (WhileCheckingPattern _p ctx)    e = prettyCheckErrorContext ctx e
prettyCheckErrorContext (WhileCheckingType _t ctx)       e = prettyCheckErrorContext ctx e

prettyCheckError :: CheckError -> [Text]
prettyCheckError (OutOfScopeError n t)                     =
  [ "I could not find a definition for the identifier"
  , ""
  , "  " <> prettyLayout n
  , ""
  , "which I have inferred to be of type: "
  , ""
  , "  " <> prettyLayout t
  ]
prettyCheckError (KindError k ts)                          =
  [ "The arities of the types do not match."
  , "I expected " <> Text.show k <> " arguments, but I found " <> Text.show (length ts) <> "."
  ]
prettyCheckError (TypeMismatch ec expected given)          =
  prettyTypeMismatch ec expected given
prettyCheckError (InconsistentNameInSignature n Nothing)   =
  [ "The names in a type signature must match those in the definition."
  , ""
  , "However, this name " <> quotedName n <> " appears in the signature"
  , "and not in the definition."
  ]
prettyCheckError (InconsistentNameInSignature n (Just n')) =
  [ "The names in a type signature must match those in the definition."
  , ""
  , "However, this name " <> quotedName n <> " appears in the signature,"
  , "and the corresponding name in the definition is"
  , ""
  , "  " <> prettyNameWithRange n'
  ]
prettyCheckError (InconsistentNameInAppForm n Nothing)     =
  [ "The names in a definition must match those in the type signature."
  , ""
  , "However, this name " <> quotedName n <> " appears in the definition"
  , "and not in the type signature."
  ]
prettyCheckError (InconsistentNameInAppForm n (Just n'))   =
  [ "The names in a definition must match those in the type signature."
  , ""
  , "However, this name " <> quotedName n <> " appears in the definition"
  , "and the corresponding name in the type signature is"
  , ""
  , "  " <> prettyNameWithRange n'
  ]
prettyCheckError (AmbiguousTermError n rs)                 =
  [ "There are multiple definitions for the identifier"
  , ""
  , "  " <> prettyLayout n
  , ""
  , "and I do not have sufficient information to make a choice between them."
  , "The options are:"
  , ""
  ] ++ map (\ (r, t) -> "  " <> prettyResolvedWithRange r <> " of type " <> prettyLayout t) rs
prettyCheckError (AmbiguousTypeError n rs)                 =
  ["There are multiple definitions for the identifier"
  , ""
  , "  " <> prettyLayout n
  , ""
  , "and I do not have sufficient information to make a choice between them."
  , "The options are:"
  , ""
  ] ++ map (\ (r, k) -> "  " <> prettyResolvedWithRange r <> " of arity " <> Text.pack (show k)) rs
prettyCheckError InternalAmbiguityError                    =
  [ "I've encountered an internal ambiguity error."
  , "This means I have encountered a name ambiguity at a position where"
  , "I did not expect this to be possible. This is an error in this system"
  , "and should be reported as a bug."
  ]
prettyCheckError (NonDistinctError ndc nns)                  =
  prettyNonDistinctContext ndc ++
  [ ""
  , "But the following names have multiple occurrences:"
  , ""
  ] ++ map prettyNameWithRange (concat nns)
prettyCheckError (IncorrectArgsNumberApp r expected given)   =
  [ "The function"
  , ""
  , "  " <> prettyResolvedWithRange r
  , ""
  , "expects " <> prettyCount expected "argument" <> ","
  , "but you are applying it to " <> prettyCount given "argument" <> " here."
  ]
prettyCheckError (IllegalApp r t n)                          =
  [ "You are trying to apply"
  , ""
  , "  " <> prettyResolvedWithRange r <> " of type " <> prettyLayout t
  , ""
  , "(which is not a function) to " <> prettyCount n "argument" <> " here."
  ]
prettyCheckError (IllegalAppNamed r t)                       =
  [ "You are trying to apply"
  , ""
  , "  " <> prettyResolvedWithRange r <> " of type " <> prettyLayout t
  , ""
  , "(which is not a function) to (named) arguments here."
  ]
prettyCheckError (IncompleteAppNamed r onts)               =
  [ "In the application of"
  , ""
  , "  " <> prettyResolvedWithRange r
  , ""
  , "you forgot to supply the following arguments:"
  , ""
  ] ++ map (\ ont -> "  " <> prettyOptionallyNamedType ont) onts
prettyCheckError (CheckInfo t)                             = [prettyLayout t]
prettyCheckError (IllegalTypeInKindSignature t)            =
  [ "In a signature of a type declaration, all parameters must be of type"
  , ""
  , "  TYPE"
  , ""
  , "but this one has type"
  , ""
  , "  " <> prettyLayout t
  ]
prettyCheckError (MissingEntityInfo r)                     =
  [ "I've encountered a resolved name that has no additional information"
  , "stored for it:"
  , ""
  , "  " <> prettyLayout r
  , ""
  , "This is an error in this system and should be reported as a bug."
  ]

-- | Forms a plural when needed.
prettyCount :: Int -> Text -> Text
prettyCount i txt = Text.pack (show i) <> " " <> txt <> (if i == 1 then "" else "s")

prettyNonDistinctContext :: NonDistinctContext -> [Text]
prettyNonDistinctContext NonDistinctConstructors =
  [ "All constructors of an enumeration type must have distinct names." ]
prettyNonDistinctContext NonDistinctSelectors =
  [ "All selectors of a record type must have distinct names." ]
prettyNonDistinctContext NonDistinctQuantifiers =
  [ "All quantified variables in a polymorphic type must have distinct names." ]
prettyNonDistinctContext NonDistinctTypeAppForm =
  [ "In a type declaration, the type and all its parameters must have distinct names." ]
prettyNonDistinctContext NonDistinctTermAppForm =
  [ "In a definition, the defined term and all its parameters must have distinct names." ]

prettyTypeMismatch :: ExpectationContext -> Type' Resolved -> Type' Resolved -> [Text]
prettyTypeMismatch ExpectIfConditionContext expected given =
  standardTypeMismatch [ "The condition in an IF-THEN-ELSE construct is expected to be of type" ] expected given
prettyTypeMismatch ExpectNotArgumentContext expected given =
  standardTypeMismatch [ "The argument of NOT is expected to be of type" ] expected given
prettyTypeMismatch ExpectConsArgument2Context expected given =
  standardTypeMismatch [ "The second argument of FOLLOWED BY is expected to be of type" ] expected given
prettyTypeMismatch (ExpectPatternScrutineeContext scrutinee) expected given =
  [ "A pattern in a WHEN-clause of a CONSIDER construct is expected to have the type of the expression being matched."
  , "The expression being matched here is"
  , ""
  , "  " <> prettyLayout scrutinee <> " (at " <> prettySrcRangeM (rangeOf scrutinee) <> ")"
  , ""
  , "of type"
  , ""
  , "  " <> prettyLayout expected
  , ""
  , "but the type of this pattern is"
  , ""
  , "  " <> prettyLayout given
  ]
prettyTypeMismatch ExpectIfBranchesContext expected given =
  [ "Both the THEN and the ELSE branch of an IF-THEN-ELSE construct must have the same type."
  , "From looking at the context, if have inferred that this type must be"
  , ""
  , "  " <> prettyLayout expected
  , ""
  , "but this branch is of type"
  , ""
  , "  " <> prettyLayout given
  ]
prettyTypeMismatch ExpectConsiderBranchesContext expected given =
  [ "All branches in a CONSIDER construct must have the same type."
  , "From looking at the context, I have inferred that this type must be"
  , ""
  , "  " <> prettyLayout expected
  , ""
  , "but this branch is of type"
  , ""
  , "  " <> prettyLayout given
  ]
prettyTypeMismatch ExpectHomogeneousListContext expected given =
  [ "All elements in a LIST literal must have the same type."
  , "From looking at the context, I have inferred that this type must be"
  , ""
  , "  " <> prettyLayout expected
  , ""
  , "but this element is of type"
  , ""
  , "  " <> prettyLayout given
  ]
prettyTypeMismatch (ExpectDecideSignatureContext Nothing) expected given =
  standardTypeMismatch [ "From looking at the context, I have inferred that the type of this definition must be" ] expected given
prettyTypeMismatch (ExpectDecideSignatureContext (Just range)) expected given =
  standardTypeMismatch [ "The type of this definition must match its type signature at " <> prettySrcRange range <> ", namely" ] expected given
prettyTypeMismatch (ExpectAssumeSignatureContext Nothing) expected given =
  standardTypeMismatch [ "From looking at the context, I have inferred that the type of this assumption must be" ] expected given
prettyTypeMismatch (ExpectAssumeSignatureContext (Just range)) expected given =
  standardTypeMismatch [ "The type of this assumption must match its type signature at " <> prettySrcRange range <> ", namely" ] expected given
prettyTypeMismatch (ExpectAppArgContext True r _i) expected given =
  standardTypeMismatch
    [ "The argument of the projection "
    , ""
    , "  " <> prettyResolvedWithRange r
    , ""
    , "is expected to be of type"
    ] expected given
prettyTypeMismatch (ExpectAppArgContext False r i) expected given =
  standardTypeMismatch
    [ "The " <> prettyOrdinal i <> " argument of function "
    , ""
    , "  " <> prettyResolvedWithRange r
    , ""
    , "is expected to be of type"
    ] expected given
prettyTypeMismatch (ExpectNamedArgContext r nr) expected given =
  standardTypeMismatch
    [ "The argument"
    , ""
    , "  " <> prettyResolvedWithRange nr
    , ""
    , "of function"
    , ""
    , "  " <> prettyResolvedWithRange r
    , ""
    , "is expected to be of type"
    ] expected given
prettyTypeMismatch (ExpectBinOpArgContext txt i) expected given =
  standardTypeMismatch
    [ "The " <> prettyOrdinal i <> " argument of the " <> txt <> " operator is expected to be" ]
    expected given

-- | Best effort, only small numbers will occur"
prettyOrdinal :: Int -> Text
prettyOrdinal 1 = "first"
prettyOrdinal 2 = "second"
prettyOrdinal 3 = "third"
prettyOrdinal n = Text.pack (show n) <> "th"


standardTypeMismatch :: [Text] -> Type' Resolved -> Type' Resolved -> [Text]
standardTypeMismatch prefix expected given =
  prefix ++
  [ ""
  , "  " <> prettyLayout expected
  , ""
  , "but is here of type"
  , ""
  , "  " <> prettyLayout given
  ]

prettyOptionallyNamedType :: OptionallyNamedType Resolved -> Text
prettyOptionallyNamedType (MkOptionallyNamedType _ Nothing  t) =
  "an unnamed argument of type " <> prettyLayout t <> " (this is most likely an internal error)"
prettyOptionallyNamedType (MkOptionallyNamedType _ (Just r) t) =
  prettyLayout r <> " of type " <> prettyLayout t

-- | Show the name with its original / definition source range.
prettyResolvedWithRange :: Resolved -> Text
prettyResolvedWithRange r =
  case rangeOf (getOriginal r) of
    Nothing    -> prettyLayout r <> " (predefined)"
    Just range -> prettyLayout r <> " (defined at " <> prettySrcRange range <>  ")"

-- | Show the name with its source range.
--
-- TODO: eventually, we will have to print a file path here for potentially external locations
prettyNameWithRange :: Name -> Text
prettyNameWithRange n =
  prettyLayout n <> " (at " <> prettySrcRangeM (rangeOf n) <> ")"

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
