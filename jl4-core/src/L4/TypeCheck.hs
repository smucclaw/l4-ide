module L4.TypeCheck
  ( module X
  , HasName(..)
  , applyFinalSubstitution
  , combineEnvironmentEntityInfo
  , doCheckProgram
  , doCheckProgramWithDependencies
  , initialCheckState
  , initialCheckEnv
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
import L4.Parser.SrcSpan (prettySrcRange, prettySrcRangeM, SrcRange (..), zeroSrcPos)
import L4.Print (prettyLayout, quotedName,)
import L4.Syntax
import L4.TypeCheck.Annotation
import L4.TypeCheck.Environment as X
import L4.TypeCheck.Types as X
import L4.TypeCheck.Unify
import L4.TypeCheck.With as X
import qualified L4.Utils.IntervalMap as IV

import Control.Applicative
import Control.Monad.Extra (mapMaybeM)
import qualified Control.Monad.Extra as Extra
import Data.Either (partitionEithers)
import qualified Data.List as List
import Data.Tuple.Extra (firstM)

mkInitialCheckState :: Substitution -> CheckState
mkInitialCheckState substitution =
  MkCheckState
    { substitution
    , supply       = 0
    , infoMap      = IV.empty
    , nlgMap       = IV.empty
    }

mkInitialCheckEnv :: NormalizedUri -> Environment -> EntityInfo -> CheckEnv
mkInitialCheckEnv moduleUri environment entityInfo =
  MkCheckEnv
    { environment
    , entityInfo
    , errorContext = None
    , functionTypeSigs = Map.empty
    , declTypeSigs = Map.empty
    , declareDeclarations = Map.empty
    , assumeDeclarations = Map.empty
    , moduleUri
    , sectionStack = []
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
doCheckProgram moduleUri = doCheckProgramWithDependencies initialCheckState (initialCheckEnv moduleUri)

initialCheckState :: CheckState
initialCheckState = mkInitialCheckState Map.empty

initialCheckEnv :: NormalizedUri -> CheckEnv
initialCheckEnv moduleUri = mkInitialCheckEnv moduleUri initialEnvironment initialEntityInfo

doCheckProgramWithDependencies :: CheckState -> CheckEnv -> Module  Name -> CheckResult
doCheckProgramWithDependencies checkState checkEnv program =
  case runCheckUnique (checkProgram program) checkEnv checkState of
    (w, s) ->
      let
        (errs, (rprog, topEnv)) = runWith w
      in
        -- might be nicer to be able to do this within the inferProgram call / at the end of it
        case runCheckUnique (traverse applySubst errs) checkEnv s of
          (w', s') ->
            let (moreErrs, substErrs) = runWith w'
                env = extendEnv topEnv checkEnv
            in MkCheckResult
              { program = rprog
              , errors = substErrs ++ moreErrs
              , substitution = s'.substitution
              , environment = env.environment
              , entityInfo = env.entityInfo
              , infoMap = s'.infoMap
              , nlgMap = s'.nlgMap
              }

checkProgram :: Module Name -> Check (Module Resolved, [CheckInfo])
checkProgram module' = do
  withScanTypeAndSigEnvironment scanTyDeclModule inferTyDeclModule scanFunSigModule module' do
    inferProgram module'

withDecides :: [FunTypeSig] -> Check a -> Check a
withDecides rdecides =
  extendKnownMany topDecides . local \s -> s
    { functionTypeSigs = Map.fromList $ mapMaybe (\d -> (,d) <$> rangeOf d.anno) rdecides
    }
  where
    topDecides = fmap (.name) rdecides

withDeclares :: [DeclChecked DeclareOrAssume] -> Check a -> Check a
withDeclares rdecls =
  let
    (rdeclares, rassumes) = partitionEithers (mapMaybe go rdecls)
    go :: DeclChecked DeclareOrAssume
          -> Maybe (Either (SrcRange, DeclChecked (Declare Resolved)) (SrcRange, DeclChecked (Assume Resolved)))
    go (MkDeclChecked (Left  a) cis) = Left  . (, MkDeclChecked a cis) <$> rangeOf a
    go (MkDeclChecked (Right a) cis) = Right . (, MkDeclChecked a cis) <$> rangeOf a
  in
    extendKnownMany topDeclares . local \s -> s
      { declareDeclarations = Map.fromList rdeclares
      , assumeDeclarations = Map.fromList rassumes
      }
    where
      topDeclares = foldMap (.publicNames) rdecls

withDeclareTypeSigs :: [DeclTypeSig] -> Check a -> Check a
withDeclareTypeSigs rdeclares =
  extendKnownMany topDeclares . local \s -> s
    { declTypeSigs = Map.fromList $ mapMaybe (\d -> (,d) <$> rangeOf d.anno) rdeclares
    }
  where
    topDeclares = fmap (.name) rdeclares

lookupFromCheckEnv :: (CheckEnv -> Map SrcRange a) -> Anno -> Check a
lookupFromCheckEnv sel ann = case rangeOf ann of
  Nothing ->
    fatalInternalError (MissingSrcRangeForDeclaration ann)
  Just r -> do
    topEnv <- asks sel
    case Map.lookup r topEnv of
      Nothing ->
        fatalInternalError (MissingDeclForSrcRange ann)
      Just x -> pure x

lookupFunTypeSigByAnno :: Anno -> Check FunTypeSig
lookupFunTypeSigByAnno = lookupFromCheckEnv (.functionTypeSigs)

lookupDeclTypeSigByAnno :: Anno -> Check DeclTypeSig
lookupDeclTypeSigByAnno = lookupFromCheckEnv (.declTypeSigs)

lookupDeclareCheckedByAnno :: Anno -> Check (DeclChecked (Declare Resolved))
lookupDeclareCheckedByAnno = lookupFromCheckEnv (.declareDeclarations)

lookupAssumeCheckedByAnno :: Anno -> Check (DeclChecked (Assume Resolved))
lookupAssumeCheckedByAnno = lookupFromCheckEnv (.assumeDeclarations)

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
    cs = mkInitialCheckState subst
    ce = mkInitialCheckEnv moduleUri Map.empty Map.empty
  in
    case runCheckUnique (applySubst t) ce cs of
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
    _ -> error $ "internal error: expected unique result, got several"

-- ------------------------------------
-- Check Primitives
-- ------------------------------------

instantiate :: Type' Resolved -> Check (Type' Resolved)
instantiate (Forall _ann ns t) = do
  substitution <- Map.fromList <$> traverse (\ n -> let (u, o) = getUniqueName n; r = rawName o in fresh r >>= \ v -> pure (u, v)) ns
  pure (substituteType substitution t)
instantiate t             = pure t

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

-- Phase 4.
inferDeclare :: Declare Name -> Check (Declare Resolved, [CheckInfo])
inferDeclare (MkDeclare ann _tysig appForm _t) =
  errorContext (WhileCheckingDeclare (getName appForm)) do
    lookupDeclareCheckedByAnno ann >>= \ d -> pure (d.payload, d.publicNames)

-- | We allow assumptions for types, but we could potentially be more
-- sophisticated here.
--
-- In particular, we currently treat assumed types as enumeration types with no known
-- constructors (empty types). It would be better to have a dedicated case for these.
--
-- TODO: I think the checking whether we have a type declaration or a term
-- declaration is off, because we can have a type declaration of the form
--
-- @
-- GIVETH A TYPE
-- ASSUME T
-- @
--
-- which would currently not match the first case.
--
inferAssume :: Assume Name -> Check (Assume Resolved, [CheckInfo])
inferAssume (MkAssume ann _tysig appForm (Just (Type _tann))) = do
  -- declaration of a type
  errorContext (WhileCheckingAssume (getName appForm)) do
    lookupAssumeCheckedByAnno ann >>= \ d -> pure (d.payload, d.publicNames)
inferAssume (MkAssume ann _tysig appForm mt) = do
  -- declaration of a term
  errorContext (WhileCheckingAssume (getName appForm)) do
    lookupFunTypeSigByAnno ann >>= \ dHead -> do
        -- check that the given result type matches the result type in the type signature
        extendKnownMany dHead.arguments do
          rmt <- case mt of
            Nothing -> pure Nothing
            Just t  -> do
              rt' <- inferType t
              expect (ExpectAssumeSignatureContext (rangeOf dHead.resultType)) dHead.resultType rt'
              pure (Just rt')

          -- See Note [Adding type information to all binders]
          assume <-
            MkAssume dHead.anno
              <$> traverse resolvedType dHead.rtysig
              <*> traverse resolvedType dHead.rappForm
              <*> pure rmt
              >>= nlgAssume
          pure (assume, [dHead.name])

inferDirective :: Directive Name -> Check (Directive Resolved)
inferDirective (StrictEval ann e) = errorContext (WhileCheckingExpression e) do
  (re, _) <- prune $ inferExpr e
  pure (StrictEval ann re)
inferDirective (LazyEval ann e) = errorContext (WhileCheckingExpression e) do
  (re, _) <- prune $ inferExpr e
  pure (LazyEval ann re)
inferDirective (Check ann e) = errorContext (WhileCheckingExpression e) do
  (re, te) <- prune $ inferExpr e
  addError (CheckInfo te)
  pure (Check ann re)
inferDirective (Contract ann e t evs) = errorContext (WhileCheckingExpression e) do
  partyT <- fresh (NormalName "party")
  actionT <- fresh (NormalName "action")
  let contractT = contract partyT actionT
      eventT = event partyT actionT
  re <- prune $ checkExpr ExpectRegulativeContractContext e contractT
  rt <- prune $ checkExpr ExpectRegulativeTimestampContext t number
  revs <- traverse (prune . flip (checkExpr ExpectRegulativeEventContext) eventT) evs
  pure (Contract ann re rt revs)

-- We process imports prior to normal scope- and type-checking. Therefore, this is trivial.
inferImport :: Import Name -> Check (Import Resolved)
inferImport (MkImport ann n mr) = do
  let otherModule = fmap (MkSrcRange zeroSrcPos zeroSrcPos 0) mr
  m <- def (overAnno (\(Anno extra _mrange _csns) -> Anno extra otherModule [mkHoleWithSrcRangeHint otherModule]) n)
  rn <- ref n m
  pure (MkImport ann rn mr)

inferSection :: Section Name -> Check (Section Resolved, [CheckInfo])
inferSection (MkSection ann mn maka topdecls) = do
  -- NOTE: we currently treat section names as defining occurrences, but they play no further role
  rmn <- traverse def mn
  rmaka <-
    case rmn of
      Nothing -> pure Nothing -- we do not support anonymous sections with AKAs
      Just rn -> traverse (inferAka rn) maka

  (rtopdecls, topDeclExtends) <- unzip <$> traverse inferTopDecl topdecls

  pure (MkSection ann rmn rmaka rtopdecls, concat topDeclExtends)

inferLocalDecl :: LocalDecl Name -> Check (LocalDecl Resolved, [CheckInfo])
inferLocalDecl (LocalDecide ann decide) = do
  (rdecide, extends) <- softprune $ inferDecide decide
  pure (LocalDecide ann rdecide, extends)
inferLocalDecl (LocalAssume ann assume) = do
  (rassume, extends) <- softprune $ inferAssume assume
  pure (LocalAssume ann rassume, extends)

inferTopDecl :: TopDecl Name -> Check (TopDecl Resolved, [CheckInfo])
inferTopDecl (Declare ann declare) = do
  (rdeclare, extends) <- prune $ inferDeclare declare
  pure (Declare ann rdeclare, extends)
inferTopDecl (Decide ann decide) = do
  (rdecide, extends) <- prune $ inferDecide decide
  pure (Decide ann rdecide, extends)
inferTopDecl (Assume ann assume) = do
  (rassume, extends) <- prune $ inferAssume assume
  pure (Assume ann rassume, extends)
inferTopDecl (Directive ann directive) = do
  rdirective <- inferDirective directive
  pure (Directive ann rdirective, [])
inferTopDecl (Import ann import_) = do
  rimport_ <- inferImport import_
  pure (Import ann rimport_, [])
inferTopDecl (Section ann sec) = do
  (sec', extends) <- inferSection sec
  pure (Section ann sec', extends)

-- TODO: Somewhere near the top we should do dependency analysis. Note that
-- there is a potential problem. If we use type-directed name resolution but
-- also allow forward references, then how are we going to determine mutual
-- recursion? Optimistically, pessimistically, something in between?
--
inferProgram :: Module  Name -> Check (Module  Resolved, [CheckInfo])
inferProgram (MkModule ann uri section) = do
  (rsections, extends) <- inferSection section
  pure (MkModule ann uri rsections, extends)

-- | This covers constants and functions being defined.
--
-- These have an optional type signature. We require some compatibility between
-- the formal arguments given in the definition itself, and the formal arguments
-- given in the type signature.
--
-- TODO: This is more complicated due to potential polymorphism.
--
inferDecide :: Decide Name -> Check (Decide Resolved, [CheckInfo])
inferDecide (MkDecide ann _tysig appForm expr) = do
  errorContext (WhileCheckingDecide (getName appForm)) do
    lookupFunTypeSigByAnno ann >>= \ dHead -> do
        decide <- extendKnownMany dHead.arguments $ do
          rexpr <- checkExpr (ExpectDecideSignatureContext (rangeOf dHead.resultType)) expr dHead.resultType
          -- See Note [Adding type information to all binders]
          MkDecide dHead.anno
            <$> traverse resolvedType dHead.rtysig
            <*> traverse resolvedType dHead.rappForm
            <*> pure rexpr
            >>= nlgDecide
        pure (decide, [dHead.name])

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
checkTermAppFormTypeSigConsistency :: AppForm Name -> TypeSig Name -> Check (AppForm Resolved, TypeSig Resolved, [CheckInfo])
checkTermAppFormTypeSigConsistency appForm@(MkAppForm _ _ ns _) (MkTypeSig tann (MkGivenSig gann []) mgiveth) =
  checkTermAppFormTypeSigConsistency'
    appForm
    (MkTypeSig tann (MkGivenSig gann ((\ n -> MkOptionallyTypedName emptyAnno n Nothing) <$> ns)) mgiveth)
checkTermAppFormTypeSigConsistency (MkAppForm aann n [] maka) tysig@(MkTypeSig _ (MkGivenSig _ otns) _) =
  checkTermAppFormTypeSigConsistency'
    (MkAppForm aann n (clearSourceAnno . getName <$> filter isTerm otns) maka)
    tysig
checkTermAppFormTypeSigConsistency appForm tysig =
  checkTermAppFormTypeSigConsistency' appForm tysig

isTerm :: OptionallyTypedName Name -> Bool
isTerm (MkOptionallyTypedName _ _ (Just (Type _))) = False
isTerm _                                           = True

-- | Handles the third case described in 'checkTermAppFormTypeSigConsistency'.
checkTermAppFormTypeSigConsistency' :: AppForm Name -> TypeSig Name -> Check (AppForm Resolved, TypeSig Resolved, [CheckInfo])
checkTermAppFormTypeSigConsistency' (MkAppForm aann n ns maka) (MkTypeSig tann (MkGivenSig gann otns) mgiveth) = do
  rn <- def n
  (rns, rotns, extends) <- ensureNameConsistency ns otns
  extendKnownMany extends $ do
    rmgiveth <- traverse inferGiveth mgiveth
    rmaka <- traverse (inferAka rn) maka
    pure (MkAppForm aann rn rns rmaka, MkTypeSig tann (MkGivenSig gann rotns) rmgiveth, extends)

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
    (MkAppForm aann n (clearSourceAnno . getName <$> otns) maka)
    tysig
checkTypeAppFormTypeSigConsistency appForm tysig =
  checkTypeAppFormTypeSigConsistency' appForm tysig

-- | Handles the third case described in 'checkTypeAppFormTypeSigConsistency'.
checkTypeAppFormTypeSigConsistency' :: AppForm Name -> TypeSig Name -> Check (AppForm Resolved, TypeSig Resolved)
checkTypeAppFormTypeSigConsistency' (MkAppForm aann n ns maka) (MkTypeSig tann (MkGivenSig gann otns) mgiveth) = do
  rn <- def n
  (rns, rotns, extends) <- ensureTypeNameConsistency ns otns
  extendKnownMany extends do
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
inferTypeGiveth (MkGivethSig ann t) =
  errorContext (WhileCheckingType t) $ do
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
ensureNameConsistency :: [Name] -> [OptionallyTypedName Name] -> Check ([Resolved], [OptionallyTypedName Resolved], [CheckInfo])
ensureNameConsistency [] [] = pure ([], [], [])
ensureNameConsistency ns (MkOptionallyTypedName ann n (Just (Type tann)) : otns) = do
  rn <- def n
  extendKnown (makeKnown rn KnownTypeVariable) do
    (rns, rotns, extends) <- ensureNameConsistency ns otns
    pure (rns, MkOptionallyTypedName ann rn (Just (Type tann)) : rotns, makeKnown rn KnownTypeVariable : extends)
ensureNameConsistency (n : ns) (otn : otns)
  | rawName n == rawName (getName otn) = do
      rn <- def n
      rotn <- mkref rn otn
      (rns, rotns, extends) <- ensureNameConsistency ns otns
      pure (rn : rns, rotn : rotns, extends)
  | otherwise = do
      addError (InconsistentNameInAppForm n (Just (getName otn)))
      addError (InconsistentNameInSignature (getName otn) (Just n))
      rn <- def n
      rotn <- mkref rn otn -- questionable, will point to wrong name!
      (rns, rotns, extends) <- ensureNameConsistency ns otns
      pure (rn : rns, rotn : rotns, extends)
ensureNameConsistency (n : ns) [] = do
  addError (InconsistentNameInAppForm n Nothing)
  rn <- def n
  (rns, _, extends) <- ensureNameConsistency ns []
  pure (rn : rns, [], extends)
ensureNameConsistency [] (MkOptionallyTypedName ann n mt : otns) = do
  addError (InconsistentNameInSignature n Nothing)
  rn <- def n
  rmt <- traverse inferType mt
  (_, rotns, extends) <- ensureNameConsistency [] otns
  pure ([], MkOptionallyTypedName ann rn rmt : rotns, extends)

-- | Checks that the names are consistent, and resolve the 'OptionallyTypedName's.
--
-- Note that the list of 'OptionallyTypedName's can in principle contain both
-- type variables and term variables, but term variables are not permitted here.
--
ensureTypeNameConsistency :: [Name] -> [OptionallyTypedName Name] -> Check ([Resolved], [OptionallyTypedName Resolved], [CheckInfo])
ensureTypeNameConsistency [] [] = pure ([], [], [])
ensureTypeNameConsistency (n : ns) (MkOptionallyTypedName ann n' (Just (Type tann)) : otns)
  | rawName n == rawName n' = do
  rn <- def n
  rn' <- ref n' rn
  (rns, rotns, extends) <- ensureTypeNameConsistency ns otns
  pure (rn : rns, MkOptionallyTypedName ann rn' (Just (Type tann)) : rotns, extends)
ensureTypeNameConsistency (n : ns) (MkOptionallyTypedName ann n' Nothing : otns)
  | rawName n == rawName n' = do
  rn <- def n
  rn' <- ref n' rn
  (rns, rotns, extends) <- ensureTypeNameConsistency ns otns
  pure (rn : rns, MkOptionallyTypedName ann rn' Nothing : rotns, extends)
ensureTypeNameConsistency (n : ns) (otn : otns) = do
  addError (InconsistentNameInAppForm n (Just (getName otn)))
  addError (InconsistentNameInSignature (getName otn) (Just n))
  rn <- def n
  rotn <- mkref rn otn -- questionable, will point to wrong name!
  (rns, rotns, extends) <- ensureTypeNameConsistency ns otns
  pure (rn : rns, rotn : rotns, extends)
ensureTypeNameConsistency (n : ns) [] = do
  addError (InconsistentNameInAppForm n Nothing)
  rn <- def n
  (rns, _, extends) <- ensureNameConsistency ns []
  pure (rn : rns, [], extends)
ensureTypeNameConsistency [] (MkOptionallyTypedName ann n mt : otns) = do
  addError (InconsistentNameInSignature n Nothing)
  rn <- def n
  rmt <- traverse inferType mt
  (_, rotns, extends) <- ensureNameConsistency [] otns
  pure ([], MkOptionallyTypedName ann rn rmt : rotns, extends)

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

inferTypeDecl :: AppForm Resolved -> TypeDecl Name -> Check (TypeDecl Resolved, [CheckInfo])
inferTypeDecl rappForm (EnumDecl ann conDecls) = do
  let
    td rcs = EnumDecl ann rcs
  ensureDistinct NonDistinctConstructors (getName <$> conDecls)
  (rconDecls, extends) <- unzip <$> traverse (inferConDecl rappForm) conDecls
  pure (td rconDecls, concat extends)
inferTypeDecl rappForm (RecordDecl ann _mcon tns) = do
  -- we currently do not allow the user to specify their own constructor name
  -- a record declaration is just a special case of an enum declaration
  (MkConDecl _ mrcon rtns, extend) <- inferConDecl rappForm (MkConDecl ann (clearSourceAnno $ getOriginal (view appFormHead rappForm)) tns)
  let
    td = RecordDecl ann (Just mrcon) rtns
  pure (td, extend)
inferTypeDecl _rappForm (SynonymDecl ann t) = do
  rt <- inferType t
  let
    td = SynonymDecl ann rt
  pure (td, [])

inferTypeName :: AppForm Resolved -> TypeDecl Name -> Check CheckInfo
inferTypeName rappForm (EnumDecl _ann _conDecls) = do
  let
    rs = appFormHeads rappForm
    kt = KnownType (kindOfAppForm rappForm) (view appFormArgs rappForm) Nothing
  pure $ makeKnownMany rs kt
inferTypeName rappForm (RecordDecl _ann _mcon _tns) = do
  let
    rs = appFormHeads rappForm
    kt = KnownType (kindOfAppForm rappForm) (view appFormArgs rappForm) Nothing
  pure $ makeKnownMany rs kt
inferTypeName rappForm (SynonymDecl _ann _t) = do
  let
    rs = appFormHeads rappForm
    -- The 'Nothing' is wrong here, however, we can only insert it, once
    -- we are fully typechecking the 'SynonymDecl'.
    kt = KnownType (kindOfAppForm rappForm) (view appFormArgs rappForm) Nothing
  pure $ makeKnownMany rs kt

inferTypeNameAndSynonym :: AppForm Resolved -> Maybe (Type' Name) -> Check CheckInfo
inferTypeNameAndSynonym rappForm Nothing = do
  let
    rs = appFormHeads rappForm
    kt = KnownType (kindOfAppForm rappForm) (view appFormArgs rappForm) Nothing
  pure $ makeKnownMany rs kt
inferTypeNameAndSynonym rappForm (Just t) = do
  let
    rs = appFormHeads rappForm
    kt = KnownType (kindOfAppForm rappForm) (view appFormArgs rappForm) . Just
  rt <- inferType t
  pure $ makeKnownMany rs (kt rt)

inferConDecl :: AppForm Resolved -> ConDecl Name -> Check (ConDecl Resolved, [CheckInfo])
inferConDecl rappForm (MkConDecl ann n tns) = do
  ensureDistinct NonDistinctSelectors (getName <$> tns)
  dn <- def n
  (rtns, extends) <- unzip <$> traverse (inferSelector rappForm) tns
  let
    conType = forall' (view appFormArgs rappForm) (fun (typedNameOptionallyNamedType <$> rtns) (appFormType rappForm))
    conInfo = KnownTerm conType Constructor


  condecl <- extendKnownMany (makeKnown dn conInfo : concat extends) do
    -- See Note [Adding type information to all binders]
    MkConDecl ann
      <$> resolvedType dn
      <*> traverse (traverse resolvedType) rtns
  pure (condecl, makeKnown dn conInfo : concat extends)

typedNameOptionallyNamedType :: TypedName n -> OptionallyNamedType n
typedNameOptionallyNamedType (MkTypedName _ n t) = MkOptionallyNamedType emptyAnno (Just n) t

inferSelector :: AppForm Resolved -> TypedName Name -> Check (TypedName Resolved, [CheckInfo])
inferSelector rappForm (MkTypedName ann n t) = do
  rt <- inferType t
  dn <- def n
  let selectorInfo = KnownTerm (forall' (view appFormArgs rappForm) (fun_ [appFormType rappForm] rt)) Selector
  pure (MkTypedName ann dn rt, [makeKnown dn selectorInfo])

-- | Infers / checks a type to be of kind TYPE.
inferType :: Type' Name -> Check (Type' Resolved)
inferType g = softprune $ do
  errorContext (WhileCheckingType g) $ case g of
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
      (rdns, rt) <- extendKnownMany (fmap (flip makeKnown KnownTypeVariable) dns) do
        rt <- inferType t
        -- See Note [Adding type information to all binders]
        rdns <- traverse resolvedType dns
        pure (rdns, rt)
      pure (Forall ann rdns rt)
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
      n' <- setAnnResolvedKind kind n
      rn <- outOfScope n' (Type emptyAnno)
      pure (rn, kind)
    [x] -> x
    xs -> choose xs <|> do
      let kind = 0
      n' <- setAnnResolvedKind kind n
      xs' <- sequenceA xs
      rn <- ambiguousType n' xs'
      pure (rn, kind)
  where
    proc :: (Unique, Name, CheckEntity) -> Maybe (Check (Resolved, Kind))
    proc (u, o, KnownTypeVariable)       =
      let
        kind = 0
      in Just do
        n' <- setAnnResolvedKind kind n
        pure (Ref n' u o, kind)
    proc (u, o, KnownType kind _ _)      = Just do
      n' <- setAnnResolvedKind kind n
      pure (Ref n' u o, kind)
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
inferTypeAppForm' :: AppForm Resolved -> TypeSig Resolved -> Check CheckInfo
inferTypeAppForm' rappForm _tysig = do
  let rs = appFormHeads rappForm
  let args = view appFormArgs rappForm
  ensureDistinct NonDistinctTypeAppForm (getName <$> (rs <> args)) -- should we do this earlier?
  pure $ makeKnownMany args KnownTypeVariable

-- | This happens after consistency checking which is in turn already doing part of
-- name resolution, so this takes a resolved appform. We do the environment handling
-- here.
inferTermAppForm :: AppForm Resolved -> TypeSig Resolved -> Check (CheckEntity, Type' Resolved, Type' Resolved, [CheckInfo])
inferTermAppForm rappForm tysig = do
  let rs = appFormHeads rappForm
  let args = view appFormArgs rappForm
  ensureDistinct NonDistinctTermAppForm (getName <$> (rs <> args)) -- should we do this earlier?
  (rt, result, extend) <- typeSigType tysig
  let termInfo = KnownTerm rt Computable
  pure (termInfo, rt, result, extend)

-- | Infer a GIVEN that occurs as part of a lambda expression.
inferLamGivens :: GivenSig Name -> Check (GivenSig Resolved, [Type' Resolved], [CheckInfo])
inferLamGivens (MkGivenSig ann otns) = do
  (rotns, rargts, extends) <- unzip3 <$> traverse inferOptionallyTypedName otns
  pure (MkGivenSig ann rotns, rargts, concat extends)
  where
    -- TODO: there is unfortunate overlap between this and optionallyTypedNameType,
    -- but perhaps it's ok ...
    inferOptionallyTypedName :: OptionallyTypedName Name -> Check (OptionallyTypedName Resolved, Type' Resolved, [CheckInfo])
    inferOptionallyTypedName (MkOptionallyTypedName ann' n Nothing) = do
      rn <- def n
      v <- fresh (rawName n)
      pure (MkOptionallyTypedName ann' rn (Just v), v, [makeKnown rn (KnownTerm v Local)])
    inferOptionallyTypedName (MkOptionallyTypedName ann' n (Just t)) = do
      rn <- def n
      rt <- inferType t
      pure (MkOptionallyTypedName ann' rn (Just rt), rt, [makeKnown rn (KnownTerm rt Local)])

-- | Turn a type signature into a type, introducing inference variables for
-- unknown types. Also returns the result type.
-- Also returns terms to be brought into scope locally for the arguments.
--
-- Returns a quantified type if type arguments are involved.
--
-- TODO: It's possibly weird that we add the names to the scope in here.
--
typeSigType :: TypeSig Resolved -> Check (Type' Resolved, Type' Resolved, [CheckInfo])
typeSigType (MkTypeSig _ (MkGivenSig _ otns) mgiveth) = do
  let (tyvars, others) = partitionEithers (isQuantifier <$> otns)
  ronts <- traverse mkOptionallyNamedType others
  rt <- extendKnownMany (foldMap proc ronts) $
    maybeGivethType mgiveth
  pure (forall' tyvars (fun ronts rt), rt, foldMap proc ronts)
  where
    mkOptionallyNamedType :: (Resolved, Maybe (Type' Resolved)) -> Check (OptionallyNamedType Resolved)
    mkOptionallyNamedType (n, Nothing) = do
      v <- fresh (rawName (getName n))
      pure (MkOptionallyNamedType emptyAnno (Just n) v)
    mkOptionallyNamedType (n, Just t)  = do
      pure (MkOptionallyNamedType emptyAnno (Just n) t)

    proc :: OptionallyNamedType Resolved -> [CheckInfo]
    proc (MkOptionallyNamedType _ Nothing  _) = [] -- should not happen
    proc (MkOptionallyNamedType _ (Just n) t) =
      [makeKnown n (KnownTerm t Local)]

isQuantifier :: OptionallyTypedName Resolved -> Either Resolved (Resolved, Maybe (Type' Resolved))
isQuantifier (MkOptionallyTypedName _ n (Just (Type _))) = Left n
isQuantifier (MkOptionallyTypedName _ n mt             ) = Right (n, mt)

maybeGivethType :: Maybe (GivethSig Resolved) -> Check (Type' Resolved)
maybeGivethType Nothing                  = fresh (NormalName "r") -- we have no obvious prefix?
maybeGivethType (Just (MkGivethSig _ t)) = pure t -- type is already resolved

ensureDistinct :: NonDistinctContext -> [Name] -> Check ()
ensureDistinct ndc ns = do
  let
    dups = filter ((>= 2) . length) (groupOn rawName (sortOn rawName ns))
  unless (null dups) $
    addError (NonDistinctError ndc dups)

checkExpr :: ExpectationContext -> Expr Name -> Type' Resolved -> Check (Expr Resolved)
checkExpr ec (IfThenElse ann e1 e2 e3) t = softprune $ do
  re <- checkIfThenElse ec ann e1 e2 e3 t
  setAnnResolvedType t Nothing re
checkExpr ec (Consider ann e branches) t = softprune $ do
  re <- checkConsider ec ann e branches t
  setAnnResolvedType t Nothing re
-- checkExpr (ParenExpr ann e) t = do
--   re <- checkExpr e t
--   pure (ParenExpr ann re)
checkExpr ec (Where ann e ds) t = softprune $ do
  let
    preScanDecl = mapMaybeM scanTyDeclLocalDecl
    scanDecl = mapMaybeM inferTyDeclLocalDecl
    scanFuns = mapMaybeM scanFunSigLocalDecl

  (rds, extends) <- withScanTypeAndSigEnvironment preScanDecl scanDecl scanFuns ds do
     unzip <$> traverse (firstM nlgLocalDecl <=< inferLocalDecl) ds
  re <- extendKnownMany (concat extends) do
    re <- checkExpr ec e t
    -- We have to immediately resolve 'Nlg' annotations, as 'ds'
    -- brings new bindings into scope.
    nlgExpr re
  setAnnResolvedType t Nothing (Where ann re rds)
checkExpr ec e t = softprune $ errorContext (WhileCheckingExpression e) do
  (re, rt) <- inferExpr e
  expect ec t rt
  pure re

checkIfThenElse :: ExpectationContext -> Anno -> Expr Name -> Expr Name -> Expr Name -> Type' Resolved -> Check (Expr Resolved)
checkIfThenElse ec ann e1 e2 e3 t = do
  e1' <- checkExpr ExpectIfConditionContext e1 boolean
  e2' <- checkExpr ec e2 t
  e3' <- checkExpr ec e3 t
  pure (IfThenElse ann e1' e2' e3')

checkObligation
  :: Anno -> Expr Name -> RAction Name
  -> Maybe (Expr Name) -> Maybe (Expr Name) -> Maybe (Expr Name)
  -> Type' Resolved -> Type' Resolved -> Check (Obligation Resolved)
checkObligation ann party action due hence lest partyT actionT = do
  partyR <- checkExpr ExpectRegulativePartyContext party partyT
  (actionR, boundByPattern) <- checkAction action actionT
  let rTy = contract partyT actionT
  dueR <- traverse (\e -> checkExpr ExpectRegulativeDeadlineContext e number) due
  henceR <- traverse (\e -> extendKnownMany boundByPattern $ checkExpr ExpectRegulativeFollowupContext e rTy) hence
  lestR <- traverse (\e -> checkExpr ExpectRegulativeFollowupContext e rTy) lest
  pure (MkObligation ann partyR actionR dueR henceR lestR)

checkAction :: RAction Name -> Type' Resolved -> Check (RAction Resolved, [CheckInfo])
checkAction MkAction {anno, action, provided = mprovided} actionT = do
  (pat, bounds) <- checkPattern ExpectRegulativeActionContext action actionT
  -- NOTE: the provided clauses must evaluate to booleans
  provided <- forM mprovided \provided ->
    extendKnownMany bounds do
      checkExpr ExpectRegulativeProvidedContext provided boolean
  pure (MkAction {anno, action = pat, provided}, bounds)


checkConsider :: ExpectationContext -> Anno -> Expr Name -> [Branch Name] -> Type' Resolved -> Check (Expr Resolved)
checkConsider ec ann e branches t = do
  (re, te) <- inferExpr e
  rbranches <- traverse (checkBranch ec re te t) branches
  pure (Consider ann re rbranches)

inferExpr :: Expr Name -> Check (Expr Resolved, Type' Resolved)
inferExpr g = softprune $ errorContext (WhileCheckingExpression g) do
  (re, te) <- inferExpr' g
  re' <- setAnnResolvedType te Nothing re
  pure (re', te)


ambiguousOperatorError :: Text ->  Expr Name -> Expr Name -> (Expr Resolved -> Expr Resolved -> Expr Resolved) -> Check (Expr Resolved, Type' Resolved)
ambiguousOperatorError opName e1 e2 mkOp = do
  addError $ AmbiguousOperatorError opName
  (e1', _) <- inferExpr e1
  (e2', _) <- inferExpr e2
  t <- fresh (PreDef opName)
  pure (mkOp e1' e2', t)

inferExpr' :: Expr Name -> Check (Expr Resolved, Type' Resolved)
inferExpr' g =
  case g of
    And ann e1 e2 -> checkBinOp boolean boolean boolean "AND"  And ann e1 e2
    Or ann e1 e2 -> checkBinOp boolean boolean boolean "OR"  Or ann e1 e2
    RAnd ann e1 e2 -> do
      partyT <- fresh (NormalName "party")
      actT <- fresh (NormalName "action")
      let contractT = contract partyT actT
      checkBinOp contractT contractT contractT "AND" RAnd ann e1 e2
    ROr ann e1 e2 -> do
      partyT <- fresh (NormalName "party")
      actT <- fresh (NormalName "action")
      let contractT = contract partyT actT
      checkBinOp contractT contractT contractT "OR" ROr ann  e1 e2
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
        , ambiguousOperatorError "AT MOST" e1 e2 (Leq ann)
        ]
    Geq ann e1 e2 ->
      choose
        [ checkBinOp boolean boolean boolean "AT LEAST" Geq ann e1 e2
        , checkBinOp number  number  boolean "AT LEAST" Geq ann e1 e2
        , checkBinOp string  string  boolean "AT LEAST" Geq ann e1 e2
        , ambiguousOperatorError "AT LEAST" e1 e2 (Geq ann)
        ]
    Lt ann e1 e2 ->
      choose
        [ checkBinOp boolean boolean boolean "LESS THAN" Lt ann e1 e2
        , checkBinOp number  number  boolean "LESS THAN" Lt ann e1 e2
        , checkBinOp string  string  boolean "LESS THAN" Lt ann e1 e2
        , ambiguousOperatorError "LESS THAN" e1 e2 (Lt ann)
        ]
    Gt ann e1 e2 ->
      choose
        [ checkBinOp boolean boolean boolean "GREATER THAN" Gt ann e1 e2
        , checkBinOp number  number  boolean "GREATER THAN" Gt ann e1 e2
        , checkBinOp string  string  boolean "GREATER THAN" Gt ann e1 e2
        , ambiguousOperatorError "GREATER THAN" e1 e2 (Gt ann)
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
      (rgivens', rargts, extends) <- inferLamGivens givens
      (re, te, rgivens) <- extendKnownMany extends $ do
        (re, te) <- inferExpr e
        -- We have to resolve NLG annotations now, as the 'Lam' brings new
        -- variables into scope.
        rgivens <-
          -- See Note [Adding type information to all binders]
          traverse resolvedType rgivens'
            >>= nlgGivenSig
        nlgRe <- nlgExpr re
        pure (nlgRe, te, rgivens)
      pure (Lam ann rgivens re, fun_ rargts te)
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
    Regulative ann (MkObligation ann'' e1 e2 me3 me4 me5) -> do
      party <- fresh (NormalName "party")
      action <- fresh (NormalName "action")
      ob <- checkObligation ann'' e1 e2 me3 me4 me5 party action
      pure (Regulative ann ob, contract party action)
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
    Where ann e ds -> do
      let
        preScanDecl = mapMaybeM scanTyDeclLocalDecl
        scanDecl = mapMaybeM inferTyDeclLocalDecl
        scanFuns = mapMaybeM scanFunSigLocalDecl

      (rds, extends) <- withScanTypeAndSigEnvironment preScanDecl scanDecl scanFuns ds do
        unzip <$> traverse inferLocalDecl ds
      (re, t) <- extendKnownMany (concat extends) $ inferExpr e
      pure (Where ann re rds, t)
    Event ann ev -> do
      (ev', ty) <- inferEvent ev
      pure (Event ann ev', ty)
    Percent ann e -> do
      e' <- checkExpr ExpectPercentArgumentContext e number
      pure (Percent ann e', number)

inferEvent :: Event Name -> Check (Event Resolved, Type' Resolved)
inferEvent (MkEvent ann party action timestamp) = do
  partyT <- fresh (NormalName "party")
  actionT <- fresh (NormalName "action")
  party' <- checkExpr ExpectRegulativePartyContext party partyT
  action' <- checkExpr ExpectRegulativeActionContext action actionT
  timestamp' <- checkExpr ExpectRegulativeTimestampContext timestamp number
  pure (MkEvent ann party' action' timestamp', event partyT actionT)

-- | The goal here is to not just infer the type of the named application,
-- but also to determine the order in which the arguments are actually being
-- supplied. This ordering is returned as well (and then stored in the
-- AST after type-checking, to be used by the evaluator).
--
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
checkBranch ec scrutinee tscrutinee tresult (When ann pat e)  = do
  (rpat', extends) <- checkPattern (ExpectPatternScrutineeContext scrutinee) pat tscrutinee
  (rpat, re) <- extendKnownMany extends do
    re' <- checkExpr ec e tresult
    (,)
      -- See Note [Adding type information to all binders]
      <$> (traverse resolvedType =<< nlgPattern rpat')
      <*> nlgExpr re'
  pure $ When ann rpat re
checkBranch ec _scrutinee _tscrutinee tresult (Otherwise ann e) = do
  re <- checkExpr ec e tresult
  Otherwise ann
    -- We have to resolve NLG annotations now because
    -- bound variables are brought into scope.
    -- In the 'Otherwise' case, there are no new variables, but
    -- for consistency, we still resolve the NLG annotations now.
    <$> nlgExpr re

checkPattern :: ExpectationContext -> Pattern Name -> Type' Resolved -> Check (Pattern Resolved, [CheckInfo])
checkPattern ec p t = errorContext (WhileCheckingPattern p) do
  (rp, rt, extend) <- inferPattern p
  expect ec t rt
  pure (rp, extend)

-- Note: PatVar doesn't really get produced by the parser. We replace
-- PatApps that are not in scope with PatVar applications here in the
-- scope and type checker.
inferPattern :: Pattern Name -> Check (Pattern Resolved, Type' Resolved, [CheckInfo])
inferPattern g@(PatVar _ann n)      = errorContext (WhileCheckingPattern g) do
  inferPatternVar n
inferPattern g@(PatApp ann n [])   = errorContext (WhileCheckingPattern g) do
  inferPatternApp ann n [] `orElse` inferPatternVar n
inferPattern g@(PatApp ann n ps)   = errorContext (WhileCheckingPattern g) do
  inferPatternApp ann n ps
inferPattern g@(PatCons ann p1 p2) = errorContext (WhileCheckingPattern g) do
  (rp1, rt1, extend1) <- inferPattern p1
  let listType = list rt1
  (rp2, extend2) <- checkPattern ExpectConsArgument2Context p2 listType

  -- Allows us to hover over the 'FOLLOWED BY',
  -- giving us a type signature.
  patCons <- setAnnResolvedType listType Nothing (PatCons ann rp1 rp2)
  pure (patCons, listType, extend1 <> extend2)
inferPattern g@(PatLit ann lit) = errorContext (WhileCheckingPattern g) do
  ty <- inferLit lit
  resPatLit <- setAnnResolvedType ty Nothing (PatLit ann lit)
  pure (resPatLit, ty, [])

inferPatternVar :: Name -> Check (Pattern Resolved, Type' Resolved, [CheckInfo])
inferPatternVar n = do
  rn <- def n
  rt <- fresh (NormalName "p")
  let
    patVar =
      PatVar
        (mkAnno [mkHoleWithSrcRange rn])
        rn
  pure (patVar, rt, [makeKnown rn (KnownTerm rt Local)])

inferPatternApp :: Anno -> Name -> [Pattern Name] -> Check (Pattern Resolved, Type' Resolved, [CheckInfo])
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
matchPatFunTy :: Resolved -> Type' Resolved -> [Pattern Name] -> Check ([Pattern Resolved], Type' Resolved, [CheckInfo])
matchPatFunTy _r t []   =
  pure ([], t, [])
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
          pure (rargs, rt, concat extends)

        Just t' -> matchPatFunTy r t' args
    Fun _ann onts rt
      -- We know already that the type of the thing we're applying
      -- is a function, good. So we can check the number of arguments
      -- and then check the arguments against their expected result
      -- types.
      | nonts == nargs -> do
        (rargs, extends) <- unzip <$> traverse (\ (j, e, t') -> checkPattern (ExpectAppArgContext False r j) e t') (zip3 [1 ..] args (optionallyNamedTypeType <$> onts))
        pure (rargs, rt, concat extends)

      | otherwise -> do
        addError (IncorrectArgsNumberApp r nonts nargs)
        (rargs, _, extends) <- unzip3 <$> traverse inferPattern args
        pure (rargs, rt, concat extends)
      where
        nonts = length onts
        nargs = length args
    _ -> do
      -- We are trying to apply a non-function.
      addError (IllegalApp r t (length args))
      (rargs, _, extends) <- unzip3 <$> traverse inferPattern args
      pure (rargs, t, concat extends)

-- ----------------------------------------------------------------------------
-- Forward References high level helpers
-- ----------------------------------------------------------------------------

scanDeclares ::
  (a -> Check [DeclTypeSig]) ->
  (a -> Check [DeclChecked DeclareOrAssume]) ->
  a ->
  Check [DeclChecked DeclareOrAssume]
scanDeclares preScanDecl scanDecl a = do
  rdeclareTypeSigs <- preScanDecl a
  withDeclareTypeSigs rdeclareTypeSigs do
    scanDecl a

withScanTypeAndSigEnvironment ::
  (a -> Check [DeclTypeSig]) ->
  (a -> Check [DeclChecked DeclareOrAssume]) ->
  (a -> Check [FunTypeSig]) ->
  a ->
  Check b ->
  Check b
withScanTypeAndSigEnvironment preScanDecls scanDecl scanTySig a act = do
  rdeclares <- scanDeclares preScanDecls scanDecl a
  withDeclares rdeclares do
    rdecides <- scanTySig a
    withDecides rdecides $ do
      act

-- | @'withQualified' rs ce@ takes a list of 'Resolved' names and creates
-- a 'CheckInfo' of @rs@ pointing to the @ce@ 'CheckInfo'.
-- Additionally, we generate qualified names for each @rs@ based on the
-- current stack of 'Section Name's in which the idenfier is defined.
--
-- For example, if we define a function like this:
--
-- @
--   § `Section A` AKA a
--   DEFINE foo ...
-- @
--
-- then @foo@ is defined within the section @`Section A`@ aka @a@.
-- This function would then generate the following names:
--
-- @
-- ["foo", "`Section A`.foo", "a.foo"]
-- @
--
-- All of these can be used to refer to @foo@.
withQualified :: [Resolved] -> CheckEntity -> Check CheckInfo
withQualified rs ce = do
  sects <- asks (.sectionStack)
  case nonEmpty sects of
    Nothing -> pure $ makeKnownMany rs ce
    Just (neSects :: NonEmpty (NonEmpty Text)) -> do
      let
        go :: Resolved -> Check [Resolved]
        go r = do
          let
            n = getName r
          case rawName n of
            NormalName t -> do
              let
                newNames =
                  [ MkName (getAnno n) (QualifiedName qual t)
                  | qual <- toList $ sequence neSects
                  ]
              traverse def newNames
            PreDef _ -> pure []
            QualifiedName _ _ -> pure []

      qualRs <- Extra.concatMapM go rs
      pure $ makeKnownMany (rs <> qualRs) ce

-- ----------------------------------------------------------------------------
-- Phase 1: Scan & Check Type Declarations (DECLARE & ASSUME)
-- ----------------------------------------------------------------------------

inferTyDeclModule :: Module Name -> Check [DeclChecked DeclareOrAssume]
inferTyDeclModule (MkModule _ _ sects) =
  inferTyDeclSection sects

inferTyDeclSection :: Section Name -> Check [DeclChecked DeclareOrAssume]
inferTyDeclSection (MkSection _ _ _ topDecls) =
  concat <$> traverse inferTyDeclTopLevel topDecls

inferTyDeclLocalDecl :: LocalDecl Name -> Check (Maybe (DeclChecked DeclareOrAssume))
inferTyDeclLocalDecl = \ case
  LocalDecide _ _ -> pure Nothing
  LocalAssume _ p -> ((Right <$>) <$>) <$> inferTyDeclAssume p

inferTyDeclTopLevel :: TopDecl Name -> Check [DeclChecked DeclareOrAssume]
inferTyDeclTopLevel = \ case
  Declare   _ p -> maybeToList <$> ((Left <$>) <$>) <$> inferTyDeclDeclare p
  Decide    _ _ -> pure []
  Assume    _ p -> maybeToList <$> ((Right <$>) <$>) <$> inferTyDeclAssume p
  Directive _ _ -> pure []
  Import    _ _ -> pure []
  Section   _ s -> inferTyDeclSection s

inferTyDeclDeclare :: Declare Name -> Check (Maybe (DeclChecked (Declare Resolved)))
inferTyDeclDeclare (MkDeclare ann _tysig appForm t) = prune $
  errorContext (WhileCheckingDeclare (getName appForm)) do
    lookupDeclTypeSigByAnno ann >>= \ declHead -> do
        extendKnownMany declHead.tyVars do
          extendTySynonym <- inferTypeNameAndSynonym declHead.rappForm declHead.typeSynonym
          (rt, extendsTyDecl) <- inferTypeDecl declHead.rappForm t
          -- See Note [Adding type information to all binders]
          -- TODO: if we did this later during typecheck, we would be
          -- able to forward reference functions.
          declare <- MkDeclare ann
            <$> traverse resolvedType declHead.rtysig
            <*> traverse resolvedType declHead.rappForm
            <*> pure rt
            >>= nlgDeclare

          pure $ Just MkDeclChecked
            { payload = declare
            , publicNames = extendTySynonym : extendsTyDecl
            }

inferTyDeclAssume :: Assume Name -> Check (Maybe (DeclChecked (Assume Resolved)))
inferTyDeclAssume (MkAssume ann _tysig appForm (Just (Type tann))) =
  -- declaration of a type
  errorContext (WhileCheckingAssume (getName appForm)) do
    lookupDeclTypeSigByAnno ann >>= \ declHead -> do
        assume <- extendKnownMany (declHead.name:declHead.tyVars) do
          traverse resolvedType (MkAssume ann declHead.rtysig declHead.rappForm (Just (Type tann)))
            >>= nlgAssume
        pure $ Just $ MkDeclChecked
          { payload = assume
          , publicNames = [declHead.name]
          }
inferTyDeclAssume (MkAssume _   _      _        _) = pure Nothing

scanTyDeclModule :: Module Name -> Check [DeclTypeSig]
scanTyDeclModule (MkModule _ _ sects) =
  scanTyDeclSection sects

scanTyDeclSection :: Section Name -> Check [DeclTypeSig]
scanTyDeclSection (MkSection _ _ _ topDecls) =
  concat <$> traverse scanTyDeclTopLevel topDecls

scanTyDeclTopLevel :: TopDecl Name -> Check [DeclTypeSig]
scanTyDeclTopLevel = \ case
  Declare   _ p -> List.singleton <$> scanTyDeclDeclare p
  Decide    _ _ -> pure []
  Assume    _ p -> maybeToList <$> scanTyDeclAssume p
  Directive _ _ -> pure []
  Import    _ _ -> pure []
  Section   _ s -> scanTyDeclSection s

scanTyDeclLocalDecl :: LocalDecl Name -> Check (Maybe DeclTypeSig)
scanTyDeclLocalDecl = \ case
  LocalDecide _ _ -> pure Nothing
  LocalAssume _ p -> scanTyDeclAssume p

scanTyDeclDeclare :: Declare Name -> Check DeclTypeSig
scanTyDeclDeclare (MkDeclare ann tysig appForm decl) = prune $
  errorContext (WhileCheckingDeclare (getName appForm)) do
    (rappForm, rtysig) <- checkTypeAppFormTypeSigConsistency appForm tysig
    extendTyVars <- inferTypeAppForm' rappForm rtysig
    extendTyName <- inferTypeName rappForm decl
    name <- withQualified extendTyName.names extendTyName.checkEntity
    pure $ MkDeclTypeSig
      { anno = ann
      , rtysig
      , typeSynonym = isTypeSynonym decl
      , rappForm
      , tyVars = [extendTyVars]
      , name
      }
  where
    isTypeSynonym = \ case
      SynonymDecl _ ty -> Just ty
      _ -> Nothing

scanTyDeclAssume :: Assume Name -> Check (Maybe DeclTypeSig)
scanTyDeclAssume (MkAssume ann tysig appForm (Just (Type _tann))) = do
  -- declaration of a type
  errorContext (WhileCheckingAssume (getName appForm)) do
    (rappForm, rtysig) <- checkTypeAppFormTypeSigConsistency appForm tysig
    extendTyVars <- inferTypeAppForm' rappForm rtysig
    -- TODO: do we ever check the result kind?
    let
      extendTyName =
        makeKnownMany
          (appFormHeads rappForm)
          (KnownType (kindOfAppForm rappForm)
            (view appFormArgs rappForm)
            Nothing
          )

    name <- withQualified extendTyName.names extendTyName.checkEntity
    pure $ Just $ MkDeclTypeSig
      { anno = ann
      , rtysig
      , typeSynonym = Nothing
      , rappForm = rappForm
      , tyVars = [extendTyVars]
      , name = name
      }
scanTyDeclAssume _ = do
  pure Nothing

-- ----------------------------------------------------------------------------
-- Phase 2: Scan Function Declarations (DECIDE & ASSUME)
-- ----------------------------------------------------------------------------

scanFunSigModule :: Module Name -> Check [FunTypeSig]
scanFunSigModule (MkModule _ _ sects) =
  scanFunSigSection sects

scanFunSigSection :: Section Name -> Check [FunTypeSig]
scanFunSigSection (MkSection _ name maka topDecls) =
  concat <$> traverse (extendSectionStack . scanFunSigTopLevel) topDecls
  where
    extendSectionStack = case name of
      Nothing -> id
      Just n -> pushSection (fmap rawNameToText $ (rawName n) :| maybe [] akaToRawNames maka )

    akaToRawNames :: Aka Name -> [RawName]
    akaToRawNames (MkAka _ ns) = fmap rawName ns

scanFunSigTopLevel :: TopDecl Name -> Check [FunTypeSig]
scanFunSigTopLevel = \ case
  Declare   _ _ -> pure []
  Decide    _ p -> List.singleton <$> scanFunSigDecide p
  Assume    _ p -> maybeToList <$> scanFunSigAssume p
  Directive _ _ -> pure []
  Import    _ _ -> pure []
  Section   _ s -> scanFunSigSection s

scanFunSigLocalDecl :: LocalDecl Name -> Check (Maybe FunTypeSig)
scanFunSigLocalDecl = \ case
  LocalDecide _ p -> Just <$> scanFunSigDecide p
  LocalAssume _ p -> scanFunSigAssume p

scanFunSigDecide :: Decide Name -> Check FunTypeSig
scanFunSigDecide d@(MkDecide _ tysig appForm _) = prune $
  errorContext (WhileCheckingDecide (getName appForm)) do
    (rappForm, rtysig, extendsTySig) <- checkTermAppFormTypeSigConsistency appForm tysig
    (ce, rt, result, extendsAppForm) <- extendKnownMany extendsTySig do
      inferTermAppForm rappForm rtysig
    dty <- setAnnResolvedType rt (Just Computable) d
    name <- withQualified (appFormHeads rappForm) ce
    pure $ MkFunTypeSig
      { anno = getAnno dty
      , rtysig
      , rappForm
      , resultType = result
      , name = name
      , arguments = extendsTySig <> extendsAppForm
      }

scanFunSigAssume :: Assume Name -> Check (Maybe FunTypeSig)
scanFunSigAssume (MkAssume _ _     _       (Just (Type _tann))) = pure Nothing
scanFunSigAssume a@(MkAssume _ tysig appForm mt) = do
  -- declaration of a term
  let tysig' = mergeResultTypeInto tysig mt
  errorContext (WhileCheckingAssume (getName appForm)) do
    (rappForm, rtysig, extendsTySig) <- checkTermAppFormTypeSigConsistency appForm tysig'
    (ce, rt, result, extendsAppForm) <- inferTermAppForm rappForm rtysig
    aty <- setAnnResolvedType rt (Just Assumed) a
    name <- withQualified (appFormHeads rappForm) ce
    pure $ Just $ MkFunTypeSig
      { anno = getAnno aty
      , rtysig
      , rappForm = rappForm
      , resultType = result
      , name
      , arguments = extendsTySig <> extendsAppForm
      }
  where
    -- In the specific case where we have no GIVETH, but a type for the ASSUME itself,
    -- we merge the GIVEN part with the ASSUME type to get a full type signature.
    --
    -- If we don't do that, something like
    --
    -- @
    -- GIVEN a
    -- ASSUME foo IS AN a
    -- @
    --
    -- would go wrong, because the pre-scan would ignore the use of the type variable
    -- and we could no longer get a polymorphic type.
    --
    mergeResultTypeInto :: TypeSig Name -> Maybe (Type' Name) -> TypeSig Name
    mergeResultTypeInto typesig@(MkTypeSig _ (MkGivenSig _ []) Nothing) (Just _) =
      typesig
    mergeResultTypeInto (MkTypeSig ann given Nothing) (Just t) =
      MkTypeSig ann given (Just (MkGivethSig emptyAnno t))
    mergeResultTypeInto typesig _ =
      typesig

-- ----------------------------------------------------------------------------
-- Typecheck Utils
-- ----------------------------------------------------------------------------

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
prettyCheckError (AmbiguousOperatorError opName)                 =
  ["There are multiple valid types for the operator"
  , ""
  , "  " <> opName
  , ""
  , "in this context and I do not have sufficient information to make a choice between them."
  ] -- TODO(mangoiv): maybe we can do better here by providing the types that are available?
    -- I don't see how to make this not ad-hoc, though.
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
prettyTypeMismatch ExpectPercentArgumentContext expected given =
  standardTypeMismatch [ "The argument of '%' is expected to be of type" ] expected given
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
prettyTypeMismatch ExpectRegulativePartyContext expected given =
  standardTypeMismatch [ "The PARTY clause of a regulative rule is expected to be of type" ] expected given
prettyTypeMismatch ExpectRegulativeActionContext expected given =
  standardTypeMismatch [ "The DO clause of a regulative rule is expected to be of type" ] expected given
prettyTypeMismatch ExpectRegulativeDeadlineContext expected given =
  standardTypeMismatch [ "The WITHIN clause of a regulative rule is expected to be of type" ] expected given
prettyTypeMismatch ExpectRegulativeFollowupContext expected given =
  standardTypeMismatch [ "The HENCE clause of a regulative rule is expected to be of type" ] expected given
prettyTypeMismatch ExpectRegulativeContractContext expected given =
  standardTypeMismatch [ "The contract passed to a PROVISION directive is expected to be of type" ] expected given
prettyTypeMismatch ExpectRegulativeTimestampContext expected given =
  standardTypeMismatch [ "The timestamp passed to an event in a PROVISION directive is expected to be of type" ] expected given
prettyTypeMismatch ExpectRegulativeEventContext expected given =
  standardTypeMismatch [ "The event expr passed to a PROVISION directive is expected to be of type" ] expected given
prettyTypeMismatch ExpectRegulativeProvidedContext expected given =
  standardTypeMismatch [ "The PROVIDED clause for filtering the ACTION is expected to be of type" ] expected given

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

-- Note [Adding type information to all binders]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- After typechecking, we add type information to the AST after
-- type checking.
--
-- Similarly to the NLG annotations, we need to be careful to add the
-- additional type information when the respective type information is
-- actually available.
-- For example, when new variables are brought into scope, consider:
--
-- @
--   ...
--   CONSIDER x
--   WHEN y FOLLOWED BY ys THEN y
--   OTHERWISE 0
-- @
--
-- then the 'Resolved' @y@ is only in scope within the 'checkBranch' code,
-- i.e. @THEN y@ part. Thus, we need to make sure to add type information
-- to @y@ after we have typechecked the body of branch.
--
-- The same logic is applied to anything that brings a new binder into
-- scope.
-- As a rule of thumb, whenever we use 'scope' within the typechecker,
-- some extra logic will need to be added.
--
-- Adding the type to the 'Resolved' will allow it to be collected into
-- the 'InfoTree' during 'toInfoTree'.


-- Note [passes]
-- ~~~~~~~~~~~~~
--
-- We need to perform the following passes / traversals:
--
-- 1. Scan for type-level definitions.
--
--    The goal of this pass is to bring names into scope for mutually recursive types. We need this
--    before we can scope- and kind-check type-level definitions.
--
--    Functions in this pass are called 'typeScan*'.
--
--    Kinds are currently *always* explicit. We can observe the kind of any type-level
--    declaration by looking at the declaration: For datatypes, this is obvious (the number of
--    explicit arguments is the kind). For type synonyms, this is less obvious, but we require
--    every type to be fully applied, so for type synonyms, all arguments also have to be
--    explicit.
--
--    Perhaps most interesting are type-level assumes. But once again, we can require that
--    type arguments are explicit, either in the type signature via a GIVEN, or in the appform.
--
--    So this means that we can build a kind environment of all known type-level entities by
--    performing one scan of the entire module. For sections, we build signatures mapping the
--    known names to their kinds and potentially containing sub-signatures.
--
-- 2. Scope and kind-check type-level definitions.
--
--    The goal of this pass is to fully resolve everything that lives on the type level. This is
--    a prerequisite for doing anything on the term-level.
--
--    Functions in this pass are called `kindInfer*` and `kindCheck*`.
--
-- 3. Scan for term-level definitions.
--
--    The goal of this pass is to bring names into scope for mutually recursive bindings. We need this
--    before we can scope- and type-check term-level definitions.
--
--    Functions in this pass are called `scan*`.
--
-- 4. Scope and type-check term-level definitions.
--
--    This also involves expanding type synonyms.
--    Functions in this pass are called `infer*`, `check*` and `match*`.
--
--    The typical types are:
--
--    inferX :: X Name -> Check (X Resolved, Type' Resolved)
--    checkX :: X Name -> Type' Resolved -> Check (X Resolved)
