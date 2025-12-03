{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant <$>" #-}
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
import L4.Names
import L4.Parser.SrcSpan (prettySrcRange, prettySrcRangeM, SrcRange (..), zeroSrcPos)
import L4.Print (prettyLayout, quotedName)
import L4.Syntax
import L4.TypeCheck.Annotation
import L4.TypeCheck.Environment as X
import L4.TypeCheck.Types as X
import L4.TypeCheck.Unify
import L4.TypeCheck.With as X
import qualified L4.Utils.IntervalMap as IV

import Control.Applicative
import Data.Monoid
import Control.Monad.Extra (mapMaybeM)
import qualified Control.Monad.Extra as Extra
import Data.Either (partitionEithers)
import qualified Data.List as List
import Data.Tuple.Extra (firstM)
import Data.List.Split (splitWhen)
import Optics ((%~))
import qualified Base.Set as Set
import Data.Function (on)
import Control.Exception (assert)

mkInitialCheckState :: Substitution -> CheckState
mkInitialCheckState substitution =
  MkCheckState
    { substitution
    , supply       = 0
    , infoMap      = IV.empty
    , nlgMap       = IV.empty
    , scopeMap     = IV.empty
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
    , mixfixRegistry = Map.empty
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
              , scopeMap = s'.scopeMap
              }

checkProgram :: Module Name -> Check (Module Resolved, [CheckInfo])
checkProgram module' = do
  withScanTypeAndSigEnvironment scanTyDeclModule inferTyDeclModule scanFunSigModule module' do
    inferProgram module'

withDecides :: [FunTypeSig] -> Check a -> Check a
withDecides rdecides =
  extendKnownMany topDecides . local \s -> s
    { functionTypeSigs = Map.fromList $ mapMaybe (\d -> (,d) <$> rangeOf d.anno) rdecides
    , mixfixRegistry = buildMixfixRegistry rdecides
    }
  where
    topDecides = fmap (.name) rdecides

-- | Build the mixfix registry from a list of FunTypeSigs.
-- The registry maps from the first keyword of each mixfix function to the list
-- of FunTypeSigs that have that keyword, enabling efficient lookup at call sites.
buildMixfixRegistry :: [FunTypeSig] -> MixfixRegistry
buildMixfixRegistry sigs = Map.fromListWith (++) $ mapMaybe toEntry sigs
  where
    toEntry :: FunTypeSig -> Maybe (RawName, [FunTypeSig])
    toEntry sig = case sig.mixfixInfo of
      Nothing -> Nothing
      Just info -> case info.keywords of
        []    -> Nothing  -- No keywords, shouldn't happen for valid mixfix
        (k:_) -> Just (k, [sig])  -- Index by first keyword

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
-- This is currently used to generate completions.
--
combineEnvironmentEntityInfo :: Environment -> EntityInfo -> Map RawName [CheckEntity]
combineEnvironmentEntityInfo env ei =
  Map.unionsWith catUnq $ foldMap (uncurry lookupUniques) $ Map.toList env
  where
  lookupUniques rn = mapMaybe \unique -> do
    (_, ce) <- ei Map.!? unique
    pure $ Map.singleton rn [ce]
  catUnq a b = nub $ a <> b -- if there are multiple of the same checkEntity, throw them out

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
    _ -> error "internal error: expected unique result, got several"

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
inferAssume (MkAssume ann _tysig appForm (Just (Type _tann)) typically) = do
  -- declaration of a type
  errorContext (WhileCheckingAssume (getName appForm)) do
    addWarning $ AssumeDeprecated (getName appForm)
    case typically of
      Just _ -> addError $ TypicallyNotAllowedOnAssume (getName appForm)
      Nothing -> pure ()
    lookupAssumeCheckedByAnno ann >>= \ d -> pure (d.payload, d.publicNames)
inferAssume (MkAssume ann _tysig appForm mt typically) = do
  -- declaration of a term
  errorContext (WhileCheckingAssume (getName appForm)) do
    addWarning $ AssumeDeprecated (getName appForm)
    case typically of
      Just _ -> addError $ TypicallyNotAllowedOnAssume (getName appForm)
      Nothing -> pure ()
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
              <*> pure Nothing
              >>= nlgAssume
          pure (assume, [dHead.name])

inferDirective :: Directive Name -> Check (Directive Resolved)
inferDirective (LazyEval ann e) = errorContext (WhileCheckingExpression e) do
  (re, _) <- prune $ inferExpr e
  pure (LazyEval ann re)
inferDirective (LazyEvalTrace ann e) = errorContext (WhileCheckingExpression e) do
  (re, _) <- prune $ inferExpr e
  pure (LazyEvalTrace ann re)
inferDirective (PresumptiveEval ann e) = errorContext (WhileCheckingExpression e) do
  (re, _) <- prune $ inferExpr e
  pure (PresumptiveEval ann re)
inferDirective (PresumptiveEvalTrace ann e) = errorContext (WhileCheckingExpression e) do
  (re, _) <- prune $ inferExpr e
  pure (PresumptiveEvalTrace ann re)
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
inferDirective (Assert ann e) = errorContext (WhileCheckingExpression e) do
  e' <- checkExpr ExpectAssertContext e boolean
  pure (Assert ann e')
inferDirective (PresumptiveAssert ann e) = errorContext (WhileCheckingExpression e) do
  e' <- checkExpr ExpectAssertContext e boolean
  pure (PresumptiveAssert ann e')

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

  -- First pass: Type-check all DECIDEs to get their resolved names
  (rtopdecls, topDeclExtends) <- unzip <$> traverse inferTopDecl topdecls

  -- Second pass: Generate presumptive wrappers and get mapping
  wrapperDeclsWithMapping <- generatePresumptiveWrappersWithMapping rtopdecls

  let wrapperDecls = map snd wrapperDeclsWithMapping
      -- Build mapping from original function name text to wrapper Resolved
      wrapperPairs = [(getNameText origName, wrapperName)
                     | (origName, Decide _ (MkDecide _ _ (MkAppForm _ wrapperName _ _) _)) <- wrapperDeclsWithMapping]
      wrapperNameMap = trace ("WRAPPER MAP: " ++ show [(k, getNameText v) | (k, v) <- wrapperPairs]) $
                       Map.fromList wrapperPairs

  -- Create CheckInfo for each wrapper with proper function type
  let wrapperExtends = [makeWrapperCheckInfo wrapperName givenSig mGivethSig
                       | Decide _ (MkDecide _ (MkTypeSig _ givenSig mGivethSig) (MkAppForm _ wrapperName _ _) _) <- wrapperDecls]
      -- Force evaluation of each CheckInfo by pattern matching and accessing fields
      forceCheckInfo ci@(MkCheckInfo {names = ns}) = length ns `seq` ci
      forceList [] = []
      forceList (x:xs) = forceCheckInfo x `seq` (x : forceList xs)
      -- Force full evaluation of wrapperExtends before entering pass 3
      !wrapperExtends' = trace ("FORCING WRAPPER EXTENDS: " ++ show (length wrapperExtends)) $ forceList wrapperExtends

  -- Third pass: Add wrappers to environment, then rewrite and recheck ONLY directives
  --  For non-directives, reuse the results from pass 1 to avoid re-registering them
  dirRewritten <- trace "PASS 3: Entering extended environment with wrappers" $
    extendKnownMany wrapperExtends' $
    forM (zip topdecls (zip rtopdecls topDeclExtends)) $ \(nameDecl, (resolvedDecl, extends)) ->
      case nameDecl of
        Directive dirAnn directive ->
          case directive of
            PresumptiveEval _ expr -> trace "PASS 3: Rewriting PEVAL directive" $ do
              -- Rewrite and recheck PEVAL directives
              let exprRewritten = trace ("REWRITTEN EXPR: " ++ take 200 (show (rewriteExprForPEvalName wrapperNameMap expr))) $
                                  rewriteExprForPEvalName wrapperNameMap expr
              rdirective <- trace "PASS 3: Type-checking rewritten PEVAL" $ inferDirective (PresumptiveEval dirAnn exprRewritten)
              pure (Directive dirAnn rdirective, [])  -- No new CheckInfo from directives
            _ -> do
              -- Other directives: just use resolved version from pass 1
              pure (resolvedDecl, extends)
        _ ->
          -- Non-directives: use resolved version from pass 1
          pure (resolvedDecl, extends)
  let (rtopdeclsFinal, topDeclExtendsFinal) = unzip dirRewritten

  pure (MkSection ann rmn rmaka (rtopdeclsFinal ++ wrapperDecls), concat topDeclExtends ++ concat topDeclExtendsFinal ++ wrapperExtends')

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
    (MkTypeSig tann (MkGivenSig gann ((\ n -> MkOptionallyTypedName emptyAnno n Nothing Nothing) <$> ns)) mgiveth)
checkTermAppFormTypeSigConsistency (MkAppForm aann n [] maka) tysig@(MkTypeSig _ (MkGivenSig _ otns) _) =
  checkTermAppFormTypeSigConsistency'
    (MkAppForm aann n (clearSourceAnno . getName <$> filter isTerm otns) maka)
    tysig
-- NEW: Handle mixfix patterns where the AppForm contains GIVEN parameters
-- mixed with keywords.
--
-- Case 1: Head is a GIVEN parameter
--   Example: GIVEN a, b; a `plus` b MEANS ...
--   AppForm is [a, plus, b] - restructure to function name: `plus`, args: [a, b]
--
-- Case 2: Head is a keyword, but args contain GIVEN params mixed with keywords
--   Example: GIVEN cond, thenVal, elseVal; `myif` cond `mythen` thenVal `myelse` elseVal MEANS ...
--   AppForm is [myif, cond, mythen, thenVal, myelse, elseVal]
--   Keep head as function name, filter args to only GIVEN params: [cond, thenVal, elseVal]
--
checkTermAppFormTypeSigConsistency appForm@(MkAppForm _ headName args maka) tysig@(MkTypeSig _ (MkGivenSig _ otns) _)
  | isMixfixPatternHeadIsParam appForm tysig =
      -- Case 1: Head is a GIVEN parameter - need to find the real function name
      let
        givenNames = Set.fromList $ map (rawName . getName) (filter isTerm otns)
        allTokens = headName : args

        -- Find the first keyword (non-GIVEN name) to be the function name
        keywords = filter (\n -> not (rawName n `Set.member` givenNames)) allTokens
        params = filter (\n -> rawName n `Set.member` givenNames) allTokens

        -- The function name is the first keyword
        funcName = case keywords of
          []    -> headName -- fallback, shouldn't happen for valid mixfix
          (k:_) -> k

        -- Restructure: function name as head, params as args
        restructuredAppForm = MkAppForm (getAnno appForm) funcName params maka
      in
        checkTermAppFormTypeSigConsistency' restructuredAppForm tysig
  | isMixfixPatternHeadIsKeyword appForm tysig =
      -- Case 2: Head is a keyword - keep head, but filter args to only GIVEN params
      let
        givenNames = Set.fromList $ map (rawName . getName) (filter isTerm otns)
        params = filter (\n -> rawName n `Set.member` givenNames) args

        -- Keep head as function name, filter args to only params
        restructuredAppForm = MkAppForm (getAnno appForm) headName params maka
      in
        checkTermAppFormTypeSigConsistency' restructuredAppForm tysig
checkTermAppFormTypeSigConsistency appForm tysig =
  checkTermAppFormTypeSigConsistency' appForm tysig

-- | Check if an AppForm represents a mixfix pattern where the head is a GIVEN parameter.
isMixfixPatternHeadIsParam :: AppForm Name -> TypeSig Name -> Bool
isMixfixPatternHeadIsParam (MkAppForm _ headName _ _) (MkTypeSig _ (MkGivenSig _ otns) _) =
  let givenNames = Set.fromList $ map (rawName . getName) (filter isTerm otns)
  in rawName headName `Set.member` givenNames

-- | Check if an AppForm represents a mixfix pattern where the head is a keyword
-- but args contain a mix of GIVEN params and keywords.
isMixfixPatternHeadIsKeyword :: AppForm Name -> TypeSig Name -> Bool
isMixfixPatternHeadIsKeyword (MkAppForm _ headName args _) (MkTypeSig _ (MkGivenSig _ otns) _) =
  let
    givenNames = Set.fromList $ map (rawName . getName) (filter isTerm otns)
    headIsKeyword = not (rawName headName `Set.member` givenNames)
    -- Check if args contain both GIVEN params and non-GIVEN keywords
    hasParams = any (\n -> rawName n `Set.member` givenNames) args
    hasKeywords = any (\n -> not (rawName n `Set.member` givenNames)) args
  in
    headIsKeyword && hasParams && hasKeywords

isTerm :: OptionallyTypedName Name -> Bool
isTerm (MkOptionallyTypedName _ _ (Just (Type _)) _) = False
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
    (MkTypeSig tann (MkGivenSig gann ((\ n -> MkOptionallyTypedName emptyAnno n (Just (Type emptyAnno)) Nothing) <$> ns)) mgiveth)
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
ensureNameConsistency ns (MkOptionallyTypedName ann n (Just (Type tann)) _ : otns) = do
  rn <- def n
  extendKnown (makeKnown rn KnownTypeVariable) do
    (rns, rotns, extends) <- ensureNameConsistency ns otns
    pure (rns, MkOptionallyTypedName ann rn (Just (Type tann)) Nothing : rotns, makeKnown rn KnownTypeVariable : extends)
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
ensureNameConsistency [] (MkOptionallyTypedName ann n mt _ : otns) = do
  addError (InconsistentNameInSignature n Nothing)
  rn <- def n
  rmt <- traverse inferType mt
  (_, rotns, extends) <- ensureNameConsistency [] otns
  pure ([], MkOptionallyTypedName ann rn rmt Nothing : rotns, extends)

-- | Checks that the names are consistent, and resolve the 'OptionallyTypedName's.
--
-- Note that the list of 'OptionallyTypedName's can in principle contain both
-- type variables and term variables, but term variables are not permitted here.
--
ensureTypeNameConsistency :: [Name] -> [OptionallyTypedName Name] -> Check ([Resolved], [OptionallyTypedName Resolved], [CheckInfo])
ensureTypeNameConsistency [] [] = pure ([], [], [])
ensureTypeNameConsistency (n : ns) (MkOptionallyTypedName ann n' (Just (Type tann)) _ : otns)
  | rawName n == rawName n' = do
  rn <- def n
  rn' <- ref n' rn
  (rns, rotns, extends) <- ensureTypeNameConsistency ns otns
  pure (rn : rns, MkOptionallyTypedName ann rn' (Just (Type tann)) Nothing : rotns, extends)
ensureTypeNameConsistency (n : ns) (MkOptionallyTypedName ann n' Nothing _ : otns)
  | rawName n == rawName n' = do
  rn <- def n
  rn' <- ref n' rn
  (rns, rotns, extends) <- ensureTypeNameConsistency ns otns
  pure (rn : rns, MkOptionallyTypedName ann rn' Nothing Nothing : rotns, extends)
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
ensureTypeNameConsistency [] (MkOptionallyTypedName ann n mt _ : otns) = do
  addError (InconsistentNameInSignature n Nothing)
  rn <- def n
  rmt <- traverse inferType mt
  (_, rotns, extends) <- ensureNameConsistency [] otns
  pure ([], MkOptionallyTypedName ann rn rmt Nothing : rotns, extends)

mkref :: Resolved -> OptionallyTypedName Name -> Check (OptionallyTypedName Resolved)
mkref r (MkOptionallyTypedName ann n mt typically) = do
  rn <- ref n r
  rmt <- traverse inferType mt
  rTypically <- case (typically, rmt) of
    (Just expr, Just ty) -> Just <$> checkExpr (ExpectTypicallyValueContext n) expr ty
    _ -> pure Nothing
  pure (MkOptionallyTypedName ann rn rmt rTypically)

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
typedNameOptionallyNamedType (MkTypedName _ n t _) = MkOptionallyNamedType emptyAnno (Just n) t

inferSelector :: AppForm Resolved -> TypedName Name -> Check (TypedName Resolved, [CheckInfo])
inferSelector rappForm (MkTypedName ann n t typically) = do
  rt <- inferType t
  dn <- def n
  -- Check TYPICALLY value if present
  rTypically <- case typically of
    Nothing -> pure Nothing
    Just expr -> Just <$> checkExpr (ExpectTypicallyValueContext n) expr rt
  let selectorInfo = KnownTerm (forall' (view appFormArgs rappForm) (fun_ [appFormType rappForm] rt)) Selector
  pure (MkTypedName ann dn rt rTypically, [makeKnown dn selectorInfo])

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
    inferOptionallyTypedName (MkOptionallyTypedName ann' n Nothing typically) = do
      rn <- def n
      v <- fresh (rawName n)
      -- Check TYPICALLY value if present
      rTypically <- case typically of
        Nothing -> pure Nothing
        Just expr -> Just <$> checkExpr (ExpectTypicallyValueContext n) expr v
      pure (MkOptionallyTypedName ann' rn (Just v) rTypically, v, [makeKnown rn (KnownTerm v Local)])
    inferOptionallyTypedName (MkOptionallyTypedName ann' n (Just t) typically) = do
      rn <- def n
      rt <- inferType t
      -- Check TYPICALLY value if present
      rTypically <- case typically of
        Nothing -> pure Nothing
        Just expr -> Just <$> checkExpr (ExpectTypicallyValueContext n) expr rt
      pure (MkOptionallyTypedName ann' rn (Just rt) rTypically, rt, [makeKnown rn (KnownTerm rt Local)])

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
isQuantifier (MkOptionallyTypedName _ n (Just (Type _)) _) = Left n
isQuantifier (MkOptionallyTypedName _ n mt             _) = Right (n, mt)

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
checkExpr ec c@(Consider ann e branches) t = softprune $ errorContext (WhileCheckingExpression c) $ do
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
  -- Store the expected type in the annotation so it's available during evaluation
  setAnnResolvedType t Nothing re

checkIfThenElse :: ExpectationContext -> Anno -> Expr Name -> Expr Name -> Expr Name -> Type' Resolved -> Check (Expr Resolved)
checkIfThenElse ec ann e1 e2 e3 t = do
  e1' <- checkExpr ExpectIfConditionContext e1 boolean
  e2' <- checkExpr ec e2 t
  e3' <- checkExpr ec e3 t
  pure (IfThenElse ann e1' e2' e3')

checkMultiWayIf :: Anno -> [GuardedExpr Name] -> Expr Name -> Type' Resolved -> Check (Expr Resolved)
checkMultiWayIf ann es e t = do
  es' <- for es \(MkGuardedExpr ann' c f) -> do
    c' <- checkExpr ExpectIfConditionContext c boolean
    f' <- checkExpr ExpectIfBranchesContext f t
    pure $ MkGuardedExpr ann' c' f'
  e' <- checkExpr ExpectIfBranchesContext e t
  pure (MultiWayIf ann es' e')

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

buildConstructorLookup :: [DeclChecked (Declare Resolved)] -> Map Unique [Resolved]
buildConstructorLookup = foldMap \decl ->
  let MkDeclare _ _ (MkAppForm _ tr _ _) td = decl.payload
   in Map.singleton (getUnique tr) case td of
    RecordDecl _ mc _ -> [fromMaybe tr mc] -- if the constructor name is 'Nothing', that means it's identical to the type name
    EnumDecl _ cds -> map (\(MkConDecl _ n _) -> n) cds
    SynonymDecl _ _ -> [] -- TODO: how to look up synonyms?

checkConsider :: ExpectationContext -> Anno -> Expr Name -> [Branch Name] -> Type' Resolved -> Check (Expr Resolved)
checkConsider ec ann e branches t = do
  (re, te) <- inferExpr e
  rbranches <- traverse (checkBranch ec re te t) branches
  resolvedDecls <- asks (Map.elems . (.declareDeclarations))
  (scrutVar, pt) <- desugarBranches re rbranches
  let cl = buildConstructorLookup resolvedDecls
      bs = concretizeInfo cl pt

  let redundant = redundantBranches $ annotateRefinement bs
      missing = nubBy ((==) `on` fmap getUnique) $ expandToPattern scrutVar $ normalizeRefinement $ uncoverRefinement bs

  unless (null missing) do
    addWarning $ PatternMatchesMissing missing
  unless (null redundant) do
    addWarning $ PatternMatchRedundant redundant



  pure (Consider ann re rbranches)

inferExpr :: Expr Name -> Check (Expr Resolved, Type' Resolved)
inferExpr g = softprune $ errorContext (WhileCheckingExpression g) do
  (re, te) <- inferExpr' g
  re' <- setAnnResolvedType te Nothing re
  pure (re', te)

inferExpr' :: Expr Name -> Check (Expr Resolved, Type' Resolved)
inferExpr' g =
  case g of
    And ann e1 e2 -> do
      dsFun <- desugarBinOpToFunction (rawName andName) g ann e1 e2
      inferExpr' dsFun
    Or ann e1 e2 -> do
      dsFun <- desugarBinOpToFunction (rawName orName) g ann e1 e2
      inferExpr' dsFun
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
    Implies ann e1 e2 -> do
      dsFun <- desugarBinOpToFunction (rawName impliesName) g ann e1 e2
      inferExpr' dsFun
    Equals ann e1 e2 -> do
      dsFun <- desugarBinOpToFunction (rawName equalsName) g ann e1 e2
      inferExpr' dsFun
    Leq ann e1 e2 -> do
      dsFun <- desugarBinOpToFunction (rawName leqName) g ann e1 e2
      inferExpr' dsFun
    Geq ann e1 e2 -> do
      dsFun <- desugarBinOpToFunction (rawName geqName) g ann e1 e2
      inferExpr' dsFun
    Lt ann e1 e2 -> do
      dsFun <- desugarBinOpToFunction (rawName ltName) g ann e1 e2
      inferExpr' dsFun
    Gt ann e1 e2 -> do
      dsFun <- desugarBinOpToFunction (rawName gtName) g ann e1 e2
      inferExpr' dsFun
    Not ann e -> do
      dsFun <- desugarUnaryOpToFunction (rawName notName) g ann e
      inferExpr' dsFun
    Plus ann e1 e2 -> do
      dsFun <- desugarBinOpToFunction (rawName plusName) g ann e1 e2
      inferExpr' dsFun
    Minus ann e1 e2 -> do
      dsFun <- desugarBinOpToFunction (rawName minusName) g ann e1 e2
      inferExpr' dsFun
    Times ann e1 e2 -> do
      dsFun <- desugarBinOpToFunction (rawName timesName) g ann e1 e2
      inferExpr' dsFun
    DividedBy ann e1 e2 -> do
      dsFun <- desugarBinOpToFunction (rawName divideName) g ann e1 e2
      inferExpr' dsFun
    Modulo ann e1 e2 -> do
      dsFun <- desugarBinOpToFunction (rawName moduloName) g ann e1 e2
      inferExpr' dsFun
    Exponent ann e1 e2 -> do
      dsFun <- desugarBinOpToFunction (rawName exponentName) g ann e1 e2
      inferExpr' dsFun
    Cons ann e1 e2 -> do
      dsFun <- desugarBinOpToFunction (rawName consName) g ann e1 e2
      inferExpr' dsFun
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

      -- First, try to match this as a mixfix call-site pattern.
      -- If the function is a registered mixfix and the args contain the expected
      -- keywords, restructure the args to remove the keywords.
      -- For param-first patterns, this also returns the correct function name.
      mMixfixMatch <- tryMatchMixfixCall n es
      let (actualFuncName, actualArgs, needsAnnoRebuild, mMixfixError) = case mMixfixMatch of
            Nothing -> (n, es, False, Nothing)  -- Not a mixfix call, use original
            Just (funcRawName, restructuredArgs, mErr) ->
              -- Create a Name with the correct function name, preserving the annotation
              let MkName nameAnno _ = n
                  newName = MkName nameAnno funcRawName
                  -- We need to rebuild annotation if args were restructured
                  -- (i.e., keyword placeholders were removed)
                  argsChanged = length restructuredArgs /= length es
              in (newName, restructuredArgs, argsChanged, mErr)

      -- Report any mixfix matching errors (e.g., wrong keyword, typo)
      case mMixfixError of
        Just err -> addError (MixfixMatchErrorCheck n err)
        Nothing -> pure ()

      -- 1.
      (rn, pt) <- resolveTerm actualFuncName
      t <- instantiate pt

      -- 2. - 5.
      -- Note that if there are no arguments, then matchFunTy does not
      -- actually insist on the type t being a function.
      (res, rt) <- matchFunTy False rn t actualArgs

      -- If mixfix args were restructured, rebuild annotation with correct holes
      let finalAnn = if needsAnnoRebuild
                     then rebuildMixfixAppAnno ann actualFuncName actualArgs
                     else ann

      pure (App finalAnn rn res, rt)
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
    MultiWayIf ann es e -> do
      v <- fresh (NormalName "multiwayif")
      re <- checkMultiWayIf ann es e v
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
    Fetch ann e -> do
      dsFun <- desugarUnaryOpToFunction (rawName fetchName) g ann e
      inferExpr' dsFun
    Env ann e -> do
      dsFun <- desugarUnaryOpToFunction (rawName envName) g ann e
      inferExpr' dsFun
    Post ann e1 e2 e3 -> do
      e1' <- checkExpr ExpectPostUrlContext e1 string
      e2' <- checkExpr ExpectPostHeadersContext e2 string
      e3' <- checkExpr ExpectPostBodyContext e3 string
      pure (Post ann e1' e2' e3', string)
    Concat ann es -> do
      res <- traverse (\ e -> checkExpr ExpectConcatArgumentContext e string) es
      pure (Concat ann res, string)
    AsString ann e -> do
      -- AsString can accept any primitive type and convert it to string
      (re, te) <- inferExpr e
      -- For now, we'll only allow NUMBER to be converted to STRING
      -- Could extend this to other types in the future
      expect ExpectAsStringArgumentContext number te
      pure (AsString ann re, string)

inferEvent :: Event Name -> Check (Event Resolved, Type' Resolved)
inferEvent (MkEvent ann party action timestamp atFirst) = do
  partyT <- fresh (NormalName "party")
  actionT <- fresh (NormalName "action")
  party' <- checkExpr ExpectRegulativePartyContext party partyT
  action' <- checkExpr ExpectRegulativeActionContext action actionT
  timestamp' <- checkExpr ExpectRegulativeTimestampContext timestamp number
  pure (MkEvent ann party' action' timestamp' atFirst, event partyT actionT)

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
checkBranch ec scrutinee tscrutinee tresult (MkBranch ann' (When ann pat) e)  = do
  (rpat', extends) <- checkPattern (ExpectPatternScrutineeContext scrutinee) pat tscrutinee
  (rpat, re) <- extendKnownMany extends do
    re' <- checkExpr ec e tresult
    (,)
      -- See Note [Adding type information to all binders]
      <$> (traverse resolvedType =<< nlgPattern rpat')
      <*> nlgExpr re'
  pure $ MkBranch ann' (When ann rpat) re
checkBranch ec _scrutinee _tscrutinee tresult (MkBranch ann' (Otherwise ann) e) = do
  re <- checkExpr ec e tresult
  MkBranch ann' (Otherwise ann)
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
inferPattern g@(PatExpr ann expr) = errorContext (WhileCheckingPattern g) do
  (rexpr, ty) <- inferExpr expr
  resPatExpr <- setAnnResolvedType ty Nothing (PatExpr ann rexpr)
  pure (resPatExpr, ty, [])
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

  ann' <- setAnnResolvedType rt (Just Constructor) ann

  pure (PatApp ann' rn rps, rt, extend)

-- | pattern tree, vaguely adapted from lower your guards
data PatTree' i n
  = PatOr (PatTree' i n) (PatTree' i n)
  -- ^ choice between two pattern trees
  | PatLeaf (Branch n)
  -- ^ select RHS
  | PatAnd (Guard i n) (PatTree' i n)
  -- ^ a constructor name that must be matched
  | PatNoBranches
  deriving stock (Eq, Show, Generic, Functor)

-- | simplified guard type
data Guard i n
  = MkGuard
  { info :: i
  , binding :: n
  , constructor :: n
  , constructorArgs :: [n] }
  deriving stock (Eq, Show, Generic, Functor)

type PatTree = PatTree' Info

desugarBranches :: Expr Resolved -> [Branch Resolved] -> Check (Resolved, PatTree Resolved)
desugarBranches scrut nebs = do
  v <- case scrut of
    Var _ x -> pure x
    _ -> newPatName
  res <- case nebs of
    [] -> pure PatNoBranches
    _ -> flip evalStateT mempty $ foldr1 PatOr <$> traverse (desugarBranch v) nebs
  pure (v, res)
 where
  desugarBranch :: Resolved -> Branch Resolved -> StateT VarEnv Check (PatTree Resolved)
  desugarBranch v  = \ case
    b@(MkBranch _ (When _ p) _) -> do
      foldr PatAnd (PatLeaf b) <$> desugarPat v p
    b@(MkBranch _ (Otherwise {}) _) -> pure (PatLeaf b)

  newPatName = do
    unq <- newUnique
    let rn = NormalName "patvar"
        n = MkName emptyAnno rn
    pure $ Def unq n

  -- NOTE: if there's no scrutinee, we create a new variable that is then putas the scrutinee.
  -- So: WHEN Foo Bar THEN expr essentially becomes WHEN Foo bar THEN CONSIDER bar WHEN Bar THEN expr

  desugarPat :: Resolved -> Pattern Resolved -> StateT VarEnv Check [Guard Info Resolved]
  desugarPat scrut' = \ case
    PatApp ((.extra.resolvedInfo) -> Just info) c ps ->
      Map.lookup (getUnique c) <$> get >>= \ case
        Just existingVars -> do
          pats <- getAp $ flip foldMap (zip existingVars ps) \(var, p) -> Ap do
            desugarPat var p

          pure (MkGuard info scrut' c existingVars : pats)

        Nothing -> do
          (vs, pats) <- getAp $ flip foldMap ps \p -> Ap do
            n <- lift newPatName
            guards <- desugarPat n p
            pure ([n], guards)

          modify' (Map.insert (getUnique c) vs)
          pure (MkGuard info scrut' c vs : pats)

    -- NOTE: this second case is very similar to the one for PatApp, because Cons in general is basically a PatApp
    PatCons ((.extra.resolvedInfo) -> Just info) ph pt ->
      Map.lookup consUnique <$> get >>= \ case
        Just [nh, nt] -> do
          ph' <- desugarPat nh ph
          pt' <- desugarPat nt pt
          pure (MkGuard info scrut' consRef [nh, nt] : ph' <> pt')

        _p -> assert (isNothing _p) do
          nh <- lift newPatName
          nt <- lift newPatName
          ph' <- desugarPat nh ph
          pt' <- desugarPat nt pt
          modify' (Map.insert consUnique [nh, nt])
          pure (MkGuard info scrut' consRef [nh, nt] : ph' <> pt')

    PatVar _ _ -> pure []
    PatExpr _ _ -> pure []
    PatLit _ _ -> pure []
    _ -> error "fatal internal error: expected type information but didn't get any"

type VarEnv = Map Unique [Resolved]

-- | replace 'Info' with the names of the constructors that the type has
concretizeInfo :: Map Unique [n] ->  PatTree n -> PatTree' [n] n
concretizeInfo cmap = \case
 PatOr t1 t2 -> PatOr (concretizeInfo cmap t1) (concretizeInfo cmap t2)
 PatLeaf e -> PatLeaf e
 PatNoBranches -> PatNoBranches
 PatAnd (MkGuard i b n ns) t -> case i of
   TypeInfo ty _
     | TyApp _ r _ <- ty
     , Just cs <- Map.lookup (getUnique r) cmap
     -> PatAnd (MkGuard cs b n ns) (concretizeInfo cmap t)
   _ -> PatAnd (MkGuard [] b n ns) (concretizeInfo cmap t)

data Refinement n
  = RefineConj (Refinement n) (Constr n)
  | RefineDisj (Refinement n) (Refinement n)
  | RefineTop
  | RefineBottom
  deriving stock (Eq, Show, Generic, Functor)

-- a constraint
data Constr n
  = IsEq n n [n]
  -- ^ in that order:
  -- - the expression that is scrutinized (most often a Var)
  -- - the constructor
  -- - the variables that are scrutinized when scrutinizing this constructor's arguments
  | IsNotEq n n [n] [n]
  -- ^ in that order:
  -- - the expression that is scrutinized (most often a Var)
  -- - the constructor
  -- - the variables that are scrutinized when scrutinizing this constructor's arguments
  -- - the other constructors of the type that this constraint is about
  deriving stock (Eq, Ord, Show, Generic, Functor, Foldable)

-- this is suposed to generate the "unconvered" set, i.e. the
-- values that are not covered by the pattern tree
uncoverRefinement :: PatTree' [n] n -> Refinement n
uncoverRefinement = uncoverRefinementWith RefineTop

uncoverRefinementWith :: Refinement n -> PatTree' [n] n -> Refinement n
uncoverRefinementWith tau = \case
  PatLeaf _e -> RefineBottom
  PatOr t1 t2 -> uncoverRefinementWith (uncoverRefinementWith tau t1) t2
  PatAnd (MkGuard cs b n ns) t -> (tau `RefineConj` IsNotEq b n ns cs) `RefineDisj`
    uncoverRefinementWith (tau `RefineConj` IsEq b n ns) t
  PatNoBranches -> tau

data AnnBranch n
  = AnnLeaf (Refinement n) (Branch n)
  | AnnEmpty
  deriving stock (Eq, Show, Generic)

redundantBranches :: [AnnBranch Resolved] -> [Branch Resolved]
redundantBranches = mapMaybe \case
  AnnLeaf r b | not (isConsistent r) -> Just b
  _ -> Nothing
 where
  isConsistent :: Refinement Resolved -> Bool
  isConsistent = go [] where
    go seen = \ case
      RefineConj r c -> go (c : seen) r && all (isConsistentWith c) seen
      RefineDisj r1 r2 -> go seen r1 || go seen r2
      RefineTop -> True
      RefineBottom -> False

-- this is supposed to generate the patterns that *are* matched
annotateRefinement :: PatTree' [n] n -> [AnnBranch n]
annotateRefinement = go RefineTop
 where
  go tau = \case
    PatLeaf e -> [AnnLeaf tau e]
    PatOr t1 t2 -> go tau t1 <> go (uncoverRefinementWith tau t1) t2
    PatAnd (MkGuard _i b n ns) t -> go (tau `RefineConj` IsEq b n ns) t
    PatNoBranches -> [AnnEmpty]

data Nabla n
 = Bottom
 | Consistent !(Set (Constr n))
 deriving stock (Foldable)

instance Ord n => Semigroup (Nabla n) where
  Bottom <> s = s
  s <> Bottom = s
  Consistent s1 <> Consistent s2 = Consistent $ s1 `Set.union` s2

instance Ord n => Monoid (Nabla n) where
  mempty = Bottom

lookupConstraints :: Eq n => n -> Nabla n -> [Constr n]
lookupConstraints n = \ case
  Bottom -> []
  Consistent s -> mapMaybe (\c -> if constraintName c == n then Just c else Nothing) $ Set.toList s

constraintName :: Constr n -> n
constraintName = \ case
  IsEq n _ _ -> n
  IsNotEq n _ _ _ -> n

normalizeRefinement :: Refinement Resolved -> Nabla Resolved
normalizeRefinement = go (Consistent mempty)
 where
  go nabla = \ case
   RefineConj r c -> go (addConsistentConstraint nabla c) r
   RefineDisj r1 r2 -> go nabla r1 <> go nabla r2
   RefineTop -> nabla
   RefineBottom -> Bottom

  addConsistentConstraint :: Nabla Resolved -> Constr Resolved -> Nabla Resolved
  addConsistentConstraint nabla c = case lookupConstraints (constraintName c) nabla of
    [] -> insertConstraint c nabla
    cs -> if all (c `isConsistentWith`) cs then insertConstraint c nabla else nabla

  insertConstraint c = \ case
    Bottom -> Consistent (Set.singleton c)
    Consistent s -> Consistent (Set.insert c s)

isConsistentWith :: Constr Resolved -> Constr Resolved -> Bool
IsEq n1 c1 _ `isConsistentWith` IsEq n2 c2 _ | n1 `sameResolved` n2 = c1 `sameResolved` c2
IsNotEq n1 c1 _ _ `isConsistentWith` IsEq n2 c2 _ | n1 `sameResolved` n2 = not $ c1 `sameResolved` c2
IsEq n1 c1 _ `isConsistentWith` IsNotEq n2 c2 _ _ | n1 `sameResolved` n2 = not $ c1 `sameResolved` c2
_ `isConsistentWith` _ = True

data ConsistentSet n
  = EqCon n [n] (ConsistentSet n)
  | NotEqCons [n] [n]
  | NoInfo

expandToPattern :: Resolved -> Nabla Resolved -> [BranchLhs Resolved]
expandToPattern scrut = \ case
  Bottom -> []
  n@Consistent {} -> map patternToBranch (go scrut n)
 where
  patternToBranch pat
    | PatVar _ var <- pat, var `sameResolved` underscoreRef
    = Otherwise emptyAnno
    | otherwise = When emptyAnno pat

  toConsistentSet :: [Constr Resolved] -> ConsistentSet Resolved
  toConsistentSet [] = NoInfo
  toConsistentSet (IsEq _ c ns : rest) = EqCon c ns (toConsistentSet rest)
  toConsistentSet (IsNotEq _ c ns cs : rest) = case toConsistentSet rest of
    NotEqCons _ ncs -> NotEqCons ns (delByUnq c ncs)
    NoInfo -> NotEqCons ns (delByUnq c cs)
    e@EqCon {} -> e

  delByUnq = deleteBy sameResolved

  go :: Resolved -> Nabla Resolved -> [Pattern Resolved]
  go scrutVar nabla = go' cnstrs
   where
    cnstrs = toConsistentSet $ lookupConstraints scrutVar nabla
    go' = \ case
      EqCon c ns more -> map (PatApp emptyAnno c) (traverse (`go` nabla) ns) <> go' more
      -- TODO: add underscores here, instead of []
      NotEqCons _ns ncs -> map (\n' -> PatApp emptyAnno n' [] ) ncs
      NoInfo -> [PatVar emptyAnno underscoreRef]

sameResolved :: Resolved -> Resolved -> Bool
sameResolved = (==) `on` getUnique

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
inferTyDeclAssume (MkAssume ann _tysig appForm (Just (Type tann)) _) =
  -- declaration of a type
  errorContext (WhileCheckingAssume (getName appForm)) do
    lookupDeclTypeSigByAnno ann >>= \ declHead -> do
        assume <- extendKnownMany (declHead.name:declHead.tyVars) do
          traverse resolvedType (MkAssume ann declHead.rtysig declHead.rappForm (Just (Type tann)) Nothing)
            >>= nlgAssume
        pure $ Just $ MkDeclChecked
          { payload = assume
          , publicNames = [declHead.name]
          }
inferTyDeclAssume (MkAssume _   _      _        _ _) = pure Nothing

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
scanTyDeclAssume (MkAssume ann tysig appForm (Just (Type _tann)) _) = do
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

-- | Extract mixfix pattern information by comparing the AppForm against
-- the GIVEN parameters in the TypeSig.
--
-- A function is mixfix if any of its AppForm tokens match GIVEN parameter names.
-- For example:
--   GIVEN person IS A Person, program IS A Program
--   person `is eligible for` program MEANS ...
--
-- Here the AppForm is [person, is eligible for, program], and the GIVEN params
-- are [person, program]. Since 'person' and 'program' are GIVEN params, this
-- is a mixfix pattern: [Param "person", Keyword "is eligible for", Param "program"]
--
extractMixfixInfo :: TypeSig Name -> AppForm Name -> Maybe MixfixInfo
extractMixfixInfo tysig appForm =
  let
    -- Get the GIVEN parameter names as a set for fast lookup
    givenParams :: Set RawName
    givenParams = Set.fromList $ givenParamNames tysig

    -- Get all tokens from the AppForm (head + args)
    appFormTokens :: [Name]
    appFormTokens = appFormHead' : appFormArgs'
      where
        MkAppForm _ appFormHead' appFormArgs' _ = appForm

    -- Classify each token as either a parameter or a keyword
    classifyToken :: Name -> MixfixPatternToken
    classifyToken n
      | rawName n `Set.member` givenParams = MixfixParam (rawName n)
      | otherwise                          = MixfixKeyword (rawName n)

    -- Build the pattern
    patternTokens :: [MixfixPatternToken]
    patternTokens = map classifyToken appFormTokens

    -- Extract just the keywords
    extractKeyword :: MixfixPatternToken -> Maybe RawName
    extractKeyword (MixfixKeyword k) = Just k
    extractKeyword (MixfixParam _)   = Nothing

    keywordList :: [RawName]
    keywordList = mapMaybe extractKeyword patternTokens

    -- Count parameters
    paramCount :: Int
    paramCount = length $ filter isParam patternTokens

    isParam :: MixfixPatternToken -> Bool
    isParam (MixfixParam _)   = True
    isParam (MixfixKeyword _) = False

  in
    -- Only return MixfixInfo if there's at least one param in non-head position
    -- (i.e., if the first token is a param, this is mixfix)
    -- OR if there are keywords between params
    if paramCount > 0 && (maybe False isParam (listToMaybe patternTokens) || paramCount < length patternTokens)
       then Just MkMixfixInfo
              { pattern = patternTokens
              , keywords = keywordList
              , arity = paramCount
              }
       else Nothing

-- | Extract parameter names from a TypeSig's GIVEN clause.
givenParamNames :: TypeSig Name -> [RawName]
givenParamNames (MkTypeSig _ givenSig _) =
  case givenSig of
    MkGivenSig _ otns -> map optionallyTypedNameToRawName otns
  where
    optionallyTypedNameToRawName :: OptionallyTypedName Name -> RawName
    optionallyTypedNameToRawName (MkOptionallyTypedName _ n _ _) = rawName n

-- ----------------------------------------------------------------------------
-- Call-site Mixfix Pattern Matching
-- ----------------------------------------------------------------------------

-- | Try to match a function application against registered mixfix patterns.
-- Given a function name and its arguments, check if this could be a mixfix call.
-- If so, return Just (function RawName, restructured arguments).
-- The function RawName is needed because for param-first patterns like `a `and` b`,
-- the first keyword (`and`) identifies the function, not the outermost operator.
--
-- For example, if we have:
--   Definition: `myif` cond `mythen` thenVal `myelse` elseVal MEANS ...
--   Call: `myif` TRUE `mythen` 42 `myelse` 0
--
-- The call parses as: App myif [TRUE, mythen, 42, myelse, 0]
-- We return: Just (myif, [TRUE, 42, 0])
--
-- For patterns that start with a param (like `a `and` b `had` c`), the binary
-- infix parsing creates nested applications:
--   Call: "Alice" `and` "Bob" `had` "Charlie"
--   Parses as: App had [App and ["Alice", "Bob"], "Charlie"]
--
-- We flatten this to find the first keyword, look up the pattern, and match.
-- We return: Just (funcName, args, maybeError)
-- - funcName: the resolved function name (may differ from input for param-first patterns)
-- - args: restructured args (keywords removed) or original args if error
-- - maybeError: Just error if a mixfix pattern partially matched but had issues
--
tryMatchMixfixCall :: Name -> [Expr Name] -> Check (Maybe (RawName, [Expr Name], Maybe MixfixMatchError))
tryMatchMixfixCall funcName args = do
  registry <- asks (.mixfixRegistry)

  -- First, check if this is a binary mixfix application that needs flattening
  -- This handles param-first patterns like `a `and` b `had` c`
  case args of
    [l, r] | isSimpleName funcName -> do
      -- Binary application with simple function name - could be:
      -- 1. A real mixfix call like `and` applied to [a, b] (funcName is a keyword)
      -- 2. A mis-parsed infix like x `plus` y -> App x [`plus`, y] (funcName is left operand)

      -- Check case 2: is the first arg a potential keyword?
      -- We only do this reinterpretation if funcName is NOT itself callable.
      -- This prevents `elem == elem` from being reinterpreted as `elem` applied to [==, elem].
      funcNameInScope <- lookupRawNameInEnvironment (rawName funcName)
      let isCallable = any isCallableEntity funcNameInScope
      case (not isCallable, getExprName l) of
        (True, Just potentialOpName) | Map.member (rawName potentialOpName) registry -> do
          -- funcName is NOT a known function, but l is a registered mixfix keyword!
          -- Reinterpret: App funcName [l, r] -> potentialOp applied to [funcName, r]
          -- For infix: `plus` applied to [x, y]
          let opRawName = rawName potentialOpName
          case Map.lookup opRawName registry of
            Just sigs -> do
              -- Create args as [funcName-as-expr, r]
              let funcAsExpr = App (getAnno funcName) funcName []
                  newArgs = [funcAsExpr, r]
              -- For reinterpreted infix, use matchParamPositions instead of tryMatchAnyPattern
              -- because we've already identified the keyword and just need to match args to params
              let result = tryMatchParamPositions newArgs sigs
              case result of
                Just restructuredArgs -> pure $ Just (opRawName, restructuredArgs, Nothing)
                Nothing -> tryFlatteningApproach registry  -- Fall through to original logic
            Nothing -> tryFlatteningApproach registry
        _ -> tryFlatteningApproach registry
    _ -> tryRegularMatch
  where
    tryFlatteningApproach registry = do
      -- Original flattening logic for nested mixfix like `a `and` b `had` c`
      -- args is guaranteed to have exactly 2 elements (binary application)
      let (arg0, arg1) = case args of
            [a, b] -> (a, b)
            _ -> error "tryFlatteningApproach: expected exactly 2 args"
          flattened = flattenBinaryMixfixApp funcName arg0 arg1
      case findFirstKeyword flattened of
        Just firstKw ->
          case Map.lookup firstKw registry of
            Just sigs ->
              -- For param-first patterns, the firstKw IS the function name
              fmap (fmap (\r -> (firstKw, r, Nothing))) $ tryMatchFlattenedPattern firstKw flattened sigs
            Nothing -> tryRegularMatch  -- First keyword not registered
        Nothing -> tryRegularMatch  -- No keywords found (shouldn't happen)

    tryRegularMatch = do
      registry <- asks (.mixfixRegistry)
      let funcRawName = rawName funcName
      case Map.lookup funcRawName registry of
        Nothing -> pure Nothing  -- Not a registered mixfix function
        Just sigs -> do
          result <- tryMatchAnyPattern funcRawName args sigs
          pure $ fmap (\(restructuredArgs, mErr) -> (funcRawName, restructuredArgs, mErr)) result

-- | Extract a Name from an Expr if it's a simple variable or nullary application.
-- Used to check if an expression could be a mixfix keyword.
getExprName :: Expr Name -> Maybe Name
getExprName (Var _ n) = Just n
getExprName (App _ n []) = Just n
getExprName _ = Nothing

-- | Check if a CheckEntity is callable (function, constructor, etc.)
-- This is used to determine if a name can be applied to arguments.
isCallableEntity :: (Unique, Name, CheckEntity) -> Bool
isCallableEntity (_, _, KnownTerm t _) = isFunctionType t
isCallableEntity _ = False

-- | Check if a type is a function type.
isFunctionType :: Type' n -> Bool
isFunctionType (Fun _ _ _) = True
isFunctionType (Forall _ _ t) = isFunctionType t
isFunctionType _ = False

-- | Try to match against any of the registered patterns for this function name.
-- Returns:
-- - Just (restructuredArgs, Nothing) on successful match
-- - Just (args, Just error) on partial match with error (use original args + report error)
-- - Nothing if no pattern matches at all
tryMatchAnyPattern :: RawName -> [Expr Name] -> [FunTypeSig] -> Check (Maybe ([Expr Name], Maybe MixfixMatchError))
tryMatchAnyPattern _ _ [] = pure Nothing
tryMatchAnyPattern funcRawName args (sig:sigs) =
  case sig.mixfixInfo of
    Nothing -> tryMatchAnyPattern funcRawName args sigs  -- Not actually mixfix
    Just info ->
      case matchMixfixPattern funcRawName args info of
        MixfixSuccess restructuredArgs -> pure (Just (restructuredArgs, Nothing))
        MixfixError err -> pure (Just (args, Just err))  -- Return error but keep original args
        MixfixNoMatch -> tryMatchAnyPattern funcRawName args sigs

-- | Match a list of expressions against a mixfix pattern.
--
-- Two cases:
-- 1. Keyword-first patterns like `myif cond mythen thenVal myelse elseVal`
--    Pattern: [Kw "myif", Param, Kw "mythen", Param, Kw "myelse", Param]
--    Args:    [cond, mythen, thenVal, myelse, elseVal]  (old nested binary repr)
--    OR Args: [cond, thenVal, Var myelse, elseVal]      (new linear repr - mythen absorbed)
--
-- 2. Param-first patterns like `a op1 b op2 c`
--    Pattern: [Param, Kw "op1", Param, Kw "op2", Param]
--    funcName = op1 (first keyword)
--    Args:    [a, b, Var op2, c]  (new linear repr)
--    - args[0] = param before first keyword
--    - args[1:] = rest, alternating [param, Var kw, param, ...]
--
-- Returns: MixfixMatchResult with extracted params if matched
matchMixfixPattern :: RawName -> [Expr Name] -> MixfixInfo -> MixfixMatchResult [Expr Name]
matchMixfixPattern funcRawName args info =
  case info.pattern of
    [] -> MixfixNoMatch  -- Empty pattern, shouldn't happen
    (headToken:restPattern) ->
      case headToken of
        MixfixKeyword k | k == funcRawName ->
          -- Keyword-first pattern: funcName is the head keyword
          -- Try new linear format first, then fall back to old format
          case matchLinearAfterHeadKeyword restPattern args of
            MixfixSuccess matched -> MixfixSuccess (extractParamArgs matched)
            MixfixError err -> MixfixError err
            MixfixNoMatch ->
              -- Fall back to old format (matchPatternTokens returns Maybe)
              case matchPatternTokens restPattern args of
                Just params -> MixfixSuccess params
                Nothing -> MixfixNoMatch

        MixfixParam _ ->
          -- Param-first pattern: funcName should be the FIRST KEYWORD in pattern
          -- Find it and check args match
          matchParamFirstPattern funcRawName args info.pattern

        _ -> MixfixNoMatch

-- | Match args for param-first patterns in the new linear representation.
-- Pattern: [Param, Kw op1, Param, Kw op2, Param, ...]
-- funcName: op1 (the first keyword)
-- Args: [a, b, Var op2, c, ...] where a is before op1, rest alternates
matchParamFirstPattern :: RawName -> [Expr Name] -> [MixfixPatternToken] -> MixfixMatchResult [Expr Name]
matchParamFirstPattern funcRawName args pattern =
  case (splitAtFirstKeyword funcRawName pattern, args) of
    (Just (paramsBefore, paramsAfter), firstArg:restArgs) ->
      -- paramsBefore should be [Param] (the param before first keyword)
      -- paramsAfter is pattern after first keyword: [Param, Kw op2, Param, ...]
      if length paramsBefore == 1 && maybe False isParam (listToMaybe paramsBefore)
      then
        -- First arg is the param before keyword
        -- Rest args should match paramsAfter pattern
        case matchLinearAfterHeadKeyword paramsAfter restArgs of
          MixfixSuccess matched -> MixfixSuccess (firstArg : extractParamArgs matched)
          MixfixError err -> MixfixError err
          MixfixNoMatch -> MixfixNoMatch
      else MixfixNoMatch
    _ -> MixfixNoMatch
  where
    isParam (MixfixParam _) = True
    isParam _ = False

-- | Split pattern at the first occurrence of the given keyword.
-- Returns (tokens before keyword, tokens after keyword)
splitAtFirstKeyword :: RawName -> [MixfixPatternToken] -> Maybe ([MixfixPatternToken], [MixfixPatternToken])
splitAtFirstKeyword _ [] = Nothing
splitAtFirstKeyword kw (MixfixKeyword k : rest) | k == kw = Just ([], rest)
splitAtFirstKeyword kw (t : rest) = do
  (before, after) <- splitAtFirstKeyword kw rest
  Just (t : before, after)

-- | Match args in the new linear format after the head keyword.
-- Pattern (after head kw): [Param, Kw op2, Param, Kw op3, Param, ...]
-- Args: [val1, val2, Var op3, val3, ...] (interleaved params and keyword vars)
--
-- This handles the "curried" representation where keywords appear as Var nodes
-- at their expected positions in the argument list.
--
-- Returns: MixfixMatchResult with info about which args are keywords vs params.
-- This allows the caller to:
-- 1. Extract only param args for type checking against function parameter types
-- 2. Keep keyword args in the AST, typed as Keyword
-- 3. Report better error messages if keywords don't match
matchLinearAfterHeadKeyword :: [MixfixPatternToken] -> [Expr Name] -> MixfixMatchResult [MixfixArgMatch (Expr Name)]
matchLinearAfterHeadKeyword [] [] = MixfixSuccess []
matchLinearAfterHeadKeyword [] (_:_) = MixfixNoMatch  -- Extra args
matchLinearAfterHeadKeyword (_:_) [] = MixfixNoMatch  -- Missing args
matchLinearAfterHeadKeyword (token:tokens) (arg:args) =
  case token of
    MixfixParam _ ->
      -- Expect a value argument - keep it in result as a param
      (MixfixParamArg arg :) <$> matchLinearAfterHeadKeyword tokens args
    MixfixKeyword expectedKw ->
      -- Expect either a Var with the keyword name (new format)
      -- or we're in old format and should skip checking here
      case arg of
        Var _ n | rawName n == expectedKw ->
          -- New format: keyword marker present, keep it as validated keyword
          (MixfixKeywordArg expectedKw :) <$> matchLinearAfterHeadKeyword tokens args
        App _ n [] | rawName n == expectedKw ->
          -- Also accept App with empty args (nullary application)
          (MixfixKeywordArg expectedKw :) <$> matchLinearAfterHeadKeyword tokens args
        Var _ n ->
          -- Found a Var that doesn't match the expected keyword
          -- This is likely a typo or wrong keyword - report as error
          MixfixError (WrongKeyword expectedKw (rawName n))
        App _ n [] ->
          -- Found an App with wrong keyword name
          MixfixError (WrongKeyword expectedKw (rawName n))
        _ ->
          -- Old format or mismatch - try treating this as old format
          -- where keyword was already consumed by parser
          -- This means arg is actually the next param value
          matchLinearAfterHeadKeyword tokens (arg:args)

-- | Extract only the param args from a mixfix match result.
-- This is what gets passed to the function's type checking.
extractParamArgs :: [MixfixArgMatch a] -> [a]
extractParamArgs = mapMaybe getParam
  where
    getParam (MixfixParamArg a) = Just a
    getParam (MixfixKeywordArg _) = Nothing

-- | Check if any keywords were matched (new format).
-- If so, the args list contains keyword placeholders that need special handling.
-- NOTE: Currently unused but kept for future enhanced error reporting.
_hasMatchedKeywords :: [MixfixArgMatch a] -> Bool
_hasMatchedKeywords = any isKeyword
  where
    isKeyword (MixfixKeywordArg _) = True
    isKeyword _ = False

-- ----------------------------------------------------------------------------
-- Fuzzy matching for keyword suggestions
-- ----------------------------------------------------------------------------

-- | Find similar keywords to a given unknown keyword.
-- Returns keywords with edit distance <= threshold from the unknown one.
-- NOTE: Prefixed with underscore as it's prepared for future use in enhanced error messages.
_suggestKeywords :: RawName -> [RawName] -> [RawName]
_suggestKeywords unknown knowns =
  let unknownText = _rawNameText unknown
      candidates = [(k, _editDistance unknownText (_rawNameText k)) | k <- knowns]
      threshold = max 2 (Text.length unknownText `div` 3)  -- Adaptive threshold
  in map fst $ filter ((<= threshold) . snd) candidates

-- | Extract text from a RawName for comparison purposes.
_rawNameText :: RawName -> Text
_rawNameText (NormalName t) = t
_rawNameText (QualifiedName _ t) = t  -- Use just the name part for comparison
_rawNameText (PreDef t) = t

-- | Compute Levenshtein edit distance between two texts.
-- This is a simple O(m*n) implementation using dynamic programming with lists.
_editDistance :: Text -> Text -> Int
_editDistance s1 s2
  | Text.null s1 = Text.length s2
  | Text.null s2 = Text.length s1
  | otherwise = editDistanceList (Text.unpack s1) (Text.unpack s2)
  where
    -- Simple DP implementation using list folding
    editDistanceList :: String -> String -> Int
    editDistanceList xs ys = last $ foldl transform [0..length xs] ys
      where
        transform :: [Int] -> Char -> [Int]
        transform row@(prevDiag:prevRow) c =
          scanl computeStep (prevDiag + 1) (zip3 xs row prevRow)
          where
            computeStep :: Int -> (Char, Int, Int) -> Int
            computeStep left (xc, diag, up) =
              minimum [left + 1, up + 1, diag + if xc == c then 0 else 1]
        transform [] _ = []  -- Should never happen, pattern for exhaustiveness

-- | Rebuild the annotation for a mixfix App when args have been restructured.
-- The original annotation has holes for all parsed args (including keyword placeholders),
-- but the restructured args only contain the actual data args.
-- We create a new annotation with holes for only the actual args.
rebuildMixfixAppAnno :: Anno -> Name -> [Expr Name] -> Anno
rebuildMixfixAppAnno origAnn funcName args =
  -- Keep the original source range from the annotation
  -- but rebuild the payload with just holes for the actual args
  let funcHole = mkHoleWithSrcRange funcName
      argHoles = map mkHoleWithSrcRange args
      newPayload = funcHole : argHoles
  in fixAnnoSrcRange $ set #payload newPayload origAnn

-- | Flatten a binary mixfix application into a list of (Either Expr Keyword).
-- Given: App had [App and [a, b], c]
-- Returns: [Left a, Right "and", Left b, Right "had", Left c]
flattenBinaryMixfixApp :: Name -> Expr Name -> Expr Name -> [Either (Expr Name) RawName]
flattenBinaryMixfixApp funcName l r =
  flattenLeft l ++ [Right (rawName funcName)] ++ [Left r]
  where
    -- Recursively flatten left-nested binary mixfix apps
    flattenLeft :: Expr Name -> [Either (Expr Name) RawName]
    flattenLeft (App _ fn [ll, lr]) | isSimpleName fn =
      flattenLeft ll ++ [Right (rawName fn), Left lr]
    flattenLeft e = [Left e]

-- | Find the first keyword (Right value) in a flattened list
findFirstKeyword :: [Either (Expr Name) RawName] -> Maybe RawName
findFirstKeyword [] = Nothing
findFirstKeyword (Right kw : _) = Just kw
findFirstKeyword (Left _ : rest) = findFirstKeyword rest

-- | Try to match a flattened expression list against any registered pattern
tryMatchFlattenedPattern :: RawName -> [Either (Expr Name) RawName] -> [FunTypeSig] -> Check (Maybe [Expr Name])
tryMatchFlattenedPattern _ _ [] = pure Nothing
tryMatchFlattenedPattern firstKw flattened (sig:sigs) =
  case sig.mixfixInfo of
    Nothing -> tryMatchFlattenedPattern firstKw flattened sigs
    Just info ->
      case matchFlattenedPattern info.pattern flattened of
        Just args -> pure (Just args)
        Nothing -> tryMatchFlattenedPattern firstKw flattened sigs

-- | Check if a Name could be a mixfix keyword.
-- Since quoted names become NormalName in the AST, we check if the name
-- could be a keyword by looking at its structure. For simplicity, we
-- check against the registry in the caller.
-- Here we just check that it's not a qualified or predefined name.
isSimpleName :: Name -> Bool
isSimpleName n = case rawName n of
  NormalName _ -> True
  _ -> False

-- | Match a flattened argument list against a full pattern.
-- The flattened list alternates: Left Expr for params, Right RawName for keywords.
matchFlattenedPattern :: [MixfixPatternToken] -> [Either (Expr Name) RawName] -> Maybe [Expr Name]
matchFlattenedPattern [] [] = Just []
matchFlattenedPattern [] (_:_) = Nothing  -- Extra elements
matchFlattenedPattern (_:_) [] = Nothing  -- Missing elements
matchFlattenedPattern (token:tokens) (arg:args) =
  case (token, arg) of
    (MixfixParam _, Left expr) ->
      -- Pattern expects param, got expression - match!
      (expr :) <$> matchFlattenedPattern tokens args
    (MixfixKeyword expectedKw, Right actualKw) ->
      -- Pattern expects keyword, got keyword - check if they match
      if expectedKw == actualKw
        then matchFlattenedPattern tokens args  -- Don't include keywords in result
        else Nothing
    _ -> Nothing  -- Mismatch: param vs keyword

-- | Match a list of pattern tokens against a list of expressions.
-- Returns the expressions at Param positions if all Keyword positions match.
matchPatternTokens :: [MixfixPatternToken] -> [Expr Name] -> Maybe [Expr Name]
matchPatternTokens [] [] = Just []
matchPatternTokens [] (_:_) = Nothing  -- Extra args
matchPatternTokens (_:_) [] = Nothing  -- Missing args
matchPatternTokens (token:tokens) (arg:args) =
  case token of
    MixfixParam _ ->
      -- This position should be an expression argument
      -- Keep it in the result
      (arg :) <$> matchPatternTokens tokens args
    MixfixKeyword expectedKeyword ->
      -- This position should be a Var with the expected keyword name
      case arg of
        Var _ n | rawName n == expectedKeyword ->
          -- Keyword matches, don't include in result
          matchPatternTokens tokens args
        _ -> Nothing  -- Not a matching keyword

-- | Match args against param positions only, for when we've already identified the keyword.
-- Used for reinterpreted infix calls like `x `plus` y` where the keyword is already known.
-- Pattern = [Param a, Keyword plus, Param b], args = [x, y], already know it's `plus`
-- Returns Just [x, y] if the number of param positions matches the number of args.
matchParamPositions :: [MixfixPatternToken] -> [Expr Name] -> Maybe [Expr Name]
matchParamPositions pattern args =
  let paramCount = length [() | MixfixParam _ <- pattern]
  in if paramCount == length args
     then Just args  -- Args line up with param positions
     else Nothing

-- | Try to match args against any signature's param positions.
tryMatchParamPositions :: [Expr Name] -> [FunTypeSig] -> Maybe [Expr Name]
tryMatchParamPositions _ [] = Nothing
tryMatchParamPositions args (sig:sigs) =
  case sig.mixfixInfo of
    Nothing -> tryMatchParamPositions args sigs
    Just info ->
      case matchParamPositions info.pattern args of
        Just result -> Just result
        Nothing -> tryMatchParamPositions args sigs

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
    -- Extract mixfix pattern info by comparing AppForm against GIVEN parameters
    let mMixfix = extractMixfixInfo tysig appForm
    pure $ MkFunTypeSig
      { anno = getAnno dty
      , rtysig
      , rappForm
      , resultType = result
      , name = name
      , arguments = extendsTySig <> extendsAppForm
      , mixfixInfo = mMixfix
      }

scanFunSigAssume :: Assume Name -> Check (Maybe FunTypeSig)
scanFunSigAssume (MkAssume _ _     _       (Just (Type _tann)) _) = pure Nothing
scanFunSigAssume a@(MkAssume _ tysig appForm mt _) = do
  -- declaration of a term
  let tysig' = mergeResultTypeInto tysig mt
  errorContext (WhileCheckingAssume (getName appForm)) do
    (rappForm, rtysig, extendsTySig) <- checkTermAppFormTypeSigConsistency appForm tysig'
    (ce, rt, result, extendsAppForm) <- inferTermAppForm rappForm rtysig
    aty <- setAnnResolvedType rt (Just Assumed) a
    name <- withQualified (appFormHeads rappForm) ce
    -- Extract mixfix pattern info by comparing AppForm against GIVEN parameters
    let mMixfix = extractMixfixInfo tysig' appForm
    pure $ Just $ MkFunTypeSig
      { anno = getAnno aty
      , rtysig
      , rappForm = rappForm
      , resultType = result
      , name
      , arguments = extendsTySig <> extendsAppForm
      , mixfixInfo = mMixfix
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
-- Desugaring Utilities
-- ----------------------------------------------------------------------------

desugarBinOpToFunction :: RawName -> Expr Name -> Anno -> Expr Name -> Expr Name -> Check (Expr Name)
desugarBinOpToFunction name g ann e1 e2 = do
  args <- rewriteBinOpAnno g e1 e2
  pure $ App (annoNoFunName ann args) (MkName emptyAnno name) args
  where
  annoNoFunName a as =
    fixAnnoSrcRange
      Anno
        { extra = a.extra
        , range = a.range
        , payload = [mkHoleWithSrcRangeHint Nothing, mkHoleWithSrcRange as]
        }

desugarUnaryOpToFunction :: RawName -> Expr Name -> Anno -> Expr Name -> Check (Expr Name)
desugarUnaryOpToFunction name g ann e  = do
  args <- rewriteUnaryOpAnno g e
  pure $ App (annoNoFunName ann args) (MkName emptyAnno name) args
  where
  annoNoFunName a as =
    fixAnnoSrcRange
      Anno
        { extra = a.extra
        , range = a.range
        , payload = [mkHoleWithSrcRangeHint Nothing, mkHoleWithSrcRange as]
        }

-- | Rewrite the 'Anno' of the given arguments @'NonEmpty' ('Expr' 'Name)'@ to
-- include the concrete syntax nodes of the 'Anno' in the @'Expr' 'Name'@.
--
-- Let's assume an 'Expr' for the code:
--
-- @
--   1 PLUS 2
-- @
--
-- We want to desugar this to use a prefix function:
--
-- @
--   \_\_PLUS\_\_ 1 2
-- @
--
-- To make exactprinting still faithful to the original sources, we need
-- to be very careful that it is printed the same way. First, let's look at the
-- 'Anno' for the @1 PLUS 2@ expression:
-- (a @_@ marks an 'AnnoHole'):
--
-- @
--   _ PLUS _
-- @
--
-- The 'Anno' of a function looks like '_ [OF] _', so two holes, one for the name of the function
-- and one for *all* arguments of the function. The first hole is easy to manage, just make
-- sure the 'Name' of the function has an 'emptyAnno', then it is basically skipped over.
-- For the second hole to accurately reproduce the same concrete syntax nodes, we need to be more
-- careful.
--
-- We need to massage now the concrete syntax nodes of '_ PLUS _' into the arguments
-- of the prefix function notation.
-- For each 'AnnoHole' in the original 'Anno', we get all the 'AnnoCsn' that come
-- afterwards and attach them to the 'Anno' of the argument.
-- Note, for the first element, we need to do the same thing for leading 'AnnoCsn' elements.
-- The finalised 'Anno' of each argument should then look like:
--
-- @
--   [[_, " PLUS "], [_]]
-- @
--
-- where the inner holes are then filled by the underlying @'Expr' 'Name'@.
--
-- This works also nicely for parenthesis, the expression @(1 PLUS 2)@
-- is then translated to:
--
-- @
--   [["(", _, " PLUS "], [_, ")"]]
-- @
rewriteBinOpAnno :: Expr Name -> Expr Name -> Expr Name -> Check [Expr Name]
rewriteBinOpAnno expr e1 e2 =
    case csnSlices of
    [beforeFirst, beforeSecond, after] ->
      pure
        [ e1 & annoOf %~ surroundWithCsn beforeFirst beforeSecond
        , e2 & annoOf %~ surroundWithCsn [] after
        ]
    _slices -> do
      addError $ DesugarAnnoRewritingError expr (HoleInfo 2 numberOfAnnoHoles)
      pure []
 where
  annoPieces = (getAnno expr).payload
  csnSlices = splitWhen (isJust . preview #_AnnoHole) annoPieces
  numberOfAnnoHoles = length $ filter (isJust . preview #_AnnoHole) annoPieces

-- | Just like 'rewriteBinOpAnno', but special case for unary function
rewriteUnaryOpAnno :: Expr Name -> Expr Name -> Check [Expr Name]
rewriteUnaryOpAnno expr e =
    case csnSlices of
    [before, after] ->
      pure
        [ e & annoOf %~ surroundWithCsn before after
        ]
    _slices -> do
      addError $ DesugarAnnoRewritingError expr (HoleInfo 2 numberOfAnnoHoles)
      pure []
 where
  annoPieces = (getAnno expr).payload
  csnSlices = splitWhen (isJust . preview #_AnnoHole) annoPieces
  numberOfAnnoHoles = length $ filter (isJust . preview #_AnnoHole) annoPieces

-- | Internal function that takes two lists of concrete syntax nodes and
-- embeds them into the given 'Anno'.
surroundWithCsn :: [AnnoElement] -> [AnnoElement] -> Anno -> Anno
surroundWithCsn before after a =
  fixAnnoSrcRange
    Anno
      { extra = a.extra
      , range = a.range
      , payload = before <> a.payload <> after
      }

-- ----------------------------------------------------------------------------
-- Typecheck Utils
-- ----------------------------------------------------------------------------

severity :: CheckErrorWithContext -> Severity
severity (MkCheckErrorWithContext e _) =
  case e of
    CheckInfo {}    -> SInfo
    CheckWarning {} -> SWarn
    _               -> SError

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
prettyCheckError (DesugarAnnoRewritingError context errorInfo) =
  [ "Error while desugaring:"
  , "While trying to desugar the expression"
  , ""
  , "  " <> prettyLayout context
  , ""
  , "We ran into the error:"
  , "The source annotation are not matching the expected number of arguments."
  , "Expected " <> Text.show (errorInfo.expected) <> " holes but got: " <> Text.show (errorInfo.got)
  ]
prettyCheckError (CheckWarning warning) = prettyCheckWarning warning
prettyCheckError (MixfixMatchErrorCheck funcName err) =
  prettyMixfixMatchError funcName err
prettyCheckError (TypicallyNotAllowedOnAssume n) =
  [ "TYPICALLY defaults are not allowed on ASSUME declarations:"
  , "  " <> prettyLayout n
  , "The ASSUME keyword is deprecated. Use GIVEN or DECLARE instead."
  ]

-- | Pretty print mixfix match errors with helpful suggestions.
prettyMixfixMatchError :: Name -> MixfixMatchError -> [Text]
prettyMixfixMatchError funcName = \case
  UnknownMixfixKeyword unknown suggestions ->
    [ "Unknown mixfix keyword `" <> prettyRawName unknown <> "`"
    , "In expression involving: " <> prettyLayout funcName
    ] <> case suggestions of
      [] -> []
      [s] -> ["Did you mean `" <> prettyRawName s <> "`?"]
      ss -> ["Did you mean one of: " <> Text.intercalate ", " (map (\s -> "`" <> prettyRawName s <> "`") ss) <> "?"]
  WrongKeyword expected actual ->
    [ "Expected keyword `" <> prettyRawName expected <> "` but found `" <> prettyRawName actual <> "`"
    , "In expression involving: " <> prettyLayout funcName
    , "Check that keywords are in the correct order."
    ]
  MissingKeyword expected ->
    [ "Missing keyword `" <> prettyRawName expected <> "`"
    , "In expression involving: " <> prettyLayout funcName
    ]
  ExtraKeyword kw ->
    [ "Unexpected keyword `" <> prettyRawName kw <> "`"
    , "In expression involving: " <> prettyLayout funcName
    ]
  ArityMismatch expected actual ->
    [ "Wrong number of arguments in mixfix expression"
    , "Expected " <> Text.show expected <> " arguments but got " <> Text.show actual
    , "In expression involving: " <> prettyLayout funcName
    ]
  where
    prettyRawName :: RawName -> Text
    prettyRawName (NormalName t) = t
    prettyRawName (QualifiedName qs t) = Text.intercalate "." (toList qs) <> "." <> t
    prettyRawName (PreDef predef) = predef

prettyCheckWarning :: CheckWarning -> [Text]
prettyCheckWarning = \ case
  PatternMatchRedundant b ->
    [ "The following CONSIDER branch is redundant: "
    , ""
    ] <>
    map (("  " <>) . prettyLayout) b
    <> [ ""
    ]
  PatternMatchesMissing b ->
    [ "The following branches still need to be considered:"
    , "" ]
    <>
    map (("  " <>) . prettyLayout) b
    <> [ "" ]
  AssumeDeprecated name ->
    [ "ASSUME is deprecated and will be removed in a future version."
    , ""
    , "The declaration of " <> quotedName name <> " uses ASSUME."
    , ""
    , "Consider using one of these alternatives:"
    , "  - GIVEN: for function parameters"
    , "  - DECLARE: for type declarations"
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
  standardTypeMismatch [ "The condition in IF-THEN-ELSE and BRANCH-IF-THEN-OTHERWISE constructs is expected to be of type" ] expected given
prettyTypeMismatch ExpectNotArgumentContext expected given =
  standardTypeMismatch [ "The argument of NOT is expected to be of type" ] expected given
prettyTypeMismatch ExpectPercentArgumentContext expected given =
  standardTypeMismatch [ "The argument of '%' is expected to be of type" ] expected given
prettyTypeMismatch ExpectPostUrlContext expected given =
  standardTypeMismatch [ "The URL argument of POST is expected to be of type" ] expected given
prettyTypeMismatch ExpectPostHeadersContext expected given =
  standardTypeMismatch [ "The headers argument of POST is expected to be of type" ] expected given
prettyTypeMismatch ExpectPostBodyContext expected given =
  standardTypeMismatch [ "The body argument of POST is expected to be of type" ] expected given
prettyTypeMismatch ExpectConcatArgumentContext expected given =
  standardTypeMismatch [ "The argument of CONCAT is expected to be of type" ] expected given
prettyTypeMismatch ExpectAsStringArgumentContext expected given =
  standardTypeMismatch [ "The argument of AS STRING is expected to be of type" ] expected given
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
  [ "Both the THEN and the ELSE branch of an IF-THEN-ELSE and BRANCH-IF-THEN-OTHERWISE constructs must have the same type."
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
  standardTypeMismatch [ "The contract passed to a TRACE directive is expected to be of type" ] expected given
prettyTypeMismatch ExpectRegulativeTimestampContext expected given =
  standardTypeMismatch [ "The timestamp passed to an event in a TRACE directive is expected to be of type" ] expected given
prettyTypeMismatch ExpectRegulativeEventContext expected given =
  standardTypeMismatch [ "The event expr passed to a TRACE directive is expected to be of type" ] expected given
prettyTypeMismatch ExpectRegulativeProvidedContext expected given =
  standardTypeMismatch [ "The PROVIDED clause for filtering the ACTION is expected to be of type" ] expected given
prettyTypeMismatch ExpectAssertContext expected given =
  standardTypeMismatch [ "An ASSERT directive is expected to be of type" ] expected given
prettyTypeMismatch (ExpectTypicallyValueContext n) expected given =
  standardTypeMismatch [ "The TYPICALLY value for " <> quotedName n <> " is expected to be of type" ] expected given

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

-- ===========================================================================
-- Presumptive Wrapper Generation for TYPICALLY Defaults
-- ===========================================================================

-- | Generate presumptive wrappers for all DECIDEs with TYPICALLY defaults.
-- Returns wrappers with mapping from original function name to wrapper.
generatePresumptiveWrappersWithMapping :: [TopDecl Resolved] -> Check [(Resolved, TopDecl Resolved)]
generatePresumptiveWrappersWithMapping decls = do
  -- Collect DECIDEs with TYPICALLY defaults and generate wrappers
  wrappers <- forM decls $ \case
    Decide ann decide | hasTypicallyDefaults decide -> do
      let MkDecide _ _ (MkAppForm _ origName _ _) _ = decide
      wrapper <- generateWrapper ann decide
      pure (Just (origName, wrapper))
    _ -> pure Nothing

  pure (catMaybes wrappers)

-- | Get the text of a function name
getNameText :: Resolved -> Text
getNameText resolved =
  let origName = getOriginal resolved
      origRawName = rawName (getName origName)
  in rawNameToText origRawName

-- | Rewrite Name-level expression to use wrapper name
rewriteExprForPEvalName :: Map Text Resolved -> Expr Name -> Expr Name
rewriteExprForPEvalName wrapperMap expr = trace ("REWRITE EXPR: " ++ take 200 (show expr)) $ case expr of
  -- Case 1: Direct function reference - replace with wrapper name
  Var ann name ->
    let nameText = rawNameToText (rawName name)
        wrapperName = case Map.lookup nameText wrapperMap of
          Just _ ->
            -- Construct wrapper name: 'presumptive <original>'
            let wrapperText = "'presumptive " <> nameText <> "'"
                wrapperRawName = NormalName wrapperText
            in MkName (getAnno name) wrapperRawName
          Nothing -> name
    in Var ann wrapperName

  -- Case 2: Function application - rewrite function name
  App ann name args ->
    let origRawName = rawName name
        nameText = rawNameToText origRawName
        wrapperName = trace ("PEVAL APP: origRawName=" ++ show origRawName ++ ", text='" ++ Text.unpack nameText ++ "' -> " ++ show (Map.member nameText wrapperMap)) $
                      case Map.lookup nameText wrapperMap of
          Just _ ->
            -- Construct wrapper name: 'presumptive <original>'
            let wrapperText = "'presumptive " <> nameText <> "'"
                wrapperRawName = NormalName wrapperText
                newName = MkName (getAnno name) wrapperRawName
            in trace ("REWRITING TO: text='" ++ Text.unpack wrapperText ++ "', newName raw=" ++ Text.unpack (rawNameToText (rawName newName))) $ newName
          Nothing -> name
        finalExpr = App ann wrapperName args
    in trace ("FINAL APP NAME: " ++ Text.unpack (rawNameToText (rawName wrapperName))) finalExpr

  -- Other cases: return unchanged
  _ -> expr

-- | Create CheckInfo for a wrapper function to add it to the environment
-- Builds the full function type from GIVEN parameters and GIVETH return type
makeWrapperCheckInfo :: Resolved -> GivenSig Resolved -> Maybe (GivethSig Resolved) -> CheckInfo
makeWrapperCheckInfo wrapperName givenSig mGivethSig =
  let MkGivenSig _ otns = givenSig
      -- Extract parameter types and names from OptionallyTypedName
      namedParams = [MkOptionallyNamedType emptyAnno (Just name) ty
                    | MkOptionallyTypedName _ name (Just ty) _ <- otns]
      returnType = case mGivethSig of
        Just (MkGivethSig _ ty) -> ty
        Nothing -> error "Wrapper should always have a return type"
      -- Build function type using Fun constructor
      funcType = Fun emptyAnno namedParams returnType
      checkInfo = MkCheckInfo
        { names = [wrapperName]
        , checkEntity = KnownTerm funcType Computable  -- Wrappers are computable functions
        }
  in trace ("REGISTERING WRAPPER: " ++ Text.unpack (getNameText wrapperName)) checkInfo

-- | Check if a DECIDE has any TYPICALLY defaults in its GIVEN clause.
-- Also ensures we don't generate wrappers for wrappers (infinite chain prevention).
hasTypicallyDefaults :: Decide Resolved -> Bool
hasTypicallyDefaults (MkDecide _ (MkTypeSig _ givenSig _) (MkAppForm _ funcName _ _) _) =
  not (isPresumptiveWrapper funcName) && not (null (extractTypicallyDefaultsFromGiven givenSig))

-- | Check if a function name is already a presumptive wrapper
-- (starts with "'presumptive ")
isPresumptiveWrapper :: Resolved -> Bool
isPresumptiveWrapper resolved =
  let origName = getOriginal resolved
      origRawName = rawName (getName origName)
      nameText = rawNameToText origRawName
  in "'presumptive " `Text.isPrefixOf` nameText

-- | Extract TYPICALLY defaults from a GivenSig
extractTypicallyDefaultsFromGiven :: GivenSig Resolved -> [(Resolved, Expr Resolved)]
extractTypicallyDefaultsFromGiven (MkGivenSig _ otns) =
  [ (n, expr)
  | MkOptionallyTypedName _ n _ (Just expr) <- otns
  ]

-- | Generate a presumptive wrapper for a single DECIDE with TYPICALLY defaults
generateWrapper :: Anno -> Decide Resolved -> Check (TopDecl Resolved)
generateWrapper ann (MkDecide _ typeSig@(MkTypeSig _ givenSig mReturnType) appForm bodyExpr) = do
  let MkAppForm _ funcName _ _ = appForm

  -- Create the wrapper function name: 'presumptive <original-name>' with fresh unique
  wrapperName <- makePresumptiveName funcName

  -- Convert GIVEN parameters to MAYBE-wrapped versions with fresh Def nodes
  let MkGivenSig gsAnn otns = givenSig
  paramsWithMapping <- mapM wrapParameterWithFreshName otns
  let (maybeOtns, paramMappings) = unzip paramsWithMapping
  let maybeGivenSig = MkGivenSig gsAnn maybeOtns

  -- Wrap return type with MAYBE (if it exists)
  maybeReturnType <- case mReturnType of
    Nothing -> pure Nothing
    Just (MkGivethSig gsAnn' ty) -> do
      wrappedType <- wrapTypeWithMaybe ty
      pure $ Just (MkGivethSig gsAnn' wrappedType)

  let maybeTypeSig = MkTypeSig (getAnno typeSig) maybeGivenSig maybeReturnType

  -- Generate CONSIDER expression for each parameter
  -- paramMappings is [(wrapperParam, origParam)], otns is [OptionallyTypedName]
  -- We need [(wrapperParam, OptionallyTypedName)]
  let paramPairs = [(wrapperParam, otn) | ((wrapperParam, _origParam), otn) <- zip paramMappings otns]
  considerExpr <- generateConsiderChain funcName paramPairs bodyExpr

  -- Create wrapper DECIDE
  let wrapperAppForm = MkAppForm (getAnno appForm) wrapperName [] Nothing
  let wrapperDecide = MkDecide ann maybeTypeSig wrapperAppForm considerExpr

  pure $ Decide ann wrapperDecide

-- | Create a presumptive wrapper name with a fresh unique
-- This ensures the wrapper has a distinct identity from the original function
makePresumptiveName :: Resolved -> Check Resolved
makePresumptiveName resolved = do
  let origName = getOriginal resolved
      origRawName = rawName (getName origName)
      newText = "'presumptive " <> rawNameToText origRawName <> "'"
      newRawName = NormalName newText
      newName = MkName (getAnno origName) newRawName
  -- Generate a fresh unique for the wrapper
  freshU <- newUnique
  pure $ Def freshU newName

-- | Create a fresh Def node for a wrapper parameter
-- This ensures parameters in the wrapper have proper scoping
makeFreshParamName :: Resolved -> Resolved
makeFreshParamName resolved =
  let origName = getOriginal resolved
      u = getUnique resolved
  in Def u origName  -- Create a Def node with the same name and unique

-- | Create an explicit Ref to the original function
-- This ensures the wrapper calls the original function, not itself
makeOriginalFuncRef :: Resolved -> Resolved
makeOriginalFuncRef resolved =
  let origName = getOriginal resolved
      u = getUnique resolved
  in Ref origName u origName  -- Create a Ref that explicitly points to the original

-- | Wrap a parameter with MAYBE type and create fresh Def node for wrapper
-- Returns (wrapped parameter, mapping from new wrapper param to original param)
wrapParameterWithFreshName :: OptionallyTypedName Resolved -> Check (OptionallyTypedName Resolved, (Resolved, Resolved))
wrapParameterWithFreshName (MkOptionallyTypedName ann origName mType _typically) = do
  -- Create a fresh Def node for this parameter in the wrapper's context
  let freshName = makeFreshParamName origName

  -- Wrap the type with MAYBE
  wrappedType <- case mType of
    Nothing -> pure Nothing
    Just ty -> Just <$> wrapTypeWithMaybe ty

  let wrappedParam = MkOptionallyTypedName ann freshName wrappedType Nothing
  pure (wrappedParam, (freshName, origName))

-- | Wrap a type with MAYBE
wrapTypeWithMaybe :: Type' Resolved -> Check (Type' Resolved)
wrapTypeWithMaybe ty = do
  let ann = getAnno ty
  let maybeTypeRef = Ref maybeName maybeUnique maybeName
  pure $ TyApp ann maybeTypeRef [ty]

-- | Generate nested CONSIDER expressions to unwrap parameters and apply defaults
-- For each parameter:
--   CONSIDER param
--     WHEN NOTHING  -> (if has TYPICALLY: use default, else: return NOTHING)
--     WHEN (JUST x) -> (continue with unwrapped x)
-- Takes: funcName, [(wrapperParam, originalParam)], bodyExpr
generateConsiderChain :: Resolved -> [(Resolved, OptionallyTypedName Resolved)] -> Expr Resolved -> Check (Expr Resolved)
generateConsiderChain funcName paramPairs bodyExpr = do
  let ann = getAnno bodyExpr

  -- Get JUST and NOTHING constructors
  let justCtor = Ref justName justUnique justName
  let nothingCtor = Ref nothingName nothingUnique nothingName

  -- Create an explicit Ref to the original function
  -- This ensures we call the original, not the wrapper, even if both are in scope
  let origFuncRef = makeOriginalFuncRef funcName

  -- Build the innermost expression: JUST (origFunc arg1_unwrapped arg2_unwrapped ...)
  -- The unwrapped variable names will be created in buildConsiderForParam
  -- For now, collect the original parameter names to pass to the original function
  let origParamVars = [origName | (_, MkOptionallyTypedName _ origName _ _) <- paramPairs]
  let funcCall = case origParamVars of
        [] -> Var ann origFuncRef
        _  -> App ann origFuncRef (map (Var ann) origParamVars)
  let innermostExpr = App ann justCtor [funcCall]

  -- Build CONSIDER chain from innermost outward
  -- Process parameters in reverse order so we build from inside out
  considerExpr <- foldM (buildConsiderForParam ann justCtor nothingCtor) innermostExpr (reverse paramPairs)

  pure considerExpr

-- | Build a CONSIDER expression for a single parameter
-- Takes (wrapperParam, originalParam) tuple
buildConsiderForParam :: Anno -> Resolved -> Resolved -> Expr Resolved
                      -> (Resolved, OptionallyTypedName Resolved) -> Check (Expr Resolved)
buildConsiderForParam ann justCtor nothingCtor innerExpr (wrapperParam, MkOptionallyTypedName paramAnn origParamName _mType mDefault) = do
  -- Create a fresh variable name for the JUST pattern (unwrapped value)
  let unwrappedName = makeUnwrappedVarName origParamName

  -- Build the NOTHING branch
  nothingBranch <- case mDefault of
    Just defaultExpr -> do
      -- Has TYPICALLY default: use it
      -- WHEN NOTHING -> (continue with default value substituted)
      -- Substitute the origParamName (used in innerExpr) with the default value
      let substitutedExpr = substituteVar origParamName defaultExpr innerExpr
      pure $ MkBranch ann (When ann (PatApp ann nothingCtor [])) substitutedExpr
    Nothing -> do
      -- No TYPICALLY default: return NOTHING (Unknown)
      pure $ MkBranch ann (When ann (PatApp ann nothingCtor [])) (Var ann nothingCtor)

  -- Build the JUST branch
  -- WHEN (JUST x) -> innerExpr (with origParamName bound to unwrapped x)
  let justPattern = PatApp ann justCtor [PatVar ann unwrappedName]
  let substitutedInner = substituteVar origParamName (Var ann unwrappedName) innerExpr
  let justBranch = MkBranch ann (When ann justPattern) substitutedInner

  -- Build CONSIDER expression
  -- Scrutinize the wrapper parameter (the MAYBE-wrapped one from the GIVEN clause)
  let scrutinee = Var ann wrapperParam
  pure $ Consider paramAnn scrutinee [nothingBranch, justBranch]

-- | Create an unwrapped variable name from a Resolved name
-- For a parameter 'age', create 'age_unwrapped' or similar
makeUnwrappedVarName :: Resolved -> Resolved
makeUnwrappedVarName resolved =
  let origName = getOriginal resolved
      origRawName = rawName (getName origName)
      newText = rawNameToText origRawName <> "_unwrapped"
      newRawName = NormalName newText
      newName = MkName (getAnno origName) newRawName
      -- Create a new Def with a fresh unique
      -- For now, reuse the original unique with a marker
      origUnique = getUnique resolved
  in Def origUnique newName

-- | Substitute a variable with an expression throughout an expression tree
-- This is a simple substitution that doesn't handle capture-avoiding properly
-- For our use case (fresh variable names), this should be sufficient
substituteVar :: Resolved -> Expr Resolved -> Expr Resolved -> Expr Resolved
substituteVar target replacement = go
  where
    go expr = case expr of
      Var _ v | v == target -> replacement
      Var ann v -> Var ann v
      App ann f args -> App ann f (map go args)
      Lam ann givens body -> Lam ann givens (go body)  -- TODO: check shadowing
      And ann e1 e2 -> And ann (go e1) (go e2)
      Or ann e1 e2 -> Or ann (go e1) (go e2)
      Implies ann e1 e2 -> Implies ann (go e1) (go e2)
      Plus ann e1 e2 -> Plus ann (go e1) (go e2)
      Minus ann e1 e2 -> Minus ann (go e1) (go e2)
      Times ann e1 e2 -> Times ann (go e1) (go e2)
      DividedBy ann e1 e2 -> DividedBy ann (go e1) (go e2)
      Equals ann e1 e2 -> Equals ann (go e1) (go e2)
      Leq ann e1 e2 -> Leq ann (go e1) (go e2)
      Geq ann e1 e2 -> Geq ann (go e1) (go e2)
      Lt ann e1 e2 -> Lt ann (go e1) (go e2)
      Gt ann e1 e2 -> Gt ann (go e1) (go e2)
      IfThenElse ann cond then' else' -> IfThenElse ann (go cond) (go then') (go else')
      Consider ann scrut branches -> Consider ann (go scrut) (map goBranch branches)
      other -> other  -- For literals, etc.

    goBranch (MkBranch ann lhs rhs) = MkBranch ann lhs (go rhs)
