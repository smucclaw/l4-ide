module L4.TypeCheckNew where

import Base
import qualified Base.Map as Map
import qualified Base.Text as Text
import L4.Annotation
import L4.Parser.SrcSpan (prettySrcRange, prettySrcRangeM)
import L4.Print (prettyLayout, quotedName,)
import L4.Syntax
import L4.TypeCheck.Annotation
import L4.TypeCheck.Environment as X
import L4.TypeCheck.Types as X
import L4.TypeCheck.Unify
import L4.TypeCheck.With as X

import Control.Applicative
import Data.Bifunctor
import Data.Either (partitionEithers)
import Optics.Core hiding (anyOf, re)
import Data.Tuple.Extra (firstM)
import Control.Monad.Extra (mapMaybeM)
import qualified Data.List as List

-- Phases:
--
-- 1. Kind prescanning (plus module structure)
-- 2. Kind checking
-- 3. Type prescanning
-- 4. Type checking
--

--
-- PHASE 4
--

inferExpr :: Expr Name -> Check (Expr Resolved, Type' Resolved)
inferExpr g = softprune $ errorContext (WhileCheckingExpression g) do
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
    Where _ _ _ -> _
{-
    Where ann e ds -> do
      let
        preScanDecl = mapMaybeM scanTyDeclLocalDecl
        scanDecl = mapMaybeM inferTyDeclLocalDecl
        scanFuns = mapMaybeM scanFunSigLocalDecl

      (rds, extends) <- withScanTypeAndSigEnvironment preScanDecl scanDecl scanFuns ds do
        unzip <$> traverse inferLocalDecl ds
      (re, t) <- extendKnownMany (concat extends) $ inferExpr e
      pure (Where ann re rds, t)
-}

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

checkExpr :: ExpectationContext -> Expr Name -> Type' Resolved -> Check (Expr Resolved)
checkExpr ec (IfThenElse ann e1 e2 e3) t = softprune $ do
  re <- checkIfThenElse ec ann e1 e2 e3 t
  let re' = setAnnResolvedType t re
  pure re'
checkExpr ec (Consider ann e branches) t = softprune $ do
  re <- checkConsider ec ann e branches t
  let re' = setAnnResolvedType t re
  pure re'
-- checkExpr (ParenExpr ann e) t = do
--   re <- checkExpr e t
--   pure (ParenExpr ann re)
checkExpr ec (Where ann e ds) t = softprune $ do
  -- let
  --   preScanDecl = const (pure []) -- mapMaybeM scanTyDeclLocalDecl
  --   scanDecl = mapMaybeM inferTyDeclLocalDecl -- calls phase 2
  --   scanFuns = mapMaybeM scanFunSigLocalDecl -- calls phase 3

  rdeclares <- mapMaybeM inferTyDeclLocalDecl ds
  withDeclares rdeclares do
    rdecides <- mapMaybeM scanFunSigLocalDecl ds
    withDecides rdecides do
      unzip <$> traverse (firstM nlgLocalDecl <=< inferLocalDecl) ds


  -- (rds, extends) <- withScanTypeAndSigEnvironment preScanDecl scanDecl scanFuns ds do
  --    unzip <$> traverse (firstM nlgLocalDecl <=< inferLocalDecl) ds
  re <- extendKnownMany (concat extends) do
    re <- checkExpr ec e t
    -- We have to immediately resolve 'Nlg' annotations, as 'ds'
    -- brings new bindings into scope.
    nlgExpr re
  let re' = setAnnResolvedType t (Where ann re rds)
  pure re'
checkExpr ec e t = softprune $ errorContext (WhileCheckingExpression e) do
  (re, rt) <- inferExpr e
  expect ec t rt
  pure re

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
      rt <- _inferType t
      pure (MkOptionallyTypedName ann' rn (Just rt), rt, [makeKnown rn (KnownTerm rt Local)])

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
inferPattern g@(PatVar ann n)      = errorContext (WhileCheckingPattern g) do
  inferPatternVar ann n
inferPattern g@(PatApp ann n [])   = errorContext (WhileCheckingPattern g) do
  inferPatternApp ann n [] `orElse` inferPatternVar ann n
inferPattern g@(PatApp ann n ps)   = errorContext (WhileCheckingPattern g) do
  inferPatternApp ann n ps
inferPattern g@(PatCons ann p1 p2) = errorContext (WhileCheckingPattern g) do
  (rp1, rt1, extend1) <- inferPattern p1
  let listType = list rt1
  (rp2, extend2) <- checkPattern ExpectConsArgument2Context p2 listType

  -- Allows us to hover over the 'FOLLOWED BY',
  -- giving us a type signature.
  let patCons = setAnnResolvedType listType $ PatCons ann rp1 rp2
  pure (patCons, listType, extend1 <> extend2)

inferPatternVar :: Anno -> Name -> Check (Pattern Resolved, Type' Resolved, [CheckInfo])
inferPatternVar ann n = do
  rn <- def n
  rt <- fresh (NormalName "p")
  pure (PatVar ann rn, rt, [makeKnown rn (KnownTerm rt Local)])

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

inferLit :: Lit -> Check (Type' Resolved)
inferLit (NumericLit _ _) =
  pure number
inferLit (StringLit _ _) =
  pure string



-- | Helper function to check a binary operator.
checkBinOp ::
     Type' Resolved -- ^ type of first argument
  -> Type' Resolved -- ^ type of second argument
  -> Type' Resolved -- ^ result type
  -> Text -- ^ operator name (for error messages)
  -> (Anno -> Expr Resolved -> Expr Resolved -> Expr Resolved) -- ^ how to construct the resulting AST
  -> Anno -- ^ anno of the original AST node
  -> Expr Name -- ^ first argument
  -> Expr Name -- ^ second argument
  -> Check (Expr Resolved, Type' Resolved)
checkBinOp t1 t2 tr opname op ann e1 e2 = do
  e1' <- checkExpr (ExpectBinOpArgContext opname 1) e1 t1
  e2' <- checkExpr (ExpectBinOpArgContext opname 2) e2 t2
  pure (op ann e1' e2', tr)


-- | Instantiate a quantified type with fresh meta variables.
instantiate :: Type' Resolved -> Check (Type' Resolved)
instantiate (Forall _ann ns t) = do
  substitution <- Map.fromList <$> traverse (\ n -> let (u, o) = getUniqueName n; r = rawName o in fresh r >>= \ v -> pure (u, v)) ns
  pure (substituteType substitution t)
instantiate t             = pure t



-- WHERE clause with declaration (which can be mutually recursive, but *cannot* declare new types)
--
-- What's needed (phases 2-4):
--
-- - Kind checking of all type signatures
-- - Pre-scanning for type-checking
-- - Type checking
