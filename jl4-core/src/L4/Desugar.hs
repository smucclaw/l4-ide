module L4.Desugar (
  -- * Caramelize Expressions
  --
  carameliseExpr,
  carameliseNode,
  -- * Computed Fields
  --
  desugarComputedFields,
  detectComputedFieldCycles,
  extractComputedFieldNames,
  ) where


import           Base
import           Data.Graph               (stronglyConnComp, SCC(..))
import qualified Data.Map.Strict          as Map
import qualified Data.Set                 as Set
import           L4.Annotation            (HasAnno (..), emptyAnno)
import           L4.Names
import           L4.Syntax
import qualified L4.TypeCheck.Environment as TypeCheck
import Control.Category ((>>>))

-- ----------------------------------------------------------------------------
-- Caramelize
-- ----------------------------------------------------------------------------

carameliseExpr :: HasName n => Expr n -> Expr n
carameliseExpr = carameliseExprWithContext InertCtxNone

-- | Caramelize expression with context tracking for inert elements.
-- Inert elements evaluate to the identity for their containing operator:
-- - In AND context: True (AND identity)
-- - In OR context: False (OR identity)
carameliseExprWithContext :: HasName n => InertContext -> Expr n -> Expr n
carameliseExprWithContext ctx = carameliseNode >>> \ case
  Not        ann e -> Not ann (carameliseExprWithContext InertCtxNone e)
  -- For AND/OR, we propagate the context to children
  And        ann e1 e2 -> And       ann (carameliseExprWithContext InertCtxAnd e1) (carameliseExprWithContext InertCtxAnd e2)
  Or         ann e1 e2 -> Or        ann (carameliseExprWithContext InertCtxOr e1) (carameliseExprWithContext InertCtxOr e2)
  RAnd       ann e1 e2 -> RAnd      ann (carameliseExprWithContext InertCtxAnd e1) (carameliseExprWithContext InertCtxAnd e2)
  ROr        ann e1 e2 -> ROr       ann (carameliseExprWithContext InertCtxOr e1) (carameliseExprWithContext InertCtxOr e2)
  Implies    ann e1 e2 -> Implies   ann (carameliseExprWithContext InertCtxNone e1) (carameliseExprWithContext InertCtxNone e2)
  Equals     ann e1 e2 -> Equals    ann (carameliseExprWithContext InertCtxNone e1) (carameliseExprWithContext InertCtxNone e2)
  Plus       ann e1 e2 -> Plus      ann (carameliseExprWithContext InertCtxNone e1) (carameliseExprWithContext InertCtxNone e2)
  Minus      ann e1 e2 -> Minus     ann (carameliseExprWithContext InertCtxNone e1) (carameliseExprWithContext InertCtxNone e2)
  Times      ann e1 e2 -> Times     ann (carameliseExprWithContext InertCtxNone e1) (carameliseExprWithContext InertCtxNone e2)
  DividedBy  ann e1 e2 -> DividedBy ann (carameliseExprWithContext InertCtxNone e1) (carameliseExprWithContext InertCtxNone e2)
  Modulo     ann e1 e2 -> Modulo    ann (carameliseExprWithContext InertCtxNone e1) (carameliseExprWithContext InertCtxNone e2)
  Exponent   ann e1 e2 -> Exponent  ann (carameliseExprWithContext InertCtxNone e1) (carameliseExprWithContext InertCtxNone e2)
  Cons       ann e1 e2 -> Cons      ann (carameliseExprWithContext InertCtxNone e1) (carameliseExprWithContext InertCtxNone e2)
  Leq        ann e1 e2 -> Leq       ann (carameliseExprWithContext InertCtxNone e1) (carameliseExprWithContext InertCtxNone e2)
  Geq        ann e1 e2 -> Geq       ann (carameliseExprWithContext InertCtxNone e1) (carameliseExprWithContext InertCtxNone e2)
  Lt         ann e1 e2 -> Lt        ann (carameliseExprWithContext InertCtxNone e1) (carameliseExprWithContext InertCtxNone e2)
  Gt         ann e1 e2 -> Gt        ann (carameliseExprWithContext InertCtxNone e1) (carameliseExprWithContext InertCtxNone e2)
  Proj       ann e n   -> Proj ann (carameliseExprWithContext InertCtxNone e) n
  Var        ann n     -> Var  ann n
  Lam        ann sig e -> Lam ann sig (carameliseExprWithContext InertCtxNone e)
  App        ann n es  -> App ann n (fmap (carameliseExprWithContext InertCtxNone) es)
  AppNamed   ann n nes morder -> AppNamed ann n (fmap caramliseNamedExpr nes) morder
  IfThenElse ann b t e -> IfThenElse ann (carameliseExprWithContext InertCtxNone b) (carameliseExprWithContext InertCtxNone t) (carameliseExprWithContext InertCtxNone e)
  MultiWayIf ann es e -> MultiWayIf ann (map (\(MkGuardedExpr ann' a b) -> MkGuardedExpr ann' (carameliseExprWithContext InertCtxNone a) (carameliseExprWithContext InertCtxNone b)) es) (carameliseExprWithContext InertCtxNone e)
  Regulative ann o -> Regulative ann (carameliseDeonton o)
  Consider   ann e branches -> Consider ann (carameliseExprWithContext InertCtxNone e) (fmap carameliseBranch branches)
  Lit        ann l -> Lit ann l
  Percent    ann e -> Percent ann (carameliseExprWithContext InertCtxNone e)
  List       ann es -> List ann (fmap (carameliseExprWithContext InertCtxNone) es)
  Where      ann e ds -> Where ann (carameliseExprWithContext ctx e) (fmap carameliseLocalDecl ds)
  LetIn      ann ds e -> LetIn ann (fmap carameliseLocalDecl ds) (carameliseExprWithContext ctx e)
  Event      ann ev -> Event ann (carameliseEvent ev)
  Fetch      ann e -> Fetch ann (carameliseExprWithContext InertCtxNone e)
  Env        ann e -> Env ann (carameliseExprWithContext InertCtxNone e)
  Post       ann e1 e2 e3 -> Post ann (carameliseExprWithContext InertCtxNone e1) (carameliseExprWithContext InertCtxNone e2) (carameliseExprWithContext InertCtxNone e3)
  Concat     ann es -> Concat ann (fmap (carameliseExprWithContext InertCtxNone) es)
  AsString   ann e -> AsString ann (carameliseExprWithContext InertCtxNone e)
  Breach     ann mParty mReason -> Breach ann (fmap (carameliseExprWithContext InertCtxNone) mParty) (fmap (carameliseExprWithContext InertCtxNone) mReason)
  -- Inert elements: update the context based on the desugaring context.
  -- The evaluator (Machine.hs) will read this context to determine the value.
  Inert      ann txt _oldCtx -> Inert ann txt ctx

carameliseLocalDecl :: HasName n => LocalDecl n -> LocalDecl n
carameliseLocalDecl = \ case
  LocalDecide ann decide -> LocalDecide ann (carameliseDecide decide)
  LocalAssume ann assume -> LocalAssume ann assume

carameliseDecide :: HasName n => Decide n -> Decide n
carameliseDecide = \ case
  MkDecide ann tySig appForm expr ->
    MkDecide ann tySig appForm (carameliseExpr expr)

carameliseBranch :: HasName n => Branch n -> Branch n
carameliseBranch = \ case
  MkBranch ann (When ann' pat) e -> MkBranch ann (When ann' (caramelisePattern pat)) (carameliseExpr e)
  MkBranch ann (Otherwise ann') e -> MkBranch ann (Otherwise ann') (carameliseExpr e)

caramelisePattern :: HasName n => Pattern n -> Pattern n
caramelisePattern = \ case
  PatVar ann n -> PatVar ann n
  PatApp ann n ps -> PatApp ann n (fmap caramelisePattern ps)
  PatCons ann p1 p2 -> PatCons ann (caramelisePattern p1) (caramelisePattern p2)
  PatExpr ann e -> PatExpr ann (carameliseExpr e)
  PatLit ann l -> PatLit ann l

carameliseEvent :: HasName n => Event n -> Event n
carameliseEvent = \ case
  MkEvent { anno, party, action, timestamp, atFirst} ->
    MkEvent
      { anno
      , party = carameliseExpr party
      , action = carameliseExpr action
      , timestamp = carameliseExpr timestamp
      , atFirst
      }

carameliseDeonton :: HasName n => Deonton n -> Deonton n
carameliseDeonton = \ case
  MkDeonton { anno, party, action, due, hence, lest} ->
    MkDeonton
      { anno
      , party = carameliseExpr party
      , action = carameliseRAction action
      , due = fmap carameliseExpr due
      , hence = fmap carameliseExpr hence
      , lest = fmap carameliseExpr lest
      }

carameliseRAction :: HasName n => RAction n -> RAction n
carameliseRAction = \ case
  MkAction { anno, modal, action, provided } ->
    MkAction
      { anno
      , modal
      , action
      , provided = fmap carameliseExpr provided
      }

caramliseNamedExpr :: HasName n => NamedExpr n -> NamedExpr n
caramliseNamedExpr = \ case
  MkNamedExpr ann n e -> MkNamedExpr ann n (carameliseExpr e)

-- | We desugar expressions during typechecking, for example, @2 PLUS 3@ is
-- turned into the function @\_\_PLUS\_\_ 2 3@. This is less readable during natural
-- language generation, so we undo some of our desugaring and add the syntactic sugar
-- back.
--
-- We call this process "caramelise". Primarily, because fendor likes caramel,
-- and it feels like a good name for the opposite of "desugaring".
carameliseNode :: HasName n => Expr n -> Expr n
carameliseNode = \ case
  App ann n es
    | Just caramelise <- isBuiltinBinary (rawName $ getName n)
    , [e1, e2] <- es ->
        setAnno ann $ caramelise e1 e2
    | Just caramelise <- isBuiltinUnary (rawName $ getName n)
    , [e1] <- es ->
        setAnno ann $ caramelise e1
  expr -> expr

-- ----------------------------------------------------------------------------
-- Builtins
-- ----------------------------------------------------------------------------

-- | Operations such as Plus and Minus are desugared to prefix function
-- notation.
-- However, we don't want to show the prefix function name
-- but rather the infix version. So, we translate the prefix function
-- notation back to the infix one. In a way, we are undoing the
-- desugaring again.
isBuiltinBinary :: RawName -> Maybe (Expr n -> Expr n -> Expr n)
isBuiltinBinary r =
  Map.lookup (rawNameToText r) builtinBinFunctions

isBuiltinUnary :: RawName -> Maybe (Expr n -> Expr n)
isBuiltinUnary r =
  Map.lookup (rawNameToText r) builtinUnaryFunctions

builtinBinFunctions :: Map Text (Expr n -> Expr n -> Expr n)
builtinBinFunctions = Map.fromList
  [ (rawNameToText $ rawName TypeCheck.plusName, Plus emptyAnno)
  , (rawNameToText $ rawName TypeCheck.minusName, Minus emptyAnno)
  , (rawNameToText $ rawName TypeCheck.timesName, Times emptyAnno)
  , (rawNameToText $ rawName TypeCheck.divideName, DividedBy emptyAnno)
  , (rawNameToText $ rawName TypeCheck.moduloName, Modulo emptyAnno)
  , (rawNameToText $ rawName TypeCheck.ltName, Lt emptyAnno)
  , (rawNameToText $ rawName TypeCheck.leqName, Leq emptyAnno)
  , (rawNameToText $ rawName TypeCheck.gtName, Gt emptyAnno)
  , (rawNameToText $ rawName TypeCheck.geqName, Geq emptyAnno)
  , (rawNameToText $ rawName TypeCheck.andName, And emptyAnno)
  , (rawNameToText $ rawName TypeCheck.orName, Or emptyAnno)
  , (rawNameToText $ rawName TypeCheck.impliesName, Implies emptyAnno)
  , (rawNameToText $ rawName TypeCheck.consName, Cons emptyAnno)
  , (rawNameToText $ rawName TypeCheck.equalsName, Equals emptyAnno)
  ]

builtinUnaryFunctions :: Map Text (Expr n -> Expr n)
builtinUnaryFunctions = Map.fromList
  [ (rawNameToText $ rawName TypeCheck.notName, Not emptyAnno)
  ]

-- ----------------------------------------------------------------------------
-- Desugar Computed Fields
-- ----------------------------------------------------------------------------

-- | Desugar computed fields in DECLARE blocks into synthetic DECIDE declarations.
--
-- For each computed field @f IS A T MEANS expr@ on record @R@, generates:
--
-- @
-- GIVEN _self IS A R
-- GIVETH A T
-- f _self MEANS LET s1 = _self\'s s1; ... IN expr
-- @
--
-- where s1..sN are all sibling fields (excluding f itself).
-- The original DECLARE retains only stored fields (without MEANS clauses).
desugarComputedFields :: Module Name -> Module Name
desugarComputedFields (MkModule ann imports section) =
  MkModule ann imports (desugarCFSection section)

desugarCFSection :: Section Name -> Section Name
desugarCFSection (MkSection sAnn sMn sMaka topDecls) =
  MkSection sAnn sMn sMaka (concatMap desugarCFTopDecl topDecls)

desugarCFTopDecl :: TopDecl Name -> [TopDecl Name]
desugarCFTopDecl (Declare dAnn decl) = desugarCFDeclare dAnn decl
desugarCFTopDecl (Section sAnn section) = [Section sAnn (desugarCFSection section)]
desugarCFTopDecl other = [other]

desugarCFDeclare :: Anno -> Declare Name -> [TopDecl Name]
desugarCFDeclare dAnn (MkDeclare declAnn tysig appForm (RecordDecl rAnn mCon tns))
  | any isComputed tns =
    let
      -- Extract type parameters from the DECLARE's GIVEN (e.g., GIVEN a IS A TYPE)
      MkTypeSig _ (MkGivenSig _ typeParams) _ = tysig
      storedFields = [tn | tn@(MkTypedName _ _ _ Nothing) <- tns]
      syntheticDecides = mapMaybe (makeComputedDecide appForm typeParams tns) tns
      newDeclare = Declare dAnn (MkDeclare declAnn tysig appForm (RecordDecl rAnn mCon storedFields))
    in newDeclare : map (uncurry Decide) syntheticDecides
desugarCFDeclare dAnn decl = [Declare dAnn decl]

isComputed :: TypedName n -> Bool
isComputed (MkTypedName _ _ _ (Just _)) = True
isComputed _ = False

-- | Generate a synthetic Decide for a computed field.
--
-- For @adult IS A BOOLEAN MEANS age >= 18@ on @DECLARE Person HAS name, age, adult@:
--
-- @
-- GIVEN _self IS A Person
-- GIVETH A BOOLEAN
-- adult _self MEANS _self's age >= 18
-- @
--
-- Bare field references in the MEANS expression are rewritten to projections
-- on @_self@.  This avoids introducing LET bindings that would create
-- duplicate names in scope (the selector and the LET local), which causes
-- ambiguity errors with polymorphic operators like @=@.
makeComputedDecide :: AppForm Name -> [OptionallyTypedName Name] -> [TypedName Name] -> TypedName Name -> Maybe (Anno, Decide Name)
makeComputedDecide appForm typeParams allFields (MkTypedName fieldAnn fieldName fieldType (Just meansExpr)) =
  let
    -- Extract record name and type args from the DECLARE's AppForm
    MkAppForm _ recordName typeArgs _ = appForm
    -- Create a self parameter name
    selfName = MkName emptyAnno (NormalName "_self")
    -- Build the record type: RecordName arg1 arg2 ...
    recordType = TyApp emptyAnno recordName (map (\n -> TyApp emptyAnno n []) typeArgs)
    -- Type signature: GIVEN <typeParams>, _self IS A <RecordType> GIVETH A <FieldType>
    -- Prepend type parameters from the DECLARE so parameterized types work.
    -- Use the field's annotation to preserve source range info.
    selfParam = MkOptionallyTypedName emptyAnno selfName (Just recordType)
    decideTypeSig = MkTypeSig fieldAnn
      (MkGivenSig emptyAnno (typeParams ++ [selfParam]))
      (Just (MkGivethSig emptyAnno fieldType))
    -- App form: <fieldName> _self
    decideAppForm = MkAppForm fieldAnn fieldName [selfName] Nothing
    -- Sibling field names (excluding the field being defined)
    siblingNames = Set.fromList
      [ rawName sibName
      | MkTypedName _ sibName _ _ <- allFields
      , rawName sibName /= rawName fieldName
      ]
    -- Rewrite bare field references to _self's field projections
    body = rewriteFieldRefs siblingNames selfName meansExpr
  in Just (fieldAnn, MkDecide fieldAnn decideTypeSig decideAppForm body)
makeComputedDecide _ _ _ _ = Nothing  -- stored field, no DECIDE needed

-- | Rewrite bare variable references to field projections on a record.
-- @rewriteFieldRefs fieldNames self expr@ replaces every @Var _ n@ where
-- @rawName n ∈ fieldNames@ with @Proj _ (Var _ self) n@.
-- Respects shadowing: binding forms (WHERE, LET, Lam) remove their bound
-- names from the rewrite set before descending into their bodies.
rewriteFieldRefs :: Set RawName -> Name -> Expr Name -> Expr Name
rewriteFieldRefs fields self = go fields
  where
    go flds expr = case expr of
      -- Variable/application: rewrite if it's a bare field reference
      App ann n args
        | null args && rawName n `Set.member` flds ->
            Proj emptyAnno (Var emptyAnno self) n
        | otherwise ->
            App ann n (map (go flds) args)
      -- Binary operators
      And ann e1 e2       -> And ann (go flds e1) (go flds e2)
      Or ann e1 e2        -> Or ann (go flds e1) (go flds e2)
      RAnd ann e1 e2      -> RAnd ann (go flds e1) (go flds e2)
      ROr ann e1 e2       -> ROr ann (go flds e1) (go flds e2)
      Implies ann e1 e2   -> Implies ann (go flds e1) (go flds e2)
      Equals ann e1 e2    -> Equals ann (go flds e1) (go flds e2)
      Not ann e           -> Not ann (go flds e)
      Plus ann e1 e2      -> Plus ann (go flds e1) (go flds e2)
      Minus ann e1 e2     -> Minus ann (go flds e1) (go flds e2)
      Times ann e1 e2     -> Times ann (go flds e1) (go flds e2)
      DividedBy ann e1 e2 -> DividedBy ann (go flds e1) (go flds e2)
      Modulo ann e1 e2    -> Modulo ann (go flds e1) (go flds e2)
      Exponent ann e1 e2  -> Exponent ann (go flds e1) (go flds e2)
      Cons ann e1 e2      -> Cons ann (go flds e1) (go flds e2)
      Leq ann e1 e2       -> Leq ann (go flds e1) (go flds e2)
      Geq ann e1 e2       -> Geq ann (go flds e1) (go flds e2)
      Lt ann e1 e2        -> Lt ann (go flds e1) (go flds e2)
      Gt ann e1 e2        -> Gt ann (go flds e1) (go flds e2)
      -- Projection: rewrite the record expression, but NOT the field name
      Proj ann e n        -> Proj ann (go flds e) n
      -- Control flow
      IfThenElse ann c t e -> IfThenElse ann (go flds c) (go flds t) (go flds e)
      MultiWayIf ann gs e ->
        MultiWayIf ann (map (goGuarded flds) gs) (go flds e)
      Consider ann e bs   -> Consider ann (go flds e) (map (goBranch flds) bs)
      -- Binding forms: remove bound names from the rewrite set
      Where ann e locals  ->
        let boundNames = localDeclNames locals
            flds' = flds `Set.difference` boundNames
        in Where ann (go flds' e) (map (goLocal flds') locals)
      LetIn ann locals e  ->
        let boundNames = localDeclNames locals
            flds' = flds `Set.difference` boundNames
        in LetIn ann (map (goLocal flds') locals) (go flds' e)
      Lam ann sig e       ->
        let boundNames = givenSigNames sig
            flds' = flds `Set.difference` boundNames
        in Lam ann sig (go flds' e)
      -- Containers
      List ann es         -> List ann (map (go flds) es)
      Concat ann es       -> Concat ann (map (go flds) es)
      Percent ann e       -> Percent ann (go flds e)
      AsString ann e      -> AsString ann (go flds e)
      -- Named application
      AppNamed ann n nes order ->
        AppNamed ann n (map (goNamed flds) nes) order
      -- Leaf nodes and everything else: unchanged
      Lit {} -> expr
      Fetch ann e         -> Fetch ann (go flds e)
      Env ann e           -> Env ann (go flds e)
      Post ann u h b      -> Post ann (go flds u) (go flds h) (go flds b)
      Breach ann mp mr    -> Breach ann (fmap (go flds) mp) (fmap (go flds) mr)
      Event {}            -> expr  -- regulative events are complex; leave as-is
      Regulative {}       -> expr  -- regulative rules: leave as-is
      Inert {}            -> expr

    goGuarded flds (MkGuardedExpr ann c e) =
      MkGuardedExpr ann (go flds c) (go flds e)

    goBranch flds (MkBranch ann lhs e) =
      let flds' = flds `Set.difference` branchLhsNames lhs
      in MkBranch ann lhs (go flds' e)

    goLocal flds (LocalDecide ann d) =
      let MkDecide dAnn ts af body = d
      in LocalDecide ann (MkDecide dAnn ts af (go flds body))
    goLocal _ ld = ld  -- LocalAssume: unchanged

    goNamed flds (MkNamedExpr ann n e) = MkNamedExpr ann n (go flds e)

    -- Extract names bound by local declarations
    localDeclNames :: [LocalDecl Name] -> Set RawName
    localDeclNames = Set.fromList . mapMaybe localName
      where
        localName (LocalDecide _ (MkDecide _ _ (MkAppForm _ n _ _) _)) = Just (rawName n)
        localName _ = Nothing

    -- Extract names bound by a GIVEN signature
    givenSigNames :: GivenSig Name -> Set RawName
    givenSigNames (MkGivenSig _ otns) =
      Set.fromList [rawName (getName otn) | otn <- otns]

    -- Extract names bound by a branch LHS (pattern)
    branchLhsNames :: BranchLhs Name -> Set RawName
    branchLhsNames (When _ pat) = patternNames pat
    branchLhsNames (Otherwise _) = Set.empty

    patternNames :: Pattern Name -> Set RawName
    patternNames (PatApp _ _ pats) = Set.unions (map patternNames pats)
    patternNames (PatVar _ n) = Set.singleton (rawName n)
    patternNames _ = Set.empty

-- ----------------------------------------------------------------------------
-- Cycle Detection for Computed Fields
-- ----------------------------------------------------------------------------

-- | Detect cycles in computed field dependencies within DECLARE blocks.
-- Returns a list of @(record type name, cycle of field names)@ for each cycle found.
detectComputedFieldCycles :: Module Name -> [(Name, [Name])]
detectComputedFieldCycles (MkModule _ _ section) = detectCFCSection section

detectCFCSection :: Section Name -> [(Name, [Name])]
detectCFCSection (MkSection _ _ _ topDecls) = concatMap detectCFCTopDecl topDecls

detectCFCTopDecl :: TopDecl Name -> [(Name, [Name])]
detectCFCTopDecl (Declare _ decl) = detectCFCDeclare decl
detectCFCTopDecl (Section _ section) = detectCFCSection section
detectCFCTopDecl _ = []

detectCFCDeclare :: Declare Name -> [(Name, [Name])]
detectCFCDeclare (MkDeclare _ _ appForm (RecordDecl _ _ tns))
  | any isComputed tns =
    let
      MkAppForm _ recordName _ _ = appForm
      -- All field names in this record (for filtering references)
      allFieldRawNames = Set.fromList [rawName fn | MkTypedName _ fn _ _ <- tns]
      -- Build SCC graph: (node=Name, key=RawName, deps=[RawName])
      graphData =
        [ (fn, rawName fn, Set.toList (exprFieldRefs allFieldRawNames e))
        | MkTypedName _ fn _ (Just e) <- tns
        ]
    in [ (recordName, cyc) | CyclicSCC cyc <- stronglyConnComp graphData ]
detectCFCDeclare _ = []

-- | Extract field name references from a MEANS expression.
-- Uses the 'Foldable' instance on 'Expr' to collect all names, then
-- intersects with the set of known field names in the record.
-- This is conservative (may over-approximate) but safe for cycle detection.
exprFieldRefs :: Set RawName -> Expr Name -> Set RawName
exprFieldRefs fieldNames expr =
  Set.fromList [rawName n | n <- toList expr, rawName n `Set.member` fieldNames]

-- ----------------------------------------------------------------------------
-- Extract Computed Field Names
-- ----------------------------------------------------------------------------

-- | Extract a map from record type names to their computed field names.
-- Runs on the original program (before desugaring) so computed fields
-- are still visible in the AST.
extractComputedFieldNames :: Module Name -> Map.Map RawName (Set RawName)
extractComputedFieldNames (MkModule _ _ section) = extractCFNSection section

extractCFNSection :: Section Name -> Map.Map RawName (Set RawName)
extractCFNSection (MkSection _ _ _ topDecls) =
  Map.unionsWith Set.union (map extractCFNTopDecl topDecls)

extractCFNTopDecl :: TopDecl Name -> Map.Map RawName (Set RawName)
extractCFNTopDecl (Declare _ (MkDeclare _ _ (MkAppForm _ recName _ _) (RecordDecl _ _ tns)))
  | any isComputed tns =
    let cfNames = Set.fromList [rawName fn | MkTypedName _ fn _ (Just _) <- tns]
    in Map.singleton (rawName recName) cfNames
extractCFNTopDecl (Section _ section) = extractCFNSection section
extractCFNTopDecl _ = Map.empty
