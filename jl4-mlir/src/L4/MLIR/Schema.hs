{-# LANGUAGE OverloadedStrings #-}

-- | Function schema emission for compiled L4 WASM modules.
--
-- A compiled @.wasm@ bundle is only useful to an external caller if
-- they know (a) what L4 functions are exported, (b) what parameter
-- names/types each function expects, and (c) how those JSON property
-- names map back to the WASM symbol table.
--
-- This module produces a sidecar @.schema.json@ file with *exactly*
-- the same parameter schema format the jl4-service HTTP API uses. A
-- compiled WASM module can therefore be dropped into any system that
-- already talks to jl4-service — the input/output JSON format is
-- identical.
--
-- Canonical sanitization and DEONTIC parameter injection match
-- @jl4-service/src/Shared.hs@ and @jl4-service/src/Compiler.hs@.
module L4.MLIR.Schema
  ( -- * Schema bundle
    WasmBundle (..)
  , FunctionExport (..)
  , StateGraphExport (..)
  , TraceMeta (..)
  , TraceNode (..)
  , bundleExports
  , applyDiagnostics
  , collectTraceNodes
  , lookupTypeAt

    -- * Name sanitization (must match jl4-service)
  , sanitizePropertyName
  , sanitizeFunctionName
  , sanitizeWasmSymbol

    -- * JSON emission
  , encodeBundle
  , writeBundleFile
  ) where

import Control.Applicative ((<|>))
import Data.Aeson ((.=), (.:), (.:?), (.!=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson.Key
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isAlphaNum)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text

import L4.Syntax
  ( Module(..), Resolved, Type'(..), Declare(..)
  , Decide(..)
  , AppForm(..)
  , Expr(..), Branch(..), GuardedExpr(..), NamedExpr(..)
  , TypeDecl(..), TypedName(..), ConDecl(..)
  , Info(..)
  , Section(..), TopDecl(..), LocalDecl(..)
  , TypeSig(..), GivenSig(..), GivethSig(..), OptionallyTypedName(..)
  , Pattern(..), BranchLhs(..)
  , rawName, rawNameToText, getActual, getUnique
  )
import L4.Annotation (HasSrcRange, rangeOf)
import L4.Parser.SrcSpan (SrcRange(..))
import L4.TypeCheck.Types (InfoMap)
import qualified L4.Utils.IntervalMap as IV
import L4.Export (ExportedFunction(..), ExportedParam(..), getExportedFunctions)
import qualified L4.Print as Print
import L4.StateGraph
  ( extractStateGraphs, stateGraphToDot, defaultStateGraphOptions
  , StateGraph(..)
  )
import L4.FunctionSchema
  ( Parameters(..), Parameter(..), declaresFromModule, typeToParameter
  )
import L4.TypeCheck.Environment (contractUnique)

-- ---------------------------------------------------------------------------
-- Bundle types
-- ---------------------------------------------------------------------------

-- | One exported L4 function in the compiled WASM module.
--
-- @apiName@ is the sanitized name that external callers use in JSON
-- (spaces → hyphens, collapsed, stripped). @wasmSymbol@ is the actual
-- C-style symbol exported from the @.wasm@ binary (alphanumeric +
-- underscore only, since wasm-ld names must be valid linker symbols).
data FunctionExport = FunctionExport
  { apiName     :: !Text
  , wasmSymbol  :: !Text
  , description :: !Text
  , parameters  :: !Parameters
  , returnType  :: !Text           -- ^ Display string (e.g., "NUMBER", "BOOLEAN", "DEONTIC")
  , isDeontic   :: !Bool
  , paramOrder  :: ![Text]         -- ^ API names in declaration order (for positional marshal)
  , supported   :: !Bool           -- ^ Can the WASM module faithfully evaluate this function?
  , unsupportedReason :: !(Maybe Text)
    -- ^ When 'supported' is 'False', why — e.g. the body contains a
    -- regulative/deontic/IO construct the backend can't compile. The
    -- proxy routes such functions to a fallback evaluator instead of
    -- calling the WASM. 'Nothing' (and @supported = True@) is the
    -- normal, fully-compilable case.
  , traceMeta   :: !(Maybe TraceMeta)
    -- ^ M5 slice 2A: pre-rendered 'L4.Print.prettyLayout' strings the
    -- runtime needs to synthesise a 'Reasoning' tree at @?trace=full@
    -- time. The runtime never re-runs the pretty-printer; everything
    -- byte-identity-relevant is baked here at compile time. 'Nothing'
    -- for unsupported functions and any function we can't pretty-print.
  }

-- | Per-function pretty-printed strings for the M5 trace tree. These
-- mirror what 'L4.Print.prettyLayout' would emit on the corresponding
-- 'Expr' / 'Name' on jl4-service's interpreter side, so we can match
-- 'traceToReasoning' output byte-for-byte without re-implementing the
-- pretty-printer in JavaScript.
data TraceMeta = TraceMeta
  { tmFnNamePretty  :: !Text   -- ^ Function name with backticks if needed (e.g. @\`is eligible\`@).
  , tmBodyPretty    :: !Text   -- ^ Pretty-printed function body — the source the trace shows.
  , tmParamPretties :: ![Text] -- ^ Pretty-printed parameter references, in declaration order.
  , tmNodes         :: ![TraceNode] -- ^ M5 slice 2C — per-subexpression trace metadata, indexed by 'tnId'.
  , tmFnValueNodeId :: !Int
    -- ^ M5 slice 4A — id of the synthetic "fn-value" trace node (the
    -- one rendered as @Result: \<function\>@ in jl4-service). The
    -- instrumented @<fn>$trace@ codegen emits a single
    -- @__l4_trace_enter(this id)@ / @__l4_trace_exit(0, 4)@ pair at the
    -- function entry, so every call site — top-level or nested — gets
    -- the fn-value as a sibling of the body trace, matching
    -- @traceToReasoning@'s @[arg-evals, fn-value, body]@ shape.
  }

-- | One trace-tree node's compile-time metadata. The lowering's
-- @<fn>$trace@ variant calls @__l4_trace_enter(tnId)@ and
-- @__l4_trace_exit(box, tnResultKind)@ around each traceable subexpression
-- in pre-order traversal; the runtime uses these to build the Reasoning
-- tree, looking up 'tnExampleCode' for each node by ID.
--
-- 'tnResultKind' matches the kind constants in @renderTraceResult@ in
-- @runtime/jl4-runtime.mjs@:
--   0 = NUMBER (rational handle) ; 1 = BOOLEAN (0/1) ;
--   2 = STRING (f64-boxed pointer) ; 3 = OTHER (best-effort).
data TraceNode = TraceNode
  { tnId          :: !Int
  , tnExampleCode :: !Text
  , tnResultKind  :: !Int
  , tnSpecial     :: !(Maybe Text)
    -- ^ M5 slice 4A — flags that ask the runtime to inject a synthetic
    -- desugar-shaped sub-tree to match jl4-service's @traceToReasoning@.
    --
    --   * @\"AND\"@ — @__AND__ a b@ desugars to @IF a THEN b ELSE FALSE@;
    --     the runtime appends an IF sub-tree and applies short-circuit
    --     filtering (drops the rhs child when lhs is FALSE).
    --   * @\"OR\"@ — @__OR__ a b@ desugars to @IF a THEN TRUE ELSE b@;
    --     symmetric — drops rhs when lhs is TRUE.
    --   * @\"NOT\"@ — @__NOT__ a@ desugars to @IF a THEN FALSE ELSE TRUE@.
    --   * @\"FN_VALUE\"@ — synthetic fn-value frame emitted at the
    --     start of every @<fn>$trace@; renders as @Result: \<function\>@.
    --   * @\"PROJ\"@ (M5 slice 4G) — property selector @record's
    --     field@; the runtime synthesises a 4-level @field OF record@ /
    --     fn-value / @CONSIDER … THEN field@ / bound-var sub-tree
    --     matching jl4-core's desugaring of @Proj@.
    --
    -- 'Nothing' for ordinary expressions (no desugar injection needed).
  , tnProj        :: !(Maybe ProjData)
    -- ^ M5 slice 4G — pre-baked strings the runtime needs to render
    -- the @Proj@ desugar sub-tree. Only set when 'tnSpecial' is @Just
    -- \"PROJ\"@.
  , tnReturnSchema :: !(Maybe RetSchema)
    -- ^ M5 — minimal type tree describing this subexpression's result.
    -- Lets the runtime walk wasm memory and reconstruct compound
    -- results (LIST, MAYBE, records) the body allocates inline —
    -- without this, the trace shows a raw pointer like @5.494e-321@.
    -- Only populated when 'tnResultKind' is 3 (compound / @OTHER@);
    -- 'Nothing' otherwise to keep the per-node JSON compact.
    --
    -- Wraps the 'Parameter' shape in an explicit @RSMaybe@ constructor
    -- because the standard @typeToParameter@ collapses @MAYBE T@ to
    -- @T@ (the JSON-schema-style \"optional field\" convention), but
    -- the runtime needs to know whether to decode a MAYBE block or a
    -- bare scalar.
  , tnBindingLabel :: !(Maybe Text)
    -- ^ M5 — when set, this node wraps a @WHERE@ / @LET IN@ binding's
    -- evaluation. The runtime prepends the label to the trace
    -- node's @exampleCode@ array (so the wire shape becomes
    -- @[\"income\", \"taxable income OF tp\"]@), matching jl4-core's
    -- @labelExample (Just resolved)@ in @traceToReasoning@.
  }

-- | M5 — minimal return-type tree the runtime walker consumes to
-- decode wasm-allocated compound results into JS values. Mirrors the
-- pieces of @Parameter@ that affect layout, plus an explicit
-- 'RSMaybe' wrapper that @typeToParameter@ otherwise collapses away.
--
-- Layout the runtime expects at the pointer:
--   * @RSScalar t@      — the raw f64 IS the value (decoded by kind).
--   * @RSMaybe inner@   — 16 bytes: tag (8B, 0 = NOTHING) + payload (8B).
--   * @RSList items@    — cons cells: { item (8B), tail (8B) }, NULL = end.
--   * @RSRecord ord f@  — @length ord * 8@ bytes; field @ord[i]@ lives at
--                         offset @i*8@; type per @lookup ord[i] f@.
data RetSchema
  = RSScalar !Text                         -- ^ "number", "boolean", "string", "date", "time", "datetime"
  | RSEnum !Text ![Text]                   -- ^ JSON type + enum members
  | RSMaybe !RetSchema
  | RSList !RetSchema
  | RSRecord !Text ![Text] !(Map Text RetSchema)
    -- ^ record type-name, field order, per-field schema

-- | M5 slice 4G — every @Proj@ trace node carries the strings its
-- desugared sub-tree needs. All of them are computed at compile time
-- via @Print.prettyLayout@ + a walk of the record type's declared
-- field list, so the runtime never has to re-pretty-print anything.
data ProjData = ProjData
  { pdAppForm     :: !Text   -- ^ e.g. @\"attendees OF order\"@
  , pdFieldName   :: !Text   -- ^ e.g. @\"attendees\"@ or @\"\`event capacity\`\"@
  , pdConsiderEx  :: !Text   -- ^ pre-baked CONSIDER body — multi-line indented form jl4-core emits
  }

-- | Complete schema for a compiled @.wasm@ module.
data WasmBundle = WasmBundle
  { bundleWasmFile :: !Text        -- ^ Relative path to the .wasm binary
  , bundleVersion  :: !Text        -- ^ Content-derived version hash
  , bundleExports  :: ![FunctionExport]
  , bundleStateGraphs :: ![StateGraphExport]
    -- ^ Precomputed state graphs for the module (M3). Empty unless the
    -- module has regulative rules.
  , bundleHelpers :: ![(Text, TraceMeta)]
    -- ^ M5 — TraceMeta for every non-@export'd 'Decide' that the
    -- lowering still emits a @<fn>$trace@ variant for. The runtime's
    -- per-frame node lookup keys off @wasmSymbol@; without this map,
    -- nested calls from an exported function into a helper resolve
    -- their node IDs against the caller's table and render garbage.
    -- Key is the helper's sanitized wasm symbol; the order is the
    -- module's source order so the JSON is stable across builds.
  }

-- | A precomputed state graph, baked into the schema at compile time.
--
-- State graphs are a pure function of the regulative structure of the
-- AST, so we extract and render them (including the Graphviz DOT) here
-- — byte-identical to what jl4-service produces at request time via
-- @StateGraph.stateGraphToDot defaultStateGraphOptions@ — and the proxy
-- serves the @state-graphs@ routes straight from this, with no runtime
-- evaluation. State graphs are module-level (keyed by their own name),
-- exactly as jl4-service treats them.
data StateGraphExport = StateGraphExport
  { sgeName :: !Text   -- ^ Graph name (= jl4-service's @StateGraphInfo.graphName@)
  , sgeDot  :: !Text   -- ^ Graphviz DOT, byte-identical to the service's single-graph route
  }

-- ---------------------------------------------------------------------------
-- Name sanitization — must match jl4-service exactly
-- ---------------------------------------------------------------------------

-- | Sanitize a property name for use as a JSON property key.
-- Matches jl4-service\/src\/Shared.hs exactly.
sanitizePropertyName :: Text -> Text
sanitizePropertyName name =
  let s = Text.map (\c -> if isAlphaNum c || c == '_' || c == '.' || c == '-' then c else '-') name
      s' = collapseHyphens $ Text.dropWhile (== '-') $ Text.dropWhileEnd (== '-') s
  in if Text.null s' then "_unnamed" else s'
 where
  collapseHyphens t =
    let collapsed = Text.replace "--" "-" t
    in if collapsed == t then t else collapseHyphens collapsed

-- | Sanitize a function name for use as an API/HTTP identifier.
-- Same rule as property names (spaces → hyphens, collapsed).
sanitizeFunctionName :: Text -> Text
sanitizeFunctionName = sanitizePropertyName

-- | Sanitize a name for use as a WASM export symbol (@func.func \@name@).
-- WASM/LLVM linker symbols must be C identifiers: alphanumeric + underscore,
-- not starting with a digit. We replace the sanitized property name's
-- hyphens with underscores to produce a valid symbol.
sanitizeWasmSymbol :: Text -> Text
sanitizeWasmSymbol name =
  let sanitized = sanitizePropertyName name
      withUnderscores = Text.map (\c -> if c == '-' || c == '.' then '_' else c) sanitized
  in if Text.null withUnderscores
       then "_unnamed"
       else case Text.uncons withUnderscores of
         Just (c, _) | not (isAlphaNum c) && c /= '_' -> "_" <> withUnderscores
         Just (c, _) | c >= '0' && c <= '9' -> "_" <> withUnderscores
         _ -> withUnderscores

-- ---------------------------------------------------------------------------
-- Bundle construction from a resolved L4 module
-- ---------------------------------------------------------------------------

-- | Build the schema bundle for all @\@export@\-ed functions in a module.
-- The bundle is then serialized to disk as @.schema.json@.
--
-- 'InfoMap' is threaded in to seed M5's per-subexpression trace metadata
-- ('TraceMeta.tmNodes') — its result-kind classification queries each
-- expression's type at request-pretty-printing time. Older call sites
-- that don't have an 'InfoMap' should use 'bundleExportsNoInfo' (which
-- passes 'Map.empty') and accept best-effort syntactic result kinds.
bundleExports :: Text -> Text -> InfoMap -> Module Resolved -> [Module Resolved] -> WasmBundle
bundleExports wasmPath version infoMap resolvedModule depModules =
  -- Merge DECLAREs from imported modules so record/enum types defined in
  -- IMPORTed files are visible when expanding parameter schemas. The main
  -- module's declares take precedence on key collision.
  let declares = Map.unions (declaresFromModule resolvedModule
                            : map declaresFromModule depModules)
      exports  = getExportedFunctions resolvedModule
      stateGraphs =
        [ StateGraphExport sg.sgName (stateGraphToDot defaultStateGraphOptions sg)
        | sg <- extractStateGraphs resolvedModule
        ]
      exportedSymbols = Set.fromList
        [ sanitizeWasmSymbol ef.exportName | ef <- exports ]
      -- Walk dependency modules (e.g. the prelude) too — they contribute
      -- helpers that the main module's body can call, and the lowering
      -- emits @<fn>$trace@ for each. Main-module decides take precedence
      -- on key collision (later entries overwrite earlier in the Map).
      --
      -- Known limitation: prelude helpers that share a source name
      -- (e.g. several WHERE-local @go@ functions across @count@,
      -- @foldr@, @foldl@, @reverse@, @sum@) collide under their
      -- sanitized symbol. The wasm side disambiguates via arity
      -- mangling after closure conversion (so the body's call site
      -- and the func.func definition agree on @go__2@/@go__3@); the
      -- schema can't replicate that without rerunning the lift, so
      -- the runtime will pick whichever helper was last in source
      -- order for the colliding key. Worst case: a helper's trace
      -- renders with another helper's text — text-only divergence
      -- past byte ~1500 in tickets-available / waitlist-position.
      -- Pair each Decide with its POST-CLOSURE arity (= the SSA arity
      -- Lower.hs's @lowerLocalDecl@ assigns after closure conversion).
      -- The lowering's @dedupAndSynthExterns@ mangles overloaded names
      -- by this exact arity; mirroring it here lets the schema build a
      -- @helperTraceMeta@ key the runtime can disambiguate on.
      allHelperDecideArities =
        concatMap collectAllDecidesWithArity depModules
        ++ collectAllDecidesWithArity resolvedModule
      helperDecides =
        [ (sym, d)
        | (d, arity) <- allHelperDecideArities
        , let bareSym = decideWasmSymbol d
              -- Always-mangle convention. Lower.hs's
              -- @__l4_trace_enter_fn@ uses the same key shape so the
              -- runtime's @nodesByFn@ lookup matches per-helper, even
              -- when several Decides share a sanitized source name
              -- (e.g. the prelude's @go@ in @foldr@'s WHERE versus
              -- @count@'s WHERE — different post-closure arities, so
              -- different mangled keys).
              sym = bareSym <> "__" <> Text.pack (show arity)
        , not (Set.member bareSym exportedSymbols)
        ]
      -- Function name → GIVETH type, gathered across every @Decide@ in
      -- the module + its deps. Used by 'collectTraceNodes' to classify
      -- the result kind of a call @App f args@ when 'infoMap' has no
      -- entry — happens for prelude bodies whose source ranges aren't
      -- in the main module's typechecker info. Same wasm-side dedup
      -- rule applies: when two Decides share a sanitized name, the
      -- FIRST in source order wins (matches 'shouldSkip').
      allFnDecides =
        concatMap collectAllDecides depModules
        ++ collectAllDecides resolvedModule
        ++ map (.exportDecide) exports
      fnReturnTypes :: Map Text (Type' Resolved)
      fnReturnTypes = Map.fromListWith (\_new old -> old)
        [ (rawNameToText (rawName (getActual headRes)), retTy)
        | MkDecide _ (MkTypeSig _ _ (Just (MkGivethSig _ retTy))) appF _ <- allFnDecides
        , let MkAppForm _ headRes _ _ = appF
        ]
      -- Decides with no @GIVETH@: their inferred return type defaults
      -- to NUMBER in the Lowering ('retType' falls back to
      -- 'l4BoolType' when the typeSig has no giveth, but most prelude
      -- helpers without a GIVETH are number-shaped — count's @go@,
      -- @at@'s recursive helper, etc.). Mirror that default here so
      -- a call into these helpers renders as kind 0 instead of an
      -- unrenderable raw-pointer kind 3.
      unannotatedFnNames :: Set.Set Text
      unannotatedFnNames = Set.fromList
        [ rawNameToText (rawName (getActual headRes))
        | MkDecide _ (MkTypeSig _ _ Nothing) appF _ <- allFnDecides
        , let MkAppForm _ headRes _ _ = appF
        ]
      -- 'Map.fromList' keeps the LAST entry per key, but the wasm
      -- side's 'registerDependencyModule' / 'lowerDecide' dedup keeps
      -- the FIRST: a second @Decide@ with a sanitized name that's
      -- already in 'funcSigs' gets skipped. Mirror that so the schema's
      -- metadata for a colliding helper (e.g. the two @max@s — one
      -- NUMBER, one MAYBE NUMBER) describes whichever wasm function
      -- actually runs, not the one whose definition came later.
      -- Match Lower.hs's @dedupByName@ semantics. The @s.functions@
      -- list is accumulated newest-first by @lowerDecide@ but
      -- @lowerModuleWithDepsInfo@ reverses it before
      -- @dedupAndSynthExterns@ sees it, so @dedupByName@ scans in
      -- source-chronological order keeping the FIRST occurrence
      -- per mangled name. Mirror by keeping the FIRST helperDecide
      -- per (mangled) key. Combined with @hasFunParam@ filtering
      -- (Lower skips Decides whose signature has a function-typed
      -- param), this matches which @go__N@ body actually exists
      -- at the wasm side.
      helperMetas =
        Map.toList $ Map.fromListWith (\_new old -> old)
          [ (sym, buildTraceMetaFromDecide infoMap declares fnReturnTypes unannotatedFnNames [] d)
          | (sym, d) <- helperDecides
          ]
  in WasmBundle
       { bundleWasmFile = wasmPath
       , bundleVersion  = version
       , bundleExports  = map (buildExport infoMap declares fnReturnTypes unannotatedFnNames) exports
       , bundleStateGraphs = stateGraphs
       , bundleHelpers     = helperMetas
       }

-- | M5 — every 'Decide' in the module tree, paired with its
-- "post-closure arity" — the SSA-level arity Lower.hs's
-- 'lowerLocalDecl' would assign after closure conversion. Top-level
-- Decides have arity equal to their source param count (no captures).
-- Nested @WHERE@ / @LET IN@ locals add captures: free vars in the
-- body that resolve to a binding visible at the local's definition
-- point (= the parent function's params + ancestors' params +
-- previously-defined siblings).
--
-- This matches the arity Lower's @dedupAndSynthExterns@ uses to
-- mangle overloaded names, so the schema can build the same
-- @helperTraceMeta@ key the runtime's @__l4_trace_enter_fn@ pushes.
collectAllDecidesWithArity :: Module Resolved -> [(Decide Resolved, Int)]
collectAllDecidesWithArity (MkModule _ _ section) = goSection topNames section
  where
    topNames = Set.fromList
      [ rawNameToText (rawName (getActual h))
      | d <- topLevelDecides
      , let MkDecide _ _ (MkAppForm _ h _ _) _ = d
      ]
    topLevelDecides = goSectionTop section
      where
        goSectionTop (MkSection _ _ _ decls) = concatMap goDeclTop decls
        goDeclTop = \case
          Decide _ d  -> [d]
          Section _ s -> goSectionTop s
          _           -> []

    goSection :: Set.Set Text -> Section Resolved -> [(Decide Resolved, Int)]
    goSection scope (MkSection _ _ _ decls) = concatMap (goDecl scope) decls

    goDecl :: Set.Set Text -> TopDecl Resolved -> [(Decide Resolved, Int)]
    goDecl scope = \case
      Decide _ d
        | hasFunParam d ->
            -- Skip: this Decide is registered as an extern by Lower
            -- and its WHERE locals never become wasm functions.
            []
        | otherwise ->
            let arity = decideArity scope d
            in (d, arity) : goDecideBody scope d
      Section _ s -> goSection scope s
      _           -> []

    -- Mirror Lower's @sigHasFunctionParam@ check — Decides with
    -- a function-typed parameter are NEVER lowered as a body in the
    -- wasm output (Lower's @registerDependencyModule@ calls
    -- @registerExternDecide@ for them). Their nested @WHERE@ helpers
    -- never become wasm functions either, since Lower never enters
    -- the body. Schema must skip them too — otherwise
    -- @helperTraceMeta@ describes helpers that don't exist at the
    -- wasm level and runtime lookups for the wasm helpers that DO
    -- exist (e.g. @count@'s @go__2@) land on the wrong entry
    -- (foldr's @go@, also at the schema's arity 2 — though only
    -- because Lower's @freeVarsOfExpr@ doesn't catch @cons@ as a
    -- captured variable in @cons OF x, go xs@).
    hasFunParam :: Decide Resolved -> Bool
    hasFunParam (MkDecide _ (MkTypeSig _ (MkGivenSig _ ps) _) _ _) =
      any optionallyTypedParamIsFun ps

    optionallyTypedParamIsFun :: OptionallyTypedName Resolved -> Bool
    optionallyTypedParamIsFun (MkOptionallyTypedName _ _ mty) =
      case mty of
        Just ty -> isFunctionType ty
        Nothing -> False

    isFunctionType :: Type' Resolved -> Bool
    isFunctionType = \case
      Fun{}      -> True
      Forall _ _ inner -> isFunctionType inner
      _          -> False

    -- Captures-aware arity for a Decide: source params + free vars in
    -- the body that're visible in @scope@ (excluding the Decide's own
    -- params, top-level names, and its own name for recursion).
    decideArity :: Set.Set Text -> Decide Resolved -> Int
    decideArity scope (MkDecide _ _ (MkAppForm _ headRes params _) body) =
      let paramSet = Set.fromList (map (rawNameToText . rawName . getActual) params)
          selfName = rawNameToText (rawName (getActual headRes))
          -- Include top-level names in the "bound" set passed to
          -- 'freeVarsOfExpr' so a reference to another top-level
          -- Decide (e.g. @single brackets@ inside @brackets for
          -- status@'s body) doesn't show up as a free variable and
          -- get miscounted as a captured closure parameter. Lower's
          -- @lowerLocalDecl@ does the same: @knownTopLevels@ is
          -- folded into the bound set before captures are computed.
          freeInBody = freeVarsOfExpr body
            (paramSet <> Set.singleton selfName <> topNames)
          captures = Set.toList (Set.intersection freeInBody scope)
      in length params + length captures

    goDecideBody :: Set.Set Text -> Decide Resolved -> [(Decide Resolved, Int)]
    goDecideBody scope (MkDecide _ _ (MkAppForm _ _ params _) body) =
      let paramSet = Set.fromList (map (rawNameToText . rawName . getActual) params)
          innerScope = scope <> paramSet
      in goExpr innerScope body

    goExpr :: Set.Set Text -> Expr Resolved -> [(Decide Resolved, Int)]
    goExpr scope e = case e of
      Where _ b locals    -> goExpr scope b ++ goLocals scope locals
      LetIn _ locals b    -> goLocals scope locals ++ goExpr scope b
      App _ _ args        -> concatMap (goExpr scope) args
      AppNamed _ _ ns _   -> concatMap (\(MkNamedExpr _ _ ex) -> goExpr scope ex) ns
      And _ a b           -> goExpr scope a ++ goExpr scope b
      Or _ a b            -> goExpr scope a ++ goExpr scope b
      RAnd _ a b          -> goExpr scope a ++ goExpr scope b
      ROr _ a b           -> goExpr scope a ++ goExpr scope b
      Implies _ a b       -> goExpr scope a ++ goExpr scope b
      Equals _ a b        -> goExpr scope a ++ goExpr scope b
      Not _ a             -> goExpr scope a
      Plus _ a b          -> goExpr scope a ++ goExpr scope b
      Minus _ a b         -> goExpr scope a ++ goExpr scope b
      Times _ a b         -> goExpr scope a ++ goExpr scope b
      DividedBy _ a b     -> goExpr scope a ++ goExpr scope b
      Modulo _ a b        -> goExpr scope a ++ goExpr scope b
      Cons _ a b          -> goExpr scope a ++ goExpr scope b
      Leq _ a b           -> goExpr scope a ++ goExpr scope b
      Geq _ a b           -> goExpr scope a ++ goExpr scope b
      Lt _ a b            -> goExpr scope a ++ goExpr scope b
      Gt _ a b            -> goExpr scope a ++ goExpr scope b
      Proj _ r _          -> goExpr scope r
      IfThenElse _ c t f  -> goExpr scope c ++ goExpr scope t ++ goExpr scope f
      MultiWayIf _ gs e2  ->
        goExpr scope e2 ++ concatMap (\(MkGuardedExpr _ g b) -> goExpr scope g ++ goExpr scope b) gs
      Consider _ scr brs  ->
        goExpr scope scr ++ concatMap (\(MkBranch _ _ b) -> goExpr scope b) brs
      Lam _ _ b           -> goExpr scope b
      List _ es           -> concatMap (goExpr scope) es
      _                   -> []

    goLocals :: Set.Set Text -> [LocalDecl Resolved] -> [(Decide Resolved, Int)]
    goLocals _ [] = []
    goLocals scope (l:ls) = case l of
      LocalDecide _ d ->
        let arity = decideArity scope d
            selfName = case d of
              MkDecide _ _ (MkAppForm _ h _ _) _ ->
                rawNameToText (rawName (getActual h))
            scopeWithSelf = Set.insert selfName scope
        in (d, arity) : goDecideBody scope d ++ goLocals scopeWithSelf ls
      LocalAssume _ _ -> goLocals scope ls

-- Kept for back-compat with callers that don't need arity info —
-- they should migrate to 'collectAllDecidesWithArity'.
collectAllDecides :: Module Resolved -> [Decide Resolved]
collectAllDecides m = map fst (collectAllDecidesWithArity m)

-- Mirror of 'Lower.freeVarsOfExpr' — kept verbatim so Schema and
-- Lower agree on which variables a body captures. (Lower can't
-- export this without inducing an import cycle: Lower already
-- imports Schema for the bundle types.)
freeVarsOfExpr :: Expr Resolved -> Set.Set Text -> Set.Set Text
freeVarsOfExpr expr0 bound0 = go bound0 expr0
  where
    go bound = \case
      App _ n [] ->
        let nm = rawNameToText (rawName (getActual n))
        in if nm `Set.member` bound
             then Set.empty
             else Set.singleton nm
      App _ _ args -> Set.unions (map (go bound) args)
      AppNamed _ _ named _ ->
        Set.unions [go bound e | MkNamedExpr _ _ e <- named]
      And _ a b        -> Set.union (go bound a) (go bound b)
      Or _ a b         -> Set.union (go bound a) (go bound b)
      RAnd _ a b       -> Set.union (go bound a) (go bound b)
      ROr _ a b        -> Set.union (go bound a) (go bound b)
      Implies _ a b    -> Set.union (go bound a) (go bound b)
      Equals _ a b     -> Set.union (go bound a) (go bound b)
      Not _ a          -> go bound a
      Plus _ a b       -> Set.union (go bound a) (go bound b)
      Minus _ a b      -> Set.union (go bound a) (go bound b)
      Times _ a b      -> Set.union (go bound a) (go bound b)
      DividedBy _ a b  -> Set.union (go bound a) (go bound b)
      Modulo _ a b     -> Set.union (go bound a) (go bound b)
      Exponent _ a b   -> Set.union (go bound a) (go bound b)
      Cons _ a b       -> Set.union (go bound a) (go bound b)
      Leq _ a b        -> Set.union (go bound a) (go bound b)
      Geq _ a b        -> Set.union (go bound a) (go bound b)
      Lt _ a b         -> Set.union (go bound a) (go bound b)
      Gt _ a b         -> Set.union (go bound a) (go bound b)
      Proj _ r _       -> go bound r
      IfThenElse _ c t e -> Set.unions [go bound c, go bound t, go bound e]
      MultiWayIf _ guards e -> Set.unions (go bound e : [Set.union (go bound g) (go bound b) | MkGuardedExpr _ g b <- guards])
      Consider _ s branches ->
        Set.union (go bound s) $ Set.unions
          [ go (bound <> patternBindNames p) b | MkBranch _ (When _ p) b <- branches ]
          <> Set.unions [ go bound b | MkBranch _ (Otherwise _) b <- branches ]
      Where _ b locals ->
        let (newBound, localFrees) = foldl stepLocal (bound, Set.empty) locals
        in Set.union (go newBound b) localFrees
      LetIn _ locals b ->
        let (newBound, localFrees) = foldl stepLocal (bound, Set.empty) locals
        in Set.union (go newBound b) localFrees
      Percent _ a   -> go bound a
      List _ xs     -> Set.unions (map (go bound) xs)
      Concat _ xs   -> Set.unions (map (go bound) xs)
      AsString _ a  -> go bound a
      Lam _ (MkGivenSig _ lamParams) b ->
        go (bound <> Set.fromList [rawNameToText (rawName (getActual p)) | MkOptionallyTypedName _ p _ <- lamParams]) b
      _ -> Set.empty

    stepLocal (bnd, frees) (LocalDecide _ (MkDecide _ _ af bdy)) =
      let MkAppForm _ h ps _ = af
          nm = rawNameToText (rawName (getActual h))
          inner = go (bnd <> Set.fromList (map (rawNameToText . rawName . getActual) ps)) bdy
      in (Set.insert nm bnd, frees <> inner)
    stepLocal (bnd, frees) (LocalAssume _ _) = (bnd, frees)

    patternBindNames :: Pattern Resolved -> Set.Set Text
    patternBindNames = \case
      PatVar _ n -> Set.singleton (rawNameToText (rawName (getActual n)))
      PatApp _ _ pats -> Set.unions (map patternBindNames pats)
      PatCons _ h t -> Set.union (patternBindNames h) (patternBindNames t)
      _ -> Set.empty

decideWasmSymbol :: Decide Resolved -> Text
decideWasmSymbol (MkDecide _ _ (MkAppForm _ headRes _ _) _) =
  sanitizeWasmSymbol (rawNameToText (rawName (getActual headRes)))

buildExport :: InfoMap -> Map Text (Declare Resolved) -> Map Text (Type' Resolved) -> Set.Set Text -> ExportedFunction -> FunctionExport
buildExport infoMap declares fnReturnTypes unannotatedFns ef =
  let name        = ef.exportName
      isDeonticFn = case ef.exportReturnType of
        Just ty -> isDeonticType ty
        Nothing -> False

      -- Build the parameter map. Keys stay in their original L4 form
      -- (spaces, backticks, etc.) exactly like jl4-service does, so
      -- @remapArguments@ can translate sanitized incoming keys back to
      -- original ones at request time.
      baseParams  = buildParamsFromExported declares ef.exportParams
      paramsWithDeontic = if isDeonticFn
        then addDeonticParameters ef.exportReturnType baseParams
        else baseParams

      -- Parameter order in declaration order, using *original* L4 names.
      paramOrder_ = map (.paramName) ef.exportParams
                 ++ (if isDeonticFn then ["startTime", "events"] else [])
      -- M5 slice 2A: bake the pretty-printed strings the runtime needs
      -- to synthesise a 'Reasoning' tree on @?trace=full@. We pretty-print
      -- the function's name, body, and parameter references via the same
      -- 'L4.Print.prettyLayout' jl4-service uses, so the trace strings
      -- match byte-for-byte. Slice 2A populates the outer shell; slice 2B
      -- adds per-subexpression node IDs alongside.
      traceMeta_ = buildTraceMeta infoMap declares fnReturnTypes unannotatedFns ef
  in FunctionExport
       { apiName     = sanitizeFunctionName name
       , wasmSymbol  = sanitizeWasmSymbol name
       , description = Text.strip ef.exportDescription
       , parameters  = paramsWithDeontic
       , returnType  = returnTypeDisplay ef.exportReturnType
       , isDeontic   = isDeonticFn
       , paramOrder  = paramOrder_
       -- Assume compilable; 'applyDiagnostics' downgrades functions the
       -- lowering flagged as unsupported.
       , supported   = True
       , unsupportedReason = Nothing
       , traceMeta   = Just traceMeta_
       }

-- | Build the pretty-printed strings the M5 trace tree needs.
-- For deontic / unsupported functions we still populate this — the JSON
-- consumer can ignore it. Body extraction goes through the 'Decide' AST:
-- 'MkDecide _ _ appForm body' — we just pull out @body@.
buildTraceMeta :: InfoMap -> Map Text (Declare Resolved) -> Map Text (Type' Resolved) -> Set.Set Text -> ExportedFunction -> TraceMeta
buildTraceMeta infoMap declares fnReturnTypes unannotatedFns ef =
  let paramPs = [ p.paramName | p <- ef.exportParams ]
  in buildTraceMetaFromDecide infoMap declares fnReturnTypes unannotatedFns paramPs ef.exportDecide

-- | TraceMeta for a 'Decide' without an enclosing 'ExportedFunction'.
-- Used for non-@export'd helpers (WHERE/LET locals and top-level helpers)
-- so the runtime can resolve node IDs against the helper's own table
-- when an instrumented body calls into it. Pretty-prints param names off
-- the source AppForm — closure-converted variants share the same head
-- name, so the wasmSymbol agrees with what the lowering emits.
buildTraceMetaFromDecide
  :: InfoMap
  -> Map Text (Declare Resolved)
  -> Map Text (Type' Resolved)
    -- ^ Function name → GIVETH type, for App-call kind classification.
  -> Set.Set Text
    -- ^ Names of @Decide@s lacking GIVETH; passed through to
    -- 'collectTraceNodes' so calls into them default to NUMBER.
  -> [Text]               -- ^ Pretty-printed param names; @[]@ asks us to derive them from the AppForm.
  -> Decide Resolved
  -> TraceMeta
buildTraceMetaFromDecide infoMap declares fnReturnTypes unannotatedFns paramHint (MkDecide _ typeSig appForm body) =
  let MkAppForm _ headRes paramRes _ = appForm
      fnNameP = Print.prettyLayout headRes
      bodyP   = Print.prettyLayout body
      paramPs = case paramHint of
        [] -> map Print.prettyLayout paramRes
        ps -> ps
      -- Param name → source-level L4 type. Lets 'collectTraceNodes'
      -- classify a Var reference (App with no args) by its declared
      -- type — useful inside prelude bodies whose ranges aren't in
      -- 'infoMap', so 'lookupTypeAt' on the Var returns Nothing and
      -- the syntactic fallback can't tell NUMBER from anything else.
      paramTypeMap =
        let MkTypeSig _ (MkGivenSig _ ps) _ = typeSig
        in Map.fromList
             [ (rawNameToText (rawName (getActual n)), ty)
             | MkOptionallyTypedName _ n (Just ty) <- ps
             ]
      unannotatedParamSet =
        let MkTypeSig _ (MkGivenSig _ ps) _ = typeSig
        in Set.fromList
             [ rawNameToText (rawName (getActual n))
             | MkOptionallyTypedName _ n Nothing <- ps
             ]
      (bodyNodes, _rangeMap) = collectTraceNodes infoMap declares paramTypeMap fnReturnTypes unannotatedParamSet unannotatedFns body
      fnValueId   = length bodyNodes
      fnValueNode = TraceNode
        { tnId          = fnValueId
        , tnExampleCode = fnNameP
        , tnResultKind  = 4
        , tnSpecial     = Just "FN_VALUE"
        , tnProj        = Nothing
        , tnReturnSchema = Nothing
        , tnBindingLabel = Nothing
        }
      nodes = bodyNodes ++ [fnValueNode]
  in TraceMeta
       { tmFnNamePretty  = fnNameP
       , tmBodyPretty    = bodyP
       , tmParamPretties = paramPs
       , tmNodes         = nodes
       , tmFnValueNodeId = fnValueId
       }

-- | Walk a function body in pre-order, allocating a 'TraceNode' for each
-- traceable subexpression. Skips 'Lit' and zero-argument @App@ (the
-- 'Var' pattern) — both are dropped by jl4-service's @simplifyEvalTrace@.
-- The walk is deterministic and stable across calls, so the lowering
-- (which re-runs it to emit @__l4_trace_enter(id)@ in '<fn>$trace') and
-- the schema (which uses 'tnExampleCode' at request time) agree on IDs.
--
-- Returns @(nodesInIdOrder, rangeMap)@ where @rangeMap[range]@ is the
-- @(id, resultKind)@ for the expression at that source range.
collectTraceNodes
  :: InfoMap
  -> Map Text (Declare Resolved)
  -> Map Text (Type' Resolved)
    -- ^ Source-level L4 type per param name. Used to classify a
    -- @Var@ reference (App with no args) by its declared type when
    -- 'infoMap' has no exact-range entry — common inside prelude
    -- bodies. Pass 'Map.empty' from call sites that don't have a
    -- function context (the lowering's IDs-only walk).
  -> Map Text (Type' Resolved)
    -- ^ Source-level L4 return type per @Decide@ name in the module
    -- (and dependencies). Used to classify the result kind of a call
    -- @App f args@ when 'infoMap' has no exact-range entry — same
    -- coverage gap as @paramTypes@ but for @taxable income tp@,
    -- @max 0 x@, etc. The lowering passes the same map so its
    -- @rangeMap@-driven trace-exit kinds agree with the schema's.
  -> Set.Set Text
    -- ^ Names of un-annotated local parameters of the current
    -- @Decide@. The Lowering ('paramToMLIR') defaults these to
    -- NUMBER; mirror that here so a force-traced @Var@ leaf inside
    -- a prelude helper (e.g. @acc@ in @go acc l MEANS …@) renders
    -- as a NUMBER instead of an unrenderable kind-3 pointer.
  -> Set.Set Text
    -- ^ Names of @Decide@s whose 'TypeSig' has no GIVETH. Acts as
    -- the function-level analogue of 'unannotatedParams': calls
    -- into these helpers default to NUMBER kind, matching the
    -- Lowering's @paramToMLIR@ / @retType@ defaults. Without this,
    -- @count@'s @go OF …@ leaf renders as kind 3 and the runtime
    -- reads the NUMBER handle as a raw pointer.
  -> Expr Resolved
  -> ([TraceNode], Map SrcRange (Int, Int))
collectTraceNodes infoMap declares paramTypes fnReturnTypes unannotatedParams unannotatedFns rootExpr =
  let (nodes, rangeMap, _next) = goE False 0 [] Map.empty rootExpr
  -- Sort by 'tnId' rather than relying on a single 'reverse'. The
  -- chainLocals path allocates a binding-wrapper node AFTER walking
  -- its rhs, so the raw accumulator no longer round-trips to
  -- id-sorted order under a simple reverse — and the runtime indexes
  -- @traceMeta.nodes@ by @id@, so any out-of-order entry would make
  -- a frame's @lookupNode@ pick up the wrong text.
  in (sortOn (.tnId) nodes, rangeMap)
  where
    -- | Decide whether a particular @Expr@ shape gets a trace node at
    -- all. Mirrors jl4-service's @simplifyEvalTrace@ pruning: leaf
    -- expressions (literals, variable references) are dropped — UNLESS
    -- the caller passes @forced=True@ (e.g. for @IfThenElse@ branches,
    -- where jl4-core traces even a bare @Lit 0@ as the taken-branch
    -- leaf).
    traceable :: Expr Resolved -> Bool
    traceable Lit{}        = False
    traceable (App _ _ []) = False   -- this is the 'Var' pattern
    traceable _            = True

    -- | Best-effort result-kind classification. Uses the InfoMap to
    -- resolve App/AppNamed/IfThenElse etc.; falls back to syntactic
    -- shape for the cases the InfoMap can't pin down.
    --
    -- M5 — for @Proj record field@, the result kind is the field's
    -- declared type, looked up directly via the parent record's
    -- DECLARE. We default to this BEFORE the InfoMap path so we never
    -- end up with kind=3 (compound) on a NUMBER field just because
    -- the typechecker didn't index that particular Proj's source range
    -- — without it, the trace renders \"Result: 0\" instead of the
    -- actual value (the runtime tried ratUnbox on the raw f64 zero).
    resultKindOf :: Expr Resolved -> Int
    resultKindOf (Proj _ recordExpr fieldRes) =
      -- For a property selector we know the field's declared type
      -- statically; prefer that over the InfoMap. The typechecker's
      -- range index sometimes attaches the surrounding expression's
      -- type to the inner Proj's range (or nothing at all), which
      -- otherwise leaves us with kind=3 on a NUMBER field and the
      -- runtime then renders \"Result: 0\" instead of the value.
      projFieldKind recordExpr fieldRes
    resultKindOf e =
      -- Try syntactic cases first — they're authoritative for
      -- operator-shaped expressions whose result type is fixed by the
      -- AST shape (Plus → NUMBER, Geq → BOOLEAN, …). The InfoMap
      -- often doesn't have an exact-range entry for these (especially
      -- inside the prelude, where 'infoMap' is keyed off the main
      -- module's ranges), and even when it does, the type sometimes
      -- comes back as an unresolved polymorphic variable that
      -- 'kindFromL4Type' can't name and that defaults to kind 3.
      case e of
        Plus{}      -> 0
        Minus{}     -> 0
        Times{}     -> 0
        DividedBy{} -> 0
        Modulo{}    -> 0
        Percent{}   -> 0
        Exponent{}  -> 0
        Equals{}    -> 1
        Lt{}        -> 1
        Gt{}        -> 1
        Leq{}       -> 1
        Geq{}       -> 1
        And{}       -> 1
        Or{}        -> 1
        Not{}       -> 1
        Implies{}   -> 1
        Concat{}    -> 2
        AsString{}  -> 2
        -- An IF / multi-way-if / CONSIDER expression's result kind is
        -- the kind of its branches. Recurse so the schema picks up
        -- NUMBER (etc.) for the IF in @max@'s body — without this it
        -- defaults to kind=3 via the InfoMap fallback (which has no
        -- entry for prelude expressions) and the runtime renders the
        -- IF's f64 result as a raw pointer.
        IfThenElse _ _ t _ -> resultKindOf t
        MultiWayIf _ ((MkGuardedExpr _ _ t) : _) _ -> resultKindOf t
        MultiWayIf _ [] e1 -> resultKindOf e1
        Consider _ _ (MkBranch _ _ b : _) -> resultKindOf b
        Where  _ x _ -> resultKindOf x
        LetIn  _ _ x -> resultKindOf x
        -- jl4-core desugars infix operators to @App "__OP__"@; we
        -- pattern-match the operator name so the result kind matches
        -- the syntactic Plus/Minus/Geq cases above.
        App _ headRes [] ->
          -- A bare Var reference. Try the per-fn param-type map first
          -- (set by 'buildTraceMetaFromDecide' from the function's
          -- 'TypeSig') so a force-traced Var inside an IF branch gets
          -- the right kind even when the typechecker's 'InfoMap' has
          -- no exact-range entry — common inside prelude bodies.
          -- Known limitation: when two prelude helpers share a source
          -- name (e.g. there are two @max@ definitions, one with
          -- NUMBER params and one with MAYBE NUMBER), only the last
          -- in source order wins under @helperTraceMeta@, so the
          -- kind may end up as 3 instead of 0 — covered by the
          -- prelude helper-name disambiguation gap noted above.
          let nameText = rawNameToText (rawName (getActual headRes))
          in case Map.lookup nameText paramTypes of
            Just ty -> kindFromL4Type ty
            Nothing -> case lookupTypeAt infoMap e of
              Just ty -> kindFromL4Type ty
              Nothing -> case Map.lookup nameText fnReturnTypes of
                Just ty -> kindFromL4Type ty
                Nothing
                  | nameText `Set.member` unannotatedParams -> 0
                  | otherwise -> 3
        App _ headRes _ -> case rawNameToText (rawName (getActual headRes)) of
          "__PLUS__"   -> 0
          "__MINUS__"  -> 0
          "__TIMES__"  -> 0
          "__DIVIDE__" -> 0
          "__MODULO__" -> 0
          "__EQUALS__" -> 1
          "__LT__"     -> 1
          "__GT__"     -> 1
          "__LEQ__"    -> 1
          "__GEQ__"    -> 1
          "__AND__"    -> 1
          "__OR__"     -> 1
          "__NOT__"    -> 1
          -- Try the per-fn return-type map FIRST. The typechecker
          -- sometimes attaches an unresolved polymorphic-instance
          -- type to the call's range that 'kindFromL4Type' can't
          -- map and that defaults to kind 3 — but the Decide's
          -- GIVETH is concrete and tells us exactly what comes back.
          other -> case Map.lookup other fnReturnTypes of
            Just ty -> kindFromL4Type ty
            Nothing -> case lookupTypeAt infoMap e of
              Just ty -> kindFromL4Type ty
              Nothing
                | other `Set.member` unannotatedFns -> 0
                | otherwise -> 3
        _ -> case lookupTypeAt infoMap e of
          Just ty -> kindFromL4Type ty
          Nothing -> 3

    -- Look up the declared type of @fieldRes@ inside the record that
    -- @recordExpr@ resolves to, and translate to a trace-result kind.
    -- Falls back to kind 3 (compound / @OTHER@) when we can't find the
    -- field — the runtime then renders the result via the
    -- valueByPtr / walkWasmValue path.
    projFieldKind :: Expr Resolved -> Resolved -> Int
    projFieldKind recordExpr fieldRes = fromMaybe 3 $ do
      typeName <- recordTypeName recordExpr
      MkDeclare _ _ _ td <- Map.lookup typeName declares
      case td of
        RecordDecl _ _ fields ->
          let fname = rawNameToText (rawName (getActual fieldRes))
          in case [ ty | MkTypedName _ fn ty _ <- fields
                       , rawNameToText (rawName (getActual fn)) == fname
                       ] of
               (ty:_) -> Just (kindFromL4Type ty)
               []     -> Nothing
        _ -> Nothing

    -- Recursive pre-order walk. The @acc@ list grows with newest nodes
    -- prepended (we reverse at the end) so we don't pay for @++@. The
    -- @forced@ flag turns off the @traceable@ check — callers (e.g.
    -- 'foldExprChildren''s @IfThenElse@ branch) use this to instrument
    -- Lit/Var children that jl4-core traces verbatim.
    goE
      :: Bool
      -> Int
      -> [TraceNode]
      -> Map SrcRange (Int, Int)
      -> Expr Resolved
      -> ([TraceNode], Map SrcRange (Int, Int), Int)
    goE forced next acc rmap e
      | not forced && not (traceable e) =
          -- Still recurse, but don't allocate an ID for this node.
          foldExprChildren goE next acc rmap e
      | otherwise =
          let kind = resultKindOf e
              text = Print.prettyLayout e
              special = specialOf e
              proj = projDataOf e
              -- Only attach a return schema for compound (kind=3) nodes —
              -- that's where the runtime renderer needs to walk wasm
              -- memory. Scalars carry their value in the result f64
              -- directly and don't need it.
              retSchema = if kind == 3
                then
                  let primary = lookupTypeAt infoMap e >>= typeToRetSchema declares Set.empty
                      fromFn = case e of
                        -- For an @App@ (with or without args) whose range
                        -- isn't in 'InfoMap' (common for prelude helpers
                        -- and out-of-module Decides), fall back to the
                        -- bundle-wide function return-type map. Without
                        -- this, a force-traced helper-call leaf carries
                        -- kind 3 but no schema, so the runtime can't walk
                        -- the result pointer and falls back to the raw
                        -- f64 bit pattern (e.g. @7.273e-321@).
                        App _ headRes _ ->
                          let nm = rawNameToText (rawName (getActual headRes))
                          in Map.lookup nm fnReturnTypes >>= typeToRetSchema declares Set.empty
                        _ -> Nothing
                  in primary <|> fromFn
                else Nothing
              node = TraceNode
                { tnId = next
                , tnExampleCode = text
                , tnResultKind = kind
                , tnSpecial = special
                , tnProj = proj
                , tnReturnSchema = retSchema
                , tnBindingLabel = Nothing
                }
              acc' = node : acc
              rmap' = case rangeOf e of
                Just rng -> Map.insert rng (next, kind) rmap
                Nothing  -> rmap
          in foldExprChildren goE (next + 1) acc' rmap' e

    -- | Recognise the few prelude-desugared shapes whose trace nodes
    -- need extra synthetic children to match @traceToReasoning@.
    specialOf :: Expr Resolved -> Maybe Text
    specialOf (And _ _ _) = Just "AND"
    specialOf (Or _ _ _)  = Just "OR"
    specialOf (Not _ _)   = Just "NOT"
    specialOf (Proj _ _ _) = Just "PROJ"
    specialOf (App _ headRes _) =
      case rawNameToText (rawName (getActual headRes)) of
        "__AND__" -> Just "AND"
        "__OR__"  -> Just "OR"
        "__NOT__" -> Just "NOT"
        _         -> Nothing
    specialOf _ = Nothing

    -- | M5 slice 4G — for @Proj record fieldRes@ expressions, bake
    -- the three pre-rendered strings the runtime needs to synthesise
    -- jl4-core's property-selector CONSIDER desugar sub-tree:
    --
    --   * @appForm@ — @\"<field> OF <recordPretty>\"@
    --     (record wrapped in parens if it's itself compound — e.g.
    --     a nested @Proj@ — to match jl4-core's @parensIfNeeded@).
    --   * @fieldName@ — the field's source-form text with backticks
    --     when needed (e.g. @\"\`bankruptcy history\`\"@).
    --   * @considerEx@ — the multi-line indented
    --     @CONSIDER <Type> WHEN <Type> <f1>\\n<indent><f2>… THEN <field>@.
    --
    -- Returns @Nothing@ when we can't resolve the record's type or
    -- find its field list (e.g. a Proj whose record is a function
    -- call result we haven't typed) — the runtime then falls back to
    -- a no-op (no synthesised children).
    projDataOf :: Expr Resolved -> Maybe ProjData
    projDataOf (Proj _ recordExpr fieldRes) = do
      let fieldName = Print.prettyLayout fieldRes
          recordPretty = recordPrettyForApp recordExpr
          appForm = fieldName <> " OF " <> recordPretty
      typeName <- recordTypeName recordExpr
      let fieldList = recordFieldNames typeName
      pure ProjData
        { pdAppForm    = appForm
        , pdFieldName  = fieldName
        , pdConsiderEx = considerExampleCode typeName fieldList fieldName
        }
    projDataOf _ = Nothing

    -- Pretty-print the record sub-expression with parens iff it's a
    -- compound that would need them as an `OF` argument. Mirrors
    -- jl4-core's `parensIfNeeded` for the few shapes the test corpus
    -- exercises (Var ⟶ bare, Proj ⟶ wrapped, App-with-args ⟶ wrapped).
    recordPrettyForApp :: Expr Resolved -> Text
    recordPrettyForApp e = case e of
      App _ _ []  -> Print.prettyLayout e  -- Var pattern, bare
      _           -> "(" <> Print.prettyLayout e <> ")"

    -- Resolve the L4 type name of a record-typed expression via the
    -- typechecker's InfoMap. Returns the bare type name (`"LoanRequest"`).
    recordTypeName :: Expr Resolved -> Maybe Text
    recordTypeName e = do
      ty <- lookupTypeAt infoMap e
      case ty of
        TyApp _ n _ -> Just (rawNameToText (rawName (getActual n)))
        _           -> Nothing

    -- Look up a type's field names in declaration order, with each
    -- name pre-formatted via Print.prettyLayout (so multi-word
    -- names get backticks). Handles both RecordDecl and the
    -- single-constructor ConDecl form jl4 uses for record-like
    -- enums.
    recordFieldNames :: Text -> [Text]
    recordFieldNames typeName =
      case Map.lookup typeName declares of
        Just (MkDeclare _ _ _ (RecordDecl _ _ fields)) ->
          [ Print.prettyLayout n | MkTypedName _ n _ _ <- fields ]
        Just (MkDeclare _ _ _ (EnumDecl _ cons)) ->
          case [ fields
               | MkConDecl _ conName fields <- cons
               , rawNameToText (rawName (getActual conName)) == typeName
               ] of
            (fields:_) -> [ Print.prettyLayout n | MkTypedName _ n _ _ <- fields ]
            []         -> []
        _ -> []

    -- Build the CONSIDER body string in jl4-core's exact format:
    -- @CONSIDER <Ty> WHEN <Ty> <f1>\\n<indent><f2>\\n... THEN <selectedField>@.
    -- @indent@ is the column where @<f1>@ starts so the field column
    -- aligns under the pattern's first field.
    considerExampleCode :: Text -> [Text] -> Text -> Text
    considerExampleCode typeName fields selectedField =
      let prefix = "CONSIDER " <> typeName <> " WHEN " <> typeName <> " "
          -- Empirically jl4-core indents subsequent field lines to
          -- @prefix.length + 1@ (one column past the first field's
          -- start column). Matches its `prettyprinter` use of
          -- `align`/`hang` which lands the next line at column+1.
          indent = Text.replicate (Text.length prefix + 1) " "
          body = case fields of
            []        -> ""
            (f0:rest) -> f0 <> mconcat [ "\n" <> indent <> r | r <- rest ]
      in prefix <> body <> " THEN " <> selectedField

    -- | Recurse into the children of @e@. We deliberately don't use
    -- 'Foldable' so each 'Expr' constructor pattern is explicit —
    -- when a new shape is added to jl4-core, the compile error here
    -- forces us to think about whether it deserves a trace node.
    foldExprChildren
      :: (Bool -> Int -> [TraceNode] -> Map SrcRange (Int, Int) -> Expr Resolved
          -> ([TraceNode], Map SrcRange (Int, Int), Int))
      -> Int
      -> [TraceNode]
      -> Map SrcRange (Int, Int)
      -> Expr Resolved
      -> ([TraceNode], Map SrcRange (Int, Int), Int)
    foldExprChildren k n a r expr = case expr of
      And        _ a' b   -> chain2 k n a r a' b
      Or         _ a' b   -> chain2 k n a r a' b
      RAnd       _ a' b   -> chain2 k n a r a' b
      ROr        _ a' b   -> chain2 k n a r a' b
      Implies    _ a' b   -> chain2 k n a r a' b
      Equals     _ a' b   -> chain2 k n a r a' b
      Not        _ x      -> k False n a r x
      Plus       _ a' b   -> chain2 k n a r a' b
      Minus      _ a' b   -> chain2 k n a r a' b
      Times      _ a' b   -> chain2 k n a r a' b
      DividedBy  _ a' b   -> chain2 k n a r a' b
      Modulo     _ a' b   -> chain2 k n a r a' b
      Exponent   _ a' b   -> chain2 k n a r a' b
      Cons       _ a' b   -> chain2 k n a r a' b
      Leq        _ a' b   -> chain2 k n a r a' b
      Geq        _ a' b   -> chain2 k n a r a' b
      Lt         _ a' b   -> chain2 k n a r a' b
      Gt         _ a' b   -> chain2 k n a r a' b
      Proj       _ x _    -> k False n a r x
      Lam        _ _ body -> k False n a r body
      App        _ _ xs   -> chainList k n a r xs
      AppNamed   _ _ ns _ -> chainList k n a r [e' | MkNamedExpr _ _ e' <- ns]
      -- M5 slice 4A: IF branches are *force*-traceable — jl4-core's
      -- `simplifyEvalTrace` doesn't drop the taken-branch leaf even
      -- when it's a Lit (e.g. the `ELSE 0` in calculate-bonus's body).
      IfThenElse _ c t el ->
        let (a1, r1, n1) = k False n  a  r  c
            (a2, r2, n2) = k True  n1 a1 r1 t
        in              k True  n2 a2 r2 el
      MultiWayIf _ gs el  ->
        let after = chainList k n a r [c | MkGuardedExpr _ c _ <- gs]
            (a1, r1, n1) = after
            after2 = chainList k n1 a1 r1 [b | MkGuardedExpr _ _ b <- gs]
            (a2, r2, n2) = after2
        in k False n2 a2 r2 el
      Consider   _ s br   ->
        let (a1, r1, n1) = k False n a r s
        in foldBranches k n1 a1 r1 br
      Lit{}                  -> (a, r, n)
      Percent    _ x         -> k False n a r x
      List       _ xs        -> chainList k n a r xs
      -- M5 — for each zero-arg @WHERE@/@LET IN@ binding, allocate a
      -- wrapper trace node + recurse through its rhs. The wrapper is
      -- what jl4-core's @traceToReasoning@ renders as
      -- @{exampleCode: [\"income\", \"taxable income OF tp\"], …}@;
      -- without it our trace collapses the binding's evaluation into
      -- the parent Where's children, missing svc's structural
      -- separation between bindings and the inner body. Multi-arg
      -- locals are lambda-lifted to separate @<fn>$trace@ functions
      -- so they don't need a wrapper here.
      Where      _ x lds     ->
        let (a1, r1, n1) = chainLocals k n a r lds
        in k False n1 a1 r1 x
      LetIn      _ lds x     ->
        let (a1, r1, n1) = chainLocals k n a r lds
        in k False n1 a1 r1 x
      Concat     _ xs        -> chainList k n a r xs
      AsString   _ x         -> k False n a r x
      Regulative{}           -> (a, r, n)   -- not lowered to compiled wasm
      Event{}                -> (a, r, n)
      Fetch{}                -> (a, r, n)
      Env{}                  -> (a, r, n)
      Post{}                 -> (a, r, n)
      Breach{}               -> (a, r, n)
      Inert{}                -> (a, r, n)

    -- M5 — force-trace a CONSIDER branch body when it's a 'Lit' or
    -- a bare @Var@ ('App' with no args). The Lit case gives the
    -- taken-branch leaf its literal text (e.g. @0.199 :: Result: 0.199@
    -- for @WHEN Uninsurable THEN 0.199@). The Var case lets the trace
    -- show the helper's call site as a single leaf with the actual
    -- evaluated value — matching jl4-core, which renders e.g.
    -- @\`single brackets\` :: Result: LIST TaxBracket OF 0, …@ at the
    -- selected @CONSIDER@ branch without expanding the helper body.
    -- The Lowering pairs this with a non-tracing call (so the helper's
    -- body events don't double up under the force-traced wrapper).
    foldBranches _ n a r []     = (a, r, n)
    foldBranches k n a r (b:bs) =
      let (a1, r1, n1) = case b of
            MkBranch _ _ body -> case body of
              Lit{}      -> k True  n a r body
              App _ _ [] -> k True  n a r body
              _          -> k False n a r body
      in foldBranches k n1 a1 r1 bs

    chain2 k n a r x y =
      let (a1, r1, n1) = k False n  a  r  x
      in              k False n1 a1 r1 y
    chainList _ n a r []     = (a, r, n)
    chainList k n a r (x:xs) =
      let (a1, r1, n1) = k False n a r x
      in chainList k n1 a1 r1 xs

    -- Walk a list of @LocalDecl@s, allocating one binding-wrapper
    -- trace node per zero-arg @LocalDecide@ (the wrapper's
    -- @bindingLabel@ + rhs prettyLayout match jl4-core's
    -- @labelExample@ + body-expr pretty), then recursing through the
    -- rhs to allocate IDs for its sub-expressions. The wrapper's
    -- range is keyed off the @Decide@'s annotation in @rangeMap@ so
    -- the lowering can look up the wrapper ID at emit time. Multi-arg
    -- locals are skipped: they get lambda-lifted to top-level
    -- functions whose own @<fn>$trace@ does the bracketing.
    chainLocals _ n a r [] = (a, r, n)
    chainLocals k n a r (l:ls) = case l of
      LocalDecide _ decide@(MkDecide _ _ (MkAppForm _ headRes params _) rhs) ->
        case params of
          (_:_) ->
            -- Multi-arg local: lambda-lifted to a separate <fn>$trace
            -- function so its bracketing happens at the call site, not
            -- here.
            chainLocals k n a r ls
          [] ->
            let label = Print.prettyLayout headRes
                rhsPretty = Print.prettyLayout rhs
                -- Use the rhs's own kind classifier. Same path the
                -- inner expressions go through when goE recurses, so
                -- when the runtime walks rhs into its helper $trace
                -- the kinds line up.
                rhsKind = resultKindOf rhs
                rhsSchema =
                  let primaryR = lookupTypeAt infoMap rhs >>= typeToRetSchema declares Set.empty
                      -- Mirror 'goE''s @App@ fallback: when the InfoMap
                      -- has no entry for the rhs's range, look up the
                      -- App head's GIVETH type so the wrapper carries a
                      -- schema the runtime can walk.
                      fromFnR = case rhs of
                        App _ rhsHead _ ->
                          let nm = rawNameToText (rawName (getActual rhsHead))
                          in Map.lookup nm fnReturnTypes >>= typeToRetSchema declares Set.empty
                        _ -> Nothing
                  in primaryR <|> fromFnR
                wrapperNode = TraceNode
                  { tnId          = n
                  , tnExampleCode = rhsPretty
                  , tnResultKind  = rhsKind
                  , tnSpecial     = Nothing
                  , tnProj        = Nothing
                  , tnReturnSchema = rhsSchema
                  , tnBindingLabel = Just label
                  }
                a0 = wrapperNode : a
                r0 = case rangeOf decide of
                  Just rng -> Map.insert rng (n, rhsKind) r
                  Nothing  -> r
                -- Recurse into the rhs's CHILDREN (via foldExprChildren,
                -- not goE) so any Proj / IF / Cmp expressions nested in
                -- the rhs get their own trace nodes — needed for slice 4G's
                -- Proj synthesis on @brackets for status (tp's filing
                -- status)@ etc. We deliberately skip allocating a node
                -- for the rhs's OUTER App: the wrapper's enter/exit
                -- already brackets the call site, and an extra outer node
                -- would re-nest the helper's $trace events one level too
                -- deep.
                (a1, r1, n1) = foldExprChildren goE (n + 1) a0 r0 rhs
            in chainLocals k n1 a1 r1 ls
      _ -> chainLocals k n a r ls

-- | M5 — convert an L4 result type to a runtime-walker schema.
-- Preserves @MAYBE@ wrapping (which @typeToParameter@ strips), and
-- expands declared record types into a field-ordered field map so
-- the runtime can decode struct memory without consulting any
-- module-level declares table at request time. @Nothing@ for shapes
-- the walker doesn't yet handle (e.g. function types) — the runtime
-- then falls back to the raw-pointer rendering.
typeToRetSchema
  :: Map Text (Declare Resolved)
  -> Set.Set Text
  -> Type' Resolved
  -> Maybe RetSchema
typeToRetSchema declares visited ty = case ty of
  TyApp _ name [] -> scalarOrRecord name
  TyApp _ name [inner] ->
    let lo = Text.toLower (rawNameToText (rawName (getActual name)))
    in if lo `elem` ["list", "listof"]
         then RSList <$> typeToRetSchema declares visited inner
         else if lo `elem` ["maybe", "optional"]
           then RSMaybe <$> typeToRetSchema declares visited inner
           else scalarOrRecord name
  TyApp _ name _  -> scalarOrRecord name
  Forall _ _ inner -> typeToRetSchema declares visited inner
  _ -> Nothing
  where
    scalarOrRecord :: Resolved -> Maybe RetSchema
    scalarOrRecord nameRes =
      let nameText = rawNameToText (rawName (getActual nameRes))
      in case Text.toLower nameText of
        "number"   -> Just (RSScalar "number")
        "int"      -> Just (RSScalar "number")
        "integer"  -> Just (RSScalar "number")
        "boolean"  -> Just (RSScalar "boolean")
        "bool"     -> Just (RSScalar "boolean")
        "string"   -> Just (RSScalar "string")
        "text"     -> Just (RSScalar "string")
        "date"     -> Just (RSScalar "date")
        "time"     -> Just (RSScalar "time")
        "datetime" -> Just (RSScalar "datetime")
        _ ->
          if Set.member nameText visited
            then Nothing
            else case Map.lookup nameText declares of
              Just (MkDeclare _ _ _ (RecordDecl _ _ fields)) ->
                buildRecord nameText
                  [ (rawNameToText (rawName (getActual fieldName)), fieldTy)
                  | MkTypedName _ fieldName fieldTy _ <- fields
                  ]
              Just (MkDeclare _ _ _ (EnumDecl _ cons)) ->
                let conNames = [ rawNameToText (rawName (getActual conName))
                               | MkConDecl _ conName _ <- cons
                               ]
                in if all (\(MkConDecl _ _ fs) -> null fs) cons
                  then Just (RSEnum "string" conNames)
                  else Nothing  -- record-like enums: not yet decodable
              _ -> Nothing
    buildRecord :: Text -> [(Text, Type' Resolved)] -> Maybe RetSchema
    buildRecord nameText fields =
      let visited' = Set.insert nameText visited
          go = mapM (\(fn, ft) -> do
                       sub <- typeToRetSchema declares visited' ft
                       pure (fn, sub)) fields
      in case go of
           Just pairs -> Just (RSRecord nameText (map fst pairs) (Map.fromList pairs))
           Nothing    -> Nothing

-- | Look up the L4 type of an expression at a given source range via
-- the typechecker's 'InfoMap'. Mirrors the strategy used by
-- 'Lower.typeOfExpr': only accept an EXACT range match so we don't pick
-- up a containing expression's type by mistake.
lookupTypeAt :: HasSrcRange e => InfoMap -> e -> Maybe (Type' Resolved)
lookupTypeAt info e = case rangeOf e of
  Nothing -> Nothing
  Just range ->
    case [ ty
         | (iv, TypeInfo ty _) <- IV.search range.start info
         , IV.intervalLow iv == range.start
         , IV.intervalHigh iv == range.end
         ] of
      (ty:_) -> Just ty
      []     -> Nothing
  where
    _ = ()  -- 'TypeInfo' comes from 'L4.Syntax' via the wildcard 'Info(..)' import above.

-- | Result kind byte from an L4 type. Matches @renderTraceResult@'s
-- kind switch in @runtime/jl4-runtime.mjs@.
kindFromL4Type :: Type' Resolved -> Int
kindFromL4Type t = case t of
  TyApp _ n _
    | resolvedNameText n `elem` ["NUMBER", "number"]   -> 0
    | resolvedNameText n `elem` ["BOOLEAN", "boolean"] -> 1
    | resolvedNameText n `elem` ["STRING", "string"]   -> 2
  _ -> 3
  where
    resolvedNameText r = rawNameToText (rawName (getActual r))

-- ---------------------------------------------------------------------------
-- Diagnostics — mark functions the lowering couldn't faithfully compile
-- ---------------------------------------------------------------------------

-- | Downgrade schema exports according to per-function lowering
-- diagnostics, keyed by sanitized WASM symbol name (matching
-- 'wasmSymbol'). A flagged function stays in the schema — so the proxy
-- still sees it exists and can serve its read-only metadata — but
-- @supported = False@ tells the proxy to route evaluation to a fallback
-- engine rather than the WASM module, which cannot evaluate it
-- correctly. Functions absent from the map are left untouched.
applyDiagnostics :: Map Text [Text] -> WasmBundle -> WasmBundle
applyDiagnostics diags wb =
  wb { bundleExports = map mark wb.bundleExports }
  where
    mark fe = case Map.lookup fe.wasmSymbol diags of
      Nothing      -> fe
      Just reasons -> fe
        { supported = False
        , unsupportedReason = Just (Text.intercalate "; " (uniq reasons))
        }
    -- De-duplicate while preserving first-seen order.
    uniq = go Set.empty
      where
        go _ [] = []
        go seen (r : rs)
          | r `Set.member` seen = go seen rs
          | otherwise           = r : go (Set.insert r seen) rs

-- | Convert an ExportedParam to a FunctionSchema Parameter entry.
buildParamsFromExported :: Map Text (Declare Resolved) -> [ExportedParam] -> Parameters
buildParamsFromExported declares params =
  let entries =
        [ (p.paramName, paramToParameter declares p)
        | p <- params
        ]
      required = [p.paramName | p <- params, p.paramRequired]
  in MkParameters
       { parameterMap = Map.fromList entries
       , required     = required
       }

paramToParameter :: Map Text (Declare Resolved) -> ExportedParam -> Parameter
paramToParameter declares p =
  let base = case p.paramType of
        Nothing -> emptyParam "object"
        Just ty -> typeToParameter declares Set.empty ty
  in base
       { parameterAlias       = Nothing
       , parameterDescription = maybe "" Text.strip p.paramDescription
       }

emptyParam :: Text -> Parameter
emptyParam t = Parameter
  { parameterType          = t
  , parameterAlias         = Nothing
  , parameterFormat        = Nothing
  , parameterEnum          = []
  , parameterDescription   = ""
  , parameterProperties    = Nothing
  , parameterPropertyOrder = Nothing
  , parameterItems         = Nothing
  , parameterRequired      = Nothing
  , parameterL4Type        = Nothing
  }


-- ---------------------------------------------------------------------------
-- DEONTIC parameter injection — mirrors jl4-service/src/Compiler.hs
-- ---------------------------------------------------------------------------

-- | Is this a DEONTIC (contract) return type?
isDeonticType :: Type' Resolved -> Bool
isDeonticType (TyApp _ name [_, _]) = getUnique name == contractUnique
isDeonticType _ = False

-- | For DEONTIC-returning functions, the HTTP API injects two extra
-- parameters: @startTime@ (number) and @events@ (array of
-- @{party, action, at}@ objects). We must add these to the schema
-- for compatibility with the service.
addDeonticParameters :: Maybe (Type' Resolved) -> Parameters -> Parameters
addDeonticParameters mReturnType (MkParameters props req) =
  let (partyParam, actionParam) = case mReturnType of
        Just (TyApp _ _ [partyTy, actionTy]) ->
          ( (genericDeonticParam "The party performing the action") { parameterType = paramTypeOf partyTy }
          , (genericDeonticParam "The action performed")             { parameterType = paramTypeOf actionTy }
          )
        _ ->
          ( genericDeonticParam "The party performing the action"
          , genericDeonticParam "The action performed"
          )
      startTimeParam = (emptyParam "number")
        { parameterDescription = "Start time for contract simulation" }
      eventsParam = (emptyParam "array")
        { parameterDescription = "Events for contract simulation (each: {party, action, at})"
        , parameterItems = Just $ (emptyParam "object")
            { parameterDescription = "A trace event"
            , parameterProperties = Just $ Map.fromList
                [ ("party", partyParam)
                , ("action", actionParam)
                , ("at", (emptyParam "number") { parameterDescription = "Timestamp" })
                ]
            , parameterPropertyOrder = Just ["party", "action", "at"]
            , parameterRequired = Just ["party", "action", "at"]
            }
        }
  in MkParameters
       { parameterMap = props
           <> Map.fromList [("startTime", startTimeParam), ("events", eventsParam)]
       , required = req <> ["startTime", "events"]
       }
  where
    paramTypeOf ty = case ty of
      TyApp _ n _ -> case Text.toLower (rawNameToText (rawName (getActual n))) of
        "number"  -> "number"
        "boolean" -> "boolean"
        "bool"    -> "boolean"
        "string"  -> "string"
        "text"    -> "string"
        _         -> "object"
      _ -> "object"

genericDeonticParam :: Text -> Parameter
genericDeonticParam desc = (emptyParam "object") { parameterDescription = desc }

-- ---------------------------------------------------------------------------
-- Return-type display
-- ---------------------------------------------------------------------------

returnTypeDisplay :: Maybe (Type' Resolved) -> Text
returnTypeDisplay Nothing = "unknown"
returnTypeDisplay (Just ty)
  | isDeonticType ty = "DEONTIC"
  | otherwise = case ty of
      TyApp _ n args ->
        let nameText = Text.toUpper (rawNameToText (rawName (getActual n)))
         in if null args
              then nameText
              else nameText <> " " <> Text.intercalate " " (map (returnTypeDisplay . Just) args)
      _ -> "TYPE"

-- ---------------------------------------------------------------------------
-- JSON serialization
-- ---------------------------------------------------------------------------

instance Aeson.ToJSON FunctionExport where
  toJSON fe = Aeson.object
    [ "name"        .= fe.apiName
    , "wasmSymbol"  .= fe.wasmSymbol
    , "description" .= fe.description
    , "parameters"  .= fe.parameters
    , "returnType"  .= fe.returnType
    , "isDeontic"   .= fe.isDeontic
    , "paramOrder"  .= fe.paramOrder
    , "supported"   .= fe.supported
    , "unsupportedReason" .= fe.unsupportedReason
    , "traceMeta"   .= fe.traceMeta
    ]

instance Aeson.FromJSON FunctionExport where
  parseJSON = Aeson.withObject "FunctionExport" $ \o -> FunctionExport
    <$> o .: "name"
    <*> o .: "wasmSymbol"
    <*> o .:? "description" .!= ""
    <*> o .: "parameters"
    <*> o .:? "returnType"  .!= "unknown"
    <*> o .:? "isDeontic"   .!= False
    <*> o .:? "paramOrder"  .!= []
    <*> o .:? "supported"   .!= True
    <*> o .:? "unsupportedReason"
    <*> o .:? "traceMeta"

instance Aeson.ToJSON TraceMeta where
  toJSON tm = Aeson.object
    [ "fnName"           .= tm.tmFnNamePretty
    , "body"             .= tm.tmBodyPretty
    , "params"           .= tm.tmParamPretties
    , "nodes"            .= tm.tmNodes
    , "fnValueNodeId"    .= tm.tmFnValueNodeId
    ]

instance Aeson.FromJSON TraceMeta where
  parseJSON = Aeson.withObject "TraceMeta" $ \o -> TraceMeta
    <$> o .: "fnName"
    <*> o .: "body"
    <*> o .:? "params" .!= []
    <*> o .:? "nodes"  .!= []
    <*> o .:? "fnValueNodeId" .!= 0

instance Aeson.ToJSON TraceNode where
  toJSON tn = Aeson.object $
    [ "id"          .= tn.tnId
    , "exampleCode" .= tn.tnExampleCode
    , "resultKind"  .= tn.tnResultKind
    ]
    ++ maybe [] (\s -> ["special" .= s]) tn.tnSpecial
    ++ maybe [] (\p -> ["proj" .= p]) tn.tnProj
    ++ maybe [] (\s -> ["returnSchema" .= s]) tn.tnReturnSchema
    ++ maybe [] (\l -> ["bindingLabel" .= l]) tn.tnBindingLabel

instance Aeson.FromJSON TraceNode where
  parseJSON = Aeson.withObject "TraceNode" $ \o -> TraceNode
    <$> o .: "id"
    <*> o .: "exampleCode"
    <*> o .:? "resultKind" .!= 3
    <*> o .:? "special"
    <*> o .:? "proj"
    <*> o .:? "returnSchema"
    <*> o .:? "bindingLabel"

instance Aeson.ToJSON ProjData where
  toJSON pd = Aeson.object
    [ "appForm"    .= pd.pdAppForm
    , "fieldName"  .= pd.pdFieldName
    , "considerEx" .= pd.pdConsiderEx
    ]

instance Aeson.FromJSON ProjData where
  parseJSON = Aeson.withObject "ProjData" $ \o -> ProjData
    <$> o .: "appForm"
    <*> o .: "fieldName"
    <*> o .: "considerEx"

instance Aeson.ToJSON RetSchema where
  toJSON s = case s of
    RSScalar t -> Aeson.object ["kind" .= ("scalar" :: Text), "type" .= t]
    RSEnum t vs -> Aeson.object ["kind" .= ("enum" :: Text), "type" .= t, "values" .= vs]
    RSMaybe inner -> Aeson.object ["kind" .= ("maybe" :: Text), "inner" .= inner]
    RSList items -> Aeson.object ["kind" .= ("list" :: Text), "items" .= items]
    RSRecord name fieldsOrder fieldMap -> Aeson.object
      [ "kind" .= ("record" :: Text)
      , "name" .= name
      , "fieldOrder" .= fieldsOrder
      , "fields" .= Aeson.object
          [ (Aeson.Key.fromText fn, Aeson.toJSON fs)
          | fn <- fieldsOrder
          , Just fs <- [Map.lookup fn fieldMap]
          ]
      ]

instance Aeson.FromJSON RetSchema where
  parseJSON = Aeson.withObject "RetSchema" $ \o -> do
    kind :: Text <- o .: "kind"
    case kind of
      "scalar" -> RSScalar <$> o .: "type"
      "enum"   -> RSEnum  <$> o .: "type" <*> o .: "values"
      "maybe"  -> RSMaybe <$> o .: "inner"
      "list"   -> RSList  <$> o .: "items"
      "record" -> do
        name <- o .: "name"
        order_ <- o .: "fieldOrder"
        fieldsObj :: Map Text RetSchema <- o .: "fields"
        pure (RSRecord name order_ fieldsObj)
      _ -> fail ("unknown RetSchema kind: " <> Text.unpack kind)

instance Aeson.ToJSON StateGraphExport where
  toJSON sge = Aeson.object
    [ "name" .= sge.sgeName
    , "dot"  .= sge.sgeDot
    ]

instance Aeson.ToJSON WasmBundle where
  toJSON wb = Aeson.object
    [ "wasmFile" .= wb.bundleWasmFile
    , "version"  .= wb.bundleVersion
    , "functions" .= Aeson.object
        [ (Aeson.Key.fromText fe.apiName, Aeson.toJSON fe)
        | fe <- wb.bundleExports
        ]
    , "stateGraphs" .= wb.bundleStateGraphs
    , "helperTraceMeta" .= Aeson.object
        [ (Aeson.Key.fromText sym, Aeson.toJSON tm)
        | (sym, tm) <- wb.bundleHelpers
        ]
    ]

encodeBundle :: WasmBundle -> LBS.ByteString
encodeBundle = Aeson.encode

writeBundleFile :: FilePath -> WasmBundle -> IO ()
writeBundleFile path bundle = LBS.writeFile path (encodeBundle bundle)
