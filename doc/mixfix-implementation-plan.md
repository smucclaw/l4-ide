# Mixfix Operators - Implementation Plan

## Current Status (2025-12)

### Completed

- ✅ `doc/mixfix-operators.md` specification kept current with regression coverage
- ✅ Type-checker reinterpretation for multi-operand mixfix calls (`tryMatchMixfixCall`, Dec 2024; see `doc/done/MIXFIX-USAGE-SPAN-FIX.md`)
- ✅ Type-checker reinterpretation for postfix mixfix calls with bare variables (`reinterpretPostfixAppIfNeeded`, Dec 2025; see `doc/issues/POSTFIX-WITH-VARIABLES-BUG.md`)
- ✅ Regression programs `jl4/examples/ok/postfix-with-variables.l4` and `jl4/examples/ok/mixfix-with-variables.l4`
- ✅ Parser hint scaffolding (`L4.Parser.MixfixRegistry`, Dec 2025) builds a mixfix keyword registry before the second parse so syntax/IDE layers can opt into registry-aware parsing
- ✅ `mixfixChainExpr` now consults the hint registry for every keyword that appears in a chain (Dec 2025), preventing unrelated identifiers on the same line from being misinterpreted as part of a multi-token operator whenever hints are supplied.
- ✅ Mixfix postfix/chains honor indentation-aligned keywords on the following line (Jan 2026), unlocking IF/THEN-style vertical layouts and regression coverage in `jl4/examples/ok/mixfix-multiline.l4`.

### In Progress

- 🔄 Parser-level mixfix awareness (lookahead + registry) per the plan below; current implementation still relies on the type-checker safety net
- 🔄 Optional underscore-oriented AST sugar (no user-facing syntax yet)

## Implementation Phases

> **Historical plan** — The sections below capture the aspirational parser-first implementation. They remain useful design notes even though today's production solution relies on the type-checker rewrite.

### Production Safety Net (Shipped)

- `tryMatchMixfixCall` (Dec 2024) rewrites greedy parses such as `App alice [kw, arg, ...]` so multi-operand mixfix expressions match their declared patterns.
- `reinterpretPostfixAppIfNeeded` (Dec 2025) flips `App operand [keyword]` when the trailing identifier is a registered postfix keyword and the operand is not callable, enabling `x squared` style calls.
- The regression suite (`jl4/examples/ok/postfix-with-variables.l4`, `jl4/examples/ok/mixfix-with-variables.l4`) prevents regressions for both postfix and general mixfix cases.

### Parser Refactor Roadmap (Planned)

We still want the parser itself to recognize mixfix/postfix patterns so the type-checker rewrite becomes optional. Proposed steps:

1. **Parser Recon** — Re-audit `mixfixChainExpr`, `baseExpr`, and `app` (with a focus on the `AnnoParser` vs `Parser` boundary) and document where safe lookahead can occur without losing annotations. Deliverable: a diagram plus notes describing each combinator’s responsibilities.
2. **Registry Bridging** — Determine when mixfix signatures become available. If necessary, introduce a lightweight first pass that only records keyword spans/arity so later parser stages can consult the registry while still streaming tokens once.
3. **Prototype Lookahead** — Teach `app` to peek at the next same-line token; if it matches a registered postfix/mixfix keyword, defer consumption so `mixfixChainExpr` can interpret it. Keep this behavior behind a flag so we can compare against the current rewrite during testing.
4. **Golden + Property Coverage** — Extend `jl4/examples/ok/*.l4` plus parser round-trip/property tests to ensure AST annotations and IDE ranges remain stable across literals, bare variables, LET bindings, and multi-line mixfix chains.
5. **Migration & Cleanup** — After confidence is high, remove the type-checker reinterpreters, refresh golden files, and update documentation to reflect a parser-native solution.

Risks include the brittle AnnoParser lookahead rules and multi-line patterns still confusing the parser. Mitigation: gate the new path, fall back to today’s safety net whenever ambiguity remains, and rely on the expanded regression suite to detect regressions early.

### Parser Recon (Dec 2025)

To unblock the roadmap above we audited the current parser to understand where mixfix-aware lookahead could live without breaking annotation handling:

1. **Entry points and responsibilities**

   - `indentedExpr` / `singleLineExpr` (jl4-core/src/L4/Parser.hs:818, 850) always begin by calling `mixfixChainExpr`, so every expression already passes through the mixfix collector before operator precedence is resolved.
   - `mixfixChainExpr` (line 1186) wraps `baseExpr`, then repeatedly captures `(keyword, expr)` pairs as long as the keyword lives on the same line as the preceding operand. It does _not_ consult the mixfix registry; every identifier that “looks” like a keyword is accepted and left for the type checker to validate.
   - `baseExpr` / `atomicExpr` (lines 1184, 1320) are both wrapped in `postfixPWithLine mixfixPostfixOp regularPostfixOperator`, so postfix mixfix tokens are intercepted before the general postfix parser sees them.
   - `app` (line 1345) is the greedy consumer that triggered the original bug: it parses a leading name, then arguments via `atomicExpr'`. Because `atomicExpr'` lacks postfix handling, bare identifiers such as `squared` are happily consumed as additional arguments.

2. **Control / data flow observations**

   - All of the functions above lift `Parser` actions into `AnnoParser` via `attachAnno`. Any lookahead we add must occur _before_ the annotation wrappers fire, otherwise we end up juggling `AnnoParser` vs `Parser` types (the issue that stalled previous attempts).
   - `mixfixPostfixOp` (lines 1143-1178) already performs the necessary same-line lookahead: it peeks at the next token’s `range.start.line`, ensures it matches `exprEndLine`, and `notFollowedBy` another same-line expression. This is the model to reuse for the general mixfix case.
   - Expression continuations (`expressionCont`, line 1063) are wrapped in `try`, so if we add additional failure conditions the parser cleanly backtracks to let `mixfixChainExpr` or `app` take over.

3. **Opportunities and risks**
   - The parser currently has no access to the runtime mixfix registry. To introduce registry-aware lookahead we either need (a) a light-weight “signature pass” that runs before parsing expressions, or (b) to stash the registry in the `Env` handed to the parser. Option (a) aligns with the `Registry Bridging` step above.
   - We can gate the greedy branch in `app` by doing a cheap “same-line + registry” peek _before_ calling `nameAsApp`. If the next token is a keyword for any registered postfix/mixfix operator, we refuse to parse the identifier as a function name and let `mixfixPostfixOp` / `mixfixChainExpr` consume it instead.
   - Multi-line patterns remain tricky: because `mixfixChainExpr` ties each keyword to the previous argument’s ending line, any registry-aware lookahead must respect that rule or we risk regressing indentation-driven grouping. The safest path is to keep the same-line restriction (as today) and revisit multi-line mixfixes only after the registry is threaded in.

### Phase 1: AST and Lexer Changes

#### 1.1 Lexer (NO CHANGES NEEDED)

- **No lexer changes required!**

Underscores (`_`) are purely conceptual - used to describe the internal pattern representation but never appear in source code. Users write `a plus b`, not `_ plus _`.

#### 1.2 Syntax AST

The current `AppForm` structure is:

```haskell
data AppForm n =
  MkAppForm Anno n [n] (Maybe (Aka n))
  -- Anno, function name, args, optional aka
```

**No AST changes needed!** The existing structure already captures the pattern:

- For `person `is eligible for` program`:
  - Function name: `is eligible for`
  - Args: `[person, program]`

What we need is to recognize **which args are actually parameter placeholders vs which are keywords** in the pattern.

**No file updates needed** - `AppForm` structure remains unchanged!

### Phase 2: Parser Changes

**No parser changes needed!** The existing parser already handles patterns like:

```l4
person `is eligible for` program MEANS ...
```

This is parsed as:

- A sequence of names: `[person, is eligible for, program]`
- Then structured as `AppForm` during parsing

The parser already supports this - it's just application syntax with backticked names.

However, we can't rely on an infix function name to be backticked, because all tokens and all identifiers are allowed to be backticked.

### Phase 3: Scanning Phase Enhancement

#### 3.1 Extract Mixfix Pattern Information

In `TypeCheck.hs`, enhance `scanFunSigDecide` and `scanFunSigAssume`:

```haskell
-- New data type to store mixfix pattern info
data MixfixInfo = MkMixfixInfo
  { keywords :: [Name]           -- keyword parts of pattern
  , arity :: Int                 -- number of parameters (underscores)
  , keywordPositions :: [Int]    -- where keywords appear in pattern
  , pattern :: [PatternToken Name]  -- full pattern for matching
  }
  deriving (Show, Eq, Generic, NFData)

-- Extract from AppForm
extractMixfixInfo :: AppForm Resolved -> Maybe MixfixInfo
extractMixfixInfo (MixfixAppForm _ tokens _) =
  let keywords = [n | Keyword n <- tokens]
      arity = length [() | ParamHole <- tokens]
      keywordPositions = [i | (i, Keyword _) <- zip [0..] tokens]
  in if arity > 0
     then Just $ MkMixfixInfo {..}
     else Nothing
extractMixfixInfo (PrefixAppForm _ _ _ _) = Nothing

-- Extend FunTypeSig to include mixfix info
data FunTypeSig = MkFunTypeSig
  { anno :: Anno
  , rtysig :: TypeSig Resolved
  , rappForm :: AppForm Resolved
  , resultType :: Type' Resolved
  , name :: CheckInfo
  , arguments :: [CheckInfo]
  , mixfixInfo :: Maybe MixfixInfo  -- NEW FIELD
  }
```

#### 3.2 Build Mixfix Registry

During scanning, build a registry of all mixfix functions:

```haskell
-- In CheckEnv, add:
data CheckEnv = MkCheckEnv
  { ...
  , mixfixFunctions :: Map RawName [FunTypeSig]  -- NEW FIELD
  }

-- During scanFunSigDecide:
scanFunSigDecide :: Decide Name -> Check FunTypeSig
scanFunSigDecide d@(MkDecide _ tysig appForm _) = do
  -- ... existing logic ...
  let mixfixInfo = extractMixfixInfo rappForm
  let funSig = MkFunTypeSig { ... , mixfixInfo = mixfixInfo }

  -- Register in mixfix functions map
  case mixfixInfo of
    Just info -> do
      let primaryName = rawName (head info.keywords)
      modify $ \s -> s { mixfixFunctions =
        Map.insertWith (++) primaryName [funSig] s.mixfixFunctions }
    Nothing -> pure ()

  pure funSig
```

### Parser Hint Pass (Dec 2025)

- Implemented `L4.Parser.MixfixRegistry`, which traverses a parsed `Module Name` and aggregates every mixfix `Decide`/`Assume` into a `MixfixHintRegistry` (first-keyword lookup map + keyword universe set).
- `L4.Parser` now exports hint-aware entry points (`execParserWithHints`, `execProgramParserWithHints`, `execProgramParserWithHintPass`, etc.) and stores the registry on the parser `Env`, so downstream combinators (e.g., `mixfixPostfixOp`) can consult authoritative keyword membership whenever hints are supplied.
- The LSP `GetParsedAst` rule runs a two-pass parse: first pass builds hints, second pass parses with hints, ensuring IDE clients benefit immediately without waiting for the parser refactor.
- Prefix application now checks the hint registry before consuming the first positional argument: if the upcoming same-line token is a declared mixfix keyword we bail out, letting `mixfixPostfixOp` / `mixfixChainExpr` reinterpret `x squared` without relying on the type-checker rewrite.
- Edge case: a deliberate attempt to call a function with the same spelling as a keyword (e.g. `n percent` where `percent` is a regular 1-ary function) will now need parentheses/backticks or a line break before the argument; we consider this a pathological scenario and can revisit if it shows up in real code.

### Phase 4: Type Checker Pattern Matching

#### 4.1 Mixfix Application Matcher

Add pattern matching logic to `inferExpr`:

```haskell
-- Pattern matching algorithm
tryMixfixApplication :: [Name] -> Check [(Expr Resolved, Type' Resolved)]
tryMixfixApplication tokens = do
  env <- getEnvironment
  mixfixFuncs <- asks (.mixfixFunctions)

  -- Try to match against all registered mixfix functions
  results <- forM (concat $ Map.elems mixfixFuncs) $ \funSig -> runMaybeT $ do
    case funSig.mixfixInfo of
      Nothing -> empty
      Just info -> do
        -- Try to match pattern
        args <- MaybeT $ pure $ matchPattern info.pattern (map rawName tokens)

        -- Resolve argument names and type check
        lift $ do
          typedArgs <- zipWithM (\argName paramType -> do
            resolved <- resolveExpr (VarName argName)
            checkExpr ExpectArgument resolved paramType
            ) args (getParamTypes funSig)

          pure (MixfixApp funSig.name typedArgs, funSig.resultType)

  pure $ catMaybes results

-- Pattern matching helper
matchPattern :: [PatternToken Name] -> [RawName] -> Maybe [RawName]
matchPattern pattern tokens = do
  -- Try to find keywords in sequence and extract args between them
  let keywordPositions = findKeywords pattern tokens
  guard (length keywordPositions == countKeywords pattern)

  -- Extract argument tokens at parameter positions
  let argRanges = computeArgRanges keywordPositions pattern
  traverse (extractArgName tokens) argRanges
```

#### 4.2 Integrate with Expression Inference

Modify `inferExpr` to try mixfix matching:

```haskell
inferExpr :: Expr Name -> Check (Expr Resolved, Type' Resolved)
inferExpr (App (VarName n : args)) = do
  -- Try traditional prefix application first
  prefixResult <- tryPrefixApplication n args

  case prefixResult of
    Just result -> pure result
    Nothing -> do
      -- Try mixfix application
      mixfixResults <- tryMixfixApplication (n : args)

      case mixfixResults of
        [] -> addError (NoMatchingFunction (n : args))
        [result] -> pure result
        results -> addError (AmbiguousMixfix (n : args) results)
```

### Phase 5: Error Handling

Add new error types:

```haskell
data CheckError
  = ...
  | NoMatchingMixfixPattern [Name] -- tried to use as mixfix but no pattern matched
  | AmbiguousMixfix [Name] [(Expr Resolved, Type' Resolved)] -- multiple patterns matched
  | MixfixArityMismatch AppForm Int Int -- pattern has N underscores but M GIVEN params
```

### Phase 6: Testing

#### 6.1 Unit Tests

Create `jl4/examples/ok/mixfix-basic.l4`:

```l4
§ `Basic Mixfix Tests`

-- Infix
GIVEN a IS A Number, b IS A Number
GIVETH Number
a `plus` b MEANS a + b

#EVAL 3 `plus` 5 = 8

-- Postfix
GIVEN amount IS A Number
GIVETH Number
amount `percent` MEANS amount / 100

#EVAL 50 `percent` = 0.5

-- Prefix
GIVEN x IS A Number
GIVETH Number
`negate` x MEANS -x

#EVAL `negate` 5 = -5

-- Ternary
GIVEN lower IS A Number, value IS A Number, upper IS A Number
GIVETH Bool
lower `<=` value `<=` upper MEANS lower <= value AND value <= upper

#EVAL 0 `<=` 5 `<=` 10 = TRUE
```

#### 6.2 Error Tests

Create `jl4/examples/not-ok/tc/mixfix-errors.l4`:

```l4
-- Pattern doesn't match GIVEN params
GIVEN a IS A Number
a `plus` b MEANS a + b  -- ERROR: 'b' not in GIVEN

-- Ambiguous (same pattern structure, same types)
GIVEN a IS A Number, b IS A Number
a `op` b MEANS a + b

GIVEN a IS A Number, b IS A Number
a `op` b MEANS a * b

#EVAL 3 `op` 5  -- ERROR: Ambiguous - multiple definitions match

-- Type mismatch at call site
GIVEN person IS A Person, program IS A Program
person `is eligible for` program MEANS ...

#EVAL 123 `is eligible for` "healthcare"  -- ERROR: wrong types
```

#### 6.3 Integration Tests

Test interaction with:

- Existing operators
- Type-directed name resolution
- Indentation-sensitive parsing
- Layout rules

### Phase 7: Documentation Updates

- Update GRAMMAR.md with mixfix syntax
- Add examples to guide documentation
- Update LSP documentation for autocomplete/hover on mixfix

## Migration Strategy

### Backward Compatibility

- All existing code continues to work (prefix application unchanged)
- Mixfix is opt-in (use parameter names in pattern)
- No breaking changes to existing APIs

### Gradual Rollout

1. Merge scanning phase changes (pattern extraction)
2. Merge type checker changes (pattern matching)
3. Add tests progressively
4. Update user-facing documentation
5. No AST/parser/lexer changes needed!

## Open Questions

1. **Precedence**: Should we allow optional precedence annotations for common cases?
2. **Partial application**: Should mixfix functions support partial application?
3. **Overloading**: Can we have multiple functions with the same pattern structure but different types?
4. **Performance**: What's the overhead of pattern matching at every application site?
5. **Ambiguity**: How to handle when both `a op b` and `op a b` are valid interpretations?

## Timeline Estimate

**Simplified due to no AST/parser/lexer changes needed!**

- Phase 3 (Scanning): 1-2 days
- Phase 4 (Type Checker): 2-3 days
- Phase 5 (Errors): 1 day
- Phase 6 (Testing): 1-2 days
- Phase 7 (Docs): 1 day

**Total**: ~1-2 weeks for full implementation (down from 2-3 weeks!)

## Next Steps

1. Implement pattern extraction in scanning phase (`scanFunSigDecide`)
   - Compare AppForm tokens against GIVEN parameters
   - Build MixfixInfo structure
   - Store in FunTypeSig
2. Implement pattern matching in type checker (`inferExpr`)
   - Find keywords in token sequences
   - Extract arguments between keywords
   - Type check and disambiguate
3. Write comprehensive tests
4. Update user-facing documentation

## References

- Spec: `doc/mixfix-operators.md`
- Grammar: `jl4/GRAMMAR.md`
- Type checker: `jl4-core/src/L4/TypeCheck.hs`
- Parser: `jl4-core/src/L4/Parser.hs`
