# Mixfix Operators - Implementation Plan

## Current Status

### Completed
- ✅ Written specification (doc/mixfix-operators.md)
- ✅ Created branch `mengwong/mixfix`
- ✅ Added `TUnderscore` token to lexer (Lexer.hs)

### In Progress
- 🔄 Extending AST and parser to handle underscore patterns

## Implementation Phases

### Phase 1: AST and Lexer Changes

#### 1.1 Lexer (DONE)
- [x] Add `TUnderscore` to `TSymbols` enum
- [x] Add `"_"` mapping in `symbols` map

#### 1.2 Syntax AST
Add a new data type to represent mixfix patterns:

```haskell
-- In Syntax.hs

-- Represents a token in a mixfix pattern
data PatternToken n
  = ParamHole      -- underscore (_)
  | Keyword n      -- backticked identifier
  deriving stock (GHC.Generic, Eq, Ord, Show, Functor, Foldable, Traversable)
  deriving anyclass (SOP.Generic, ToExpr, NFData)

-- Extend AppForm to support mixfix patterns
data AppForm n
  = PrefixAppForm Anno n [n] (Maybe (Aka n))      -- traditional: name args
  | MixfixAppForm Anno [PatternToken n] (Maybe (Aka n))  -- mixfix: pattern
  deriving stock (GHC.Generic, Eq, Ord, Show, Functor, Foldable, Traversable)
  deriving anyclass (SOP.Generic, ToExpr, NFData)
```

**Alternative (less invasive)**: Keep current `AppForm`, add optional mixfix info:
```haskell
data AppForm n =
  MkAppForm Anno n [n] (Maybe (Aka n)) (Maybe [PatternToken n])
```

#### 1.3 Update Pattern Matchers
Files that need updates for new `AppForm` structure:
- `L4/Parser.hs` - parsing
- `L4/TypeCheck.hs` - type checking
- `L4/Print.hs` - pretty printing
- `L4/Desugar.hs` - desugaring
- `L4/EvaluateLazy/Machine.hs` - evaluation
- `L4/Names.hs` - name handling
- `L4/TypeCheck/Annotation.hs` - annotations
- `L4/Parser/ResolveAnnotation.hs` - annotation resolution
- `jl4-lsp/src/LSP/L4/SemanticTokens.hs` - LSP support
- `jl4-lsp/src/LSP/L4/Viz/Ladder.hs` - visualization
- `jl4-decision-service/src/Backend/Jl4.hs` - decision service

### Phase 2: Parser Changes

#### 2.1 Parse Underscore Symbol
In `Parser.hs`, add parser for underscore:

```haskell
underscore :: Parser ()
underscore = spacedSymbol_ TUnderscore
```

#### 2.2 Parse Mixfix Pattern
Update `appForm` parser to handle mixfix patterns:

```haskell
appForm :: Parser (AppForm Name)
appForm = mixfixAppForm <|> prefixAppForm
  where
    prefixAppForm = do
      -- existing logic
      current <- Lexer.indentLevel
      attachAnno $
        PrefixAppForm emptyAnno  -- or MkAppForm with Nothing for mixfix
          <$> annoHole name
          <*> (...)
          <*> annoHole (optional aka)

    mixfixAppForm = do
      -- Parse pattern: sequence of _ and `names`
      pattern <- many1 (patternHole <|> patternKeyword)
      -- Verify at least one underscore exists
      guard (any isParamHole pattern)
      aka <- optional aka
      pure $ MixfixAppForm emptyAnno pattern aka

    patternHole = ParamHole <$ underscore
    patternKeyword = Keyword <$> name
```

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
_ `plus` _ MEANS a + b

#EVAL 3 `plus` 5 = 8

-- Postfix
GIVEN amount IS A Number
GIVETH Number
_ `percent` MEANS amount / 100

#EVAL 50 `percent` = 0.5

-- Prefix
GIVEN x IS A Number
GIVETH Number
`negate` _ MEANS -x

#EVAL `negate` 5 = -5

-- Ternary
GIVEN lower IS A Number, value IS A Number, upper IS A Number
GIVETH Bool
_ `<=` _ `<=` _ MEANS lower <= value AND value <= upper

#EVAL 0 `<=` 5 `<=` 10 = TRUE
```

#### 6.2 Error Tests
Create `jl4/examples/not-ok/tc/mixfix-errors.l4`:

```l4
-- Arity mismatch
GIVEN a IS A Number
_ `plus` _ MEANS a + a  -- ERROR: 2 underscores but only 1 GIVEN param

-- Ambiguous
GIVEN a IS A Number, b IS A Number
_ `op` _ MEANS a + b

GIVEN a IS A Number, b IS A Number
_ `op` _ MEANS a * b

#EVAL 3 `op` 5  -- ERROR: Ambiguous
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
- Mixfix is opt-in (requires underscores in pattern)
- No breaking changes to existing APIs

### Gradual Rollout
1. Merge lexer changes (minimal risk)
2. Merge AST changes with backward-compatible constructors
3. Merge parser changes (new syntax only)
4. Merge type checker changes
5. Add tests progressively
6. Update documentation

## Open Questions

1. **Precedence**: Should we allow optional precedence annotations for common cases?
2. **Partial application**: Should mixfix functions support partial application?
3. **Overloading**: Can we have both `_ plus _` and `plus _ _` (same name, different patterns)?
4. **Performance**: What's the overhead of pattern matching at every application site?

## Timeline Estimate

- Phase 1 (AST): 2-3 days
- Phase 2 (Parser): 2-3 days
- Phase 3 (Scanning): 1-2 days
- Phase 4 (Type Checker): 3-4 days
- Phase 5 (Errors): 1 day
- Phase 6 (Testing): 2-3 days
- Phase 7 (Docs): 1 day

**Total**: ~2-3 weeks for full implementation

## Next Steps

1. Decide on AST representation (new constructors vs extending existing)
2. Update all pattern matchers for AppForm
3. Implement parser for underscore patterns
4. Add extraction logic in scanning phase
5. Implement pattern matching in type checker
6. Write comprehensive tests
7. Update documentation

## References

- Spec: `doc/mixfix-operators.md`
- Grammar: `jl4/GRAMMAR.md`
- Type checker: `jl4-core/src/L4/TypeCheck.hs`
- Parser: `jl4-core/src/L4/Parser.hs`
