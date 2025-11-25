# Mixfix Operators - Current Status

## What We've Built

### Branch: `mengwong/mixfix`

This branch implements the foundation for user-defined mixfix operators in L4, enabling natural language-style function definitions like:

```l4
GIVEN person IS A Person, program IS A Program
GIVETH Bool
_ `is eligible for` _ MEANS
  person's age >= 18 AND ...

-- Call site:
alice `is eligible for` healthcare
```

## Completed Work

### 1. Specification (✅ Complete)
- **File**: `doc/mixfix-operators.md`
- Comprehensive design document covering:
  - Motivation and goals
  - Syntax at definition and call sites
  - Precedence and disambiguation strategy
  - Type checking approach
  - Examples and use cases

### 2. Implementation Plan (✅ Complete)
- **File**: `doc/mixfix-implementation-plan.md`
- Detailed roadmap with:
  - Phase-by-phase breakdown
  - Timeline estimates (~2-3 weeks total)
  - Open questions and design decisions
  - Migration strategy

### 3. Lexer Changes (✅ Complete)
- **File**: `jl4-core/src/L4/Lexer.hs`
- Added `TUnderscore` token to `TSymbols` enum
- Added `"_"` mapping in symbols map
- **Status**: ✅ Compiles successfully

## Remaining Work

### Phase 1: AST Changes (🔲 Not Started)
**Estimated**: 2-3 days

Extend `AppForm` in `Syntax.hs` to represent mixfix patterns:

```haskell
data PatternToken n
  = ParamHole      -- underscore (_)
  | Keyword n      -- backticked identifier

data AppForm n
  = PrefixAppForm Anno n [n] (Maybe (Aka n))
  | MixfixAppForm Anno [PatternToken n] (Maybe (Aka n))
```

**Files to update** (15 total):
- `L4/Syntax.hs` - AST definition
- `L4/Parser.hs` - parsing
- `L4/TypeCheck.hs` - type checking
- `L4/Print.hs` - pretty printing
- `L4/Desugar.hs` - desugaring
- `L4/EvaluateLazy/Machine.hs` - evaluation
- `L4/Names.hs` - name handling
- And 8 more...

### Phase 2: Parser Extension (🔲 Not Started)
**Estimated**: 2-3 days

Update `appForm` parser to handle:
- Underscore symbols
- Mixed sequences of underscores and keywords
- Validation (must have at least one underscore)

```haskell
appForm :: Parser (AppForm Name)
appForm = mixfixAppForm <|> prefixAppForm
```

### Phase 3: Scanning Phase (🔲 Not Started)
**Estimated**: 1-2 days

Extract mixfix pattern info during `scanFunSigDecide`:

```haskell
data MixfixInfo = MkMixfixInfo
  { keywords :: [Name]
  , arity :: Int
  , keywordPositions :: [Int]
  , pattern :: [PatternToken Name]
  }

-- Add to FunTypeSig:
data FunTypeSig = MkFunTypeSig
  { ...
  , mixfixInfo :: Maybe MixfixInfo
  }
```

### Phase 4: Type Checker (🔲 Not Started)
**Estimated**: 3-4 days

Implement pattern matching at call sites:

```haskell
tryMixfixApplication :: [Name] -> Check [(Expr Resolved, Type' Resolved)]
-- Matches token sequences against registered mixfix patterns
-- Uses types to disambiguate when multiple patterns match
```

### Phase 5: Error Handling (🔲 Not Started)
**Estimated**: 1 day

Add new error types:
- `NoMatchingMixfixPattern`
- `AmbiguousMixfix`
- `MixfixArityMismatch`

### Phase 6: Testing (🔲 Not Started)
**Estimated**: 2-3 days

Create test files:
- `jl4/examples/ok/mixfix-basic.l4` - basic functionality
- `jl4/examples/ok/mixfix-complex.l4` - advanced patterns
- `jl4/examples/not-ok/tc/mixfix-errors.l4` - error cases

### Phase 7: Documentation (🔲 Not Started)
**Estimated**: 1 day

Update:
- `GRAMMAR.md` - formal syntax
- `doc/guide-index.md` - user guide
- `doc/GLOSSARY.md` - terminology

## Design Decisions Made

1. **Explicit underscores**: Required in patterns (not inferred from GIVEN)
2. **No underscores = prefix**: Backward compatible with existing code
3. **No implicit precedence**: Require parentheses/indentation for disambiguation
4. **Type-directed resolution**: Types help select correct function at call sites
5. **Leverages existing scanning**: Multi-pass infrastructure already in place

## Key Insights from Discussion

1. **Backticks are for whitespace**: All identifiers can use backticks, not just functions
2. **Forward references work**: L4's scanning phase handles this already
3. **TDNR precedent**: L4 already uses type-directed name resolution
4. **Compositional mixfix**: Binary operators compose to create complex patterns

## Example Use Cases

### Infix
```l4
GIVEN a IS A Number, b IS A Number
_ `plus` _ MEANS a + b

3 `plus` 5  -- Result: 8
```

### Postfix
```l4
GIVEN amount IS A Number
_ `percent` MEANS amount / 100

50 `percent`  -- Result: 0.5
```

### Ternary
```l4
GIVEN lower IS A Number, value IS A Number, upper IS A Number
_ `<=` _ `<=` _ MEANS lower <= value AND value <= upper

0 `<=` 5 `<=` 10  -- Result: TRUE
```

### Complex Pattern
```l4
GIVEN person1 IS A Person, person2 IS A Person, child IS A Person
_ `copulated with` _ `to make` _ MEANS ...

alice `copulated with` bob `to make` charlie
```

## Next Steps

1. **Decide on AST approach**: New constructors vs extending existing
2. **Begin Phase 1**: Update AppForm and all pattern matchers
3. **Iterative testing**: Add tests after each phase
4. **Get feedback**: Share examples with legal domain experts

## How to Continue

```bash
# Checkout the branch
git checkout mengwong/mixfix

# See current status
git log --oneline

# Continue with Phase 1 (AST changes)
# Edit jl4-core/src/L4/Syntax.hs
# Then update all 15 files that pattern match on AppForm
```

## Related Files

- Spec: `doc/mixfix-operators.md`
- Implementation Plan: `doc/mixfix-implementation-plan.md`
- Lexer: `jl4-core/src/L4/Lexer.hs`
- Syntax: `jl4-core/src/L4/Syntax.hs`
- Parser: `jl4-core/src/L4/Parser.hs`
- Type Checker: `jl4-core/src/L4/TypeCheck.hs`

## Questions or Feedback?

See the spec and implementation plan for detailed design rationale. The foundation is in place - ready for the next developer to continue!
