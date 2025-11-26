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

### 3. Design Simplification (✅ Complete)
- **No lexer changes needed!**
- Underscores (`_`) are purely conceptual - never appear in source code
- Users write `a plus b`, not `_ plus _`
- Pattern extraction works by comparing tokens against GIVEN parameters
- **Status**: ✅ Compiles successfully, all tests pass

## Remaining Work

### Phase 1: Scanning Phase (🔲 Not Started)
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

### Phase 2: Type Checker (🔲 Not Started)
**Estimated**: 2-3 days

Implement pattern matching at call sites:
- Find keywords in token sequence
- Extract arguments between keywords
- Type check arguments
- Disambiguate using types

### Phase 3: Error Handling (🔲 Not Started)
**Estimated**: 1 day

Add new error types:
- `NoMatchingMixfixPattern`
- `AmbiguousMixfix`
- `MixfixArityMismatch`

### Phase 4: Testing (🔲 Not Started)
**Estimated**: 1-2 days

Create test files:
- `jl4/examples/ok/mixfix-basic.l4`
- `jl4/examples/not-ok/tc/mixfix-errors.l4`

### Phase 5: Documentation (🔲 Not Started)
**Estimated**: 1 day

Update user-facing docs

## Design Decisions Made

1. **Parameter names in patterns**: Users write `a plus b`, not `_ plus _`
2. **Underscores are conceptual**: Only used internally to represent pattern structure
3. **Pattern extraction via comparison**: Compare AppForm tokens against GIVEN params
4. **No AST/parser changes**: Existing structures already support this
5. **No implicit precedence**: Require parentheses/indentation for disambiguation
6. **Type-directed resolution**: Types help select correct function at call sites
7. **Leverages existing scanning**: Multi-pass infrastructure already in place

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
