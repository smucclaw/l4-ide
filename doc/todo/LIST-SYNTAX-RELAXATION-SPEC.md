# Specification: Relaxed LIST Literal Syntax

**Status:** 📋 Proposed
**Priority:** Low (Quality of Life)
**Effort:** Small (Parser change)

## Executive Summary

L4 currently supports multiple LIST literal syntaxes but has inconsistent indentation requirements. Users naturally expect vertical list syntax (elements below `LIST`) to work, but it currently fails with parse errors. This spec proposes relaxing the parser to accept more intuitive list construction patterns.

## Current State

### Syntax Options That Work

L4 currently accepts these LIST syntaxes:

**Option 1: Inline with commas**
```l4
LIST 1, 2, 3
```

**Option 2: Multiline with commas**
```l4
LIST
    1,
    2,
    3
```

**Option 3: Indented without commas (horizontal alignment)**
```l4
LIST 1
     2
     3
```

**Option 4: FOLLOWED BY (cons operator)**
```l4
1 FOLLOWED BY 2 FOLLOWED BY 3 FOLLOWED BY EMPTY
```

### The Problem

**Option 3 requires horizontal alignment with `LIST` keyword**, which is non-intuitive. Users naturally expect this to work:

```l4
LET myList BE
  LIST
    1
    2
    3
IN ...
```

But it fails with: `unexpected <element>, expecting =, BE, IS, MEAN, MEANS`

The working version requires awkward alignment:

```l4
LET myList BE
      LIST 1
           2
           3
IN ...
```

### Why This Matters

1. **Intuitiveness**: Vertical block syntax is more natural for multiline lists
2. **Consistency**: Other L4 constructs (like `CONSIDER`) use vertical blocks
3. **Discoverability**: Users try the vertical syntax first, hit errors, and get confused
4. **Readability**: Long lists would be cleaner with vertical layout

## Proposed Enhancement

### New Syntax: Vertical LIST Blocks

Allow `LIST` to start an indented block where elements appear on subsequent lines:

```l4
LET providers BE
  LIST
    tryOpenRouter prompt
    tryOpenAI prompt
    tryAnthropic prompt
IN firstJust providers
```

This would be equivalent to:

```l4
LET providers BE
      LIST tryOpenRouter prompt
           tryOpenAI prompt
           tryAnthropic prompt
IN firstJust providers
```

### Syntax Variants to Support

**Variant 1: Simple values**
```l4
LIST
  1
  2
  3
```

**Variant 2: Complex expressions**
```l4
LIST
  tryProvider "openrouter"
  tryProvider "openai"
  tryProvider "anthropic"
```

**Variant 3: With commas (currently works, keep supporting)**
```l4
LIST
  1,
  2,
  3
```

**Variant 4: Records**
```l4
LIST
  Person WITH name IS "Alice", age IS 25
  Person WITH name IS "Bob", age IS 30
  Person WITH name IS "Carol", age IS 35
```

## Implementation Notes

### Parser Changes

The parser would need to:

1. **Detect `LIST` followed by newline + indent** → Enter block mode
2. **Collect all indented expressions** at the same level as list elements
3. **Exit block** when indentation returns to LIST level or less
4. **Support both comma and newline separators** (currently commas work)

### Affected Grammar Rules

Location: Likely in `jl4-core/src/L4/Syntax/Parser.hs` or similar

Current grammar (approximate):
```haskell
listLiteral = do
  reserved "LIST"
  elems <- commaSep1 expr <|> horizontalAlignedExprs
  return $ ListLit elems
```

Proposed addition:
```haskell
listLiteral = do
  reserved "LIST"
  choice
    [ commaSep1 expr              -- LIST 1, 2, 3
    , horizontalAlignedExprs      -- LIST 1\n     2\n     3
    , verticalBlockExprs          -- LIST\n  1\n  2\n  3  (NEW!)
    ]
```

Where `verticalBlockExprs` would:
```haskell
verticalBlockExprs = do
  -- After LIST keyword, expect newline + indent
  newline
  indentLevel <- getIndentLevel
  elems <- many1 (indentedExpr indentLevel)
  return elems
```

### Backward Compatibility

This change is **100% backward compatible**:
- All existing LIST syntaxes continue to work
- New syntax is purely additive
- No breaking changes to existing code

### Type System Impact

**None.** This is purely a surface syntax change. The AST node for list literals remains the same.

## Examples in the Wild

### Current Workarounds

Users currently work around this limitation by:

1. **Using horizontal alignment** (awkward):
   ```l4
   LIST item1
        item2
        item3
   ```

2. **Using commas** (verbose for long lists):
   ```l4
   LIST
     item1,
     item2,
     item3
   ```

3. **Using FOLLOWED BY** (wrong - creates nested types!):
   ```l4
   LIST item1
   FOLLOWED BY item2
   FOLLOWED BY item3
   ```

### After This Enhancement

All users could write the natural syntax:

```l4
LIST
  item1
  item2
  item3
```

## Use Cases

### Use Case 1: Provider Fallback Lists

**Before:**
```l4
LET attempts BE
      LIST tryOpenRouter prompt
           tryOpenAI prompt
           tryAnthropic prompt
IN firstJust attempts
```

**After:**
```l4
LET attempts BE
  LIST
    tryOpenRouter prompt
    tryOpenAI prompt
    tryAnthropic prompt
IN firstJust attempts
```

### Use Case 2: Configuration Lists

**Before:**
```l4
DECIDE supportedCurrencies IS
      LIST "USD"
           "EUR"
           "GBP"
           "JPY"
```

**After:**
```l4
DECIDE supportedCurrencies IS
  LIST
    "USD"
    "EUR"
    "GBP"
    "JPY"
```

### Use Case 3: Test Data

**Before:**
```l4
DECIDE testCases IS
      LIST TestCase WITH input IS 1, expected IS 2
           TestCase WITH input IS 5, expected IS 10
           TestCase WITH input IS 0, expected IS 0
```

**After:**
```l4
DECIDE testCases IS
  LIST
    TestCase WITH input IS 1, expected IS 2
    TestCase WITH input IS 5, expected IS 10
    TestCase WITH input IS 0, expected IS 0
```

## Edge Cases to Handle

### Empty Lists

Should this work?
```l4
LIST
  -- No elements
```

**Recommendation**: No, use `EMPTY` for empty lists. `LIST` with no elements should be a parse error.

### Mixed Separators

Should commas be optional within vertical blocks?
```l4
LIST
  1, 2,
  3, 4,
  5
```

**Recommendation**: Yes, allow commas as optional separators (like trailing commas in many languages).

### Nested Lists

```l4
LIST
  LIST 1, 2
  LIST 3, 4
  LIST 5, 6
```

**Recommendation**: Should work naturally - each inner LIST is an expression.

## Testing Requirements

### Parser Tests

Add test cases for:

1. **Basic vertical syntax**
   ```l4
   LIST
     1
     2
     3
   ```

2. **With complex expressions**
   ```l4
   LIST
     foo bar
     baz qux
     quux corge
   ```

3. **With records**
   ```l4
   LIST
     Person WITH name IS "Alice", age IS 25
     Person WITH name IS "Bob", age IS 30
   ```

4. **Nested lists**
   ```l4
   LIST
     LIST 1, 2
     LIST 3, 4
   ```

5. **Mixed with commas**
   ```l4
   LIST
     1, 2,
     3, 4
   ```

### Golden Tests

Update golden test outputs for examples using the new syntax.

## Implementation Tasks

- [ ] **Task 1**: Modify parser to accept vertical LIST blocks
  - Location: `jl4-core/src/L4/Syntax/Parser.hs` (or equivalent)
  - Add `verticalBlockExprs` alternative to `listLiteral` parser
  - Handle indentation tracking

- [ ] **Task 2**: Add parser tests
  - Location: `jl4-core/test/` or `jl4/test/`
  - Add positive test cases for new syntax
  - Add negative test cases (empty blocks, bad indentation)

- [ ] **Task 3**: Update golden tests
  - Run test suite to regenerate golden files
  - Verify outputs are correct

- [ ] **Task 4**: Update documentation
  - Update `doc/20-basic-syntax.md` to show vertical syntax as primary
  - Update `doc/foundation-course-ai/module-4-lists-maybe.md`
  - Mark horizontal alignment as "advanced" or "alternative"

## Non-Goals

This spec does NOT propose:

- Changing type system behavior
- Changing AST representation
- Adding new list operators
- Changing runtime semantics

This is **purely a surface syntax enhancement** to make L4 more intuitive.

## Success Criteria

1. ✅ All existing LIST syntaxes continue to work (backward compatible)
2. ✅ Vertical `LIST\n  elem1\n  elem2` syntax parses correctly
3. ✅ Type checking behavior unchanged
4. ✅ Golden tests pass
5. ✅ Documentation updated to show new syntax

## Related Work

### Similar Patterns in L4

L4 already uses vertical blocks for:

- **CONSIDER/WHEN** - Pattern matching
- **GIVEN** - Multiple parameters
- **DECLARE ... HAS** - Record fields

The LIST vertical syntax would align with these existing patterns.

### Inspiration from Other Languages

**Python:**
```python
my_list = [
    item1,
    item2,
    item3
]
```

**Haskell:**
```haskell
myList =
  [ item1
  , item2
  , item3
  ]
```

**JavaScript:**
```javascript
const myList = [
  item1,
  item2,
  item3
]
```

All support vertical list literals. L4 should too!

## Questions for Discussion

1. Should commas be required, optional, or forbidden in vertical blocks?
   - **Recommendation**: Optional (most flexible)

2. Should we deprecate horizontal alignment syntax?
   - **Recommendation**: No, keep it as an alternative

3. Should empty vertical blocks be allowed?
   - **Recommendation**: No, use `EMPTY` instead

4. Maximum indentation level for nested vertical lists?
   - **Recommendation**: No artificial limit, follow standard indentation rules

## References

- Current implementation: `jl4-core/src/L4/Syntax/Parser.hs`
- Existing LIST docs: `doc/foundation-course-ai/module-4-lists-maybe.md`
- Syntax guide: `doc/20-basic-syntax.md`
- Related: Indentation rules in expression parsing

## Changelog

- **2025-12-27**: Initial spec created based on user feedback during LLM integration work
