# Specification: Relaxed LIST Literal Syntax

**Status:** 📋 Proposed
**Priority:** Low (Quality of Life)
**Effort:** Small (Parser change + tests)

## Executive Summary

L4 supports multiple LIST literal syntaxes. Users naturally expect vertical list syntax (elements below `LIST`) to work everywhere, including inside `LET ... IN`. In practice, vertical `LIST` blocks already work in most contexts; the confusing failures happen specifically with _inline_ `LET` bindings where the RHS starts on the next line. This spec proposes a small, constrained parser relaxation so that inline `LET` bindings can use normal block indentation on their RHS.

## Clarification (Author Misunderstanding)

The original motivation text assumed “vertical LIST blocks don’t parse”. That’s not quite true in the current parser:

- `LIST` already accepts a newline + indented block of element expressions in general expression contexts.
- The failure people hit is triggered by the indentation policy for **inline** `LET` bindings (`LET name BE` on one line). In that form, the RHS expression used to be required to start _to the right of the binding name_, which makes a normal block-indented `LIST` (and other multiline RHS forms) look “incorrectly indented”.

So the fix is not “teach LIST about vertical blocks”, but “relax inline-LET RHS indentation (in a constrained way)”.

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

Users naturally expect this to work inside `LET ... IN`:

```l4
LET myList BE
  LIST
    1
    2
    3
IN ...
```

In older versions / current behavior before this change, it fails due to indentation rules in inline `LET` bindings (the RHS is required to start to the right of the binding name, not just under `LET`).

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

### Enable Normal Block Indentation for Inline LET RHS

When `LET` and the binding name are on the same line, allow the RHS expression to begin at normal block indentation under `LET`:

```l4
LET providers BE
  LIST
    tryOpenRouter prompt
    tryOpenAI prompt
    tryAnthropic prompt
IN firstJust providers
```

This is equivalent to (and should remain supported):

```l4
LET providers BE
      LIST tryOpenRouter prompt
           tryOpenAI prompt
           tryAnthropic prompt
IN firstJust providers
```

### Constraint (to keep parsing unambiguous)

This relaxation applies only when the `LET ... IN` contains **exactly one** binding in that `LET` block. If you need multiple bindings, use the multiline `LET` layout:

```l4
LET
  x BE
    ...
  y BE
    ...
IN ...
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

Vertical `LIST` blocks are already parsed as a normal multi-line expression list. The required change is in the `LET` binding parser:

1. Detect inline `LET <name> BE` bindings (binding name on the same line as `LET`).
2. In the single-binding case, parse the RHS expression with indentation relative to `LET` (not relative to the binding name).
3. Require `IN` immediately after that one binding (enforces the “single binding” constraint).

### Affected Grammar Rules

Location: `jl4-core/src/L4/Parser.hs`

Relevant rules:

- `list` expression parser (already accepts vertical blocks)
- `letInExpr` / LET binding parsing (needs the relaxation)

### Backward Compatibility

This change is intended to be backward compatible for existing programs. It is additive in the sense that it allows additional indentation layouts for inline single-binding `LET` blocks.

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

**Note**: Current parser behavior accepts `LIST` with no elements; tightening that is out of scope for this change (and would be a breaking change).

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
  - Update: vertical LIST blocks are already supported; instead, relax inline single-binding LET RHS indentation
  - Location: `jl4-core/src/L4/Parser.hs`

- [ ] **Task 2**: Add parser tests
  - Location: `jl4/examples/ok/` and `jl4/examples/not-ok/tc/`
  - Add a regression example for inline LET + vertical LIST RHS (previously failing)
  - Add a regression example for inline LET + record construction RHS (same indentation issue)

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
const myList = [item1, item2, item3];
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
