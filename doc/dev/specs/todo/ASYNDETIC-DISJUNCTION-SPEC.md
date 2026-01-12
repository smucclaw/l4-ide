# Specification: Asyndetic Disjunction (`..`) and Ellipsis Linting

**Status:** Planned
**Related:** [Asyndetic Conjunction](20-basic-syntax.md#asyndetic-conjunction-)

## Summary

Extend L4's ellipsis syntax to support both conjunction and disjunction:

- **`...`** (three dots) — asyndetic conjunction (implicit AND)
- **`..`** (two dots) — asyndetic disjunction (implicit OR)

Add LSP diagnostics to warn when ellipsis forms appear adjacent to mismatched operators.

## Motivation

### Symmetry

L4 already supports `...` for implicit AND. Adding `..` for implicit OR provides:

1. **Consistent syntax** for both boolean operators
2. **Cleaner encoding** of legal "or" chains
3. **Natural pairing** with inert elements (which already handle OR context)

### Addressing the Typo Concern

The visual similarity between `..` and `...` raises a legitimate concern about typos. Rather than choosing a more visually distinct syntax (like `||`), we address this through **linting**:

- The LSP backend warns when ellipsis forms appear in unexpected contexts
- This catches typos at edit-time, before they cause bugs
- Maintains L4's "no punctuation" CNL aesthetic

### Why Not `||` and `&&`?

Introducing punctuation-based operators would:

1. Depart from L4's low-code, natural-language promise
2. Create pressure to support both forms (`||`/`&&` alongside `OR`/`AND`)
3. Fragment the syntax with multiple ways to express the same thing
4. Move away from legal drafters toward programmers as the primary audience

## Syntax

### Current (AND only)

```l4
DECIDE `rule applies` IF
        "subject to"
    ... `condition one`
    ... `condition two`
    AND `condition three`
```

### Proposed (AND and OR)

```l4
DECIDE `qualifies` IF
        `is adult`
    AND     `has license`
        ..  `has permit`           -- implicit OR
        OR  `has exemption`
```

Equivalent to:

```l4
DECIDE `qualifies` IF
        `is adult`
    AND     `has license`
        OR  `has permit`
        OR  `has exemption`
```

## Linting Rules

### Rule 1: Three-dot ellipsis must be adjacent to AND

**Warning:** `...` used in OR context

```l4
DECIDE `bad example` IF
        `a`
    OR  `b`
    ... `c`      -- WARNING: `...` (AND ellipsis) used adjacent to OR
```

**Suggestion:** Did you mean `..` (OR ellipsis)?

### Rule 2: Two-dot ellipsis must be adjacent to OR

**Warning:** `..` used in AND context

```l4
DECIDE `bad example` IF
        `a`
    AND `b`
    ..  `c`      -- WARNING: `..` (OR ellipsis) used adjacent to AND
```

**Suggestion:** Did you mean `...` (AND ellipsis)?

### Rule 3: Mixed ellipsis in same chain

**Warning:** Mixing `..` and `...` in ambiguous ways

```l4
DECIDE `confusing` IF
        `a`
    ... `b`
    ..  `c`      -- WARNING: Mixed ellipsis forms; consider explicit AND/OR
```

### Determining "Adjacent Context"

The linter determines context by examining the nearest explicit boolean operator:

1. **Sibling operators** — Look at other operators at the same indentation level
2. **Parent operator** — If no siblings, look at the enclosing boolean expression
3. **Default** — If no context can be determined, assume AND (existing behavior)

## Implementation Notes

### Lexer Changes

Add `..` as a new token, distinct from `...`:

```haskell
-- In L4/Lexer.hs
ellipsisAnd  = symbol "..."  -- existing
ellipsisOr   = symbol ".."   -- new (must be checked AFTER ... to avoid prefix match)
```

**Important:** The lexer must try `...` before `..` to avoid `...` being tokenized as `..` + `.`.

### Parser Changes

```haskell
-- In L4/Parser.hs
pEllipsisAnd :: Parser ()
pEllipsisAnd = void $ symbol "..."

pEllipsisOr :: Parser ()
pEllipsisOr = void $ symbol ".."
```

Both desugar to their respective operators during parsing or in the desugaring phase.

### Type Checker / Desugarer

No changes needed—`..` simply becomes `Or` just as `...` becomes `And`.

### LSP Diagnostics

Add a new diagnostic pass that:

1. Walks the AST looking for `EllipsisAnd` and `EllipsisOr` nodes
2. Determines the expected context from surrounding operators
3. Emits warnings for mismatches

Severity: **Warning** (not error)—the code is still valid, just potentially confusing.

### Visualization

The ladder diagram already handles OR correctly. The `..` syntax should flow through unchanged, appearing as implicit OR in the visualization.

## Terminology

| Syntax | Name                 | Semantics                            |
| ------ | -------------------- | ------------------------------------ |
| `...`  | Ellipsis (three-dot) | Asyndetic conjunction (implicit AND) |
| `..`   | Ellipsis (two-dot)   | Asyndetic disjunction (implicit OR)  |

Both are forms of **asyndeton**—the omission of conjunctions/disjunctions between list items.

## Test Cases

### Valid Usage

```l4
-- Three-dot with AND
DECIDE `valid1` IF `a` AND `b` ... `c`

-- Two-dot with OR
DECIDE `valid2` IF `a` OR `b` .. `c`

-- Mixed with clear structure
DECIDE `valid3` IF
        `a`
    AND     `b`
        OR  `c`
        ..  `d`
    ... `e`
```

### Should Warn

```l4
-- Three-dot in OR context
DECIDE `warn1` IF `a` OR `b` ... `c`

-- Two-dot in AND context
DECIDE `warn2` IF `a` AND `b` .. `c`
```

## Migration

This is a purely additive feature. Existing code using `...` continues to work unchanged. The linting rules are warnings, not errors, so they won't break existing builds.

## Open Questions

1. **Should the linter be enabled by default?** Probably yes, as warnings.

2. **Should there be a quick-fix action?** The LSP could offer "Replace `...` with `..`" as a code action.

3. **What about nested contexts?** E.g., `... (a OR b)` — the outer `...` is in AND context even though it contains OR. This should NOT warn.

## References

- [Asyndetic Conjunction](20-basic-syntax.md#asyndetic-conjunction-)
- [Inert Elements](10-boolean-logic.md#inert-elements-grammatical-scaffolding)
- [Boolean Logic](10-boolean-logic.md)
