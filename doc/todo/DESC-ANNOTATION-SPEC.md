# `@desc` Annotation Specification

**Status**: Core Implementation Complete, LSP Hover Integration In Progress

**Related Issues**: #635 (YAML deprecation), NESTED-YAML-SUPPORT.md

## Overview

The `@desc` annotation allows documentation to be embedded directly in L4 source code. This supports the goal of making L4 the single source of truth for function/type documentation, eventually deprecating YAML sidecar files.

## Attachment Rules

### Function-Level Descriptions

A `@desc` on the line immediately before a `DECIDE` or `GIVEN` attaches to the function:

```l4
@desc This function calculates tax
GIVEN income IS A NUMBER
DECIDE calculateTax IS income * 0.2
```

### Parameter Descriptions

Parameters (`GIVEN x IS A type`) can have descriptions attached in two ways:

**1. Leading desc (on preceding line):**

```l4
GIVEN
  @desc The user's income amount
  income IS A NUMBER
DECIDE ...
```

**2. Inline desc (after the type):**

```l4
GIVEN income IS A NUMBER @desc The user's income amount
DECIDE ...
```

### Priority Rules

When both leading and inline `@desc` exist for the same parameter:

1. **Inline takes priority** - The inline desc is used
2. **Warning emitted** - `DescDuplicatePreferInline` warning alerts that the leading desc is ignored

Example that triggers warning:

```l4
GIVEN
  @desc This will be ignored (leading)
  income IS A NUMBER @desc This one wins (inline)
DECIDE ...
```

### Type/Record Field Descriptions

Similar rules apply to `DECLARE` record fields:

**Leading desc:**

```l4
DECLARE Person HAS
  @desc The person's full name
  name IS A STRING
```

**Inline desc:**

```l4
DECLARE Person HAS
  name IS A STRING @desc The person's full name
```

Both styles work; inline takes priority if both are present.

### Type-Level Descriptions

A `@desc` before a `DECLARE` attaches to the type itself:

```l4
@desc Represents a person in the system
DECLARE Person HAS
  name IS A STRING
```

## Implementation Details

### Location in Code

- `jl4-core/src/L4/Parser/ResolveAnnotation.hs` - Main annotation resolution logic
- Key function: `attachLeadingOrInlineDesc` - Handles both leading and inline attachment

### Warning Types

- `DescMissingLocation Desc` - A desc couldn't be attached to any AST node
- `DescDuplicatePreferInline DescWithSpan DescWithSpan` - Both leading and inline exist; inline wins

### Extraction

The `L4.Export` module extracts param descriptions via `extractParams`, which is used by the decision service to generate JSON schema.

### Parameter Description Fallback (Dec 2025)

When a parameter has no explicit `@desc`, the system now falls back to the `@desc` on the parameter's declared type:

```l4
@desc a natural person assumed to have capacity
DECLARE Person HAS name IS A STRING

@desc export Check if person is adult
GIVEN p IS A Person        -- No @desc here, falls back to "a natural person..."
DECIDE isAdult IS p's age >= 18
```

Implementation in `L4.Export`:

- `buildTypeDescMap :: Module Resolved -> Map Unique Text` - Collects `@desc` from all DECLARE statements
- `extractParams` uses `paramDesc <|> fallbackDesc` - Prefers explicit param `@desc`, falls back to type's `@desc`
- `getTypeDesc` looks up the type's Unique in the map

## Test Files

- `jl4/examples/ok/desc.l4` - Original test cases (inline only)
- `jl4/examples/ok/desc-extended.l4` - Extended test cases (leading + mixed styles)

## Known Test Gaps

The following functionality lacks dedicated unit tests:

1. **`buildTypeDescMap`** - No tests verify the map is correctly built from DECLARE statements
2. **Parameter → Type fallback** - No tests verify that parameters without explicit `@desc` fall back to their type's `@desc`
3. **End-to-end swagger output** - No integration tests verify the fallback descriptions appear in generated OpenAPI/swagger.json

**Suggested test cases:**

- Parameter with explicit `@desc` → uses param desc (existing behavior)
- Parameter without `@desc`, type has `@desc` → uses type desc (fallback)
- Parameter without `@desc`, type has no `@desc` → no description
- Multiple parameters of same type → all get the type's desc
- Primitive types (NUMBER, STRING, BOOLEAN) → no fallback (no DECLARE)

## Future Work

1. **JSON Schema Generation** - Generate JSON schema from L4 annotations to fully deprecate YAML sidecars
2. **LSP Integration** - Show `@desc` content in hover tooltips (in progress, see below)
3. **Export Format** - Define standard export format for desc annotations

## LSP Hover Integration (In Progress)

### Goal

Show `@desc` content in hover tooltips when users hover over identifiers. Priority: GIVEN parameter's desc first, falling back to the type's DECLARE desc if absent (same fallback logic as the decision service export).

### Architecture: DescMap Approach (Chosen)

Following the `nlgMap` pattern, we add a parallel `descMap :: RangeMap Text` to the type-checking infrastructure:

1. **Type definitions** (`L4/TypeCheck/Types.hs`):
   - `type DescMap = RangeMap Text`
   - `descMap :: !DescMap` field in `CheckState`
   - `addDescForSrcRange :: SrcRange -> Text -> Check ()` helper

2. **LSP integration** (`LSP/L4/Rules.hs`, `LSP/L4/Actions.hs`):
   - `descMap :: TypeCheck.DescMap` field in `TypeCheckResult`
   - `typeHover` queries `descMap` alongside `infoMap` and `nlgMap`
   - `infoToHover` renders desc below type signature

3. **Wiring** (TODO):
   - Call `addDescForSrcRange` when processing GIVEN parameters with `@desc`
   - Call `addDescForSrcRange` when processing DECLARE types with `@desc`
   - Implement fallback: param desc → type desc

### Alternative Architectures Considered

#### Option A: Embed desc in `Info` type

Instead of a separate `descMap`, modify the `Info` type to carry an optional description:

```haskell
data Info
  = TypeInfo (Type' Resolved) ScopeInfo (Maybe Text)  -- add desc here
  | KindInfo Kind
  | TypeVariable
```

**Pros:**

- Single lookup at hover time (no separate map)
- Related data stays together
- Reuses existing `infoMap` infrastructure

**Cons:**

- Requires modifying core `Info` type
- Must propagate desc through type-checking environment
- Every use site stores its own copy of desc (duplication, but O(1) lookup)

**Challenge for GIVEN parameters:**

When hovering on a _use_ of a GIVEN parameter (e.g., `x` in `x + 1`), the `@desc` lives on the _definition_ site (the `GIVEN x IS A Integer @desc "..."` declaration). To make embedding work:

1. When adding GIVEN param to environment, also store its `@desc`
2. When resolving a use of the param, retrieve desc from environment and include in `Info`

This is essentially the same propagation work as the `descMap` approach, just storing the result differently.

#### Option B: On-demand AST traversal

At hover time, search the AST for the definition of the hovered identifier and extract its `@desc` annotation directly.

**Pros:**

- No pre-computation during type checking
- No additional storage

**Cons:**

- O(n) AST traversal per hover request
- Complex logic to find definition site from use site
- Doesn't fit existing hover infrastructure

#### Why DescMap Was Chosen

1. **Consistency** - Follows existing `nlgMap` pattern exactly
2. **Minimal core changes** - Doesn't modify `Info` or `CheckEntity` types
3. **Efficient lookup** - O(log n) interval map lookup at hover time
4. **Separation of concerns** - Desc handling is isolated to its own map

The tradeoff is an additional map to maintain, but this is outweighed by architectural consistency and isolation of the new feature.

## Migration Notes

Existing L4 files using inline `@desc` will continue to work unchanged. The leading desc style is additive and optional.
