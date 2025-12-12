# `@desc` Annotation Specification

**Status**: Implementation Complete

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

## Test Files

- `jl4/examples/ok/desc.l4` - Original test cases (inline only)
- `jl4/examples/ok/desc-extended.l4` - Extended test cases (leading + mixed styles)

## Future Work

1. **JSON Schema Generation** - Generate JSON schema from L4 annotations to fully deprecate YAML sidecars
2. **LSP Integration** - Show `@desc` content in hover tooltips
3. **Export Format** - Define standard export format for desc annotations

## Migration Notes

Existing L4 files using inline `@desc` will continue to work unchanged. The leading desc style is additive and optional.
