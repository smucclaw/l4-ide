# AI Reference for L4

Terse technical reference optimized for AI assistants, RAG systems, and context-limited LLMs.

## Contents

- [keywords.md](keywords.md) - All keywords with syntax, types, examples
- [patterns.md](patterns.md) - Canonical code templates
- [types.md](types.md) - Type system and FP correspondence
- [errors.md](errors.md) - Common errors and fixes

## L4 in Brief

Domain-specific language for computational law. Haskell-inspired: layout-sensitive, strongly typed, algebraic data types, pure functional core with deontic effects.

**Key paradigm:** Constitutive rules (DECIDE/MEANS) + Regulative rules (PARTY MUST/MAY/SHANT) + Pattern matching (CONSIDER) + Effects (DEONTIC).

## Quick Syntax

```l4
-- Type definitions
DECLARE T HAS field1 IS A Type1, field2 IS A Type2
DECLARE Status IS ONE OF Active | Suspended STRING | Closed

-- Function signatures
GIVEN param IS A Type
GIVETH ReturnType
identifier MEANS expression

-- Decisions (boolean predicates)
DECIDE `is valid` IF condition

-- Pattern matching
CONSIDER value
WHEN Constructor args THEN result
OTHERWISE default

-- Regulative rules
PARTY actor MUST action TO beneficiary IF condition
```

## For Humans

See [doc/](../doc/) for narrative tutorials and learning paths.
