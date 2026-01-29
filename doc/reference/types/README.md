# L4 Types Reference

L4's type system provides strong static typing with type inference, algebraic data types, and polymorphism. This reference documents all types available in L4.

## Overview

L4's type system is inspired by functional programming languages like Haskell and includes:

- **Primitive types** - Basic data types (NUMBER, STRING, BOOLEAN, DATE)
- **Algebraic types** - Records (products) and enums (sums)
- **Polymorphic types** - Generic types parameterized by other types
- **Function types** - First-class functions
- **Type constructors** - Building complex types from simpler ones

---

## Primitive Types

### NUMBER

Numeric values including integers and rationals.

- Integers: `42`, `-17`, `0`
- Rationals: `3.14`, `-0.5`, `2.718`
- Arbitrary precision arithmetic
- **Examples:** Age calculations, financial amounts, quantities

### STRING

Text strings enclosed in double quotes.

- Unicode support
- Escape sequences: `\"`, `\\`, `\n`, etc.
- String concatenation with CONCAT or APPEND
- **Examples:** Names, descriptions, legal text

### BOOLEAN

Truth values for logical operations.

- Values: `TRUE`, `FALSE`
- Three-valued logic support (with UNKNOWN for partial evaluation)
- Logical operators: AND, OR, NOT, IMPLIES
- **Examples:** Eligibility checks, conditions, yes/no answers

### DATE

Calendar dates for temporal reasoning.

- ISO 8601 format: `YYYY-MM-DD`
- Date arithmetic and comparison
- Integration with daydate library
- **Examples:** Deadlines, effective dates, time periods

---

## Algebraic Types

### Records

Product types with named fields.

**Example:** [record-example.l4](record-example.l4)

- Named fields with types
- Field access with `'s` or `THE ... OF`
- Construction with `WITH` keyword
- **Examples:** Structured data, entities, configurations

### Enums

Sum types with named constructors.

**Example:** [enum-example.l4](enum-example.l4)

- Pattern matching with CONSIDER/WHEN
- Constructors with optional fields
- Discriminated unions
- **Examples:** Status values, categories, variants

### PAIR

Two-element product type from prelude.

**Note:** PAIR is defined in the prelude library. See [prelude.l4](https://github.com/smucclaw/l4-ide/blob/main/jl4-core/libraries/prelude.l4) for implementation.

- Generic over both element types
- Used in key-value data structures
- Tuple-like functionality
- **Examples:** Coordinates, key-value pairs, associations

---

## Polymorphic Types

### LIST

Ordered collection of elements of the same type.

**Example:** [list-example.l4](list-example.l4)

- Homogeneous (all elements same type)
- Recursive structure
- Rich prelude functions: map, filter, fold, etc.
- **Examples:** Collections, sequences, ordered data

### MAYBE

Optional values that may be present or absent.

**Example:** [maybe-example.l4](maybe-example.l4)

- Handles nullability explicitly
- No null pointer errors
- Pattern matching for safety
- **Examples:** Optional fields, lookup results, partial functions

### EITHER

Choice between two alternative values.

**Example:** [either-example.l4](either-example.l4)

- Discriminated union of two types
- Often used for error handling
- Pattern matching to handle both cases
- **Examples:** Success/failure, validation, branching logic

### Dictionary

Associative map from keys to values (from prelude).

**Note:** Dictionary is defined in the prelude library. See [prelude.l4](https://github.com/smucclaw/l4-ide/blob/main/jl4-core/libraries/prelude.l4) for implementation.

- Generic key-value store
- Lookup by key
- Rich API: insert, delete, update, union, etc.
- **Examples:** Configuration, mappings, indexes

---

## Type Constructors

### TYPE

The kind of types (a type of types).

- Used in type signatures
- `IS A TYPE` declares a type parameter
- Enables polymorphism
- **Examples:** Generic functions, type parameters

### FUNCTION

Function types.

**Example:** [function-type-example.l4](function-type-example.l4)

- First-class functions
- Multi-parameter with AND
- Higher-order functions supported
- **Examples:** Callbacks, operators, combinators

---

## Type System Features

### Type Inference

L4 can often infer types automatically.

**Example:** [type-inference-example.l4](type-inference-example.l4)

### Type Annotations

Explicit types improve clarity and catch errors. See examples throughout the type reference pages.

### Polymorphism

Generic types work with any type.

**Example:** [polymorphic-example.l4](polymorphic-example.l4)

### Algebraic Data Types

Combine product (AND) and sum (OR) types:

- Products: Multiple fields together (records)
- Sums: One of several alternatives (enums)
- Composable and nestable

---

## Type Compatibility

### Coercion

Built-in conversion functions:

- **TOSTRING** - Convert to STRING
- **TONUMBER** - Convert to NUMBER
- **TODATE** - Convert to DATE
- **TRUNC** - Truncate to integer

See [Built-ins](../builtins/README.md) and [coercions](coercions.md) for details.

### Comparison

- Equality (`EQUALS`, `=`) works for most types
- Ordering (`<`, `>`, `<=`, `>=`) for NUMBER, STRING, DATE
- Custom comparison via functions

### Maybe and Nullability

L4 uses MAYBE for optional values, not null.

[maybe-example.l4](maybe-example.l4) for usage patterns.

---

## Common Patterns

### Option Type Pattern

Use MAYBE for nullable values. See prelude's `lookup` function

[maybe-example.l4](maybe-example.l4)

### Either for Errors

Use EITHER for success/failure.

[either-example.l4](either-example.l4)

### Lists for Collections

Use LIST for ordered data. See prelude's `sum` function

[list-example.l4](list-example.l4)

### Records for Structured Data

Use records for entities.

[record-example.l4](record-example.l4)

---

## Type Hierarchy

```
TYPE
  ├─ Primitive Types
  │   ├─ NUMBER
  │   ├─ STRING
  │   ├─ BOOLEAN
  │   └─ DATE
  │
  ├─ Algebraic Types
  │   ├─ Records (product types)
  │   └─ Enums (sum types)
  │
  ├─ Polymorphic Types
  │   ├─ LIST OF a
  │   ├─ MAYBE a
  │   ├─ EITHER a b
  │   ├─ PAIR OF a, b
  │   └─ Dictionary k v
  │
  └─ Function Types
      └─ FUNCTION FROM args TO result
```

---

## Learning Path

### Beginners

1. Start with primitive types: NUMBER, STRING, BOOLEAN
2. Learn records for structured data
3. Understand LIST for collections

### Intermediate

1. Master MAYBE for optional values
2. Use enums for variants
3. Explore EITHER for error handling

### Advanced

1. Write polymorphic functions with type parameters
2. Use Dictionary for complex data structures
3. Master higher-order functions

---

## See Also

- **[GLOSSARY](../GLOSSARY.md)** - Complete language index
- **[Functions](../functions/README.md)** - Function keywords
- **[Operators](../operators/README.md)** - Operations on types
- **[Specifications](https://github.com/smucclaw/l4-ide/tree/main/specs)** - Technical specifications
