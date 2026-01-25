# Type Declaration Keywords

These keywords are used in type declarations and type expressions.

## Overview

| Keyword | Purpose |
|---------|---------|
| **IS** | Type assertion / definition |
| **HAS** | Record field declaration |
| **ONE OF** | Enum variant declaration |
| **OF** | Type application |
| **WITH** | Record construction |
| **LIST** | List type / literal |
| **FUNCTION FROM TO** | Function type |
| **FOR ALL** | Universal quantification |
| **TYPE** | Kind of types |

## IS

Connects a name to its type or definition.

### In Type Declarations

```l4
DECLARE Person HAS
  name IS A STRING
  age IS A NUMBER
```

### In Assumptions

```l4
ASSUME x IS A NUMBER
ASSUME f IS A FUNCTION FROM NUMBER TO NUMBER
```

### In Definitions

```l4
DECIDE answer IS 42
DECIDE greeting IS "Hello"
```

## HAS

Declares fields in a record type.

### Syntax

```l4
DECLARE TypeName HAS
  field1 IS A Type1
  field2 IS A Type2
```

### Example

```l4
DECLARE Person HAS
  name IS A STRING
  age IS A NUMBER
  employed IS A BOOLEAN
```

**See [DECLARE](DECLARE.md) and [Types: Records](../types/record-example.l4).**

## ONE OF

Declares variants in an enum (sum) type.

### Syntax

```l4
DECLARE TypeName IS ONE OF
  Variant1
  Variant2
  Variant3 HAS field IS A Type
```

### Example

```l4
DECLARE Status IS ONE OF
  Active
  Inactive
  Pending HAS reason IS A STRING
```

**See [DECLARE](DECLARE.md) and [Types: Enums](../types/enum-example.l4).**

## OF

Used for type application (applying type constructors to arguments).

### With LIST

```l4
LIST OF NUMBER
LIST OF STRING
LIST OF Person
```

### With MAYBE

```l4
MAYBE NUMBER          -- Short form
MAYBE OF NUMBER       -- Explicit form
```

### With Custom Types

```l4
DECLARE Container a HAS value IS A a

ASSUME box IS A Container OF NUMBER
```

## WITH

Constructs record values by specifying field values.

### Syntax

```l4
TypeName WITH
  field1 IS value1
  field2 IS value2
```

### Example

```l4
DECLARE Person HAS
  name IS A STRING
  age IS A NUMBER

DECIDE john IS Person WITH
  name IS "John"
  age IS 30
```

## LIST

Both a type constructor and literal syntax.

### As Type

```l4
ASSUME numbers IS A LIST OF NUMBER
ASSUME names IS A LIST OF STRING
```

### As Literal

```l4
DECIDE myList IS LIST 1, 2, 3, 4, 5
DECIDE empty IS EMPTY
```

**See [Types: Lists](../types/list-example.l4).**

## FUNCTION FROM TO

Declares function types.

### Syntax

```l4
FUNCTION FROM InputType TO OutputType
FUNCTION FROM Type1 AND Type2 TO ResultType
```

### Examples

```l4
-- Single parameter
ASSUME f IS A FUNCTION FROM NUMBER TO NUMBER

-- Multiple parameters (using AND)
ASSUME g IS A FUNCTION FROM NUMBER AND STRING TO BOOLEAN
```

**See [Types: Functions](../types/function-type-example.l4).**

## FOR ALL

Universal type quantification for polymorphic types.

### Syntax

```l4
FOR ALL a . TypeExpression
```

### Example

```l4
-- Identity function works for any type
GIVEN a IS A TYPE
      x IS A a
GIVETH A a
identity x MEANS x
```

## TYPE

The kind of types (used in type-level programming).

### Example

```l4
GIVEN a IS A TYPE
      x IS A a
identity x MEANS x
```

## Related Pages

- **[DECLARE](DECLARE.md)** - Type declarations
- **[GIVEN](GIVEN.md)** - Type parameters in functions
- **[GIVETH](GIVETH.md)** - Return type declarations
- **[ARTICLES](ARTICLES.md)** - A, AN, THE

## See Also

- **[Types Reference](../types/README.md)** - Complete type system documentation
