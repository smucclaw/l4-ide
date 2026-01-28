# UNLESS

Exception clause operator. Syntactic sugar for `AND NOT` with precedence lower than OR, designed to express legal exceptions naturally.

## Syntax

```l4
condition UNLESS exception
```

## Semantics

`A UNLESS B` is equivalent to `A AND NOT B`

```l4
-- These are equivalent:
x UNLESS y
x AND NOT y
```

## Precedence

**Key Feature:** UNLESS binds *looser* than both AND and OR. This makes it act as an exception to the entire preceding expression:

```l4
-- UNLESS applies to the ENTIRE preceding expression:
A AND B AND C UNLESS D  ≡  (A AND B AND C) AND NOT D
A OR B OR C UNLESS D    ≡  (A OR B OR C) AND NOT D

-- NOT like this (which would happen with higher precedence):
-- A OR B UNLESS C  ≠  A OR (B AND NOT C)
```

## Examples

**Example file:** [unless-example.l4](unless-example.l4)

### Basic Usage

```l4
-- Simple exception
DECIDE result IF TRUE UNLESS FALSE
#ASSERT result  -- TRUE AND NOT FALSE = TRUE

DECIDE blocked IF TRUE UNLESS TRUE
#ASSERT NOT blocked  -- TRUE AND NOT TRUE = FALSE
```

### Legal Exception Pattern

```l4
-- "The applicant is eligible if they are a citizen
--  and have resided for 5 years, UNLESS they have been disqualified"

ASSUME `is a citizen` IS A BOOLEAN
ASSUME `has resided for 5 years` IS A BOOLEAN
ASSUME `has been disqualified` IS A BOOLEAN

DECIDE `is eligible` IF
      `is a citizen`
  AND `has resided for 5 years`
  UNLESS `has been disqualified`
```

This reads naturally and correctly evaluates as:
```
(is a citizen AND has resided for 5 years) AND NOT has been disqualified
```

### Multi-line Format

UNLESS works well with L4's layout-sensitive syntax:

```l4
DECIDE `can enter premises` IF
         `has valid badge`
  AND    `during business hours`
  AND    `not on restricted list`
  UNLESS `building is closed`
```

### With OR Conditions

UNLESS applies to the entire OR expression:

```l4
-- "Eligible if employee OR contractor, UNLESS terminated"
DECIDE `has access` IF
         `is employee`
  OR     `is contractor`
  UNLESS `has been terminated`

-- Evaluates as: (is employee OR is contractor) AND NOT has been terminated
```

## Why UNLESS?

### Natural Legal Language

Legal text commonly uses "unless" to express exceptions:

- "The contract is valid **unless** signed under duress"
- "Benefits apply **unless** the applicant has been disqualified"
- "Access is permitted **unless** revoked by administrator"

### Avoids Parentheses

Without UNLESS, you'd need explicit grouping:

```l4
-- Without UNLESS (requires parentheses):
DECIDE eligible IF (condA AND condB AND condC) AND NOT exception

-- With UNLESS (reads naturally):
DECIDE eligible IF condA AND condB AND condC UNLESS exception
```

### Correct Precedence by Default

If UNLESS had normal AND-level precedence, this common pattern would break:

```l4
-- If UNLESS bound like AND, this would be WRONG:
A OR B UNLESS C  would mean  A OR (B AND NOT C)  -- WRONG!

-- With correct low precedence:
A OR B UNLESS C  means  (A OR B) AND NOT C  -- CORRECT!
```

## Truth Table

| A | B | A UNLESS B |
|---|---|------------|
| TRUE | TRUE | FALSE |
| TRUE | FALSE | TRUE |
| FALSE | TRUE | FALSE |
| FALSE | FALSE | FALSE |

## Comparison with AND NOT

| Expression | Equivalent To | Result |
|------------|---------------|--------|
| `TRUE UNLESS TRUE` | `TRUE AND NOT TRUE` | FALSE |
| `TRUE UNLESS FALSE` | `TRUE AND NOT FALSE` | TRUE |
| `A OR B UNLESS C` | `(A OR B) AND NOT C` | depends |
| `A AND B UNLESS C` | `(A AND B) AND NOT C` | depends |

## Related Keywords

- **[AND](AND.md)** - Logical conjunction
- **[OR](OR.md)** - Logical disjunction
- **[NOT](NOT.md)** - Logical negation
- **[IMPLIES](IMPLIES.md)** - Logical implication

## See Also

- **[Operators Reference](README.md)** - All operators
- **[Logical Operators](README.md#logical-operators)** - Boolean operations
