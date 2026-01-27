# IMPLIES

Logical implication operator. "A IMPLIES B" is TRUE unless A is TRUE and B is FALSE.

## Syntax

```l4
expression1 IMPLIES expression2
expression1 => expression2
```

## Truth Table

| A     | B     | A IMPLIES B |
| ----- | ----- | ----------- |
| TRUE  | TRUE  | TRUE        |
| TRUE  | FALSE | FALSE       |
| FALSE | TRUE  | TRUE        |
| FALSE | FALSE | TRUE        |

## Semantics

`A IMPLIES B` is logically equivalent to `NOT A OR B`.

In legal terms: "If A, then B" - the rule is violated only when A is true but B is false.

## Examples

**Example file:** [implies-example.l4](implies-example.l4)

### Basic Usage

```l4
#EVAL TRUE IMPLIES TRUE    -- TRUE
#EVAL TRUE IMPLIES FALSE   -- FALSE
#EVAL FALSE IMPLIES TRUE   -- TRUE
#EVAL FALSE IMPLIES FALSE  -- TRUE
```

### Symbolic Alternative

```l4
#EVAL TRUE => TRUE    -- TRUE
#EVAL TRUE => FALSE   -- FALSE
```

### Legal Rule Example

```l4
GIVEN isEmployee IS A BOOLEAN
GIVEN hasBadge IS A BOOLEAN
-- "If you are an employee, you must have a badge"
DECIDE badgeRule IS isEmployee IMPLIES hasBadge
```

### Chaining Implications

```l4
-- A => B => C means A => (B => C)
DECIDE chainedImpl IS TRUE => (TRUE => FALSE)
```

## Use in Legal Logic

Implication is common in legal rules:

```l4
-- "A minor must have parental consent"
GIVEN age IS A NUMBER
GIVEN hasParentalConsent IS A BOOLEAN
DECIDE consentRule IS (age < 18) IMPLIES hasParentalConsent
```

## Related Keywords

- **[AND](AND.md)** - Logical conjunction
- **[OR](OR.md)** - Logical disjunction
- **[NOT](NOT.md)** - Logical negation

## See Also

- **[Logical Operators](../operators/README.md#logical-operators)** - All logical operators
