# AND

Logical conjunction operator. Returns TRUE only if both operands are TRUE.

## Syntax

```l4
expression1 AND expression2
expression1 && expression2
```

## Truth Table

| A     | B     | A AND B |
| ----- | ----- | ------- |
| TRUE  | TRUE  | TRUE    |
| TRUE  | FALSE | FALSE   |
| FALSE | TRUE  | FALSE   |
| FALSE | FALSE | FALSE   |

## Examples

**Example file:** [and-example.l4](and-example.l4)

### Basic Usage

```l4
#EVAL TRUE AND TRUE     -- TRUE
#EVAL TRUE AND FALSE    -- FALSE
#EVAL FALSE AND TRUE    -- FALSE
#EVAL FALSE AND FALSE   -- FALSE
```

### In Conditions

```l4
GIVEN age IS A NUMBER
GIVEN hasLicense IS A BOOLEAN
DECIDE canDrive IS age >= 16 AND hasLicense
```

### Symbolic Alternative

```l4
DECIDE result IS TRUE && FALSE
```

### Chaining Multiple Conditions

```l4
GIVEN a IS A BOOLEAN
GIVEN b IS A BOOLEAN
GIVEN c IS A BOOLEAN
allTrue a b c MEANS a AND b AND c
```

## Layout-Sensitive AND

L4 supports vertical AND with indentation:

```l4
DECIDE eligible IS
      age >= 18
  AND income > 30000
  AND NOT hasCriminalRecord
```

## Asyndetic AND

Implicit operators using punctuation.

**Ellipsis** (`...`) - Implicit AND

**Example:** [asyndetic-example.l4](asyndetic-example.l4)

## Short-Circuit Evaluation

AND evaluates lazily - if the first operand is FALSE, the second is not evaluated.

## Related Keywords

- **[OR](OR.md)** - Logical disjunction
- **[NOT](NOT.md)** - Logical negation
- **[IMPLIES](IMPLIES.md)** - Logical implication

## See Also

- **[Logical Operators](../operators/README.md#logical-operators)** - All logical operators
