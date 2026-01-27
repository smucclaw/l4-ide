# Arithmetic Keywords

Arithmetic keywords perform mathematical operations on numbers. Each has both a textual form (readable) and symbolic form (concise).

## Keywords

| Keyword    | Symbol | Operation      |
| ---------- | ------ | -------------- |
| PLUS       | `+`    | Addition       |
| MINUS      | `-`    | Subtraction    |
| TIMES      | `*`    | Multiplication |
| DIVIDED BY | `/`    | Division       |
| MODULO     | `%`    | Remainder      |

## PLUS

Adds two numbers.

### Syntax

```l4
value1 PLUS value2
value1 + value2
```

### Examples

**Example file:** [arithmetic-example.l4](arithmetic-example.l4)

```l4
#EVAL 5 PLUS 3        -- 8
#EVAL 5 + 3           -- 8
#EVAL 10.5 + 2.5      -- 13
```

## MINUS

Subtracts the second number from the first.

### Syntax

```l4
value1 MINUS value2
value1 - value2
```

### Examples

```l4
#EVAL 10 MINUS 4      -- 6
#EVAL 10 - 4          -- 6
#EVAL 5 - 8           -- -3
```

## TIMES

Multiplies two numbers.

### Syntax

```l4
value1 TIMES value2
value1 * value2
```

### Examples

```l4
#EVAL 6 TIMES 7       -- 42
#EVAL 6 * 7           -- 42
#EVAL 3.14 * 2        -- 6.28
```

## DIVIDED BY

Divides the first number by the second.

### Syntax

```l4
value1 DIVIDED BY value2
value1 / value2
```

### Examples

```l4
#EVAL 15 DIVIDED BY 3    -- 5
#EVAL 15 / 3             -- 5
#EVAL 10 / 4             -- 2.5 (rational result)
```

**Note:** Division returns exact rational numbers, not truncated integers.

## MODULO

Returns the remainder after division.

### Syntax

```l4
value1 MODULO value2
value1 % value2
```

### Examples

```l4
#EVAL 17 MODULO 5     -- 2
#EVAL 17 % 5          -- 2
#EVAL 10 MODULO 3     -- 1
```

## Operator Precedence

Standard mathematical precedence:

1. Parentheses `()` (highest)
2. Multiplication, Division, Modulo (`*`, `/`, `%`)
3. Addition, Subtraction (`+`, `-`) (lowest)

### Examples

```l4
#EVAL 2 + 3 * 4        -- 14 (not 20)
#EVAL (2 + 3) * 4      -- 20
#EVAL 10 - 4 / 2       -- 8 (not 3)
#EVAL (10 - 4) / 2     -- 3
```

## Combining with Other Operators

Arithmetic results can be used with comparisons and logic:

```l4
ASSUME price IS A NUMBER
ASSUME quantity IS A NUMBER

-- Calculate total and check threshold
DECIDE total IS price TIMES quantity
DECIDE needsApproval IS total GREATER THAN 10000
```

## Math Library Functions

For advanced operations, import the math library:

```l4
IMPORT math

#EVAL sqrt 16          -- 4
#EVAL exp 1            -- 2.718...
```

See **[Libraries Reference](../libraries/README.md)** for more functions.

## Related Keywords

- **[COMPARISONS](COMPARISONS.md)** - Compare arithmetic results
- **[IF](IF.md)** - Conditional arithmetic

## See Also

- **[Operators Reference](../operators/README.md)** - Full operator documentation
- **[Types: NUMBER](../types/README.md)** - Numeric types
- **[Syntax: Precedence](../syntax/README.md)** - Operator precedence rules
