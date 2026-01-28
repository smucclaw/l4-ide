# Comparison Keywords

Comparison keywords are used to compare values and return BOOLEAN results. Most have both textual (keyword) and symbolic forms.

## Keywords

| Keyword      | Symbol | Meaning                  |
| ------------ | ------ | ------------------------ |
| EQUALS       | `=`    | Equality                 |
| GREATER THAN | `>`    | Greater than             |
| LESS THAN    | `<`    | Less than                |
| AT LEAST     | `>=`   | Greater or equal         |
| AT MOST      | `<=`   | Less or equal            |
| ABOVE        | `>`    | Synonym for GREATER THAN |
| BELOW        | `<`    | Synonym for LESS THAN    |

## Equality: EQUALS

Tests if two values are equal.

### Syntax

```l4
value1 EQUALS value2
value1 = value2
```

### Examples

**Example file:** [comparisons-example.l4](comparisons-example.l4)

```l4
#EVAL 5 EQUALS 5           -- TRUE
#EVAL "hello" = "hello"    -- TRUE
#EVAL 5 EQUALS 3           -- FALSE
```

## Ordering: GREATER THAN, LESS THAN

Compare ordered values (numbers, strings, dates).

### Syntax

```l4
value1 GREATER THAN value2
value1 > value2

value1 LESS THAN value2
value1 < value2
```

### Examples

```l4
#EVAL 10 GREATER THAN 5    -- TRUE
#EVAL 10 > 5               -- TRUE

#EVAL 3 LESS THAN 7        -- TRUE
#EVAL 3 < 7                -- TRUE
```

## Inclusive: AT LEAST, AT MOST

Compare with inclusive bounds.

### Syntax

```l4
value1 AT LEAST value2     -- value1 >= value2
value1 >= value2

value1 AT MOST value2      -- value1 <= value2
value1 <= value2
```

### Examples

```l4
#EVAL 5 AT LEAST 5         -- TRUE (equal counts)
#EVAL 5 >= 5               -- TRUE

#EVAL 4 AT MOST 10         -- TRUE
#EVAL 4 <= 10              -- TRUE
```

## Synonyms: ABOVE, BELOW

Alternative keywords for greater/less than.

```l4
#EVAL 10 ABOVE 5           -- TRUE (same as GREATER THAN)
#EVAL 3 BELOW 7            -- TRUE (same as LESS THAN)
```

## Combining Comparisons

Comparisons return BOOLEAN values and can be combined with logical operators.

```l4
ASSUME age IS A NUMBER
ASSUME income IS A NUMBER

-- Combined conditions
DECIDE isEligible IS
      age AT LEAST 18
  AND age AT MOST 65
  AND income GREATER THAN 30000
```

## Type Compatibility

Comparisons work on:

- **NUMBER** - Numeric comparison
- **STRING** - Lexicographic comparison
- **DATE** - Chronological comparison
- **BOOLEAN** - FALSE < TRUE
- **Custom types** - If ordered

## Related Keywords

- **[AND](../control-flow/AND.md)** - Combine comparisons
- **[OR](../control-flow/OR.md)** - Alternative conditions
- **[IF](../control-flow/IF.md)** - Use comparisons in conditionals

## See Also

- **[Operators Reference](../operators/README.md)** - Full operator documentation
- **[Types Reference](../types/README.md)** - Comparable types
