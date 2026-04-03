# LET

Introduces local bindings before an expression. The bindings are visible only within the IN clause.

## Syntax

```l4
LET name IS expression IN body
LET name1 IS expr1, name2 IS expr2 IN body
LET
  name1 IS expression1
  name2 IS expression2
IN body
```

## Purpose

LET allows you to:

1. Name intermediate values before using them
2. Break complex expressions into readable parts
3. Create local scope for temporary variables

## Examples

**Example file:** [let-example.l4](let-example.l4)

### Single Binding

```l4
DECIDE result IS
  LET x IS 5
  IN x TIMES 2
```

### Multiple Bindings (Inline)

```l4
DECIDE result IS
  LET x IS 5, y IS 10
  IN x PLUS y
```

### Multiple Bindings (Multi-line)

```l4
DECIDE result IS
  LET
    x IS 5
    y IS 10
    z IS 15
  IN x PLUS y PLUS z
```

### Forward References

Bindings can reference earlier bindings:

```l4
DECIDE result IS
  LET
    x IS 5
    y IS x TIMES 2
    z IS y PLUS 3
  IN z
```

## Binding Keywords

Multiple keywords can introduce bindings:

```l4
LET
  a IS 1
  b BE 2
  c MEAN 3
  d MEANS 4
  e = 5
IN a PLUS b PLUS c PLUS d PLUS e
```

## LET vs WHERE

- **LET** - Definitions come before the main expression
- **WHERE** - Definitions come after the main expression

```l4
-- Using LET (definitions before)
result1 MEANS LET x IS 1, y IS 2 IN x PLUS y

-- Using WHERE (definitions after)
result2 MEANS x PLUS y WHERE x MEANS 1, y MEANS 2
```

## Related Keywords

- **[WHERE](WHERE.md)** - Alternative local binding syntax
- **[MEANS](MEANS.md)** - Alternative assignment keyword

> Note: IN, IS, and BE are part of the LET syntax, not separate keyword pages.

## See Also

- **[Syntax: Local Declarations](../syntax/README.md)** - Scope rules
