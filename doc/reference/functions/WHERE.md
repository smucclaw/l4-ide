# WHERE

Introduces local definitions within an expression. Allows defining helper values or functions that are only visible within the enclosing scope.

## Syntax

```l4
expression
WHERE
  name1 MEANS expression1
  name2 MEANS expression2
```

## Purpose

WHERE clauses let you:

1. Break complex expressions into named parts
2. Avoid repetition by naming common subexpressions
3. Create helper functions local to a definition

## Examples

**Example file:** [where-example.l4](where-example.l4)

### Basic Local Definitions

```l4
circleArea radius MEANS
  pi TIMES radius TIMES radius
  WHERE
    pi MEANS 3.14159
```

### Multiple Local Definitions

```l4
calculation x y MEANS
  sum TIMES diff
  WHERE
    sum MEANS x PLUS y
    diff MEANS x MINUS y
```

### Nested WHERE Clauses

```l4
outer MEANS
  middle
  WHERE
    middle MEANS
      inner
      WHERE
        inner MEANS 42
```

### Local Functions

```l4
sumSquares x y MEANS
  square x PLUS square y
  WHERE
    square n MEANS n TIMES n
```

## WHERE vs LET

- **WHERE** - Definitions come after the main expression
- **LET** - Definitions come before the main expression

```l4
-- Using WHERE (definitions after)
result1 MEANS x PLUS y WHERE x MEANS 1, y MEANS 2

-- Using LET (definitions before)
result2 MEANS LET x IS 1, y IS 2 IN x PLUS y
```

## Related Keywords

- **[LET](LET.md)** - Alternative local binding syntax
- **[MEANS](MEANS.md)** - Defines the binding

## See Also

- **[Syntax: Local Declarations](../syntax/README.md)** - Scope rules
