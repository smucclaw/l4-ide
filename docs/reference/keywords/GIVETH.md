# GIVETH

Specifies the return type of a function. Placed after GIVEN clauses and before the function definition.

## Syntax

```l4
GIVETH A Type
GIVETH Type
GIVES A Type
```

## Purpose

GIVETH declares what type a function returns. While L4 has type inference, explicit return types improve code clarity and catch errors early.

## Examples

**Example file:** [giveth-example.l4](giveth-example.l4)

### Basic Return Types

```l4
GIVEN n IS A NUMBER
GIVETH A NUMBER
square n MEANS n TIMES n

GIVEN s IS A STRING
GIVETH A NUMBER
stringLength s MEANS length s
```

### Boolean Return

```l4
GIVEN age IS A NUMBER
GIVETH A BOOLEAN
isAdult age MEANS age >= 18
```

### Complex Return Types

```l4
GIVEN xs IS A LIST OF NUMBER
GIVETH A MAYBE NUMBER
safeHead xs MEANS
  CONSIDER xs
  WHEN EMPTY THEN NOTHING
  WHEN h FOLLOWED BY _ THEN JUST h
```

### Parameterized Return Types

```l4
GIVEN a IS A TYPE
GIVEN x IS AN a
GIVEN y IS AN a
GIVETH A PAIR OF a, a
makePair x y MEANS Pair WITH first IS x, second IS y
```

## GIVES vs GIVETH

GIVES is a synonym for GIVETH:

```l4
GIVEN x IS A NUMBER
GIVES A NUMBER
triple x MEANS x TIMES 3
```

## Without GIVETH

L4 can infer return types, so GIVETH is optional:

```l4
-- Type inferred as NUMBER
GIVEN x IS A NUMBER
double x MEANS x TIMES 2
```

## Related Keywords

- **[GIVES](GIVES.md)** - Synonym for GIVETH
- **[GIVEN](GIVEN.md)** - Introduces parameters
- **[DECIDE](DECIDE.md)** - Function definition
- **[MEANS](MEANS.md)** - Function body

## See Also

- **[Types Reference](../types/README.md)** - Available types
- **[Function Types](../types/function-type-example.l4)** - Function examples
