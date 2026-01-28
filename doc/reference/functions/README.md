# Functions Reference

This section documents the keywords used to define functions and computations in L4. Functions represent individual rules.

## Overview

L4 functions are defined using a combination of keywords that specify parameters, return types, and the function body. The typical structure is:

```l4
GIVEN parameter IS A Type      -- Parameters (optional)
GIVETH A ReturnType            -- Return type (optional)
DECIDE name MEANS expression   -- Definition
```

## Keywords

| Keyword | Purpose | Required |
|---------|---------|----------|
| [GIVEN](GIVEN.md) | Declares function parameters with types | For functions with inputs |
| [GIVETH](GIVETH.md) | Specifies the return type | Optional (can be inferred) |
| [DECIDE](DECIDE.md) | Defines a named value or function | Yes (or use bare MEANS) |
| [MEANS](MEANS.md) | Connects the function name to its body | Yes |
| [YIELD](YIELD.md) | Creates anonymous functions (lambdas) | For inline functions |
| [WHERE](WHERE.md) | Adds local definitions after an expression | Optional |
| [LET](LET.md) | Adds local definitions before an expression | Optional |
| [AKA](AKA.md) | Creates aliases for definitions | Optional |

## Quick Examples

### Simple Function

```l4
GIVEN x IS A NUMBER
double x MEANS x TIMES 2
```

### Fully Annotated Function

```l4
GIVEN n IS A NUMBER
GIVETH A NUMBER
DECIDE factorial n MEANS
  IF n EQUALS 0
  THEN 1
  ELSE n TIMES factorial (n MINUS 1)
```

### With Local Definitions

```l4
circleArea radius MEANS
  pi TIMES radius TIMES radius
  WHERE
    pi MEANS 3.14159
```

### With Aliases

```l4
`total cost` AKA `final price` MEANS
  basePrice PLUS tax
```

### Anonymous Functions (Lambdas)

Use `GIVEN ... YIELD` for inline functions:

```l4
GIVEN numbers IS A LIST OF NUMBER
`all positive` MEANS all (GIVEN n YIELD n > 0) numbers
```

## Function Definition Patterns

### Named Constants

No parameters needed:

```l4
DECIDE pi IS 3.14159
DECIDE greeting IS "Hello, World!"
```

### Single Parameter

```l4
GIVEN x IS A NUMBER
square x MEANS x TIMES x
```

### Multiple Parameters

```l4
GIVEN a IS A NUMBER
      b IS A NUMBER
add a b MEANS a PLUS b
```

### Boolean Decisions

Use `DECIDE ... IF` for boolean-returning functions:

```l4
GIVEN age IS A NUMBER
DECIDE `is adult` IF age >= 18
```

## Local Bindings

Two styles for introducing local definitions:

### WHERE (definitions after)

```l4
result MEANS x PLUS y
  WHERE
    x MEANS 10
    y MEANS 20
```

### LET (definitions before)

```l4
result MEANS
  LET x IS 10, y IS 20
  IN x PLUS y
```

## See Also

- **[Types Reference](../types/README.md)** - Available types for parameters and returns
- **[Operators Reference](../operators/README.md)** - Operators for expressions
- **[Control Flow Reference](../control-flow/README.md)** - IF, CONSIDER, and other constructs
