# DECIDE

Defines a named value or function with a body. The primary way to create computations in L4.

## Syntax

```l4
DECIDE name IS expression
DECIDE name MEANS expression
name MEANS expression
```

## Forms

### Named Values

```l4
DECIDE pi IS 3.14159
DECIDE greeting IS "Hello, World!"
```

### Functions with GIVEN

```l4
GIVEN x IS A NUMBER
DECIDE double x IS x TIMES 2
```

### Using MEANS

MEANS introduces the body of a function:

```l4
GIVEN n IS A NUMBER
DECIDE factorial n MEANS
  IF n EQUALS 0
  THEN 1
  ELSE n TIMES factorial (n MINUS 1)
```

### Omitting DECIDE

For functions, DECIDE can be omitted:

```l4
GIVEN x IS A NUMBER
double x MEANS x TIMES 2
```

## Examples

**Example file:** [decide-example.l4](decide-example.l4)

### Simple Values

```l4
DECIDE answer IS 42
DECIDE isEnabled IS TRUE
DECIDE message IS "Welcome"
```

### Conditional Decisions

```l4
GIVEN age IS A NUMBER
DECIDE isAdult age IS
  IF age >= 18
  THEN TRUE
  ELSE FALSE
```

### With WHERE Clauses

```l4
DECIDE circleArea radius IS
  pi TIMES radius TIMES radius
  WHERE
    pi MEANS 3.14159
```

### Boolean Decisions with IF

For boolean-returning functions, use `DECIDE ... IF`:

```l4
GIVEN age IS A NUMBER
GIVEN income IS A NUMBER
DECIDE isEligible IF
  age >= 18 AND income > 30000
```

## Related Keywords

- **[MEANS](MEANS.md)** - Introduces the function body
- **[GIVEN](GIVEN.md)** - Introduces parameters
- **[GIVETH](GIVETH.md)** - Specifies return type
- **[WHERE](WHERE.md)** - Local definitions
- **[IF](../control-flow/IF.md)** - Boolean condition form

> Note: IS is part of the DECIDE syntax, not a separate keyword page.

## See Also

- **[Keywords Reference](README.md)** - All keywords
