# IF

Conditional expression that evaluates one of two branches based on a boolean condition.

## Syntax

```l4
IF condition THEN consequent ELSE alternative
```

## Purpose

IF expressions select between two values based on a condition. Unlike imperative if-statements, L4's IF is an expression that always returns a value.

## Examples

**Example file:** [if-example.l4](if-example.l4)

### Basic If-Then-Else

```l4
IF TRUE THEN "yes" ELSE "no"

GIVEN age IS A NUMBER
DECIDE status age IS
  IF age >= 18 THEN "adult" ELSE "minor"
```

### Nested Conditions

```l4
GIVEN score IS A NUMBER
DECIDE grade score IS
  IF score >= 90 THEN "A"
  ELSE IF score >= 80 THEN "B"
  ELSE IF score >= 70 THEN "C"
  ELSE IF score >= 60 THEN "D"
  ELSE "F"
```

### Multi-line Format

```l4
GIVEN x IS A NUMBER
DECIDE absolute x IS
  IF x >= 0
  THEN x
  ELSE 0 MINUS x
```

### Boolean Expressions in Condition

```l4
GIVEN age IS A NUMBER
GIVEN hasLicense IS A BOOLEAN
DECIDE canDrive IS
  IF age >= 16 AND hasLicense
  THEN TRUE
  ELSE FALSE
```

## DECIDE ... IF Form

For boolean-returning functions, use `DECIDE ... IF`:

```l4
GIVEN age IS A NUMBER
DECIDE isEligible IF age >= 18
```

This is shorthand for:

```l4
GIVEN age IS A NUMBER
DECIDE isEligible IS IF age >= 18 THEN TRUE ELSE FALSE
```

## Related Keywords

- **[THEN](THEN.md)** - Consequent branch
- **[ELSE](ELSE.md)** - Alternative branch
- **[AND](AND.md)** - Logical conjunction in conditions
- **[OR](OR.md)** - Logical disjunction in conditions
- **[CONSIDER](CONSIDER.md)** - Pattern matching alternative

## See Also

- **[Logical Operators](../operators/README.md#logical-operators)** - Combining conditions
