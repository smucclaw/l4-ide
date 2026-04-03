# Control Flow Keywords

Control flow keywords direct program execution based on conditions and patterns.

## Overview

| Keyword   | Used With  | Purpose                |
| --------- | ---------- | ---------------------- |
| IF        | THEN, ELSE | Conditional expression |
| THEN      | IF, WHEN   | Consequent result      |
| ELSE      | IF         | Alternative result     |
| CONSIDER  | WHEN       | Pattern matching       |
| WHEN      | CONSIDER   | Pattern case           |
| OTHERWISE | CONSIDER   | Default case           |
| BRANCH    | IF         | Multi-way conditional  |

## IF / THEN / ELSE

Conditional expressions that choose between two alternatives.

### Syntax

```l4
IF condition THEN trueResult ELSE falseResult
```

### Examples

```l4
DECIDE status IS
  IF age >= 18 THEN "adult" ELSE "minor"
```

**See [IF](IF.md) for complete documentation.**

## CONSIDER / WHEN / OTHERWISE

Pattern matching for examining values and executing different branches.

### Syntax

```l4
CONSIDER value
  WHEN pattern1 THEN result1
  WHEN pattern2 THEN result2
  OTHERWISE defaultResult
```

### Examples

```l4
DECLARE Color IS ONE OF Red, Green, Blue

GIVEN c IS A Color
colorName c MEANS
  CONSIDER c
    WHEN Red THEN "red"
    WHEN Green THEN "green"
    WHEN Blue THEN "blue"
```

**See [CONSIDER](CONSIDER.md) for complete documentation.**

## BRANCH

Multi-way conditional for flat comparison chains. Alternative to nested IF statements.

### Syntax

```l4
BRANCH IF condition1 THEN result1
       IF condition2 THEN result2
       IF condition3 THEN result3
       OTHERWISE default
```

### Examples

```l4
DECLARE Category IS ONE OF Standard, Premium, Enterprise

GIVEN cat IS A Category
GIVETH A NUMBER
priceFor cat MEANS
  BRANCH IF cat EQUALS Standard   THEN 10
         IF cat EQUALS Premium    THEN 25
         IF cat EQUALS Enterprise THEN 100
         OTHERWISE 0
```

### BRANCH vs CONSIDER vs IF

| Use Case                         | Best Choice           |
| -------------------------------- | --------------------- |
| Binary decision                  | IF...THEN...ELSE      |
| Multiple equality checks         | BRANCH                |
| Pattern matching / destructuring | CONSIDER              |
| Nested conditions                | Nested IF or CONSIDER |

## THEN

Marks the result expression in conditionals.

- **With IF:** `IF condition THEN result ELSE alternative`
- **With WHEN:** `WHEN pattern THEN result`
- **With BRANCH:** `BRANCH IF condition THEN result`

## ELSE

Marks the alternative result when condition is false.

```l4
IF x > 0 THEN "positive" ELSE "not positive"
```

**Note:** ELSE is required in IF expressions — there's no "dangling else" ambiguity.

## OTHERWISE

Default case in CONSIDER/BRANCH when no pattern matches.

```l4
CONSIDER status
  WHEN Active THEN "running"
  WHEN Paused THEN "waiting"
  OTHERWISE "unknown"
```

**Best Practice:** Always include OTHERWISE unless you've covered all cases.

## Related Keywords

- **[IF](IF.md)** - Detailed conditional documentation
- **[CONSIDER](CONSIDER.md)** - Detailed pattern matching documentation
- **[AND](../operators/AND.md)** / **[OR](../operators/OR.md)** - Combine conditions

## See Also

- **[Foundation Course: Control Flow](../../courses/foundation/module-3-control-flow.md)** - Tutorial with examples
