# Math Library

Mathematical functions beyond basic arithmetic. Provides safe wrappers around numeric builtins that handle edge cases (division by zero, domain errors) by returning `MAYBE` types. Import it into L4 files with `IMPORT math`.

### Location

[jl4-core/libraries/math.l4](https://github.com/smucclaw/l4-ide/blob/main/jl4-core/libraries/math.l4)

### Constants

- `EULER` - Euler's number (2.718281828459045)
- `TAN_EPSILON` - Threshold for detecting tan asymptotes

### Functions

| Function    | Signature               | Description                       |
| ----------- | ----------------------- | --------------------------------- |
| `exp`       | `NUMBER → NUMBER`       | e^x via `EXPONENT EULER x`        |
| `ln`        | `NUMBER → MAYBE NUMBER` | Natural log (NOTHING if x ≤ 0)    |
| `log10`     | `NUMBER → MAYBE NUMBER` | Base-10 log (NOTHING if x ≤ 0)    |
| `sqrt`      | `NUMBER → MAYBE NUMBER` | Square root (NOTHING if x < 0)    |
| `absNumber` | `NUMBER → NUMBER`       | Absolute value                    |
| `sin`       | `NUMBER → MAYBE NUMBER` | Sine (always JUST)                |
| `cos`       | `NUMBER → MAYBE NUMBER` | Cosine (always JUST)              |
| `tan`       | `NUMBER → MAYBE NUMBER` | Tangent (NOTHING near asymptotes) |
| `asin`      | `NUMBER → MAYBE NUMBER` | Arc sine (NOTHING if \|x\| > 1)   |
| `acos`      | `NUMBER → MAYBE NUMBER` | Arc cosine (NOTHING if \|x\| > 1) |
| `atan`      | `NUMBER → MAYBE NUMBER` | Arc tangent (always JUST)         |

**Note:** All trigonometric functions expect/return angles in **radians**.

### Example: Math Functions

\n\n[math-example.l4](math-example.l4)

**See [math.l4](https://github.com/smucclaw/l4-ide/blob/main/jl4-core/libraries/math.l4) source for all functions.**
