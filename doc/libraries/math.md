# Math Library (`IMPORT math`)

The math library bundles lightweight numerical helpers that build on top of the
core `prelude`. It is intentionally small so downstream projects can import it
selectively when they need exponential/logarithmic helpers without pulling in
date utilities.

## Provided Definitions

| Helper | Type | Description |
|--------|------|-------------|
| `EULER` | `NUMBER` | Constant approximation of Euler's number (2.718281828459045). |
| `exp` | `NUMBER → NUMBER` | Computes \(e^x\) via `EXPONENT EULER x`. |
| `ln` | `NUMBER → MAYBE NUMBER` | Safe natural logarithm: returns `NOTHING` when the input is ≤ 0, otherwise `JUST (LN x)`. |
| `log10` | `NUMBER → MAYBE NUMBER` | Base-10 logarithm with the same domain guard as `ln`. |
| `sqrt` | `NUMBER → MAYBE NUMBER` | Safe square root: returns `NOTHING` when the input is negative, otherwise `JUST (SQRT x)`. |

## Usage

```l4
IMPORT math

GIVEN rate IS A NUMBER
      years IS A NUMBER
GIVETH A NUMBER
`compound growth factor` rate years MEANS exp (rate TIMES years)

GIVEN discountRate IS A NUMBER
GIVETH A MAYBE NUMBER
`safe log discount` discountRate MEANS log10 discountRate
```

The table will continue to grow as additional OPM numerical functions are
required; keep logarithmic/trigonometric helpers co-located here so downstream
translators only need to `IMPORT math` once.
