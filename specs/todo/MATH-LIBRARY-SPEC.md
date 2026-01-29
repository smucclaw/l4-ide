# Math Helper Library for L4

## Context

`opm2l4` needs to translate Oracle Policy Modeling functions such as `Ex(x)` (natural exponential) into idiomatic L4. While jl4-core already supports the `TO THE POWER OF` operator, there is no convenient helper that exposes Euler's constant or a reusable `exp` function to consumers. Rather than adding a new primitive to the runtime, we can solve this inside existing L4 libraries.

## Requirements

1. Provide an `exp` helper in a shared L4 library so downstream projects can call it.
2. Expose a named constant for Euler's number, using the same numeric literal semantics as the rest of L4 (rational approximation is fine).
3. Make the helper importable independently of `prelude`/`daydate`, so projects only bring in math helpers when required.
4. Lay groundwork for future numerical helpers (`ln`, `log10`, etc.) by creating a dedicated `math.l4`.

## Proposed Design

- Create `jl4-core/libraries/math.l4`.
  - Declare `EULER` as a `NUMBER` with value `2.718281828459045`.
  - Define `exp x MEANS EXPONENT EULER x`.
  - Document placeholders for `ln`, `log10`, etc. so later tickets can extend the same file.
- Add golden tests (analogous to `prelude`/`daydate`) under `jl4-core/libraries/tests` to snapshot the new library.
- Update documentation to mention the new library.
- Consumers (e.g., `opm2l4`) can import `math` whenever an OPM rule uses `Ex()`.

## Acceptance Criteria

1. `math.l4` compiles and exports `exp`.
2. Library tests have passing golden files.
3. Docs explain how to import `math` and when to use `exp`.
4. No jl4-core runtime changes are required.

## Phase 2: Logarithms (Dec 2025)

- Extend `math.l4` with `ln` and `log10`, each returning `MAYBE NUMBER` to mirror OPM's domain rules.
- Introduce `LN` and `LOG10` runtime primitives so helpers can delegate to native floating-point operations.
- Update docs/goldens to mention the new helpers.
- Ensure downstream translators (e.g., opm2l4) can request the math import to satisfy `Ln()` / `Log()` tickets.

## Phase 3: Trigonometry (Jan 2026)

### Scope

Implement the remaining Tier-2 OPM numerical functions:

- `Sin(x)`, `Cos(x)`, `Tan(x)` — forward trigonometric helpers
- `Asin(x)`, `Acos(x)`, `Atan(x)` — inverse trigonometric helpers

### Requirements

1. All trig helpers live in `jl4-core/libraries/math.l4` so downstream code only needs `IMPORT math`.
2. jl4 runtime exposes primitives `SIN`, `COS`, `TAN`, `ASIN`, `ACOS`, `ATAN` (radian-based, doubles under the hood).
3. Helpers return `MAYBE NUMBER`:
   - `sin`, `cos`, `atan` always return `JUST (…)`.
   - `tan` guards singularities by inspecting `COS x`; if |cos(x)| < ε (≈1e‑9) it returns `NOTHING`.
   - `asin` / `acos` validate `-1 ≤ x ≤ 1`, otherwise return `NOTHING`.
4. Runtime primitives raise descriptive `RuntimeTypeError`s when invoked outside their valid domain so that non-helper uses still fail loudly.
5. Documentation (`doc/libraries/math.md`) explains that all trig helpers operate in radians (matching the Oracle docs) and describes the guard semantics.
6. Library golden tests continue to parse/type-check with the additional definitions.

### Acceptance Criteria

- New primitives wired through `TypeCheck.Environment`, `EvaluateLazy.Machine`, and `Evaluate.ValueLazy`.
- `math.l4` exposes helper definitions + shared absolute-value utility for the tan guard.
- Regression tests (existing math golden) pass; add unit coverage in downstream consumers (`opm2l4`) to ensure translators request the math import and treat outputs as `MAYBE NUMBER`.
