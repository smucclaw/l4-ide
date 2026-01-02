# Coercion & Truncation Builtins

L4 now exposes a small set of coercion helpers to mirror the OPM/Excel primitives:

- `TOSTRING value` — `GIVETH A STRING`. Supports `NUMBER`, `BOOLEAN`, `DATE`, and `STRING`. Numbers render with canonical decimal text (no trailing zeros), booleans render as `TRUE`/`FALSE`, dates render as `YYYY-MM-DD`.
- `TONUMBER text` — `GIVETH A MAYBE NUMBER`. Accepts trimmed ASCII numeric literals with optional sign, decimal point, and scientific notation (e.g. `1.2E3`). Returns `NOTHING` on parse failure.
- `TODATE text` — `GIVETH A MAYBE DATE`. Parses `YYYY-MM-DD`, `YYYY/MM/DD`, `DD-MMM-YYYY`, `DD/MM/YYYY`, and `MMM DD, YYYY` (month names are case-insensitive). Rejects dates outside `0001-01-01`–`9999-12-31`. The resulting value uses the in-scope `DATE` type (from `daydate.l4`).
- `TRUNC value digits` — `GIVETH A NUMBER`. Truncates toward zero; `digits` is rounded to the nearest integer before applying. Non-negative `digits` truncate fractional places, negative `digits` truncate to the left of the decimal point.
- `AS STRING` — now mirrors `TOSTRING` semantics so explicit casts work for booleans and dates in addition to numbers/strings.

All functions are deterministic and locale-independent.
