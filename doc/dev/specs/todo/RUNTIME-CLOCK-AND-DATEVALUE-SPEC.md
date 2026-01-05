# Specification: Runtime Clock & Date Parsing Builtins

## Executive Summary

Add four runtime-provided helpers to unblock Excel parity work:

| Keyword     | Type signature                                       | Behavior                                                                                                  |
| ----------- | ---------------------------------------------------- | --------------------------------------------------------------------------------------------------------- |
| `TODAY`     | `GIVETH DATE`                                        | Returns the current calendar date in the evaluator’s clock (no time-of-day).                              |
| `NOW`       | `GIVETH NUMBER`                                      | Returns the current datestamp (same unit as `Day Date`) plus the fractional day representing time-of-day. |
| `DATEVALUE` | `GIVEN text IS A STRING GIVETH EITHER STRING DATE`   | Parses an Excel-style date string; `LEFT` carries the validation error.                                   |
| `TIMEVALUE` | `GIVEN text IS A STRING GIVETH EITHER STRING NUMBER` | Parses an Excel-style time string into a fraction between 0 (inclusive) and 1 (exclusive).                |

All four surface as uppercase builtins, live at the Haskell level, and are consumed by the `excel-date` library (`excelToday`, `excelNow`, `ExcelDateValue`, `ExcelTimeValue`). They are also generally useful outside Excel parity (e.g., relative deadlines).

## Motivation

The Excel compatibility layer still lacks constructors (`DATEVALUE`, `TIMEVALUE`) and volatile functions (`TODAY`, `NOW`). Those cannot be implemented purely in L4 because:

- We need a trusted clock rooted in the host runtime (and an easy way to freeze it for deterministic tests).
- Parsing must mimic Excel’s permissive formats, which is brittle to re‑implement by hand.

Without these builtins, Excel parity stalls and we keep copying literal dates into tests instead of calling the true API.

## Requirements

### Function semantics

1. `TODAY`
   - Returns a `DATE`.
   - Strips the time-of-day component (floor to midnight in the evaluator’s clock).
   - Deterministic when the clock is pinned (see Clock Source below).
2. `NOW`
   - Returns a `NUMBER` equal to `Day (today)` plus `secondsSinceMidnight / 86400`.
   - Fraction keeps millisecond precision (round to nearest microsecond before division).
3. `DATEVALUE`
   - Accepts trimmed text (leading/trailing whitespace ignored).
   - Recognized formats (case-insensitive for month names):
     - ISO: `YYYY-MM-DD`, `YYYY/MM/DD`, `YYYY.MM.DD`.
     - Excel default (US): `M/D/YYYY`, `MM-DD-YY`, `Mmm D, YYYY`.
     - Day-first: `DD-MMM-YYYY`, `DD/MM/YYYY`.
   - Numbers must be in Gregorian range `0001-01-01` .. `9999-12-31`.
   - Returns `LEFT "DATEVALUE: <message>"` if parsing fails.
4. `TIMEVALUE`
   - Accepts `HH:MM[:SS[.mmm]]` plus optional ` AM| PM` suffix.
   - Accepts 24-hour or 12-hour forms (`0:00`, `12:30 PM`, `23:59:59`).
   - Returns a fraction in `[0, 1)`; reject 24:00 (Excel treats it as next day).
   - Errors propagate as `LEFT "TIMEVALUE: <message>"`.

### Clock Source

- Introduce `ClockSource = SystemClock | FixedClock UTCTime`.
- `jl4-cli` gets a `--fixed-now=YYYY-MM-DDTHH:MM:SSZ` flag; tests can also set `JL4_FIXED_NOW` (same format). CLI precedence: flag > env var > system clock.
- The LSP oneshot harness (used by golden tests) picks up the same environment variable so local tests stay deterministic.
- Clock lives in the evaluation environment (e.g., stored inside `EvalConfig`), so directives, `TODAY`, `NOW`, and any future temporal builtins share it.

### Purity & Evaluation Model

- `TODAY` and `NOW` are the first _impure_ builtins: their results depend on evaluator context rather than arguments. We will surface this in documentation and tooling (e.g., optional compiler warning label such as “⚠️ impure builtin” in hover text).
- To keep most of the system pure, the evaluation datetime should conceptually be an explicit argument to the top-level evaluation environment. Practically, we thread the clock through `EvalConfig`; long term we can expose it as an explicit parameter that callers pass when invoking `jl4-cli` or embedding JL4, making statefulness visible at the API boundary.

### Error handling

- Builtins never throw Haskell exceptions. They always yield `LEFT <string>` when parsing fails.
- Error text must explain the failure (`"DATEVALUE: expected YYYY-MM-DD or M/D/YYYY"` etc.).

### Interaction with `excel-date`

- `excel-date.l4` will wrap `TODAY`/`NOW` so that:
  - `excelToday` returns the `DATE` from `TODAY`.
  - `excelNow` converts the `NUMBER` result of `NOW` into Excel’s serial (respecting the 1900 bug).
  - `ExcelDateValue` delegates to `DATEVALUE` but maps the error strings to the Excel error messaging in the library.

### Performance / Dependencies

- Use `time` + `attoparsec`/`megaparsec` for parsing; no new huge dependencies.
- Parsing must be pure/deterministic (no locale lookups).

## Design Overview

1. **Evaluation runtime**
   - Extend `EvalConfig` (or the structure returned by `initialEnvironment`) with a `clockSource`.
   - Add helper `currentUtc :: EvalM UTCTime` that reads the source and returns either the fixed value or `getCurrentTime`.
   - `TODAY`/`NOW` call `currentUtc` and convert to appropriate L4 values.
2. **Builtins exposure**
   - Add names to `mkBuiltins` with uniques (`"today" rename "TODAY"`, etc.).
   - Provide type signatures alongside other builtins in `TypeCheck.Environment`.
3. **Parsing helpers**
   - Implement `parseExcelDate :: Text -> Either Text Day` and `parseExcelTimeOfDay :: Text -> Either Text DiffTime`.
   - Compose them for `DATEVALUE`/`TIMEVALUE`.
4. **CLI / LSP plumbing**
   - Extend `jl4-cli` options parser with `--fixed-now`.
   - Update `LSP.L4.Oneshot` and Shake runner to read the same environment variable once and pass it down the evaluation stack.

## Implementation Plan

1. Add the spec’d builtins to the type environment (`TODAY`, `NOW`, `DATEVALUE`, `TIMEVALUE`).
2. Thread `ClockSource` through evaluation (CLI, oneshot tests, potential env var) and implement `TODAY`/`NOW`.
3. Implement the parser helpers for `DATEVALUE`/`TIMEVALUE`, returning the required `EITHER`.
4. Wire the new builtins into `excel-date.l4` (excelToday/excelNow wrappers plus DateValue/TimeValue functions).
5. Add regression tests:
   - Golden test file invoking the builtins with `JL4_FIXED_NOW` set.
   - Unit tests for parsing edge cases (valid/invalid formats, AM/PM boundaries).

## Testing

- CLI: `JL4_FIXED_NOW=2025-01-31T15:45:30Z cabal run jl4-cli -- ...` uses the fixed timestamp.
- Golden file under `jl4/examples/ok/excel-date-builtins.l4` to assert `TODAY`/`NOW`/`DATEVALUE`/`TIMEVALUE` outputs.
- Property-style tests (Haskell) verifying `NOW - TODAY` equals fraction within `[0,1)` and `DATEVALUE` round-trips with `Day`.

## Open Questions

1. Should we expose the fixed-now clock via LSP settings instead of env var? (env var is sufficient for now.)
2. Do we need locale-specific parsing beyond the listed formats? (Out of scope—documented limitation.)
3. Should `TIMEVALUE` accept durations like `1.5` hours? (No, stick to textual time forms.)

## References

- Microsoft Support — [TODAY function](https://support.microsoft.com/en-us/office/today-function-41ec846a-241d-4988-8f7a-af294fefe601)
- Microsoft Support — [NOW function](https://support.microsoft.com/en-us/office/now-function-3337fd29-145a-4347-b2e6-20c904739c46)
- Microsoft Support — [DATEVALUE](https://support.microsoft.com/en-us/office/datevalue-function-df8b07d4-7761-4a93-bc33-b7471bbff252)
- Microsoft Support — [TIMEVALUE](https://support.microsoft.com/en-us/office/timevalue-function-d6b0b1e0-4f64-4B0f-bd7b-c8d7a64b9183)
- ISO/IEC 29500-1 §18.17 (OOXML) — defines the spreadsheet date/time serial semantics that Google Sheets and Numbers follow when consuming Excel workbooks.
