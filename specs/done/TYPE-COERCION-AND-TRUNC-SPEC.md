# Spec: Type Coercion & TRUNC Builtins for OPM2L4 (SSG Priority)

## Executive Summary

SSG relies heavily on OPM’s type-conversion helpers and Excel-style rounding. The OPM2L4 translator cannot faithfully emit these constructs until L4 ships first-class primitives. This spec requests four additions to the L4 surface area:

**Implementation status:** `TOSTRING`, `TONUMBER`, `TODATE`, `TRUNC`, and expanded `AS STRING` semantics are implemented in `jl4-core` (see `doc/libraries/coercions.md`). DATE is now a builtin type (not a userland record), so `TODATE` directly constructs a `ValDate` value without needing the `daydate` library's constructor.

| Feature            | OPM Function(s)             | Proposed L4 Surface               | Notes                                                                    |
| ------------------ | --------------------------- | --------------------------------- | ------------------------------------------------------------------------ |
| Text coercion      | `Text(value)`               | `TOSTRING(value)`                 | Deterministic string formatting for NUMBER/BOOLEAN/DATE/CURRENCY inputs. |
| Number parsing     | `Number(text)`              | `TONUMBER(text)` → `MAYBE NUMBER` | Accepts trimmed ASCII numeric literals (Excel/OPM syntax).               |
| Date parsing       | `Date(text)`                | `TODATE(text)` → `MAYBE DATE`     | Parses ISO + OPM default formats into an L4 `DATE`.                      |
| Decimal truncation | `Trunc(number, numDigits?)` | `TRUNC(number, numDigits)`        | Mirrors Excel/OPM truncation semantics (default `numDigits = 0`).        |

All four surface as uppercase builtins (similar to `ROUND`/`STRINGLENGTH`) and will live in the type environment so generated L4 code is portable across CLI/LSP reactors.

## Motivation

- **SSG coverage gap:** The SSG team flagged `Text`, `Number`, `Date`, and `Trunc` as frequently used; without them, entire tables currently degrade to `UNKNOWN_ATTR` placeholders or TODO comments.
- **Translator work blocked:** `docs/COVERAGE_ANALYSIS.md` now lists type coercion and remaining numeric helpers as Tier-1/Tier-2 gaps driven by SSG. We can finish the translator side immediately once L4 offers these primitives.
- **Avoid re-implementing parsing/formatting in every consumer:** Today we would have to re-create locale rules inside every generated L4 file. Centralizing them in `jl4-core` keeps semantics consistent and allows future optimization (e.g., deterministic formatting across CLI/LSP).

## Requirements

### 1. `TOSTRING`

| Aspect          | Requirement                                                                                                                                                                                                                                                                                                                                                                          |
| --------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| Type signature  | Overloaded builtin. Minimal set: <br>`GIVEN n IS A NUMBER GIVETH A STRING TOSTRING n`<br>`GIVEN b IS A BOOLEAN GIVETH A STRING TOSTRING b`<br>`GIVEN d IS A DATE GIVETH A STRING TOSTRING d`<br>`GIVEN c IS A CURRENCY GIVETH A STRING TOSTRING c`<br>(Strings should be identity.)                                                                                                  |
| Formatting      | - NUMBER: canonical decimal string using `.` as the radix, no thousand separators, trims trailing zeros unless needed (e.g., `1.20` → `"1.2"`, `1.0` → `"1"`).<br>- BOOLEAN: `"TRUE"` / `"FALSE"` (uppercase to match OPM exports).<br>- DATE: ISO `YYYY-MM-DD`.<br>- CURRENCY: same as number, but leave currency symbol handling to upstream UI (OPM’s `Text()` drops the symbol). |
| Knowledge-state | Caller is responsible for unwrapping `MAYBE`. No implicit `fromMaybe`.                                                                                                                                                                                                                                                                                                               |
| Performance     | Pure function—no allocation beyond the resulting `STRING`.                                                                                                                                                                                                                                                                                                                           |

### 2. `TONUMBER`

| Aspect          | Requirement                                                                                                                                                  |
| --------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| Type signature  | `GIVEN text IS A STRING GIVETH A MAYBE NUMBER TONUMBER text`.                                                                                                |
| Accepted syntax | - Optional leading/trailing whitespace.<br>- Optional leading `+`/`-`.<br>- Digits with optional single decimal point.<br>- Scientific notation (`1.23E+4`). |
| Failure cases   | Return `NOTHING` if the trimmed text is empty, contains invalid characters, or overflows `Double`. No exceptions.                                            |
| Determinism     | Parsing must not depend on locale (always `.` decimal).                                                                                                      |

### 3. `TODATE`

| Aspect                      | Requirement                                                                                                                                                        |
| --------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| Type signature              | `GIVEN text IS A STRING GIVETH A MAYBE DATE TODATE text`.                                                                                                          |
| Formats                     | Minimum viable set to unlock SSG exports: `YYYY-MM-DD`, `YYYY/MM/DD`, `DD-MMM-YYYY`, `DD/MM/YYYY`, `MMM DD, YYYY`. Case-insensitive month names, trims whitespace. |
| Validation                  | Reject out-of-range dates (`< 0001-01-01` or `> 9999-12-31`). Return `NOTHING` if parsing fails.                                                                   |
| Timezones                   | Text is assumed to be a calendar date (no time-of-day).                                                                                                            |
| Relationship to `DATEVALUE` | `TODATE` is the low-level parser; `excel-date.l4` can still offer Excel-specific wrappers that return `EITHER` with verbose error text.                            |

### 4. `TRUNC`

| Aspect         | Requirement                                                                                                                                                                                                                                                                                    |
| -------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Type signature | `GIVEN value IS A NUMBER, digits IS A NUMBER GIVETH A NUMBER TRUNC value digits`. Second parameter defaults to `0` when omitted (translator will emit literal `0`).                                                                                                                            |
| Semantics      | Matches Excel/OPM `Trunc(number, [num_digits])`:<br>- `digits >= 0`: truncate to `digits` decimal places toward zero.<br>- `digits < 0`: truncate digits to the left of the decimal point (e.g., `digits = -1` => tens).<br>- Works for negative numbers (always toward zero, unlike `FLOOR`). |
| Edge cases     | `digits` is rounded to nearest integer before applying (Excel behavior). Inputs of `NaN`/`Infinity` propagate as evaluation errors (same as other numeric ops).                                                                                                                                |

### 5. Tooling & Documentation

- Add keyword entries to `TypeCheck.Environment` and lexer/printer so uppercase forms parse correctly.
- Document the new builtins under `doc/libraries/prelude.md` (or a new `doc/libraries/coercions.md`).
- Mention in `doc/foundation-course-ai` that these are deterministic (no locale).

## Implementation Plan

1. **Backend plumbing**
   - Introduce helper modules in `jl4-core` implementing the parsing/formatting logic (likely using `Text.Printf` for numbers and `time` for dates).
   - Register the four builtins in `mkBuiltins` with appropriate uniques and type schemes.
2. **Prelude wrappers**
   - Optionally expose user-facing helpers (`textOfNumber`, `parseDate`) in `prelude.l4` so handwritten L4 code can `IMPORT prelude` and call them.
   - Re-export from `excel-date.l4` as needed (e.g., `excelToday` already exists; add `excelText` only if helpful).
3. **CLI/LSP**
   - No new flags required—the functions are pure.
4. **Translator unblocker**
   - Once the builtins land, opm2l4 can swap its current TODO stubs (`TONUMBER(expr)` comments) for real function calls and add regression fixtures for SSG rules.

## Acceptance Tests

- **Golden L4**: Add `jl4/examples/ok/type-coercion.l4` exercising each builtin. Include expectation comments showing outputs for representative inputs (positive/negative numbers, booleans, ISO dates, invalid text).
- **Haskell unit tests**: Cover edge cases (scientific notation, invalid strings, leap-day parsing, `Trunc(-123.45, -2)`).
- **CLI integration**: Extend `jl4/examples/ok/tests/float.ep.golden` (or new file) to ensure `TRUNC` prints expected values in evaluator output.
- **Documentation lint**: Ensure `doc/foundation-course-ai` references the new builtins so onboarding materials stay current.

## Open Questions

1. Should `TONUMBER`/`TODATE` return `EITHER STRING X` instead of `MAYBE` to preserve error text? (MAYBE is sufficient for OPM2L4, but returning `EITHER` would aid diagnostics.)
2. Do we also need `TOTEXT(date, format)` style overloads (e.g., localized formatting)? For now we stick to a single canonical representation; locales can be layered later.
3. Should `TRUNC` accept non-integer `digits`? Excel truncates the parameter before use; we mirror that behavior but need to confirm no additional validation is required.

## References

- OPM2L4 Coverage: `/Users/mengwong/src/legalese/opm2l4/docs/COVERAGE_ANALYSIS.md#ssg-feature-guidance`
- Mapping guide: `/Users/mengwong/src/legalese/opm2l4/docs/OPM_L4_TRANSLATION_SPECS.md` (Sections 1.1–1.3 for type coercion, §4 for remaining math functions).
- Oracle Intelligent Advisor Function Reference (Text, Number, Date, Trunc).
