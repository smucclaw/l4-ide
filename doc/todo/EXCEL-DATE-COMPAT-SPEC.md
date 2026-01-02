# Specification: Excel Date Compatibility Library

## Executive Summary

We need an `excel-date` standard library that replicates the most widely used Excel date and working-day functions (e.g., `DATEDIF`, `NETWORKDAYS`, `EOMONTH`). This compatibility layer will let L4 decision logic behave exactly like Excel models when lawyers or analysts port spreadsheets into L4. The library should sit on top of the existing `daydate` primitives, expose familiar Excel signatures, and document every incompatibility (e.g., the 1900 leap-year bug) so that users understand where results may diverge.

## Motivation

- Most intake artefacts are Excel spreadsheets. Porting them requires “trust parity” for date arithmetic, otherwise reviewers re-run everything in Excel.
- Excel’s serial-date basis (1900 or 1904) differs from L4’s proleptic Gregorian `Date`. The compatibility layer normalizes this without forcing authors to learn new idioms.
- Advanced functions (`NETWORKDAYS.INTL`, `YEARFRAC`) encode workplace policy and accrual rules that appear directly in legal determinations (leave balances, interest accrual, compliance time limits).

## Scope & Goals

1. Ship an `excel-date` library (L4 + Haskell runtime support) that:
   - Accepts either Excel serial numbers or native `Date` literals as arguments.
   - Returns results identical to Excel for the covered functions, barring documented deviations.
   - Supplies helper predicates to convert between Excel serial numbers and L4 `Date`.
2. Cover the “top 20” Excel date/time functions gathered from Microsoft telemetry and law-office usage (constructors, extractors, differences, month-ends, workday counts).
3. Document edge cases (1900 leap bug, negative serials, optional arguments) and align unit tests to Excel’s examples/golden workbooks.

## Non-Goals

- Supporting every Excel function. Only the ones listed below are in-scope for the first milestone; others can follow incremental specs.
- Re-implementing Excel formatting (`TEXT(date,"mmmm dd")`); we only handle calculations.
- Building a full time-of-day type system. Fractions of a day will be represented as `Number` until L4 grows richer temporal types.

## Design Overview

### Data Interop

- **Excel Serial Numbers**: Integers count days since 1899‑12‑31 (with the 1900-02-29 phantom day). Fractions represent time-of-day.
- **L4 Dates**: The `daydate` library already converts between a proleptic Gregorian date tuple and an absolute day count from 0000-01-01.
- **Bridging strategy**:
  - Introduce helper functions:
    - `excelSerial1900 : Date -> Number`
    - `excelSerial1904 : Date -> Number`
    - `dateFromExcelSerial : Number -> Date`
  - Accept either `Date` or `Number` by overloading via pattern synonyms: if a function receives a `Number`, treat it as a serial; if it receives a `Date`, convert with `Day`.
- **Error Signalling**: Excel raises `#VALUE!`/`#NUM!`. L4 should raise descriptive runtime exceptions (or MAYBE outputs) that tests can assert against. Document mapping in this spec.

### Array-aware Variants

Dynamic arrays are a first-class Excel feature and Google Sheets/Numbers mirror that behavior. We will therefore provide overloads where every scalar-taking function also accepts `LIST OF DATE` / `LIST OF NUMBER` arguments and returns a `LIST` of results. L4’s type-directed resolution lets us publish multiple clauses under the same Excel name:

- Scalar definitions act as the base case (already implemented for `DATE`, `DATEDIF`, `WORKDAY`, etc.).
- A thin wrapper detects `LIST` inputs and `map`s the scalar helper across each element, preserving `EITHER` / `MAYBE` wrappers so partial failures are surfaced item-by-item.

Phase 1 ships only the scalar forms (plus list-valued `holidays` arguments). The follow-up patch will add the array-lifted overloads once we confirm the ergonomics with real spreadsheet ingests; the decision to use overloading rather than bespoke suffixes is now locked in.

### Library Layout

- `jl4-core/libraries/excel-date.l4`: surface API with L4 syntax sugar.
- `jl4-core/src/L4/Excel/Date.hs`: helper functions where Haskell runtime math is cleaner (e.g., 30/360 algorithms).
- Documentation under `doc/libraries/excel-date.md` referencing this spec once implemented.

## API Overview

| Category                   | Excel Function(s)                                                                      | Proposed L4 surface                                                               | Priority | Notes                                                                          |
| -------------------------- | -------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------- | -------- | ------------------------------------------------------------------------------ |
| Constructors & “now”       | `DATE`, `DATEVALUE`, `TIME`, `TIMEVALUE`, `TODAY`, `NOW`                               | `DATE`, `ExcelDateValue`, `ExcelTime`, `ExcelTimeValue`, `excelToday`, `excelNow` | P0       | Provide Excel semantics + volatile `NOW/TODAY`.                                |
| Component extractors       | `DAY`, `MONTH`, `YEAR`, `HOUR`, `MINUTE`, `SECOND`, `WEEKDAY`, `WEEKNUM`, `ISOWEEKNUM` | Keep Excel names (lower snake? TBD)                                               | P0       | `WEEKDAY` must support all return_type options (1,2,3,11–17).                  |
| Differences & fractions    | `DATEDIF`, `DAYS`, `DAYS360`, `YEARFRAC`                                               | `DATEDIF`, `ExcelDays`, `ExcelDays360`, `ExcelYearFrac`                           | P0       | `DATEDIF` supports units `Y`, `M`, `D`, `MD`, `YM`, `YD`.                      |
| Month arithmetic           | `EDATE`, `EOMONTH`                                                                     | `ExcelEDate`, `ExcelEOMonth`                                                      | P0       | Must replicate Excel overflow rules (day clamps).                              |
| Workday counts             | `NETWORKDAYS`, `NETWORKDAYS.INTL`, `WORKDAY`, `WORKDAY.INTL`                           | `ExcelNetworkDays`, `ExcelNetworkDaysIntl`, `ExcelWorkday`, `ExcelWorkdayIntl`    | P1       | Accept optional holiday list + custom weekend masks.                           |
| Day-of-week helpers        | `WEEKDAY`, `WORKDAY` dependencies                                                      | Reuse `daydate` weekdays                                                          | P0       | Already partially exist; need Excel-compatible numbering.                      |
| Misc helpers               | `DATESTRING` parsing, `excelSerial1900`, `excelSerial1904`, `excelEpoch`               | New helper functions                                                              | P0       | Provide canonical conversions for other libraries/tests.                       |
| Supporting Excel-like math | `INT`, `ROUND`, `ROUNDDOWN`, `ROUNDUP`, `ABS`, `SIGN`, `CHOOSE`                        | Already exist elsewhere? If not, stub inside `excel-date` for dependencies        | P2       | Only if needed to faithfully port workbook formulas that pair with date funcs. |

## Detailed Function Requirements

### 1. Constructors & Current Date/Time

| Function                     | Signature                              | Behavior                                                                                                  | Notes                                                                                                                 |
| ---------------------------- | -------------------------------------- | --------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------- |
| `DATE(year, month, day)`     | `Number -> Number -> Number -> Date`   | Same as Excel: months/days overflow into adjacent months/years; year 0–1899 treated literally (no +1900). | Reuse `daydate` normalizer; add Excel-specific validation (#VALUE! if year < 0 or > 9999).                            |
| `DATEVALUE(text)`            | `String -> MAYBE Date`                 | Parse ISO (`YYYY-MM-DD`), US short (`M/D/YYYY`), long month names, and relative forms (“1-Jan-25”).       | Document locale assumptions; Excel uses OS locale—L4 will enforce ISO + configurable parser table.                    |
| `TIME(hour, minute, second)` | `Number -> Number -> Number -> Number` | Return serial fraction (0–1) for time-of-day.                                                             | Accept >24 hours (wrap).                                                                                              |
| `TIMEVALUE(text)`            | `String -> Number`                     | Parse `hh:mm[:ss]` plus AM/PM.                                                                            | Should handle `2:30 PM`.                                                                                              |
| `TODAY()`                    | `() -> Date`                           | Volatile; obtains from evaluation context clock.                                                          | Hook existing `today` primitive if present.                                                                           |
| `NOW()`                      | `() -> { date: Date, serial: Number }` | Return both date and serial for fractions.                                                                | Downstream may only need serial; spec leaves structure open but must include fractional day precision to millisecond. |

### 2. Component Extractors

- `DAY(date|serial)`, `MONTH(...)`, `YEAR(...)`: Return integer component. Excel accepts either serial or Date. Implement via `Day`.
- `HOUR`, `MINUTE`, `SECOND`: operate on fractional part. Accept `DateTime` record or serial number.
- `WEEKDAY(date,[return_type])`: Support Excel’s return_type mapping:
  - `1` (default): Sunday=1..Saturday=7.
  - `2`: Monday=1..Sunday=7.
  - `3`: Monday=0..Sunday=6.
  - `11`–`17`: Monday–Sunday = 1..7, Tuesday=1..7, ..., Sunday=1. Document table.
- `WEEKNUM(date,[return_type])`: Support types `1` (Sunday week start), `2` (Monday), `11`–`17` align with `WEEKDAY`.
- `ISOWEEKNUM(date)`: Must follow ISO 8601 (weeks start on Monday; week 1 contains first Thursday). Use `daydate` week helpers where possible.

### 3. Difference & Fraction Functions

- `DATEDIF(start_date, end_date, unit)`:
  - Inputs: `Date | Number`, `Date | Number`, `String`.
  - Units:
    - `"Y"`: Completed years.
    - `"M"`: Completed months.
    - `"D"`: Days between.
    - `"MD"`: Days disregarding months/years.
    - `"YM"`: Months disregarding years.
    - `"YD"`: Days disregarding years.
  - Excel errors if `start_date > end_date`; spec should mimic via runtime error.
- `DAYS(end, start)`: Return `end - start` in days as signed integer; accepts Date or serial.
- `DAYS360(start, end, [method])`:
  - Methods: `FALSE`/missing = US (NASD); `TRUE` = European.
  - Document end-of-month handling exactly like Excel (e.g., treat Feb 28 differently).
- `YEARFRAC(start, end, [basis])`:
  - `basis ∈ {0,1,2,3,4}` mapping to 30/360 US, Actual/Actual, Actual/360, Actual/365, 30/360 European.
  - Return double precision fraction.

### 4. Month Navigation

- `EDATE(start_date, months)`: Add `months` preserving day-of-month; clamp to last valid day if overflow.
- `EOMONTH(start_date, months)`: Add `months`, then return last day of resulting month.

### 5. Workday / Network-Day Functions

- `WORKDAY(start_date, days, [holidays])`:
  - `days` may be negative.
  - Default weekend = Saturday/Sunday.
  - `holidays` list accepts Date or serial, deduplicated.
  - Implementation algorithm: iterate or use arithmetic approach (convert to business-day counts).
- `WORKDAY.INTL(start_date, days, [weekend_pattern], [holidays])`:
  - `weekend_pattern` either numeric code (1–17) or 7-character string (Mon→Sun, `1`=weekend, `0`=workday).
  - Validate string length exactly 7; error otherwise.
- `NETWORKDAYS(start_date, end_date, [holidays])`: Inclusive count of working days.
- `NETWORKDAYS.INTL(...)`: Same options as `WORKDAY.INTL`.

### 6. Serial Conversion Helpers

- `excelSerial1900(date)` / `excelSerial1904(date)`: Convert L4 Date to Excel serial. Need to add offsets:
  - 1900 system: `serial = Day(date) - Day(1899-12-31) + 1`, then insert fake 1900-02-29 day for dates ≥ 1900-03-01.
  - 1904 system: base = 1904-01-01.
- `dateFromExcelSerial(serial, [system])`: Reverse conversion; default to 1900 system.
- `excelDateInput(x)`: Utility to normalize argument to Date (Serial -> Date, Date -> Date).

### 7. Optional Excel-Inspired Utilities

- `ExcelChoose(index, list)` to mimic `CHOOSE`.
- `ExcelIf(test, then, else)` – thin wrapper over existing `IF` semantics if necessary for parity.
- `ExcelRound`, `ExcelInt`: Provide Excel rounding rules (banker’s vs away-from-zero) because `YEARFRAC`, `WORKDAY` examples often combine them.

## Implementation Plan

1. **Foundations (P0)**
   - Create `jl4-core/libraries/excel-date.l4` scaffolding that re-exports `daydate`.
   - Implement serial ↔ Date helpers and argument normalization utilities.
   - Provide `TODAY`, `NOW`, `DATE`, `DATEVALUE`, `TIME`, `TIMEVALUE`, component extractors.
2. **Differences & Month math (P0)**
   - Port `DAYS`, `DATEDIF`, `EDATE`, `EOMONTH`, `DAYS360`, `YEARFRAC`.
   - Ensure tests compare against known Excel value pairs (documented in `.golden` files).
3. **Workday set (P1)**
   - Implement `WORKDAY`, `NETWORKDAYS`, `.INTL` variants with helper to decode weekend masks.
   - Add optional `ExcelHolidayCalendar` record for reuse.
4. **Documentation & Examples**
   - Create `doc/libraries/excel-date.md` with side-by-side Excel vs L4 examples.
   - Provide sample `.l4` programs (e.g., leave accrual) under `jl4/examples/excel-date/`.
5. **Testing Strategy**
   - New Cabal test suite `excel-date-tests` referencing Microsoft-published examples.
   - Golden files verifying textual output for typical formulas.
   - Property tests that `dateFromExcelSerial (excelSerial1900 d) == d` for supported range (1900-03-01..9999-12-31).

## Edge Cases & Compatibility Notes

- **1900 Leap-Year Bug**: Excel treats 1900-02-29 as valid. Spec decision: keep bug for compatibility; add `excelSerialStrict` that omits bug for authors needing ISO correctness.
- **1904 Date System**: Provide optional parameter or separate entry points; default to 1900 for backward compatibility.
- **Negative Serials**: Excel permits dates before the epoch; some functions reject them. Document per function (e.g., `DATEVALUE` accepts).
- **Volatile Functions**: `TODAY`/`NOW` should read from evaluation context so repeated runs within a test harness can pin the date.
- **Error surfaces**: Provide error constructors analog to Excel’s (#VALUE!, #NUM!) so IDE can highlight mismatched arguments.
- **Serial 60 gap**: The core `DATE` type cannot represent 1900‑02‑29. `dateFromExcelSerial` will therefore return `LEFT` for serial `60` with a descriptive message until we introduce an internal sentinel value.

## Testing & Validation

- Mirror Microsoft documentation tables: e.g., `DATEDIF("1/1/2012","7/1/2012","M") == 6`.
- Include round-trip tests for `excelSerial1900` conversions.
- Provide scenario tests for workday calculations with weekend codes and custom holiday lists.
- Validate `YEARFRAC` basis values against authoritative calculators (e.g., FinAid or MS docs).

## Documentation Tasks

- Update `doc/libraries/daydate.md` to reference `excel-date` for compatibility use cases.
- Add migration guide snippet to `doc/README.md` describing when to prefer Excel-compatible functions vs native `daydate` ones.
- Record outstanding questions/bugs in `doc/todo/EXCEL-DATE-COMPAT-SPEC.md` until feature completed, then move to `doc/done/`.

## Open Questions

1. Should we expose Excel-style error literals (`#VALUE!`) as strings, or map them to L4 exceptions/MAYBE? Decision pending on broader error-handling design.
2. Do we need timezone-aware `NOW()`? Excel ignores timezones; L4 runtime might need UTC vs local choices.
3. Is there demand for other Excel date functions (`DATEDIF`, `WORKDAY`) to accept arrays/ranges? Spreadsheet imports often pass named ranges.
4. Should we honour Excel’s leap-year bug for all functions or only conversions? (E.g., should `DATEDIF` think 1900-02-29 exists?)
5. How do we freeze `TODAY`/`NOW` during deterministic tests (env var override vs dependency injection)?

Once these questions resolve and implementations land, this spec should move to `doc/done/` with a changelog of any differences from Excel behavior.

## References

- Excel does not have a single cross-vendor normative standard. Microsoft’s documentation is treated as the authoritative source for parity, while the ISO/IEC and OASIS specs below document the overlapping function grammar used by Google Sheets and Apple Numbers. Divergences are recorded in this spec as they are discovered.
- Microsoft Support, “Excel functions (alphabetical)” — master list of official semantics and argument tables. <https://support.microsoft.com/en-us/office/functions-by-category-5f91f4e9-7b42-46d2-9bd1-63f26a86c0eb>
- Microsoft Support, “DATEDIF function” — canonical description of DATEDIF units and error conditions. <https://support.microsoft.com/en-us/office/datedif-function-25dba1a4-2812-480b-84dd-8b32a451b35c>
- Microsoft Support, “NETWORKDAYS and NETWORKDAYS.INTL functions” — inclusive workday counting rules and weekend/holiday semantics. <https://support.microsoft.com/en-us/office/networkdays-function-f9d6023b-6c23-47f8-8d39-7a12eb7519c6>
- Microsoft Support, “WORKDAY and WORKDAY.INTL functions” — forward/backward business-day arithmetic, weekend codes. <https://support.microsoft.com/en-us/office/workday-function-f764a49a-2fc9-4fc1-b86b-5017f05a48bb>
- Microsoft Support, “YEARFRAC function” — day-count basis definitions. <https://support.microsoft.com/en-us/office/yearfrac-function-3844141e-c76d-4143-82b6-208454ddc6a8>
- Microsoft Support, “DAYS and DAYS360 functions” — day-difference semantics and US vs European 30/360. <https://support.microsoft.com/en-us/office/days-function-57740535-d549-4395-8728-0f07bff0b9df> and <https://support.microsoft.com/en-us/office/days360-function-e0199d66-2a4e-4474-821a-390bcdfe2fa0>
- Microsoft Support, “EDATE and EOMONTH functions” — month offset rules and date clamping. <https://support.microsoft.com/en-us/office/edate-function-771a0b95-b3a5-4b93-bf80-5f3f0eb47b66> and <https://support.microsoft.com/en-us/office/eomonth-function-dc9740f9-7c46-4464-9d0c-c46c4a52a9c6>
- Microsoft Learn, “Excel Date Systems” — authoritative explanation of the 1900 vs 1904 serial bases. <https://learn.microsoft.com/en-us/office/troubleshoot/excel/1900-and-1904-date-system>
- ECMA International, “ECMA-376 (Office Open XML File Formats), Part 1 §18.17” — ISO/IEC 29500-1:2021 text covering spreadsheet formula grammar shared by Numbers (via OOXML import). <https://www.ecma-international.org/publications-and-standards/standards/ecma-376/>
- OASIS, “OpenDocument Format 1.3 Part 4: OpenFormula” — vendor-neutral definition of spreadsheet functions implemented by Google Sheets/LibreOffice; use as a secondary reference when Excel leaves behavior unspecified. <https://docs.oasis-open.org/office/OpenDocument/v1.3/os/part4-formula/OpenDocument-v1.3-os-part4-formula.html>
