# Excel Date Compatibility Library

The `excel-date` library mirrors Excel’s most relied-upon date and workday functions so spreadsheet logic can be ported into L4 without rewriting every calculation. It builds on the existing [`daydate`](daydate.md) primitives and adds Excel-specific behavior such as serial number conversion (with the 1900 leap-year bug), 30/360 day-count conventions, and working-day helpers.

Import it alongside `daydate`:

```l4
IMPORT daydate
IMPORT `excel-date`
```

## Serial Number Helpers

| Function                                      | Description                                                                                                                                      |
| --------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------ |
| `excelSerial1900 date`                        | Convert a `DATE` into the Excel 1900 serial system (inserting the phantom 1900‑02‑29 day for compatibility).                                     |
| `dateFromExcelSerial serial`                  | Convert a serial number back to a `DATE`. Returns `LEFT` if the value has a fractional component or equals 60 (Excel’s non-existent 1900‑02‑29). |
| `excelSerial1904` / `dateFromExcelSerial1904` | Serial/date conversions for the 1904 system.                                                                                                     |

All conversion helpers validate integers via `EITHER STRING …`; call sites should `CONSIDER` the result.

## Clock & Parsing Helpers

- `excelToday` — Wraps the runtime `TODAY` builtin and returns a `DATE`.
- `excelNow` — Combines `NOW` with `excelSerial1900` to yield an Excel serial number (including the 1900 leap-year shim).
- `ExcelDateValue text` — Calls `DATEVALUE`, converting successful results into `DATE`.
- `ExcelTimeValue text` — Thin wrapper over `TIMEVALUE` that keeps Excel’s `EITHER STRING NUMBER` API.

## Difference & Fraction Functions

- `DATEDIF start end unit` — Supports units `Y`, `M`, `D`, `MD`, `YM`, `YD`, matching Excel’s rounding rules and returning `LEFT` if `start > end` or the unit is unknown.
- `ExcelDays end start` — Simple day difference for either `DATE` or serial arguments.
- `ExcelDays360 start end [isEuropean]` — 30/360 day count (US default, set `TRUE` for European).
- `ExcelYearFrac start end [basis]` — Bases 0–4 (US 30/360, Actual/Actual, Actual/360, Actual/365, European 30/360). Negative spans are handled by swapping and negating.

## Month Arithmetic

- `ExcelEDate start months` — Adds months, clamping the day to the target month length.
- `ExcelEOMonth start months` — Returns the last day of the month offset by `months`.

Both require integer `months` and return `EITHER STRING DATE` to report invalid input.

## Workday & Network-Day Helpers

- `ExcelWorkday start offset [holidays]` — Moves forward/backward by working days (Mon–Fri), optionally skipping a list of holiday `DATE`s.
- `ExcelNetworkDays start end [holidays]` — Counts working days inclusively. Returns negative counts when `start > end`, just like Excel.

Weekend masks (`WORKDAY.INTL`, `NETWORKDAYS.INTL`) are not yet implemented; add a custom predicate if you need region-specific weekends.

## Limitations

- Serial value **60** cannot be represented because `DATE` is strictly Gregorian. `dateFromExcelSerial` returns `LEFT` with an explanatory message; downstream code can remap it if necessary.
- `TODAY`/`NOW` read from the evaluation clock supplied via CLI/LSP flags or the `JL4_FIXED_NOW` environment variable. They are impure and may eventually raise a compiler warning; treat the evaluation datetime as part of your inputs.
- `YEARFRAC` basis 1 follows the common Actual/Actual (ISDA-style) split-by-year approach and may differ from Excel in edge cases where the spreadsheet uses different day-count standards.

## Testing Notes

The library ships with golden tests under `jl4-core/libraries/tests/excel-date.*` and executable examples in `jl4/examples/ok/excel-date-tests.l4`. When adding functions, update the `.l4` source first, run `cabal test jl4:test:jl4-test` to regenerate the `*.actual` outputs, inspect, then promote them to the `.golden` files so CI remains stable.
