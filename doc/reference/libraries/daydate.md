# Daydate Library

Date arithmetic and temporal logic for legal deadlines and time-based rules. Follows ISO 8601 conventions.
Can be imported into L4 files with `IMPORT daydate`. Automatically imports `date-compat` for backwards-compatible DATE accessors.

### Location

[jl4-core/libraries/daydate.l4](https://github.com/smucclaw/l4-ide/blob/main/jl4-core/libraries/daydate.l4)

### Features

- Date construction from day/month/year, serial numbers, strings, or DATETIME extraction
- Date arithmetic (add/subtract days, weeks, months, years)
- Weekday and weekend detection
- Week-of-year calculations
- Month and year helpers
- Leap year detection
- String parsing for JSON interoperability
- DATETIME date extraction

### Key Functions

**Constants:**

- `Monday` through `Sunday` (0-6)
- `January` through `December` (1-12)
- `Days in a week`, `Days in a year`, etc.

**Date Construction:**

- `Date day month year` - Create date from components
- `Date serialNumber` - Create date from serial number
- `Date str` - Parse date string (ISO format, ignores time/timezone if present), returns MAYBE DATE
- `Date dt` - Extract date from DATETIME (ignores time and timezone)
- `Week weekNumber year` - First day of week
- `Month month year` - First day of month
- `Year year` - First day of year

**Date Arithmetic:**

- `date PLUS days` - Add days to date
- `date MINUS days` - Subtract days from date
- `the day after`, `the week before`, etc.

**Queries:**

- `Weekday of date` - Get weekday (0-6)
- `Week of the year date` - Get ISO week number
- `is weekend`, `is weekday` - Check day type
- `is leap year` - Check leap year

### Example: Date Calculations

[daydate-example.l4](daydate-example.l4)

**See [daydate.l4](https://github.com/smucclaw/l4-ide/blob/main/jl4-core/libraries/daydate.l4) source for all functions.**
