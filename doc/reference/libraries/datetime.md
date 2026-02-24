# DateTime Library

Absolute points in time combining date, time, and timezone for legal deadlines and cross-timezone comparisons. Stored internally as UTC; extractors return local values in the stored timezone.
Can be imported into L4 files with `IMPORT datetime`. Automatically imports `daydate` for date operations.

### Location

[jl4-core/libraries/datetime.l4](https://github.com/smucclaw/l4-ide/blob/main/jl4-core/libraries/datetime.l4)

### Prerequisites

Requires a `TIMEZONE IS` declaration in the document when using constructors without an explicit timezone parameter. Use with the [timezone](timezone.md) library for convenient timezone constants.

```l4
IMPORT datetime
IMPORT timezone

TIMEZONE IS SGT
```

### Features

- DateTime construction with flexible overloads (date + time + optional timezone)
- Automatic use of document `TIMEZONE IS` declaration
- Explicit timezone override per value using IANA timezone strings
- Date, time, and timezone extraction from DateTime values
- Time-of-day predicates (morning, afternoon, evening)
- Cross-timezone comparison (UTC-based equality)
- Relative datetime operations (at midnight, at noon, at specific time)

### Key Functions

**DateTime Construction:**

- `Datetime date time` - Date + time in document timezone
- `Datetime date time tz` - Date + time in explicit IANA timezone
- `Datetime date h m s` - Date + hours/minutes/seconds in document timezone
- `Datetime date h m s tz` - Date + hours/minutes/seconds in explicit timezone
- `Datetime date` - Date at midnight in document timezone
- `Datetime date tz` - Date at midnight in explicit timezone

**DateTime Extractors:**

- `Date of` dt - Get the date component
- `Time of` dt - Get the time component (local)
- `Timezone of` dt - Get the IANA timezone name
- `the hour of` dt, `the minute of` dt, `the second of` dt
- `the day of` dt, `the month of` dt, `the year of` dt

**DateTime Serial:**

- dt `Datetime to serial` - UTC-based serial number

**Predicates:**

- `is morning`, `is afternoon`, `is evening`
- `is before noon`, `is after noon`

**Relative Operations:**

- `at midnight` dt - Same date, midnight
- `at noon` dt - Same date, noon
- dt `at` time - Same date, different time

**Comparators:**

- `the earlier of` dt1 dt2
- `the later of` dt1 dt2
- Standard comparison operators: LESS THAN, GREATER THAN, AT MOST, AT LEAST

### Example: DateTime Operations

[datetime-example.l4](datetime-example.l4)

**See [datetime.l4](https://github.com/smucclaw/l4-ide/blob/main/jl4-core/libraries/datetime.l4) source for all functions.**
