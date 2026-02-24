# Time Library

Wall-clock time-of-day for legal rules involving time constraints. Uses 24-hour format internally with no date or timezone component.
Can be imported into L4 files with `IMPORT time`.

### Location

[jl4-core/libraries/time.l4](https://github.com/smucclaw/l4-ide/blob/main/jl4-core/libraries/time.l4)

### Features

- Time construction from hours/minutes/seconds, serial numbers, strings, or DATETIME extraction
- AM/PM support via `Meridiem Indicator` enum
- Time arithmetic (add/subtract hours, minutes, seconds) with midnight wrapping
- Time-of-day predicates (morning, afternoon, evening)
- Serial conversion (fraction of day: 0.0 = midnight, 0.5 = noon)
- Comparison operators and min/max functions
- String parsing for JSON interoperability

### Key Functions

**Constants:**

- `Midnight` - 00:00:00
- `Noon` - 12:00:00
- `Hours in a day`, `Minutes in an hour`, `Seconds in a minute`, etc.

**Time Construction:**

- `Time h m s` - Create time from hours, minutes, seconds
- `Time h m` - Create time (seconds default to 0)
- `Time h` - Create time (minutes and seconds default to 0)
- `Time h m s am` / `Time h m s pm` - 12-hour AM/PM format
- `Time h am` / `Time h pm` - Shorthand 12-hour format
- `Time str` - Parse time string ("HH:MM:SS" or "HH:MM"), returns MAYBE TIME
- `Time dt` - Extract time from DATETIME (ignores date and timezone)
- `serial Serial to time` - Create time from day fraction

**Time Extractors:**

- `the hour of` t - Get hour (0-23)
- `the minute of` t - Get minute (0-59)
- `the second of` t - Get second (0-59)

**Time Arithmetic (wraps at midnight):**

- t `plus hours` n, t `minus hours` n
- t `plus minutes` n, t `minus minutes` n
- t `plus seconds` n, t `minus seconds` n

**Predicates:**

- `is morning` - Before noon (hour < 12)
- `is afternoon` - 12:00 to 17:59
- `is evening` - 18:00 onwards
- `is before noon`, `is after noon`

**Comparators:**

- `the earlier of` t1 t2
- `the later of` t1 t2
- t1 `is before` t2, t1 `is after` t2
- Standard comparison operators: LESS THAN, GREATER THAN, AT MOST, AT LEAST
- TIME MINUS TIME returns difference in seconds

**Clock:**

- `CURRENTTIME` - Returns the current local time as a TIME value (requires `TIMEZONE IS`)

### Example: Time Operations

[time-example.l4](time-example.l4)

**See [time.l4](https://github.com/smucclaw/l4-ide/blob/main/jl4-core/libraries/time.l4) source for all functions.**
