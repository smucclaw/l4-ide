# Date-compat library

Backwards compatibility layer for legacy DATE syntax. Import with `IMPORT date-compat`.

### Location

[jl4-core/libraries/date-compat.l4](https://github.com/smucclaw/l4-ide/blob/main/jl4-core/libraries/date-compat.l4)

### Purpose

Provides the uppercase DATE constructor for code that used the old record-based date syntax. New code should use Date from daydate instead.

**Legacy syntax:** DATE 15 3 1990
**Modern syntax:** Date 15 3 1990

### Field Accessor Helpers

Named functions to extract date components:

- `` `Day of` `` / `` `day of` `` — extract day from date (`DATE_DAY`)
- `` `Month of` `` / `` `month of` `` — extract month from date (`DATE_MONTH`)
- `` `Year of` `` / `` `year of` `` — extract year from date (`DATE_YEAR`)

Each accessor has a capitalised primary name and a lowercase `AKA` alias.
