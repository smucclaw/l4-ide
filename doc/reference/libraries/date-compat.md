# Date-compat library

Backwards compatibility layer for legacy DATE syntax. Import with `IMPORT date-compat`.

### Location

[jl4-core/libraries/date-compat.l4](https://github.com/smucclaw/l4-ide/blob/main/jl4-core/libraries/date-compat.l4)

### Purpose

Provides the uppercase DATE constructor for code that used the old record-based date syntax. New code should use Date from daydate instead.

**Legacy syntax:** DATE 15 3 1990
**Modern syntax:** Date 15 3 1990
