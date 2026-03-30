# L4 Core Libraries

L4 ships with a set of core libraries that provide essential functions for common tasks. These libraries are written in L4 itself and serve as both utilities and examples of idiomatic L4 code.

## Overview

Core libraries are located in the [jl4-core/libraries/](https://github.com/smucclaw/l4-ide/tree/main/jl4-core/libraries) directory:

- **[prelude](prelude.md)** - Standard functions (automatically imported)
- **[daydate](daydate.md)** - Date calculations and temporal logic
- **[time](time.md)** - Wall-clock time-of-day operations
- **[datetime](datetime.md)** - Absolute points in time with timezones
- **[timezone](timezone.md)** - IANA timezone constants
- **[excel-date](excel-date.md)** - Excel date compatibility
- **[math](math.md)** - Mathematical functions
- **[currency](currency.md)** - Currency handling (ISO 4217)
- **[legal-persons](legal-persons.md)** - Legal entity types and capacity
- **[jurisdiction](jurisdiction.md)** - Jurisdiction definitions
- **[holdings](holdings.md)** - Holdings and ownership
- **[actus](actus.md)** - ACTUS financial contract types and evaluation
- **[date-compat](date-compat.md)** - Legacy DATE syntax compatibility
- **[llm](llm.md)** - LLM API integration

---

## Using Libraries

Libraries require explicit import:

```l4
IMPORT daydate
IMPORT math
IMPORT currency
```

### Import Paths

Import by library name or file path:

```l4
IMPORT daydate                   -- Standard/core library
IMPORT "my-custom-lib.l4"        -- Custom library
```

---

## Library Quick Reference

Condensed function tables for each core library. See individual library pages for full details.

### prelude

Automatically imported. Provides foundational functions for lists, numbers, and data types.

**List Functions:**

| Function           | Type                            | Description            |
| ------------------ | ------------------------------- | ---------------------- |
| `null list`        | `[a] → Bool`                    | Is list empty?         |
| `count list`       | `[a] → Number`                  | List length            |
| `map f list`       | `(a→b) → [a] → [b]`             | Transform each element |
| `filter f list`    | `(a→Bool) → [a] → [a]`          | Keep matching elements |
| `foldr f z list`   | `(a→r→r) → r → [a] → r`         | Right fold             |
| `foldl f z list`   | `(r→a→r) → r → [a] → r`         | Left fold              |
| `append l1 l2`     | `[a] → [a] → [a]`               | Concatenate lists      |
| `concat lists`     | `[[a]] → [a]`                   | Flatten list of lists  |
| `reverse list`     | `[a] → [a]`                     | Reverse list           |
| `at list i`        | `[a] → Number → a`              | Index access (0-based) |
| `take n list`      | `Number → [a] → [a]`            | First n elements       |
| `drop n list`      | `Number → [a] → [a]`            | Drop first n elements  |
| `elem x list`      | `a → [a] → Bool`                | Membership test        |
| `sort list`        | `[Number] → [Number]`           | Sort ascending         |
| `zip l1 l2`        | `[a] → [b] → [Pair a b]`        | Pair up elements       |
| `nub list`         | `[a] → [a]`                     | Remove duplicates      |
| `partition f list` | `(a→Bool) → [a] → Pair [a] [a]` | Split by predicate     |

**Boolean Quantifiers:**

| Function     | Type                    | Description            |
| ------------ | ----------------------- | ---------------------- |
| `all f list` | `(a→Bool) → [a] → Bool` | All satisfy predicate? |
| `any f list` | `(a→Bool) → [a] → Bool` | Any satisfy predicate? |
| `and list`   | `[Bool] → Bool`         | All true?              |
| `or list`    | `[Bool] → Bool`         | Any true?              |

**Numeric:**

| Function       | Type                       | Description        |
| -------------- | -------------------------- | ------------------ |
| `sum list`     | `[Number] → Number`        | Sum of numbers     |
| `product list` | `[Number] → Number`        | Product of numbers |
| `max x y`      | `Number → Number → Number` | Maximum of two     |
| `min x y`      | `Number → Number → Number` | Minimum of two     |
| `maximum list` | `[Number] → Number`        | Maximum of list    |
| `minimum list` | `[Number] → Number`        | Minimum of list    |

**Maybe (Optional):**

| Function              | Type                        | Description         |
| --------------------- | --------------------------- | ------------------- |
| `fromMaybe default x` | `a → Maybe a → a`           | Unwrap with default |
| `isJust x`            | `Maybe a → Bool`            | Is Just?            |
| `isNothing x`         | `Maybe a → Bool`            | Is Nothing?         |
| `mapMaybe f list`     | `(a → Maybe b) → [a] → [b]` | Filter-map          |
| `catMaybes list`      | `[Maybe a] → [a]`           | Collect Justs       |

**Either, Pair, Utility:**

| Function              | Type                             | Description         |
| --------------------- | -------------------------------- | ------------------- |
| `either left right x` | `(a→c) → (b→c) → Either a b → c` | Fold over Either    |
| `PAIR OF x, y`        | constructor                      | Create pair         |
| `p's fst`             | `Pair a b → a`                   | First element       |
| `p's snd`             | `Pair a b → b`                   | Second element      |
| `id x`                | `a → a`                          | Identity function   |
| `const x y`           | `a → b → a`                      | Always return first |

**Dictionary:**

| Function                  | Type                                      | Description       |
| ------------------------- | ----------------------------------------- | ----------------- |
| `emptyDict`               | `Dictionary k v`                          | Empty dictionary  |
| `listToDict pairs`        | `[Pair k v] → Dictionary k v`             | Create from pairs |
| `dictLookup key dict`     | `k → Dictionary k v → Maybe v`            | Query by key      |
| `dictInsert key val dict` | `k → v → Dictionary k v → Dictionary k v` | Add/replace entry |
| `dictDelete key dict`     | `k → Dictionary k v → Dictionary k v`     | Remove entry      |
| `dictKeys dict`           | `Dictionary k v → [k]`                    | All keys          |
| `dictElems dict`          | `Dictionary k v → [v]`                    | All values        |
| `mapDict f dict`          | `(v→w) → Dict k v → Dict k w`             | Map over values   |
| `filterDict f dict`       | `(v→Bool) → Dict → Dict`                  | Filter by value   |

See [prelude.md](prelude.md) for the complete function list including `sortBy`, `zipWith`, `deleteBy`, `dictUnion`, and more.

### math

Requires: `IMPORT prelude`

| Function      | Type                    | Description               |
| ------------- | ----------------------- | ------------------------- |
| `EULER`       | `NUMBER`                | Euler's number (2.718...) |
| `exp x`       | `NUMBER → NUMBER`       | e^x                       |
| `ln x`        | `NUMBER → MAYBE NUMBER` | Natural logarithm         |
| `log10 x`     | `NUMBER → MAYBE NUMBER` | Base-10 logarithm         |
| `sqrt x`      | `NUMBER → MAYBE NUMBER` | Square root               |
| `sin x`       | `NUMBER → MAYBE NUMBER` | Sine                      |
| `cos x`       | `NUMBER → MAYBE NUMBER` | Cosine                    |
| `tan x`       | `NUMBER → MAYBE NUMBER` | Tangent                   |
| `asin x`      | `NUMBER → MAYBE NUMBER` | Arcsine (domain [-1,1])   |
| `acos x`      | `NUMBER → MAYBE NUMBER` | Arccosine (domain [-1,1]) |
| `atan x`      | `NUMBER → MAYBE NUMBER` | Arctangent                |
| `absNumber x` | `NUMBER → NUMBER`       | Absolute value            |

Trig functions return MAYBE to handle domain errors safely.

### daydate

Date arithmetic library. ISO 8601 conventions.

**Constants:** `Monday`(1) through `Sunday`(0), `January`(1) through `December`(12), `Days in a week`(7), `Days in a year`(365.2425), `Days in a month`(30.44)

**Constructors:**

| Function              | Type                              | Description                    |
| --------------------- | --------------------------------- | ------------------------------ |
| `Date day month year` | `NUMBER → NUMBER → NUMBER → DATE` | From d/m/y                     |
| `Date days`           | `NUMBER → DATE`                   | From serial (days since epoch) |
| `Year year`           | `NUMBER → DATE`                   | Jan 1 of year                  |
| `Month month year`    | `NUMBER → NUMBER → DATE`          | 1st of month                   |
| `Week week year`      | `NUMBER → NUMBER → DATE`          | Monday of ISO week             |

**Queries:**

| Function                 | Description                   |
| ------------------------ | ----------------------------- |
| `Weekday of date`        | Day of week (0=Sun, 1=Mon...) |
| `Week of the year date`  | ISO week number               |
| `Month of the year date` | Month number                  |
| `is weekend date`        | Saturday or Sunday?           |
| `is weekday date`        | Monday through Friday?        |
| `is leap year year`      | Leap year?                    |

**Relative Dates:** `the day after date`, `the day before date`, `the week after date`, `the month after date`, `the year after date`, `the earlier of date1 date2`, `the later of date1 date2`

**Arithmetic:** `date + n` (add days), `date - n` (subtract days), `date1 - date2` (difference in days), `date1 >= date2` (comparison)

See [daydate.md](daydate.md) for full documentation.

### time

Wall-clock time of day (no date, no timezone). 24-hour format.

**Constants:** `Midnight` (00:00:00), `Noon` (12:00:00)

**Constructors:**

| Function           | Description                  |
| ------------------ | ---------------------------- |
| `Time h m s`       | From hours, minutes, seconds |
| `Time h m`         | From hours, minutes (s=0)    |
| `Time h m s am/pm` | 12-hour format with meridiem |

**Extractors:** `the hour of t`, `the minute of t`, `the second of t`

**Arithmetic:** ``t `plus hours` n``, ``t `plus minutes` n``, ``t `plus seconds` n``, ``t `minus hours` n``, ``t `minus minutes` n``, ``t `minus seconds` n``

**Predicates:** `` t `is before noon` ``, `` t `is after noon` ``, `` t `is morning` ``, `` t `is afternoon` ``, `` t `is evening` ``

**Comparators:** `the earlier of t1 t2`, `the later of t1 t2`, `t1 >= t2`, `t1 - t2` (difference in seconds)

See [time.md](time.md) for full documentation.

### datetime

Combined date + time + timezone. Stored internally as UTC.

**Constructors:**

| Function                | Description                   |
| ----------------------- | ----------------------------- |
| `Datetime date time`    | Using document TIMEZONE       |
| `Datetime date time tz` | Explicit IANA timezone        |
| `Datetime date h m s`   | From date + h/m/s             |
| `Datetime date`         | Midnight in document TIMEZONE |

**Extractors:** `Date of dt`, `Time of dt`, `Timezone of dt`, `the hour of dt`, `the minute of dt`, `the second of dt`, `the day of dt`, `the month of dt`, `the year of dt`

**Relative:** `at midnight dt`, `at noon dt`, ``dt `at` time``

**Comparators:** `the earlier of dt1 dt2`, `the later of dt1 dt2`, `dt1 >= dt2`, `dt1 <= dt2`

See [datetime.md](datetime.md) for full documentation.

### currency

ISO 4217 currency handling. Uses integer minor units (cents) to avoid floating-point issues. Key functions: `Money minorUnits code`, `major to minor units`, `add money`, `multiply money`. Supports USD, EUR, GBP, JPY, SGD, and others.

**Status:** Prototype. API may change. See [currency.md](currency.md).

### legal-persons

Legal entity types, identity documents, and capacity checks. Key functions: `age in years`, `is adult birthDate jurisdictionCode`, `can enter contract`, `is beneficial owner`. Includes corporate entity types and jurisdiction-aware majority age.

**Status:** Prototype. API may change. See [legal-persons.md](legal-persons.md).

---

## See Also

- **[GLOSSARY](../GLOSSARY.md)** - Language feature index
- **[Types](../types/README.md)** - Type system documentation
- **[IMPORT keyword](IMPORT.md)** - IMPORT keyword details
- **[Specs](https://github.com/smucclaw/l4-ide/tree/main/specs)** - Technical specifications
