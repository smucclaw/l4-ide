# L4 Standard Libraries

This directory contains the standard libraries for L4, providing common functionality for dates, currency, legal entities, and more.

## Available Libraries

- **prelude.l4** - Core functions and types (lists, maybe, either, basic operations)
- **daydate.l4** - Date arithmetic and calendar functions
- **date-compat.l4** - Backwards compatibility layer for legacy DATE syntax
- **excel-date.l4** - Excel-compatible date functions
- **temporal-prelude.l4** - Temporal logic and time-based reasoning
- **currency.l4** - Currency types and operations
- **legal-persons.l4** - Legal entity types (persons, corporations, etc.)
- **jurisdiction.l4** - Jurisdiction and legal domain modeling
- **holdings.l4** - Financial holdings and ownership structures
- **math.l4** - Mathematical functions (trigonometry, logarithms, etc.)

## Important: Understanding DATE vs Date

**TL;DR:** `DATE` is the type, `Date` is the constructor function. They're different things with confusingly similar names.

### ⚠️ Upcoming Breaking Change

**The DATE/Date naming is scheduled for a breaking change to improve clarity.** We're making this change now while adoption is still limited.

The current naming conflates type constructors and value constructors, which is confusing. The proposed change:

**Current (confusing):**
- `DATE` - the type (builtin)
- `Date` - value constructor (recommended, in daydate.l4)
- `DATE` - value constructor (legacy compat, in date-compat.l4)

**Proposed (clearer):**
- `DATE` - the type (unchanged)
- `makeDate` or `newDate` - primary value constructor
- `DATE` - legacy constructor (date-compat.l4 only, for migration)

This will eliminate the ambiguity where `DATE` serves dual roles and `Date` differs from `DATE` only by capitalization. If you're writing new code, be prepared to update constructor calls when this change lands.

### The Type System Perspective

L4 distinguishes between **type constructors** and **value constructors** (data constructors):

#### Type Constructor
- **`DATE`** - The builtin type (defined at the Haskell level in `Environment.hs`)
- Used in type signatures: `GIVEN birthDate IS A DATE`
- This is what the type checker uses to verify type correctness

#### Value Constructors (Data Constructors)
- **`Date` function** (in `daydate.l4`) - **Recommended** way to construct DATE values
  - `Date 15 3 1990` - from day, month, year
  - `Date 738000` - from serial number
  - `Date someDate` - identity/normalization

- **`DATE` function** (in `date-compat.l4`) - Legacy compatibility constructor
  - `DATE 28 6 1971` - backwards compatible with old record syntax
  - `DATE 738000` - from serial number

- **Builtin primitives** (not typically called directly):
  - `DATE_FROM_DMY` - constructs from day/month/year
  - `DATE_FROM_SERIAL` - constructs from serial number

### Historical Context: The Migration

**Before December 2025:**
- `DATE` was a userland record type defined in `daydate.l4`:
  ```l4
  DATE IS A RECORD WITH
      day   IS A NUMBER
      month IS A NUMBER
      year  IS A NUMBER
  ```
- Syntax: `DATE OF 15, 3, 1990` or `DATE WITH day IS 15 month IS 3 year IS 1990`
- Field access: `someDate's day`

**After December 2025:**
- `DATE` is now a **builtin type** (no longer a record)
- Performance improvement: opaque value at runtime, no field access overhead
- The empty `§§ 'Type'` section at `daydate.l4:42` is a vestige of the old definition

**For Migration:**
- Old syntax `DATE OF d, m, y` → New syntax `Date d m y`
- Old field access `someDate's day` → New syntax `DATE_DAY someDate`
- Import `date-compat` library for gradual migration

### Naming Confusion

The naming is potentially confusing because:

1. **Same-name pattern**: In languages like Haskell, it's common to use the same name:
   ```haskell
   data Person = Person String Int  -- first is type, second is constructor
   ```

2. **Historical L4**: The old record syntax conflated type and constructor:
   ```l4
   DATE IS A RECORD WITH ...  -- DATE defined the type
   DATE OF 15, 3, 1990        -- DATE used as constructor
   ```

3. **Post-migration L4**: We have:
   - `DATE` - the type (builtin, uppercase)
   - `Date` - recommended value constructor (function, titlecase)
   - `DATE` - legacy value constructor (function in date-compat.l4, uppercase)

### Recommendations

**For new code:**
```l4
IMPORT daydate

GIVEN birthDate IS A DATE  -- DATE is the type
DECIDE myBirthday IS Date 15 3 1990  -- Date is the constructor

#EVAL DATE_DAY myBirthday   -- Use DATE_DAY, DATE_MONTH, DATE_YEAR for field access
```

**For legacy code migration:**
```l4
IMPORT daydate
IMPORT date-compat  -- Provides backwards-compatible DATE constructor

-- Old style still works:
DECIDE legacyDate IS DATE 28 6 1971

-- New style preferred:
DECIDE modernDate IS Date 28 6 1971
```

## Documentation

Detailed documentation for each library is available in `doc/libraries/`:
- [daydate.md](../../doc/libraries/daydate.md) - Comprehensive date/time documentation
- [excel-date.md](../../doc/libraries/excel-date.md) - Excel compatibility guide
- [coercions.md](../../doc/libraries/coercions.md) - Type coercion functions
- [math.md](../../doc/libraries/math.md) - Mathematical functions reference

## Testing

Library tests are in the `tests/` subdirectory. Each library has corresponding golden test files:
- `tests/daydate.l4` - Date arithmetic tests
- `tests/excel-date.l4` - Excel compatibility tests
- etc.

Run tests with: `cabal test jl4:test:jl4-test`
