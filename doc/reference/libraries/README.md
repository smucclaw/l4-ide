# L4 Core Libraries

L4 ships with a set of core libraries that provide essential functions for common tasks. These libraries are written in L4 itself and serve as both utilities and examples of idiomatic L4 code.

## Overview

Core libraries are located in the [jl4-core/libraries/](https://github.com/smucclaw/l4-ide/tree/main/jl4-core/libraries) directory:

- **[prelude](#prelude)** - Standard functions (automatically imported)
- **[daydate](#daydate)** - Date calculations and temporal logic
- **[excel-date](#excel-date)** - Excel date compatibility
- **[math](#math)** - Mathematical functions
- **[currency](#currency)** - Currency handling (ISO 4217)
- **[legal-persons](#legal-persons)** - Legal entity types and capacity
- **[jurisdiction](#jurisdiction)** - Jurisdiction definitions
- **[holdings](#holdings)** - Holdings and ownership
- **[date-compat](#date-compat)** - Legacy DATE syntax compatibility
- **[llm](#llm)** - LLM API integration

---

## Built-in Functions (Not a Library)

L4 includes several **built-in functions** that are always available without importing any library. These are implemented in the compiler core:

### Type Coercion Builtins

| Function | Signature | Description |
|----------|-----------|-------------|
| `TOSTRING` | `NUMBER/BOOLEAN/DATE → STRING` | Convert to string representation |
| `TONUMBER` | `STRING → MAYBE NUMBER` | Parse string to number |
| `TODATE` | `STRING → MAYBE DATE` | Parse string to date |
| `TRUNC` | `NUMBER, NUMBER → NUMBER` | Truncate decimal places |
| `AS STRING` | `value AS STRING` | Inline string conversion |

### Numeric Builtins

| Function | Description |
|----------|-------------|
| `FLOOR` | Round down to integer |
| `CEILING` | Round up to integer |
| `ROUND` | Round to nearest integer |
| `EXPONENT` | Exponentiation (base, power) |
| `LN` | Natural logarithm |
| `LOG10` | Base-10 logarithm |
| `SQRT` | Square root |
| `SIN`, `COS`, `TAN` | Trigonometric functions (radians) |
| `ASIN`, `ACOS`, `ATAN` | Inverse trigonometric functions |

### String Builtins

| Function | Description |
|----------|-------------|
| `STRINGLENGTH` | Length of string |
| `SUBSTRING` | Extract substring |
| `TOUPPER` | Convert to uppercase |
| `TOLOWER` | Convert to lowercase |

### Date Builtins

| Function | Description |
|----------|-------------|
| `DATE_FROM_DMY` | Construct DATE from day, month, year |
| `DATE_FROM_SERIAL` | Construct DATE from serial number |
| `DATE_SERIAL` | Get serial number from DATE |
| `DATE_DAY` | Extract day from DATE |
| `DATE_MONTH` | Extract month from DATE |
| `DATE_YEAR` | Extract year from DATE |
| `TODAY` | Current date |

For detailed coercion documentation, see [coercions.md](coercions.md).

---

## prelude

The prelude is automatically imported into every L4 program and provides foundational functions for working with lists, Maybe types, Booleans, and more.

### Location
[jl4-core/libraries/prelude.l4](https://github.com/smucclaw/l4-ide/blob/main/jl4-core/libraries/prelude.l4)

### Key Functions

#### List Functions

**Construction and Deconstruction:**
- `null` - Check if list is empty
- `reverse` - Reverse a list
- `replicate` - Create list of n copies
- `range` - Generate numeric range

**Transformation:**
- `map` - Apply function to each element
- `filter` - Keep elements matching predicate
- `take` - First n elements
- `drop` - All but first n elements
- `takeWhile` / `dropWhile` - Conditional take/drop

**Combination:**
- `append` - Concatenate two lists
- `concat` - Flatten list of lists
- `zip` / `zipWith` - Combine two lists
- `partition` - Split by predicate

**Aggregation:**
- `foldr` / `foldl` - Fold (reduce) a list
- `sum` - Sum of numbers
- `product` - Product of numbers
- `maximum` / `minimum` - Largest/smallest element
- `and` / `or` - Logical aggregation
- `all` / `any` - Check if all/any satisfy predicate

**Searching:**
- `elem` - Check if element is in list
- `at` - Get element at index
- `lookup` - Find value by key in association list

**Sorting:**
- `sort` - Sort numbers
- `sortBy` - Sort with custom comparator
- `insertBy` - Insert maintaining order

**Uniqueness:**
- `nub` / `nubBy` - Remove duplicates
- `delete` / `deleteBy` - Remove element

#### Maybe Functions

- `isJust` / `isNothing` - Check Maybe status
- `fromMaybe` - Extract with default
- `maybe` - Fold over Maybe
- `orElse` - Alternative Maybe
- `mapMaybe` - Filter map
- `catMaybes` - Extract all JUST values
- `asum` / `firstJust` - First successful Maybe
- `maybeToList` / `listToMaybe` - Convert between Maybe and List

#### Either Functions

- `either` - Fold over Either

#### Pair Functions

- `pmap` / `mapSnd` - Map over second element
- `fmap` / `mapPairs` - Map over list of pairs

#### Dictionary Functions

**Construction:**
- `emptyDict` - Create empty dictionary
- `singleton` / `singleToDict` - Single key-value entry
- `pairToDict` - From pair
- `listToDict` / `fromList` - From association list
- `fromListGrouped` - Group values by key

**Query:**
- `dictLookup` - Find value by key
- `dictMember` / `dictNotMember` - Check key existence
- `dictFindWithDefault` - Lookup with default
- `dictKeys` - All keys
- `dictElems` - All values
- `dictToList` - Convert to association list
- `dictSize` - Number of entries
- `dictIsEmpty` - Check if empty

**Modification:**
- `dictInsert` - Add or update entry
- `dictInsertWith` - Insert with combining function
- `dictDelete` - Remove entry
- `dictAdjust` - Modify value at key
- `dictUpdate` - Modify or delete

**Combination:**
- `dictUnion` - Merge dictionaries
- `dictUnionWith` - Merge with combining function

**Higher-Order:**
- `mapDict` - Map over values
- `dictMapWithKey` - Map with key access
- `filterDict` - Filter by value predicate
- `dictFilterWithKey` - Filter by key-value predicate
- `foldrDict` / `foldlDict` - Fold over values
- `dictFoldrWithKey` / `dictFoldlWithKey` - Fold with keys

**Grouping:**
- `insertValue` - Insert into grouped pairs
- `groupPairs` - Group flat pairs by key

#### Utility Functions

- `id` - Identity function
- `const` - Constant function
- `even` / `odd` - Number parity
- `max` / `min` - Maximum/minimum of two values
- `TBD` - Polymorphic placeholder

### Example: Using Prelude Functions

[prelude-example.l4](prelude-example.l4).

**See [prelude.l4](https://github.com/smucclaw/l4-ide/blob/main/jl4-core/libraries/prelude.l4) source for all functions.**

---

## daydate

Date arithmetic and temporal logic for legal deadlines and time-based rules. Follows ISO 8601 conventions.

### Location
[jl4-core/libraries/daydate.l4](https://github.com/smucclaw/l4-ide/blob/main/jl4-core/libraries/daydate.l4)

### Features

- Date construction from day/month/year or serial numbers
- Date arithmetic (add/subtract days, weeks, months, years)
- Weekday and weekend detection
- Week-of-year calculations
- Month and year helpers
- Leap year detection

### Key Functions

**Constants:**
- `Monday` through `Sunday` (0-6)
- `January` through `December` (1-12)
- `Days in a week`, `Days in a year`, etc.

**Date Construction:**
- `Date day month year` - Create date from components
- `Date serialNumber` - Create date from serial number
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

[daydate-example.l4](daydate-example.l4).

**See [daydate.l4](https://github.com/smucclaw/l4-ide/blob/main/jl4-core/libraries/daydate.l4) source for all functions.**

---

## excel-date

Excel date compatibility for importing and exporting dates in Excel format.

### Location
[jl4-core/libraries/excel-date.l4](https://github.com/smucclaw/l4-ide/blob/main/jl4-core/libraries/excel-date.l4)

### Features

- Excel serial date conversion
- Date format conversion
- Excel-compatible calculations
- Handles Excel's 1900 leap year bug

**Note:** Excel date tests are computationally intensive due to large library imports.

**See [excel-date.l4](https://github.com/smucclaw/l4-ide/blob/main/jl4-core/libraries/excel-date.l4) source.**

---

## math

Mathematical functions beyond basic arithmetic. Provides safe wrappers around numeric builtins that handle edge cases (division by zero, domain errors) by returning `MAYBE` types.

### Location
[jl4-core/libraries/math.l4](https://github.com/smucclaw/l4-ide/blob/main/jl4-core/libraries/math.l4)

### Constants

- `EULER` - Euler's number (2.718281828459045)
- `TAN_EPSILON` - Threshold for detecting tan asymptotes

### Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `exp` | `NUMBER → NUMBER` | e^x via `EXPONENT EULER x` |
| `ln` | `NUMBER → MAYBE NUMBER` | Natural log (NOTHING if x ≤ 0) |
| `log10` | `NUMBER → MAYBE NUMBER` | Base-10 log (NOTHING if x ≤ 0) |
| `sqrt` | `NUMBER → MAYBE NUMBER` | Square root (NOTHING if x < 0) |
| `absNumber` | `NUMBER → NUMBER` | Absolute value |
| `sin` | `NUMBER → MAYBE NUMBER` | Sine (always JUST) |
| `cos` | `NUMBER → MAYBE NUMBER` | Cosine (always JUST) |
| `tan` | `NUMBER → MAYBE NUMBER` | Tangent (NOTHING near asymptotes) |
| `asin` | `NUMBER → MAYBE NUMBER` | Arc sine (NOTHING if \|x\| > 1) |
| `acos` | `NUMBER → MAYBE NUMBER` | Arc cosine (NOTHING if \|x\| > 1) |
| `atan` | `NUMBER → MAYBE NUMBER` | Arc tangent (always JUST) |

**Note:** All trigonometric functions expect/return angles in **radians**.

### Example: Math Functions

[math-example.l4](math-example.l4).

**See [math.l4](https://github.com/smucclaw/l4-ide/blob/main/jl4-core/libraries/math.l4) source for all functions.**

---

## currency

Currency handling with ISO 4217 currency codes. Stores amounts as integer minor units (cents) to avoid floating-point errors.

### Location
[jl4-core/libraries/currency.l4](https://github.com/smucclaw/l4-ide/blob/main/jl4-core/libraries/currency.l4)

### Features

- ISO 4217 currency codes and metadata
- Decimal place lookup per currency
- Money construction and formatting
- Arithmetic operations (add, subtract, multiply, divide)
- Comparison operations
- Validation functions

### Supported Currencies

USD, EUR, GBP, JPY, CHF, CAD, AUD, CNY, HKD, SGD, INR, BRL, MXN, SEK, NZD, KRW

### Key Functions

- `Money minorUnits currencyCode` - Create money value
- `major to minor units`, `minor to major units` - Conversion
- `add money`, `subtract money`, `multiply money`, `divide money`
- `is valid currency code`, `is non-negative`, `is positive`

### Example: Currency Operations

[currency-example.l4](currency-example.l4).

---

## legal-persons

Legal entity type definitions for natural persons and corporate entities.

### Location
[jl4-core/libraries/legal-persons.l4](https://github.com/smucclaw/l4-ide/blob/main/jl4-core/libraries/legal-persons.l4)

### Features

**Natural Persons:**
- Name formatting functions
- Address validation (US ZIP, UK postcode, Canadian postal code)
- Identity document validation (SSN, NINO, SIN, etc.)
- Age calculations and legal capacity checks
- Citizenship and residency functions

**Corporate Entities:**
- Entity type constants (Corporation, LLC, LLP, Partnership, etc.)
- Identifier validation (EIN, CRN, BN)
- Corporate status checks
- Beneficial ownership calculations
- Jurisdiction functions

### Key Functions

- `age in years birthDate referenceDate`
- `is adult birthDate jurisdictionCode`
- `can enter contract birthDate jurisdictionCode`
- `is beneficial owner ownershipPercentage`
- `is majority owner ownershipPercentage`

### Example: Legal Persons

[legal-persons-example.l4](legal-persons-example.l4).

---

## jurisdiction

Jurisdiction definitions for multi-jurisdictional legal rules. Uses ISO 3166 country codes.

### Location
[jl4-core/libraries/jurisdiction.l4](https://github.com/smucclaw/l4-ide/blob/main/jl4-core/libraries/jurisdiction.l4)

### Features

- ISO 3166-1 alpha-2 country codes
- US state codes
- UK constituent country codes
- Canadian province codes
- Regional groupings (EU, etc.)

---

## holdings

Ownership and holdings definitions for financial structures.

### Location
[jl4-core/libraries/holdings.l4](https://github.com/smucclaw/l4-ide/blob/main/jl4-core/libraries/holdings.l4)

---

## date-compat

Backwards compatibility layer for legacy DATE syntax.

### Location
[jl4-core/libraries/date-compat.l4](https://github.com/smucclaw/l4-ide/blob/main/jl4-core/libraries/date-compat.l4)

### Purpose

Provides the uppercase `DATE` constructor for code that used the old record-based date syntax. New code should use `Date` from `daydate` instead.

**Legacy syntax:** `DATE 15 3 1990`  
**Modern syntax:** `Date 15 3 1990`

---

## llm

LLM API integration for calling language models from L4 code.

### Location
[jl4-core/libraries/llm.l4](https://github.com/smucclaw/l4-ide/blob/main/jl4-core/libraries/llm.l4)

### Supported Providers

- **Anthropic Claude** (direct API)
- **OpenAI** (direct API)
- **OpenRouter** (unified gateway for 500+ models)

### Key Functions

- `callAnthropic apiKey model prompt maxTokens`
- `callOpenAI apiKey model prompt maxTokens`
- `callOpenRouter apiKey model prompt maxTokens`
- `queryLLMWithDefaults prompt` - Auto-detects available API keys
- `callClaude apiKey prompt` - Convenience for Claude Sonnet
- `callGPT4 apiKey prompt` - Convenience for GPT-4

### Model Constants

- `claudeOpusModel`, `claudeSonnetModel`
- `gpt4Model`, `gpt4TurboModel`, `gpt35Model`
- `llama3Model`, `geminiProModel`

### Configuration

Set environment variables: `ANTHROPIC_API_KEY`, `OPENAI_API_KEY`, or `OPENROUTER_API_KEY`

---

## Using Libraries

### Automatic Import

The prelude is automatically imported - no IMPORT needed. Functions like `map`, `filter`, etc. are available immediately.

### Manual Import

Other libraries require explicit import:

```l4
IMPORT daydate
IMPORT math
IMPORT currency
```

### Import Paths

Import by library name or file path:

```l4
IMPORT daydate                    -- Standard library
IMPORT "libraries/daydate.l4"    -- Explicit path
IMPORT "my-custom-lib.l4"        -- Custom library
```

---

## Library Reference

| Library | Auto-Import | Purpose | Typical Use |
|---------|-------------|---------|-------------|
| **prelude** | ✅ Yes | Standard functions | Every program |
| **daydate** | ❌ No | Date/time logic | Deadlines, temporal rules |
| **excel-date** | ❌ No | Excel compatibility | Data import/export |
| **math** | ❌ No | Safe math wrappers | Scientific calculations |
| **currency** | ❌ No | ISO 4217 currencies | Financial calculations |
| **legal-persons** | ❌ No | Entity types | Legal entity modeling |
| **jurisdiction** | ❌ No | Jurisdictions | Multi-jurisdiction rules |
| **holdings** | ❌ No | Ownership | Holdings tracking |
| **date-compat** | ❌ No | Legacy syntax | Migration support |
| **llm** | ❌ No | LLM APIs | AI-assisted queries |

---

## Performance Notes

- **Prelude** - Efficient, well-tested
- **Excel-date** - Slow due to large imports (use sparingly)
- **Custom libraries** - Import cost depends on size

For performance-critical code:
- Minimize imports
- Use built-in operations where possible
- Profile with `#EVALTRACE`

---

## See Also

- **[GLOSSARY](../GLOSSARY.md)** - Language feature index
- **[Types](../types/README.md)** - Type system documentation
- **[Keywords](../keywords/README.md)** - Language keywords
- **[Specs](https://github.com/smucclaw/l4-ide/tree/main/specs)** - Technical specifications

---

## Contributing

To add or improve library documentation:

1. Check implementation in [jl4-core/libraries/](https://github.com/smucclaw/l4-ide/tree/main/jl4-core/libraries)
2. Add working examples
3. Document all functions with type signatures
4. Test thoroughly
5. Submit pull request

See [contributing guidelines](https://github.com/smucclaw/l4-ide/blob/main/CONTRIBUTING.md).
