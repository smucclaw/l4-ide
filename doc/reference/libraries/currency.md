# Currency library

Currency handling with ISO 4217 currency codes. Stores amounts as integer minor units (cents) to avoid floating-point errors.
Can be imported into L4 files with `IMPORT currency`.

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

\n\n[currency-example.l4](currency-example.l4)
