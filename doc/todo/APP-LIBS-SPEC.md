# Application Libraries Specification

**Version:** 1.0.0
**Date:** 2024-12-16
**Status:** Initial Implementation

## Overview

This specification describes three foundational L4 libraries for building legal and commercial applications:

1. **Jurisdiction Library** - Representing countries, regions, states, and municipalities
2. **Currency Library** - Proper monetary amount handling with ISO currency codes
3. **Legal Persons Library** - Natural persons and corporate entities

These libraries provide standard, versioned ontologies that can be imported and used across L4 applications, similar to how programming languages provide standard libraries.

## Design Principles

### Versioning

Each library includes version information to allow evolution of the ontology over time. As new countries emerge, currencies change, or legal requirements evolve, the libraries can be updated with new versions while maintaining backward compatibility.

### Standards-Based

All libraries lean heavily on existing international standards:

- **ISO 3166-1** (alpha-2, alpha-3) for country codes
- **ISO 3166-2** for subdivision codes (states, provinces)
- **ISO 4217** for currency codes and decimal places
- Common identity document formats (SSN, NINO, EIN, etc.)

### Extensibility

While the libraries include common jurisdictions and currencies, they are designed to be extended with new entries as needed. The validation functions allow custom codes to be added following the same standards.

### Type Safety

Following L4's functional programming roots, all functions return `EITHER STRING result` types for operations that might fail, enabling proper error handling in contracts and rules.

## Library Details

### 1. Jurisdiction Library

**File:** `jl4-core/libraries/jurisdiction.l4`

**Purpose:** Represent geographic and political jurisdictions for use in citizenship, residency, incorporation, and regulatory compliance contexts.

**Key Features:**

- ISO 3166-1 alpha-2 and alpha-3 country codes for 20+ major countries
- Numeric country codes for machine processing
- Supranational regions (EU, ASEAN, USMCA)
- US state codes (ISO 3166-2:US)
- Canadian province codes (ISO 3166-2:CA)
- UK constituent country codes (ISO 3166-2:GB)
- Validation functions for code formats
- Bidirectional conversion between alpha-2 and alpha-3 codes
- Country name lookup from codes

**Included Jurisdictions:**

- Countries: US, UK, Canada, Australia, Germany, France, China, Japan, Singapore, India, Brazil, Mexico, Switzerland, Netherlands, Sweden, New Zealand, South Korea, Italy, Spain, Hong Kong
- US States: California, New York, Texas, Florida, Illinois, Pennsylvania, Massachusetts, Washington, Delaware
- Canadian Provinces: Ontario, Quebec, British Columbia, Alberta
- UK Countries: England, Scotland, Wales, Northern Ireland

**Example Usage:**

```l4
IMPORT jurisdiction

`my country` MEANS `United States alpha-2`  -- "US"
`my state` MEANS `California code`  -- "US-CA"

-- Validate and convert codes
`is valid ISO 3166-1 alpha-2` "US"  -- TRUE
`country name from alpha-2` "US"  -- RIGHT "United States"
`alpha-2 to alpha-3` "US"  -- RIGHT "USA"
```

### 2. Currency Library

**File:** `jl4-core/libraries/currency.l4`

**Purpose:** Represent monetary amounts correctly using integer arithmetic to avoid floating-point rounding errors, following the pattern used in financial systems.

**Key Features:**

- ISO 4217 currency codes (16 major currencies included)
- Integer-based storage in minor units (cents, pence, yen)
- Automatic decimal place handling (0 for JPY/KRW, 2 for most others)
- Proper rounding for multiplication and division
- Amount formatting with correct decimal places
- Conversion between major and minor units
- Arithmetic operations: add, subtract, multiply, divide
- Comparison operations: equal, greater than, less than
- Link to jurisdiction library for currency-country relationships

**Included Currencies:**

- USD (United States Dollar, 2 decimals)
- EUR (Euro, 2 decimals)
- GBP (British Pound, 2 decimals)
- JPY (Japanese Yen, 0 decimals)
- CHF (Swiss Franc, 2 decimals)
- CAD (Canadian Dollar, 2 decimals)
- AUD (Australian Dollar, 2 decimals)
- CNY (Chinese Yuan, 2 decimals)
- HKD (Hong Kong Dollar, 2 decimals)
- SGD (Singapore Dollar, 2 decimals)
- INR (Indian Rupee, 2 decimals)
- BRL (Brazilian Real, 2 decimals)
- MXN (Mexican Peso, 2 decimals)
- SEK (Swedish Krona, 2 decimals)
- NZD (New Zealand Dollar, 2 decimals)
- KRW (South Korean Won, 0 decimals)

**Example Usage:**

```l4
IMPORT currency

-- Create amounts using integer minor units (cents)
`one hundred dollars` MEANS 10000  -- 10000 cents = $100.00
`fifty euro` MEANS 5000  -- 5000 cents = €50.00
`thousand yen` MEANS 1000  -- 1000 yen = ¥1000 (no decimals)

-- Format for display
`Money` 10000 `US Dollar code`  -- RIGHT "100.00 USD"
`Money` 1000 `Japanese Yen code`  -- RIGHT "1000 JPY"

-- Arithmetic
`add money` 10000 5000 "USD"  -- RIGHT 15000 ($150.00)
`multiply money` 10000 1.5 "USD"  -- RIGHT 15000 ($150.00)

-- Comparisons
`money greater than` 10000 5000  -- TRUE
```

**Why Integer Storage?**

Storing monetary amounts as floating-point numbers leads to rounding errors:

```
0.1 + 0.2 = 0.30000000000000004  ❌
10 + 20 = 30  ✅
```

This is why financial systems store amounts as integer cents. The currency library handles the conversion to/from major units (dollars) automatically based on each currency's decimal places.

### 3. Legal Persons Library

**File:** `jl4-core/libraries/legal-persons.l4`

**Purpose:** Represent individuals (natural persons) and organizations (corporate entities) as they appear in legal contexts.

**Key Features:**

#### Natural Persons

- Name formatting (full name, middle name, formal titles)
- Birth date and age calculations
- Age of majority checks by jurisdiction
- Legal capacity determinations (can contract, can vote)
- Multiple citizenship support
- Identity documents (passport, national ID, driver's license, SSN, etc.)
- Document format validation (US SSN, UK NINO, Canadian SIN)
- Address formatting and validation

#### Corporate Entities

- Entity type definitions (Corporation, LLC, LLP, Partnership, etc.)
- Incorporation date and jurisdiction
- Corporate identifier validation (US EIN, UK CRN, Canadian BN)
- Corporate status checks (active, dissolved, established for X years)
- Ownership relationships (subsidiary, parent, affiliate)
- Beneficial ownership thresholds (25%, 50%+)
- Registered agent requirements
- Foreign qualification checks

**Example Usage:**

```l4
IMPORT legal-persons
IMPORT daydate
IMPORT jurisdiction

-- Natural person
`john birth date` MEANS Date 15 6 1985
`john name` MEANS `full name` "John" "Smith"
`john age` MEANS `current age` `john birth date`  -- 39
`john is adult` MEANS `is adult` `john birth date` `United States alpha-2`  -- TRUE
`john can vote` MEANS `can vote` `john birth date` `United States alpha-2`  -- TRUE

-- Multiple citizenship
`john citizenships` MEANS "US" FOLLOWED BY "GB" FOLLOWED BY EMPTY
`john dual citizen` MEANS `has multiple citizenships` `john citizenships`  -- TRUE
`john is US citizen` MEANS `has citizenship in` `john citizenships` "US"  -- TRUE

-- Corporate entity
`acme incorporated` MEANS Date 1 1 2010
`acme jurisdiction` MEANS `Delaware code`
`acme is delaware corp` MEANS `is Delaware corporation` `acme jurisdiction`  -- TRUE
`acme age` MEANS `years since incorporation` `acme incorporated`  -- 14

-- Ownership
`owns 60 percent` MEANS 60
`is majority owner` 60  -- TRUE
`is beneficial owner` 30  -- TRUE (>25%)
```

## Integration with Existing Libraries

These libraries integrate with the existing L4 standard libraries:

- **daydate.l4**: Used for birth dates, incorporation dates, age calculations
- **prelude.l4**: Uses list operations (FOLLOWED BY, EMPTY, map, filter) for collections

## Use Cases

### Natural Person Example: Age-Restricted Contract

```l4
IMPORT legal-persons
IMPORT daydate

GIVEN party IS A DATE  -- birth date
DECIDE `can sign contract` IF
    `is adult` party `United States alpha-2`
```

### Currency Example: Insurance Premium Calculation

```l4
IMPORT currency

`base premium cents` MEANS 50000  -- $500.00
`risk multiplier` MEANS 1.15
`final premium` MEANS `multiply money` `base premium cents` `risk multiplier` "USD"
-- Result: RIGHT 57500 ($575.00)
```

### Jurisdiction Example: Multi-National Compliance

```l4
IMPORT jurisdiction

GIVEN customer country code IS A STRING
DECIDE `requires GDPR compliance` IF
    customer country code EQUALS `European Union code`
    OR customer country code EQUALS `United Kingdom alpha-2`
```

## Testing Strategy

Each library should have comprehensive tests covering:

1. **Jurisdiction Library**
   - Code validation (alpha-2, alpha-3, ISO 3166-2 format)
   - Code conversion (alpha-2 ↔ alpha-3)
   - Country name lookup
   - Error handling for unknown codes

2. **Currency Library**
   - Amount formatting with correct decimal places
   - Arithmetic operations (add, subtract, multiply, divide)
   - Comparison operations
   - Major/minor unit conversions
   - Handling of zero-decimal currencies (JPY, KRW)
   - Integer validation (minor units must be whole numbers)

3. **Legal Persons Library**
   - Age calculations across year boundaries
   - Age of majority by jurisdiction
   - Multiple citizenship operations
   - Identity document format validation
   - Corporate age and status checks
   - Ownership threshold determinations

## Future Enhancements

### Jurisdiction Library

- Full ISO 3166-1 coverage (all 249 countries)
- ISO 3166-2 codes for all subdivisions
- Historical jurisdiction tracking (USSR → Russia, etc.)
- Jurisdiction hierarchy (municipality → state → country)
- Timezone associations

### Currency Library

- Historical exchange rates
- Currency conversion functions
- Special rounding modes (banker's rounding)
- Cryptocurrency support
- Three-decimal currencies (BHD, JOD, KWD)

### Legal Persons Library

- Gender and pronoun handling
- Multiple name formats (Eastern vs. Western order)
- Corporate beneficial ownership chains
- Power of attorney relationships
- Guardianship for minors
- Corporate officer roles (CEO, CFO, Secretary)
- Shareholder records and cap tables

## Implementation Notes

### File Locations

```
jl4-core/libraries/
├── jurisdiction.l4
├── currency.l4
├── legal-persons.l4
└── tests/
    ├── jurisdiction-tests.l4
    ├── currency-tests.l4
    └── legal-persons-tests.l4
```

### Import Dependencies

```
legal-persons.l4
  ├── IMPORT daydate
  └── IMPORT jurisdiction

currency.l4
  └── IMPORT jurisdiction

jurisdiction.l4
  └── (no dependencies)
```

### Versioning Scheme

Each library includes version constants:

```l4
`Library Name Version` MEANS "1.0.0"
`Library Name Date` MEANS "2024-12-16"
```

Version numbers follow semantic versioning:

- **Major**: Breaking changes to function signatures
- **Minor**: New jurisdictions, currencies, or functions (backward compatible)
- **Patch**: Bug fixes and documentation updates

## Related Work

These libraries draw inspiration from:

- **ISO Standards**: 3166 (countries), 4217 (currencies)
- **Financial Systems**: Integer cent storage pattern used in Stripe, banking systems
- **Legal Ontologies**: LegalRuleML, LKIF (Estrella project)
- **Programming Libraries**: Joda-Money (Java), Money (Ruby), dinero.js (JavaScript)

## References

- ISO 3166-1: Country codes - https://www.iso.org/iso-3166-country-codes.html
- ISO 3166-2: Subdivision codes - https://www.iso.org/standard/72482.html
- ISO 4217: Currency codes - https://www.iso.org/iso-4217-currency-codes.html
- Age of majority by country - https://en.wikipedia.org/wiki/Age_of_majority
- Corporate entity types - Various jurisdictional corporation statutes
