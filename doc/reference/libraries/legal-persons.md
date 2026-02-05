# Legal-persons library

Legal entity type definitions for natural persons and corporate entities. Import it into L4 files with `IMPORT legal-persons`.

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

[legal-persons-example.l4](legal-persons-example.l4)
