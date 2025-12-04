# Module 1 — Enums & Records Using DECLARE

## Overview

In this module, we'll learn how to define custom types in L4 using `DECLARE`. We'll work in the **WorkPass Authority (WPA)** domain—a fictional agency that approves work permits for foreign employees.

## Why Custom Types Matter

Types serve as documentation and compile-time validation. When you declare:
```l4
DECLARE EmploymentStatus IS ONE OF ...
```

You're saying: "These are the *only* valid employment statuses." The compiler will reject any code that tries to use an invalid status.

This catches bugs at compile-time that would otherwise become runtime errors or, worse, legal disputes.

## Enums: Fixed Sets of Values

### Basic Enum Syntax

```l4
DECLARE EnumName IS ONE OF
    Value1
    Value2
    Value3
```

### WPA Example: Employment Categories

```l4
DECLARE EmploymentCategory IS ONE OF
    TechProfessional
    HealthcareWorker
    Researcher
    FinancialServices
    EntertainmentArts
    Other
```

This defines a closed set. A position *must* be one of these categories—no other values allowed.

### Enums With Data (Algebraic Data Types)

Sometimes enum values need to carry additional information:

```l4
DECLARE ApplicationOutcome IS ONE OF
    Approved HAS
        permitNumber IS A STRING
        expiryDate   IS A DATE
    Rejected HAS
        reason       IS A STRING
    Pending HAS
        daysRemaining IS A NUMBER
```

Now each outcome carries relevant data:
- `Approved` includes a permit number and expiry date
- `Rejected` includes a rejection reason
- `Pending` includes days until decision deadline

## Records: Types With Named Fields

### Basic Record Syntax

```l4
DECLARE RecordName HAS
    field1 IS A Type1
    field2 IS A Type2
```

All fields are required—there are no optional fields at the type level. (For optional data, use `MAYBE`, which we'll cover in Module 4.)

### WPA Example: Employee Record

```l4
DECLARE Employee HAS
    name              IS A STRING
    passportNumber    IS A STRING
    nationality       IS A STRING
    dateOfBirth       IS A DATE
    educationLevel    IS A STRING
    yearsExperience   IS A NUMBER
    monthlySalary     IS A NUMBER
    category          IS AN EmploymentCategory
```

### WPA Example: Company Record

```l4
DECLARE Company HAS
    registrationNumber IS A STRING
    businessName       IS A STRING
    industry           IS A STRING
    foundedYear        IS A NUMBER
    localEmployeeCount IS A NUMBER
    paidUpCapital      IS A NUMBER
```

### Nested Records

Records can contain other records:

```l4
DECLARE WorkPassApplication HAS
    applicationId     IS A STRING
    employee          IS AN Employee
    employer          IS A Company
    proposedStartDate IS A DATE
    contractDuration  IS A NUMBER
```

## Constructing Values

### Enum Values

Simple enums are constructed by name:
```l4
TechProfessional
HealthcareWorker
```

Enums with data use constructor syntax:
```l4
Approved WITH
    permitNumber IS "WP-2025-001234"
    expiryDate   IS DATE OF 31, 12, 2027
```

### Record Values

Records use `WITH` syntax:
```l4
`John Doe` MEANS Employee WITH
    name              IS "John Doe"
    passportNumber    IS "P1234567"
    nationality       IS "Canada"
    dateOfBirth       IS DATE OF 15, 3, 1990
    educationLevel    IS "Master's Degree"
    yearsExperience   IS 8
    monthlySalary     IS 8500
    category          IS TechProfessional
```

Order doesn't matter—field names identify the values:
```l4
Employee WITH
    category IS TechProfessional
    name     IS "John Doe"
    -- ... other fields ...
```

## Accessing Fields

Use the possessive `'s` operator:

```l4
employee's name
employee's monthlySalary
application's employee's nationality
```

The possessive chains naturally:
```l4
GIVEN app IS A WorkPassApplication
GIVETH A STRING
`applicant nationality` app MEANS
    app's employee's nationality
```

## Lists of Custom Types

You can create lists of your types:

```l4
DECLARE Country HAS
    name IS A STRING
    code IS A STRING

`eligible countries` MEANS LIST
    Country WITH name IS "Singapore",      code IS "SG",
    Country WITH name IS "United States",  code IS "US",
    Country WITH name IS "United Kingdom", code IS "UK",
    Country WITH name IS "Canada",         code IS "CA"
```

## Type Aliases vs New Types

### Type Alias (NOT in L4)
Some languages let you alias existing types:
```
type PermitNumber = String  -- NOT L4 syntax
```

L4 doesn't support type aliases. Instead, use wrapper records when you need type safety:

```l4
DECLARE PermitNumber HAS value IS A STRING
DECLARE PassportNumber HAS value IS A STRING
```

Now you can't accidentally pass a `PermitNumber` where a `PassportNumber` is expected.

## Default Values and Partiality

**Important:** L4 has no concept of default values or optional fields in records. Every field must be provided when constructing a record.

For optional data, use `MAYBE` (covered in Module 4):
```l4
DECLARE ApplicationDocument HAS
    title        IS A STRING
    uploadDate   IS A DATE
    expiryDate   IS A MAYBE DATE  -- might not have an expiry
```

## Complete WPA Domain Model

Here's a comprehensive type system for the WorkPass Authority:

```l4
§ `WorkPass Authority Domain Model`

§§ `Enumerations`

DECLARE EmploymentCategory IS ONE OF
    TechProfessional
    HealthcareWorker
    Researcher
    FinancialServices
    EntertainmentArts
    Other

DECLARE EducationLevel IS ONE OF
    HighSchool
    Diploma
    Bachelor
    Master
    Doctorate

DECLARE ApplicationStatus IS ONE OF
    Draft
    Submitted
    UnderReview
    Approved
    Rejected
    Withdrawn

§§ `Core Records`

DECLARE Employee HAS
    name              IS A STRING
    passportNumber    IS A STRING
    nationality       IS A STRING
    dateOfBirth       IS A DATE
    educationLevel    IS AN EducationLevel
    yearsExperience   IS A NUMBER
    monthlySalary     IS A NUMBER
    category          IS AN EmploymentCategory

DECLARE Company HAS
    registrationNumber IS A STRING
    businessName       IS A STRING
    industry           IS A STRING
    foundedYear        IS A NUMBER
    localEmployeeCount IS A NUMBER
    foreignWorkerQuota IS A NUMBER
    paidUpCapital      IS A NUMBER

DECLARE WorkPassApplication HAS
    applicationId      IS A STRING
    employee           IS AN Employee
    employer           IS A Company
    proposedStartDate  IS A DATE
    contractDuration   IS A NUMBER
    status             IS AN ApplicationStatus
    submissionDate     IS A DATE

§§ `Result Types`

DECLARE EligibilityResult HAS
    eligible           IS A BOOLEAN
    reasons            IS A LIST OF STRING

DECLARE QuotaCheckResult HAS
    withinQuota        IS A BOOLEAN
    currentCount       IS A NUMBER
    maxAllowed         IS A NUMBER
    availableSlots     IS A NUMBER
```

## Exercises

### Exercise 1: Define Your Domain
Create enums and records for a library management system:
- Book (with title, ISBN, author, yearPublished)
- Member (with memberId, name, membershipType)
- BookStatus (Available, CheckedOut, Reserved, Lost)

### Exercise 2: Nested Records
Define a `Loan` record that includes a `Book` and a `Member`, plus:
- checkoutDate
- dueDate
- status

### Exercise 3: Construct Values
Create sample data:
- At least 3 books
- At least 2 members
- At least 1 loan

## Key Takeaways

1. **Enums** define closed sets of values (use `IS ONE OF`)
2. **Records** define types with named fields (use `HAS`)
3. **Enums can carry data** (algebraic data types)
4. **Field order doesn't matter** in record construction
5. **Use possessive `'s`** to access fields
6. **All fields are required** (no defaults; use MAYBE for optional data)
7. **Types catch errors at compile-time**, before they become legal disputes

## Next Steps

In **Module 2**, we'll write functions that operate on these types, building reusable business logic for the WorkPass Authority.
