# Module 2 — Functions Using GIVEN / GIVETH / MEANS

## Overview

If you've written functions in JavaScript or Python, you already understand the basics. L4 functions are similar, but with two key differences:

1. **Type signatures are mandatory** (like TypeScript, not JavaScript)
2. **Functions return expressions, not statements** (no `return` keyword)

L4's **mixfix notation** lets you write function names that read like natural language, making your code isomorphic to legal text.

## Basic Function Syntax

### The Standard Form

```l4
GIVEN parameter1 IS A Type1
      parameter2 IS A Type2
GIVETH A ReturnType
functionName parameter1 parameter2 MEANS expression
```

Think of it as:

- **GIVEN**: Function parameters and their types (like function signature)
- **GIVETH**: Return type
- **MEANS**: Function body (the expression to evaluate)

### Your First Function

```l4
GIVEN salary IS A NUMBER
GIVETH A NUMBER
`annual income` salary MEANS
    salary TIMES 12
```

In JavaScript, this would be:

```javascript
function annual_income(salary) {
  return salary * 12;
}
```

## Mixfix Notation: Functions That Read Like English

Unlike most programming languages where functions look like `calculate(x, y)`, L4 lets you **weave parameters into the function name**:

```l4
GIVEN employee IS AN Employee
      employer IS A Company
GIVETH A BOOLEAN
`employee` `works for` `employer` MEANS
    -- implementation
```

This reads naturally: "employee works for employer"

### WPA Examples Using Mixfix

```l4
-- Natural: "employee meets age requirement"
GIVEN employee IS AN Employee
GIVETH A BOOLEAN
`employee` `meets age requirement` MEANS
    `age of` employee AT LEAST 18
```

```l4
-- Natural: "salary exceeds minimum for category"
GIVEN salary IS A NUMBER
      category IS AN EmploymentCategory
GIVETH A BOOLEAN
`salary` `exceeds minimum for` `category` MEANS
    CONSIDER category
    WHEN TechProfessional    THEN salary AT LEAST 5000
    WHEN HealthcareWorker    THEN salary AT LEAST 4500
    WHEN Researcher          THEN salary AT LEAST 5500
    WHEN FinancialServices   THEN salary AT LEAST 6000
    WHEN EntertainmentArts   THEN salary AT LEAST 3500
    WHEN Other               THEN salary AT LEAST 3000
```

The mixfix style creates **controlled natural language (CNL)**—code that could pass as a legal document's business rules section.

## Decision Functions

Functions that return `BOOLEAN` often use the `DECIDE ... IF` syntax:

```l4
GIVEN employee IS AN Employee
GIVETH A BOOLEAN
DECIDE `meets education requirement` IF
        employee's educationLevel EQUALS Bachelor
    OR  employee's educationLevel EQUALS Master
    OR  employee's educationLevel EQUALS Doctorate
```

This is shorthand for:

```l4
`meets education requirement` employee MEANS
    (employee's educationLevel EQUALS Bachelor) OR ...
```

Use `DECIDE ... IF` when your function body is a single Boolean expression.

## Multi-Parameter Functions

### Standard Parameter List

```l4
GIVEN employee IS AN Employee
      company IS A Company
      startDate IS A DATE
GIVETH A BOOLEAN
`employee` `can start with` `company` `on` `startDate` MEANS
    -- Check various conditions
        `meets work eligibility` employee
    AND `has capacity for workers` company
    AND startDate AT LEAST `today`
```

### Date Arithmetic Example

```l4
IMPORT daydate

GIVEN birthDate IS A DATE
      asOfDate IS A DATE
GIVETH A NUMBER
`age of person born on` `birthDate` `as of` `asOfDate` MEANS
    FLOOR ((asOfDate MINUS birthDate) DIVIDED BY 365.2425)
```

## Helper Functions with WHERE

The `WHERE` clause lets you define local helpers and intermediate values:

```l4
GIVEN employee IS AN Employee
      employer IS A Company
GIVETH A BOOLEAN
`passes salary check` employee employer MEANS
    salary AT LEAST minimumForCategory
    WHERE
        salary MEANS employee's monthlySalary
        category MEANS employee's category
        minimumForCategory MEANS `minimum salary for` category
```

Think of `WHERE` like `let` bindings in JavaScript:

```javascript
function passes_salary_check(employee, employer) {
  const salary = employee.monthlySalary;
  const category = employee.category;
  const minimumForCategory = minimum_salary_for(category);
  return salary >= minimumForCategory;
}
```

### Complex WHERE Example

```l4
GIVEN application IS A WorkPassApplication
GIVETH A NUMBER
`application score` application MEANS
    educationScore PLUS experienceScore PLUS salaryScore
    WHERE
        emp MEANS application's employee

        educationScore MEANS
            CONSIDER emp's educationLevel
            WHEN Doctorate THEN 30
            WHEN Master    THEN 25
            WHEN Bachelor  THEN 20
            WHEN Diploma   THEN 10
            WHEN HighSchool THEN 5

        experienceScore MEANS
            IF emp's yearsExperience AT LEAST 10
            THEN 30
            ELSE emp's yearsExperience TIMES 3

        salaryScore MEANS
            IF emp's monthlySalary AT LEAST 10000
            THEN 40
            ELSE IF emp's monthlySalary AT LEAST 7000
            THEN 30
            ELSE IF emp's monthlySalary AT LEAST 5000
            THEN 20
            ELSE 10
```

## Calling Functions

### Direct Calls

```l4
`age of person born on` birthDate `as of` `today`
```

### With OF for Single Arguments

When a function takes one argument, you can use `OF`:

```l4
`minimum salary for` OF TechProfessional
```

Instead of:

```l4
`minimum salary for` TechProfessional
```

Both work. `OF` is optional but can improve readability.

## Type Annotations and Inference

While L4 requires type signatures for top-level functions, it **infers types for local helpers** in WHERE clauses:

```l4
WHERE
    doubled x MEANS x TIMES 2  -- x inferred as NUMBER
    salary MEANS emp's monthlySalary  -- inferred as NUMBER
```

## Boolean Logic Operators

L4 provides natural language operators:

| L4 Syntax         | Meaning     | JavaScript  |
| ----------------- | ----------- | ----------- |
| `AND`             | Conjunction | `&&`        |
| `OR`              | Disjunction | `\|\|`      |
| `NOT`             | Negation    | `!`         |
| `IMPLIES` or `=>` | Implication | `!a \|\| b` |

```l4
DECIDE eligible IF
        `meets age requirement` employee
    AND `meets education requirement` employee
    AND NOT `has disqualifying record` employee
```

## Comparison Operators

| L4 Syntax      | Meaning  | JavaScript |
| -------------- | -------- | ---------- |
| `EQUALS`       | Equality | `===`      |
| `GREATER THAN` | >        | `>`        |
| `LESS THAN`    | <        | `<`        |
| `AT LEAST`     | >=       | `>=`       |
| `AT MOST`      | <=       | `<=`       |

Natural language versions enhance readability:

```l4
employee's age AT LEAST 18
salary AT MOST 50000
```

## Numeric Operations

| L4 Syntax    | JavaScript |
| ------------ | ---------- |
| `PLUS`       | `+`        |
| `MINUS`      | `-`        |
| `TIMES`      | `*`        |
| `DIVIDED BY` | `/`        |
| `MODULO`     | `%`        |

```l4
netSalary MEANS grossSalary MINUS taxes
annualIncome MEANS monthlySalary TIMES 12
taxRate MEANS taxAmount DIVIDED BY income
```

You can also use symbolic operators:

```l4
netSalary MEANS grossSalary - taxes
annualIncome MEANS monthlySalary * 12
```

But the word forms often read better in legal contexts.

## Advanced Numeric Operations

L4 also provides advanced mathematical functions:

| L4 Syntax         | Meaning        | Example                               |
| ----------------- | -------------- | ------------------------------------- |
| `SQRT`            | Square root    | `SQRT 16` → `4`                       |
| `EXPONENT` or `^` | Exponentiation | `2 EXPONENT 3` → `8` or `2 ^ 3` → `8` |

```l4
-- Calculate compound interest
GIVEN principal IS A NUMBER
      rate IS A NUMBER
      years IS A NUMBER
GIVETH A NUMBER
`compound interest` principal rate years MEANS
    principal TIMES ((1 PLUS rate) EXPONENT years)

-- Calculate distance from area of circle
GIVEN area IS A NUMBER
GIVETH A NUMBER
`radius from area` area MEANS
    SQRT (area DIVIDED BY 3.14159)
```

## String Operations

L4 provides comprehensive string manipulation functions for working with text:

### Unary String Functions (STRING → STRING or NUMBER)

| Function       | Returns | Description                        | Example                        |
| -------------- | ------- | ---------------------------------- | ------------------------------ |
| `STRINGLENGTH` | NUMBER  | Length of string                   | `STRINGLENGTH "hello"` → `5`   |
| `TOUPPER`      | STRING  | Convert to uppercase               | `TOUPPER "hello"` → `"HELLO"`  |
| `TOLOWER`      | STRING  | Convert to lowercase               | `TOLOWER "HELLO"` → `"hello"`  |
| `TRIM`         | STRING  | Remove leading/trailing whitespace | `TRIM "  hello  "` → `"hello"` |

### Binary String Functions (STRING → STRING → ...)

| Function     | Returns        | Description                             | Example                                     |
| ------------ | -------------- | --------------------------------------- | ------------------------------------------- |
| `CONTAINS`   | BOOLEAN        | Check if contains substring             | `CONTAINS "hello world" "world"` → `TRUE`   |
| `STARTSWITH` | BOOLEAN        | Check if starts with prefix             | `STARTSWITH "hello world" "hello"` → `TRUE` |
| `ENDSWITH`   | BOOLEAN        | Check if ends with suffix               | `ENDSWITH "hello world" "world"` → `TRUE`   |
| `INDEXOF`    | NUMBER         | Find first occurrence (-1 if not found) | `INDEXOF "hello world" "o"` → `4`           |
| `SPLIT`      | LIST OF STRING | Split by delimiter                      | `SPLIT "a,b,c" ","` → `["a", "b", "c"]`     |

### Ternary String Functions

| Function    | Returns | Description                              | Example                                          |
| ----------- | ------- | ---------------------------------------- | ------------------------------------------------ |
| `SUBSTRING` | STRING  | Extract substring(string, start, length) | `SUBSTRING "hello world" 0 5` → `"hello"`        |
| `REPLACE`   | STRING  | Replace all occurrences                  | `REPLACE "hello hello" "hello" "hi"` → `"hi hi"` |

### Character Access

| Function | Returns | Description                                     | Example                    |
| -------- | ------- | ----------------------------------------------- | -------------------------- |
| `CHARAT` | STRING  | Get character at index (empty if out of bounds) | `CHARAT "hello" 0` → `"h"` |

### WPA String Examples

```l4
§§ `Document Name Validation`

GIVEN docName IS A STRING
GIVETH A BOOLEAN
DECIDE `is valid document name` IF
        STRINGLENGTH docName AT LEAST 5
    AND NOT CONTAINS docName "/"
    AND NOT CONTAINS docName "\\"
    AND TRIM docName EQUALS docName  -- No leading/trailing spaces

§§ `Passport Number Formatting`

GIVEN passportNumber IS A STRING
GIVETH A STRING
`formatted passport number` passportNumber MEANS
    TOUPPER (TRIM passportNumber)

§§ `Name Parsing`

GIVEN fullName IS A STRING
GIVETH A MAYBE STRING
`first name from` fullName MEANS
    IF `length of` nameParts GREATER THAN 0
    THEN JUST (at nameParts 0)
    ELSE NOTHING
    WHERE
        nameParts MEANS SPLIT fullName " "

GIVEN fullName IS A STRING
GIVETH A MAYBE STRING
`last name from` fullName MEANS
    IF `length of` nameParts GREATER THAN 0
    THEN JUST (at nameParts (lastIndex))
    ELSE NOTHING
    WHERE
        nameParts MEANS SPLIT fullName " "
        lastIndex MEANS (`length of` nameParts) MINUS 1

§§ `Email Validation`

GIVEN email IS A STRING
GIVETH A BOOLEAN
DECIDE `is valid email format` IF
        CONTAINS email "@"
    AND CONTAINS email "."
    AND NOT STARTSWITH email "@"
    AND NOT ENDSWITH email "@"
    AND atSignIndex GREATER THAN 0
    AND atSignIndex LESS THAN (STRINGLENGTH email MINUS 1)
    WHERE
        atSignIndex MEANS INDEXOF email "@"
```

These string operations are especially useful when:

- Validating user input (passport numbers, names, emails)
- Parsing structured text (CSV files, form data)
- Formatting output for reports and documents
- Cleaning and normalizing data

## WPA Functions: Complete Examples

### Eligibility Checks

```l4
IMPORT prelude
IMPORT daydate

§§ `Age Requirements`

GIVEN employee IS AN Employee
      asOfDate IS A DATE
GIVETH A NUMBER
`age of` `employee` `as of` `asOfDate` MEANS
    FLOOR ((asOfDate MINUS employee's dateOfBirth) DIVIDED BY 365.2425)

GIVEN employee IS AN Employee
GIVETH A BOOLEAN
DECIDE `meets minimum age` IF
    `age of` employee `as of` `today` AT LEAST 18

GIVEN employee IS AN Employee
GIVETH A BOOLEAN
DECIDE `meets maximum age` IF
    `age of` employee `as of` `today` AT MOST 65

§§ `Salary Requirements`

GIVEN category IS AN EmploymentCategory
GIVETH A NUMBER
`minimum monthly salary for` category MEANS
    CONSIDER category
    WHEN TechProfessional  THEN 5000
    WHEN HealthcareWorker  THEN 4500
    WHEN Researcher        THEN 5500
    WHEN FinancialServices THEN 6000
    WHEN EntertainmentArts THEN 3500
    WHEN Other             THEN 3000

GIVEN employee IS AN Employee
GIVETH A BOOLEAN
DECIDE `salary meets requirement` IF
    employee's monthlySalary AT LEAST minimumSalary
    WHERE
        minimumSalary MEANS
            `minimum monthly salary for` employee's category

§§ `Education Requirements`

GIVEN category IS AN EmploymentCategory
GIVETH AN EducationLevel
`minimum education for` category MEANS
    CONSIDER category
    WHEN TechProfessional  THEN Bachelor
    WHEN HealthcareWorker  THEN Diploma
    WHEN Researcher        THEN Master
    WHEN FinancialServices THEN Bachelor
    WHEN EntertainmentArts THEN Diploma
    WHEN Other             THEN HighSchool

GIVEN level IS AN EducationLevel
GIVETH A NUMBER
`education level rank` level MEANS
    CONSIDER level
    WHEN Doctorate  THEN 5
    WHEN Master     THEN 4
    WHEN Bachelor   THEN 3
    WHEN Diploma    THEN 2
    WHEN HighSchool THEN 1

GIVEN employee IS AN Employee
GIVETH A BOOLEAN
DECIDE `education meets requirement` IF
    employeeRank AT LEAST requiredRank
    WHERE
        employeeRank MEANS
            `education level rank` employee's educationLevel
        requiredRank MEANS
            `education level rank` requiredLevel
        requiredLevel MEANS
            `minimum education for` employee's category

§§ `Company Quota Checks`

GIVEN company IS A Company
GIVETH A NUMBER
`foreign worker quota for` company MEANS
    company's foreignWorkerQuota

GIVEN company IS A Company
      currentForeignWorkers IS A NUMBER
GIVETH A BOOLEAN
DECIDE `company` `within foreign worker quota` `with` `currentForeignWorkers` IF
    currentForeignWorkers LESS THAN company's foreignWorkerQuota

§§ `Overall Eligibility`

GIVEN employee IS AN Employee
      employer IS A Company
GIVETH A BOOLEAN
DECIDE `employee` `eligible for work pass with` `employer` IF
        `meets minimum age` employee
    AND `meets maximum age` employee
    AND `salary meets requirement` employee
    AND `education meets requirement` employee
```

## Key Takeaways

1. **GIVEN/GIVETH/MEANS** is L4's function syntax (like function signature + body)
2. **Mixfix notation** lets you write functions that read like English
3. **DECIDE ... IF** is shorthand for Boolean-returning functions
4. **WHERE clauses** define local helpers (like `let` or `const` in JavaScript)
5. **Natural language operators** (AND, OR, AT LEAST) enhance readability
6. **No `return` keyword**—functions are expressions that evaluate to a value

## Exercises

### Exercise 1: Basic Function

Write a function that calculates annual income from monthly salary with a bonus percentage.

### Exercise 2: Mixfix Function

Write a function: `employee` `qualifies for` `category` that checks if an employee meets the requirements for a given employment category.

### Exercise 3: Complex WHERE

Write a function that calculates a "company suitability score" based on:

- Years in business (older = better)
- Local vs foreign worker ratio (higher local = better)
- Paid-up capital (higher = better)

## Next Steps

In **Module 3**, we'll learn pattern matching with `CONSIDER/WHEN`, enabling sophisticated control flow based on data structure shape.
