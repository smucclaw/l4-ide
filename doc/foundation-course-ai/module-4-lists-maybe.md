# Module 4 — Lists, MAYBE, and Higher-Order Helpers

## Overview

Coming from JavaScript or Python, you're familiar with arrays and null/undefined. L4 provides:

- **Lists**: Like arrays, but homogeneous (all elements same type)
- **MAYBE**: Like Optional in Java—explicitly represents "value or no value"
- **Higher-order functions**: Like `map`, `filter`, `reduce` in JavaScript

These tools let you handle collections and missing data safely, without null pointer exceptions.

## Lists: Homogeneous Collections

### Creating Lists

```l4
-- Empty list
EMPTY

-- List literal
LIST 1, 2, 3, 4, 5

-- Cons notation (building up)
1 FOLLOWED BY 2 FOLLOWED BY 3 FOLLOWED BY EMPTY

-- List of records
LIST
    Employee WITH name IS "Alice", age IS 25, ...,
    Employee WITH name IS "Bob", age IS 30, ...
```

### List Types

Every element must be the same type:

```l4
LIST OF NUMBER        -- [1, 2, 3]
LIST OF STRING        -- ["a", "b", "c"]
LIST OF Employee      -- [employee1, employee2]
LIST OF LIST OF NUMBER -- [[1,2], [3,4], [5,6]]
```

This prevents bugs:

```l4
-- ERROR: Can't mix types
LIST 1, "hello", TRUE  -- Type error!
```

## Pattern Matching on Lists (Revisited)

Two patterns (from Module 3):

```l4
CONSIDER someList
WHEN EMPTY THEN
    -- Handle empty list
WHEN head FOLLOWED BY tail THEN
    -- Handle non-empty: head is first element, tail is rest
```

### Common List Patterns

```l4
-- Process first element
GIVEN list IS A LIST OF Employee
GIVETH A MAYBE STRING
`first name in` list MEANS
    CONSIDER list
    WHEN EMPTY THEN
        NOTHING
    WHEN employee FOLLOWED BY others THEN
        JUST employee's name

-- Process exactly two elements
GIVEN list IS A LIST OF NUMBER
GIVETH A MAYBE NUMBER
`sum of two` list MEANS
    CONSIDER list
    WHEN a FOLLOWED BY b FOLLOWED BY EMPTY THEN
        JUST (a PLUS b)
    OTHERWISE
        NOTHING

-- Skip first element
GIVEN list IS A LIST OF STRING
GIVETH A LIST OF STRING
`drop first` list MEANS
    CONSIDER list
    WHEN EMPTY THEN EMPTY
    WHEN first FOLLOWED BY rest THEN rest
```

## MAYBE: Explicit Optional Values

In JavaScript, you'd use `null` or `undefined` to represent missing values. This causes runtime errors:

```javascript
const person = findPerson("John");
console.log(person.name); // Error if person is null!
```

L4 makes optionality explicit with `MAYBE`:

```l4
GIVEN name IS A STRING
GIVETH A MAYBE Employee
`find employee named` name MEANS
    -- Returns JUST employee OR NOTHING
```

### MAYBE Values

```l4
NOTHING              -- No value present
JUST value          -- Value is present
```

### Pattern Matching on MAYBE

```l4
GIVEN maybeEmployee IS A MAYBE Employee
GIVETH A STRING
`display name` maybeEmployee MEANS
    CONSIDER maybeEmployee
    WHEN NOTHING THEN
        "No employee found"
    WHEN JUST employee THEN
        employee's name
```

This forces you to handle the "no value" case—no null pointer exceptions!

### WPA Example: Optional Expiry Date

Some documents expire, others don't:

```l4
DECLARE Document HAS
    title      IS A STRING
    uploadDate IS A DATE
    expiryDate IS A MAYBE DATE  -- May or may not expire

GIVEN doc IS A Document
      today IS A DATE
GIVETH A BOOLEAN
`document is expired` doc today MEANS
    CONSIDER doc's expiryDate
    WHEN NOTHING THEN
        FALSE  -- No expiry date = never expires
    WHEN JUST expiry THEN
        today GREATER THAN expiry
```

### fromMaybe: Provide a Default

```l4
IMPORT prelude

GIVEN maybeValue IS A MAYBE NUMBER
GIVETH A NUMBER
`get or default to 0` maybeValue MEANS
    fromMaybe 0 maybeValue
```

This returns:

- The value inside `JUST x` → `x`
- The default `0` if `NOTHING`

## Higher-Order Functions

**Higher-order functions** take other functions as arguments—just like `array.map()` or `array.filter()` in JavaScript.

### map: Transform Each Element

JavaScript:

```javascript
const salaries = employees.map((e) => e.monthlySalary);
```

L4:

```l4
IMPORT prelude

GIVEN employees IS A LIST OF Employee
GIVETH A LIST OF NUMBER
`all salaries` employees MEANS
    map getSalary employees
    WHERE
        getSalary emp MEANS emp's monthlySalary
```

Or more concisely:

```l4
map (GIVEN emp YIELD emp's monthlySalary) employees
```

### filter: Select Elements

JavaScript:

```javascript
const techWorkers = employees.filter((e) => e.category === "TechProfessional");
```

L4:

```l4
IMPORT prelude

GIVEN employees IS A LIST OF Employee
GIVETH A LIST OF Employee
`tech professionals in` employees MEANS
    filter isTech employees
    WHERE
        isTech emp MEANS
            emp's category EQUALS TechProfessional
```

### any: Check if Any Element Satisfies Condition

JavaScript:

```javascript
const hasHighEarner = employees.some((e) => e.monthlySalary > 10000);
```

L4:

```l4
IMPORT prelude

GIVEN employees IS A LIST OF Employee
GIVETH A BOOLEAN
`has high earner` employees MEANS
    any isHighEarner employees
    WHERE
        isHighEarner emp MEANS
            emp's monthlySalary GREATER THAN 10000
```

### all: Check if All Elements Satisfy Condition

JavaScript:

```javascript
const allEligible = employees.every((e) => e.age >= 18);
```

L4:

```l4
IMPORT prelude

GIVEN employees IS A LIST OF Employee
GIVETH A BOOLEAN
`all adults` employees MEANS
    all isAdult employees
    WHERE
        isAdult emp MEANS
            `age of` emp `as of` `today` AT LEAST 18
```

## Common List Operations from Prelude

The `prelude` library provides many useful functions:

### Basic Operations

```l4
IMPORT prelude

-- Length
GIVEN list IS A LIST OF Employee
GIVETH A NUMBER
`count employees` list MEANS
    `length of` list

-- Concatenate two lists
GIVEN list1 IS A LIST OF STRING
      list2 IS A LIST OF STRING
GIVETH A LIST OF STRING
`combine` list1 list2 MEANS
    append list1 list2

-- Get element at index
GIVEN list IS A LIST OF NUMBER
      index IS A NUMBER
GIVETH A NUMBER
`element at` index `in` list MEANS
    at list index

-- Reverse
GIVEN list IS A LIST OF STRING
GIVETH A LIST OF STRING
`reversed` list MEANS
    reverse list
```

### Aggregation

```l4
-- Sum of numbers
GIVEN list IS A LIST OF NUMBER
GIVETH A NUMBER
`total` list MEANS
    sum list

-- Product of numbers
GIVEN list IS A LIST OF NUMBER
GIVETH A NUMBER
`multiply all` list MEANS
    product list

-- Maximum
GIVEN list IS A LIST OF NUMBER
GIVETH A NUMBER
`highest` list MEANS
    maximum list

-- Minimum
GIVEN list IS A LIST OF NUMBER
GIVETH A NUMBER
`lowest` list MEANS
    minimum list
```

### Taking and Dropping

```l4
-- Take first N elements
GIVEN n IS A NUMBER
      list IS A LIST OF Employee
GIVETH A LIST OF Employee
`first` n `employees from` list MEANS
    take n list

-- Drop first N elements
GIVEN n IS A NUMBER
      list IS A LIST OF Employee
GIVETH A LIST OF Employee
`skip` n `employees from` list MEANS
    drop n list
```

## Combining MAYBE and Lists

### mapMaybe: Transform and Filter in One Pass

```l4
IMPORT prelude

-- Extract salaries only from eligible employees
GIVEN employees IS A LIST OF Employee
GIVETH A LIST OF NUMBER
`salaries of eligible` employees MEANS
    mapMaybe extractIfEligible employees
    WHERE
        extractIfEligible emp MEANS
            IF `is eligible for work pass` emp
            THEN JUST emp's monthlySalary
            ELSE NOTHING
```

### catMaybes: Remove NOTHING Values

```l4
IMPORT prelude

GIVEN maybeList IS A LIST OF MAYBE Employee
GIVETH A LIST OF Employee
`only employees present` maybeList MEANS
    catMaybes maybeList
```

Converts `LIST JUST emp1, NOTHING, JUST emp2, NOTHING`
to `LIST emp1, emp2`

## WPA Examples: Lists and MAYBE in Action

### Finding Employees by Category

```l4
IMPORT prelude

§§ `Employee Filtering`

GIVEN employees IS A LIST OF Employee
      category IS AN EmploymentCategory
GIVETH A LIST OF Employee
`filter by` `category` `in` `employees` MEANS
    filter matches employees
    WHERE
        matches emp MEANS emp's category EQUALS category

-- Find all tech professionals
GIVEN employees IS A LIST OF Employee
GIVETH A LIST OF Employee
`tech professionals in` employees MEANS
    `filter by` TechProfessional `in` employees
```

### Calculating Statistics

```l4
IMPORT prelude

§§ `Salary Statistics`

GIVEN employees IS A LIST OF Employee
GIVETH A NUMBER
`average salary of` employees MEANS
    totalSalary DIVIDED BY employeeCount
    WHERE
        salaries MEANS map getSalary employees
        getSalary emp MEANS emp's monthlySalary
        totalSalary MEANS sum salaries
        employeeCount MEANS `length of` employees

GIVEN employees IS A LIST OF Employee
GIVETH A MAYBE NUMBER
`median salary of` employees MEANS
    IF null employees
    THEN NOTHING
    ELSE JUST middleValue
    WHERE
        salaries MEANS map getSalary employees
        getSalary emp MEANS emp's monthlySalary
        sortedSalaries MEANS sort salaries
        middleIndex MEANS (`length of` sortedSalaries) DIVIDED BY 2
        middleValue MEANS at sortedSalaries middleIndex
```

### Quota Management

```l4
IMPORT prelude

§§ `Foreign Worker Quota Checks`

GIVEN employees IS A LIST OF Employee
GIVETH A NUMBER
`count foreign workers` employees MEANS
    `length of` foreignWorkers
    WHERE
        foreignWorkers MEANS
            filter isForeign employees
        isForeign emp MEANS
            NOT emp's nationality EQUALS "Singapore"

GIVEN company IS A Company
      currentEmployees IS A LIST OF Employee
GIVETH A NUMBER
`remaining quota for` company `given` currentEmployees MEANS
    company's foreignWorkerQuota MINUS currentForeignCount
    WHERE
        currentForeignCount MEANS
            `count foreign workers` currentEmployees

GIVEN company IS A Company
      currentEmployees IS A LIST OF Employee
GIVETH A BOOLEAN
`can hire foreign worker` company currentEmployees MEANS
    `remaining quota for` company `given` currentEmployees GREATER THAN 0
```

### Application Batch Processing

```l4
IMPORT prelude

§§ `Batch Application Processing`

GIVEN applications IS A LIST OF WorkPassApplication
GIVETH A LIST OF WorkPassApplication
`approved applications` applications MEANS
    filter isApproved applications
    WHERE
        isApproved app MEANS
            app's status EQUALS Approved

GIVEN applications IS A LIST OF WorkPassApplication
GIVETH A BOOLEAN
`all applications processed` applications MEANS
    all isProcessed applications
    WHERE
        isProcessed app MEANS
            NOT app's status EQUALS Draft
            AND NOT app's status EQUALS Submitted

GIVEN applications IS A LIST OF WorkPassApplication
GIVETH A BOOLEAN
`any pending applications` applications MEANS
    any isPending applications
    WHERE
        isPending app MEANS
            app's status EQUALS Submitted
```

## Recursive List Processing

When prelude doesn't have what you need, write your own:

```l4
-- Count employees over a salary threshold
GIVEN employees IS A LIST OF Employee
      threshold IS A NUMBER
GIVETH A NUMBER
`count employees earning over` threshold `from` employees MEANS
    CONSIDER employees
    WHEN EMPTY THEN
        0
    WHEN emp FOLLOWED BY rest THEN
        IF emp's monthlySalary GREATER THAN threshold
        THEN 1 PLUS `count employees earning over` threshold `from` rest
        ELSE `count employees earning over` threshold `from` rest

-- Find first employee matching condition
GIVEN employees IS A LIST OF Employee
      category IS AN EmploymentCategory
GIVETH A MAYBE Employee
`first` `category` `employee in` `employees` MEANS
    CONSIDER employees
    WHEN EMPTY THEN
        NOTHING
    WHEN emp FOLLOWED BY rest THEN
        IF emp's category EQUALS category
        THEN JUST emp
        ELSE `first` category `employee in` rest
```

## Key Takeaways

1. **Lists are homogeneous**—all elements must be the same type
2. **MAYBE** explicitly represents optional values (no null/undefined errors)
3. **Pattern match on MAYBE**: `WHEN NOTHING` and `WHEN JUST value`
4. **Higher-order functions** (`map`, `filter`, `any`, `all`) work like JavaScript array methods
5. **Prelude provides** common list operations—import it!
6. **fromMaybe** extracts MAYBE values with a default
7. **mapMaybe** transforms and filters in one pass
8. **Recursive patterns** handle custom list processing

## Exercises

### Exercise 1: List Operations

Write a function that takes a list of employees and returns the total of all salaries over $5,000.

### Exercise 2: MAYBE Handling

Write a function `find employee by passport` that returns `MAYBE Employee` given a passport number and list of employees.

### Exercise 3: Higher-Order Functions

Write a function that checks if a company has _any_ employees in a given category, using `any` from prelude.

### Exercise 4: Combining Concepts

Write a function that:

1. Filters employees by category
2. Extracts their salaries
3. Calculates the average
4. Returns MAYBE NUMBER (NOTHING if no employees in that category)

## Next Steps

In **Module 5**, we'll combine everything learned so far to build a **complete WPA eligibility pipeline**—a multi-step assessment that processes applications from start to finish.
