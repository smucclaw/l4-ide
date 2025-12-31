# L4 Syntax Quick Reference

## File Structure

```l4
§ `Section Title`                 -- Top-level section
§§ `Subsection Title`              -- Subsection

IMPORT prelude                     -- Standard library
IMPORT daydate                     -- Date library
IMPORT module_name                 -- Custom module

-- Comments
{- Multi-line
   comment -}
```

## Type Declarations

### Enums

```l4
DECLARE EnumName IS ONE OF
    Constructor1
    Constructor2
    Constructor3
```

### Enums with Data (ADTs)

```l4
DECLARE Result IS ONE OF
    Success HAS value IS A NUMBER
    Failure HAS
        code    IS A NUMBER
        message IS A STRING
```

### Records

```l4
DECLARE RecordName HAS
    field1 IS A Type1
    field2 IS A Type2
    field3 IS A MAYBE Type3    -- Optional field
```

## Function Definitions

### Basic Function

```l4
GIVEN param IS A Type
GIVETH A ReturnType
functionName param MEANS expression
```

### Multiple Parameters

```l4
GIVEN param1 IS A Type1
      param2 IS A Type2
      param3 IS A Type3
GIVETH A ReturnType
functionName param1 param2 param3 MEANS expression
```

### Mixfix Notation

```l4
GIVEN x IS A Type1
      y IS A Type2
GIVETH A ReturnType
`x` `custom operator` `y` MEANS expression
```

### Decision Rules

```l4
GIVEN param IS A Type
GIVETH A BOOLEAN
DECIDE ruleName IF condition1 AND condition2
```

### WITH WHERE Clause

```l4
mainExpression
WHERE
    helper1 MEANS expr1
    helper2 param MEANS expr2
    value3 MEANS expr3
```

## Pattern Matching

### CONSIDER/WHEN

```l4
CONSIDER value
WHEN Pattern1 THEN Result1
WHEN Pattern2 THEN Result2
WHEN Pattern3 THEN Result3
OTHERWISE DefaultResult
```

### With Data Extraction

```l4
CONSIDER outcome
WHEN Success WITH value THEN
    "Success: " APPEND (STRING value)
WHEN Failure WITH code, message THEN
    "Error " APPEND (STRING code) APPEND ": " APPEND message
```

### List Patterns

```l4
CONSIDER list
WHEN EMPTY THEN baseCase
WHEN head FOLLOWED BY tail THEN
    combine head (recurse tail)
```

### Pattern Guards

```l4
CONSIDER value
WHEN Constructor WITH field PROVIDED field GREATER THAN threshold THEN
    result1
OTHERWISE
    result2
```

### BRANCH (Flat Multi-Way)

```l4
BRANCH IF expr EQUALS value1 THEN result1
       IF ^    EQUALS value2 THEN result2
       IF ^    EQUALS value3 THEN result3
       OTHERWISE defaultResult
```

## Conditionals

```l4
IF condition
THEN expression1
ELSE expression2

-- Nested
IF condition1
THEN result1
ELSE IF condition2
THEN result2
ELSE IF condition3
THEN result3
ELSE defaultResult
```

## Operators

### Boolean Logic

```l4
condition1 AND condition2
condition1 OR condition2
NOT condition
condition1 IMPLIES condition2  -- or =>
```

### Comparisons

```l4
x EQUALS y
x GREATER THAN y
x LESS THAN y
x AT LEAST y              -- >=
x AT MOST y               -- <=
x NOT EQUALS y
```

### Arithmetic

```l4
x PLUS y                  -- or x + y
x MINUS y                 -- or x - y
x TIMES y                 -- or x * y
x DIVIDED BY y            -- or x / y
x MODULO y                -- or x % y
SQRT x
x EXPONENT y              -- or x ^ y
FLOOR x
CEILING x
```

### String Operations

```l4
str1 APPEND str2                    -- Concatenation
STRINGLENGTH str                    -- Length
TOUPPER str                         -- To uppercase
TOLOWER str                         -- To lowercase
TRIM str                            -- Remove whitespace
CONTAINS str substring              -- Check contains
STARTSWITH str prefix               -- Check starts with
ENDSWITH str suffix                 -- Check ends with
INDEXOF str substring               -- Find index (-1 if not found)
SPLIT str delimiter                 -- Split into list
SUBSTRING str start length          -- Extract substring
REPLACE str oldStr newStr           -- Replace all occurrences
CHARAT str index                    -- Get character at index
```

## Record Operations

### Construction

```l4
RecordType WITH
    field1 IS value1
    field2 IS value2
    field3 IS value3
```

### Field Access

```l4
record's fieldName
record's field1's nestedField    -- Chaining
```

## List Operations

### Construction

```l4
EMPTY                           -- Empty list
LIST item1, item2, item3        -- List literal
item FOLLOWED BY rest           -- Cons
```

### Common Functions (from prelude)

```l4
map function list               -- Apply function to each element
filter predicate list           -- Keep matching elements
fold function init list         -- Reduce to single value
any predicate list              -- TRUE if any matches
all predicate list              -- TRUE if all match
`length of` list                -- Count elements
at list index                   -- Get element at index
append list1 list2              -- Concatenate lists
reverse list                    -- Reverse order
head list                       -- First element (MAYBE)
tail list                       -- Rest of list (MAYBE)
```

## Maybe (Optional Values)

### Construction

```l4
NOTHING                         -- No value
JUST value                      -- Has value
```

### Pattern Matching

```l4
CONSIDER maybeValue
WHEN NOTHING THEN defaultResult
WHEN JUST x THEN processX
```

### Common Functions

```l4
fromMaybe default maybeValue    -- Extract or use default
isJust maybeValue               -- Check if has value
isNothing maybeValue            -- Check if no value
```

## Date Operations (from daydate)

```l4
DATE OF day, month, year        -- Construct date
`today`                         -- Current date
date1 MINUS date2               -- Days between
date PLUS days                  -- Add days
date MINUS days                 -- Subtract days
```

## Test Directives

```l4
#EVAL expression                -- Evaluate and display
#ASSERT boolean_expr            -- Assert must be TRUE
#TRACE expression               -- Show execution trace
```

## Type Conversions

```l4
STRING number                   -- Number to string
STRING boolean                  -- Boolean to string
NUMBER string                   -- String to number (if valid)
```

## Common Patterns

### Recursive List Processing

```l4
GIVEN list IS A LIST OF T
GIVETH A Result
process list MEANS
    CONSIDER list
    WHEN EMPTY THEN baseCase
    WHEN x FOLLOWED BY xs THEN
        combineResults (processElement x) (process xs)
```

### Building Results with WHERE

```l4
GIVEN input IS A Input
GIVETH A Result
computeResult input MEANS
    Result WITH
        field1 IS computation1
        field2 IS computation2
        field3 IS computation3
    WHERE
        computation1 MEANS ...
        computation2 MEANS ...
        computation3 MEANS ...
```

### Multi-Stage Decision Tree

```l4
GIVEN input IS A Input
GIVETH A Decision
makeDecision input MEANS
    IF NOT check1 input
    THEN Rejected WITH reason IS "Check 1 failed"
    ELSE IF NOT check2 input
    THEN Rejected WITH reason IS "Check 2 failed"
    ELSE IF NOT check3 input
    THEN Rejected WITH reason IS "Check 3 failed"
    ELSE Approved
    WHERE
        check1 inp MEANS ...
        check2 inp MEANS ...
        check3 inp MEANS ...
```

## Naming Conventions

### Multi-Word Identifiers

```l4
`can transfer ownership`        -- With backticks (reads like prose)
canTransferOwnership            -- camelCase (no spaces)
```

### Recommendation

Use backticks for function names that should read like natural language legal text. Use camelCase for simple variable names.

## Import System

```l4
IMPORT prelude                  -- Lists, Maybe, Pair operations
IMPORT daydate                  -- Date arithmetic
IMPORT jurisdiction             -- Jurisdiction-specific rules
IMPORT entities                 -- Entity definitions
IMPORT currency                 -- Currency operations
```

## Validation

```bash
# Type-check your L4 file
jl4-cli your-file.l4

# Pin evaluation time for reproducible results
jl4-cli --fixed-now=2025-01-01T00:00:00Z your-file.l4

# Or set environment variable
export JL4_FIXED_NOW=2025-01-01T00:00:00Z
jl4-cli your-file.l4
```
