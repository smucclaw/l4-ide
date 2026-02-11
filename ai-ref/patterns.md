# Canonical L4 Patterns

Copy-paste templates for common cases. Each pattern: name, structure, example.

---

## Product Type (Record)

```l4
DECLARE TypeName HAS
    field1 IS A Type1
    field2 IS A Type2
    field3 IS A Type3
```

**Access:** `record's field1`, `record's field2's nested`

---

## Sum Type (Enum/ADT)

```l4
DECLARE TypeName IS ONE OF
    Constructor1
    | Constructor2 ArgType
    | Constructor3 ArgType1 ArgType2
```

**Pattern match:**
```l4
CONSIDER value
WHEN Constructor1 THEN result1
WHEN Constructor2 arg THEN result2
WHEN Constructor3 x y THEN result3
```

---

## Boolean Predicate

```l4
-- With explicit type annotation (recommended)
GIVEN param IS A Type
GIVETH A BOOLEAN
DECIDE `predicate name` IF
    condition1
    AND condition2
    AND NOT condition3

-- Type inference also works (GIVETH optional)
GIVEN param IS A Type
DECIDE `predicate name` IF condition1 AND condition2
```

---

## Multi-Way Classification

```l4
GIVEN param IS A Type
GIVETH A Category
DECIDE `category name` IS
    BRANCH
        IF condition1 THEN category1
        IF condition2 THEN category2
        IF condition3 THEN category3
        OTHERWISE defaultCategory
```

---

## Calculation with Intermediates

```l4
GIVEN input1 IS A Type1, input2 IS A Type2
GIVETH A Number
`result name` MEANS
    `step1` + `step2` * `step3`
    WHERE
        `step1` MEANS input1 * factor1
        `step2` MEANS input2 - adjustment
        `step3` MEANS `helper` input1 input2
        `helper` MEANS ...
```

---

## Progressive Tax Brackets

```l4
GIVEN income IS A NUMBER
GIVETH A NUMBER
`tax owed` MEANS
    `bracket1 tax` + `bracket2 tax` + `bracket3 tax`
    WHERE
        `bracket1 tax` MEANS `amount in bracket1` * rate1
        `bracket2 tax` MEANS `amount in bracket2` * rate2
        `bracket3 tax` MEANS `amount in bracket3` * rate3

        `amount in bracket1` MEANS
            BRANCH
                IF income <= threshold1 THEN income
                OTHERWISE threshold1

        `amount in bracket2` MEANS
            BRANCH
                IF income <= threshold1 THEN 0
                IF income <= threshold2 THEN income - threshold1
                OTHERWISE threshold2 - threshold1

        `amount in bracket3` MEANS
            IF income <= threshold2 THEN 0
            ELSE income - threshold2
```

---

## Eligibility with Record

```l4
DECLARE Person HAS
    age IS A NUMBER
    citizenship IS A STRING
    income IS A NUMBER
    disqualified IS A BOOLEAN

GIVEN person IS A Person
GIVETH A BOOLEAN
DECIDE `is eligible` IF
    `age requirement met`
    AND `citizenship requirement met`
    AND `income requirement met`
    AND NOT `is disqualified`
    WHERE
        `age requirement met` MEANS
            person's age >= minAge
            AND person's age < maxAge

        `citizenship requirement met` MEANS
            person's citizenship = "citizen"
            OR person's citizenship = "permanent resident"

        `income requirement met` MEANS
            person's income < incomeThreshold

        `is disqualified` MEANS
            person's disqualified
```

---

## List Quantifiers

```l4
-- All items satisfy condition
all (GIVEN x YIELD `predicate` x) items

-- Any item satisfies condition
any (GIVEN x YIELD `predicate` x) items

-- Filter matching items
filter (GIVEN x YIELD `predicate` x) items

-- Transform all items
map (GIVEN x YIELD `transform` x) items

-- Check membership
elem targetValue items
```

**Field access in quantifiers:** Wrap in parens
```l4
all (GIVEN g YIELD g's age >= 18) (charity's governors)
```

---

## Maybe Handling

```l4
CONSIDER maybeValue
WHEN Nothing THEN defaultResult
WHEN Just actualValue THEN processActualValue
```

---

## List Pattern Matching

```l4
CONSIDER list
WHEN EMPTY THEN emptyResult
WHEN single FOLLOWED BY EMPTY THEN singletonResult
WHEN first FOLLOWED BY rest THEN
    process first (recurse rest)
```

---

## Simple Obligation

```l4
PARTY actor
MUST action
TO beneficiary
WITHIN duration
IF precondition
```

---

## Conditional Obligation

```l4
GIVEN context IS A Context
GIVETH A DEONTIC
`obligation name` MEANS
    IF `triggering condition` THEN
        PARTY actor
        MUST action
        TO beneficiary
        WITHIN duration
    ELSE
        NoObligation
```

---

## Sequential Obligations (HENCE)

```l4
PARTY party1
MUST action1
TO party2
HENCE
    PARTY party2
    MUST action2
    TO party1
```

---

## Obligation with Penalty (LEST)

```l4
PARTY obligor
MUST action
TO obligee
WITHIN deadline
LEST
    PARTY obligor
    MUST `pay penalty`
    TO obligee
```

---

## Nested WHERE Scopes

```l4
result MEANS
    outer1 + outer2
    WHERE
        outer1 MEANS inner1 + inner2
            WHERE
                inner1 MEANS x * 2
                inner2 MEANS y * 3

        outer2 MEANS inner3
            WHERE
                inner3 MEANS outer1 * 4  -- Can reference outer1
```

---

## Recursive Definition

```l4
GIVEN n IS A NUMBER
GIVETH A NUMBER
factorial MEANS
    IF n <= 1
    THEN 1
    ELSE n * (factorial (n - 1))
```

**Note:** Ensure base case prevents infinite recursion

---

## Lambda (Anonymous Function)

```l4
GIVEN x YIELD expression

-- Example in context
filter (GIVEN n YIELD n > 0) numbers
map (GIVEN p YIELD p's age) people
```

---

## Exhaustive Sum Type Match

```l4
DECLARE TrafficLight IS ONE OF Red | Yellow | Green

GIVEN light IS A TrafficLight
GIVETH A STRING
action MEANS
    CONSIDER light
    WHEN Red THEN "stop"
    WHEN Yellow THEN "slow"
    WHEN Green THEN "go"
-- No OTHERWISE needed (exhaustive)
```

---

## Non-Exhaustive with Default

```l4
CONSIDER value
WHEN SpecificCase1 THEN result1
WHEN SpecificCase2 THEN result2
OTHERWISE defaultResult  -- Catch remaining cases
```

---

## Testing Pattern

```l4
-- At end of file
#EVAL `function name` arg1 arg2 arg3
-- Expected result shown in output

#TRACE `deontic rule` context
-- Shows obligation trace
```

---

## Import and Use Prelude

```l4
IMPORT prelude

-- Now available: map, filter, all, any, elem, null, length, head, tail,
--                foldl, foldr, sum, product, concat, reverse
```

---

## Comparison Operators

```l4
=       -- equality
>       -- greater than
<       -- less than
>=      -- greater or equal
<=      -- less or equal

-- Alternative syntax
EQUALS
GREATER THAN
LESS THAN
AT LEAST
AT MOST
```

---

## Boolean Combinators

```l4
cond1 AND cond2
cond1 OR cond2
NOT condition
cond1 IMPLIES cond2  -- Material implication

-- Precedence: NOT > AND > OR > IMPLIES
-- Use parens for clarity: (a OR b) AND c
```

---

## Type Annotations

```l4
-- In DECLARE
field IS A Type
field IS A LIST OF Type
field IS A MAYBE Type

-- In GIVEN
param IS A Type
param IS AN Entity  -- Use AN before vowel sound

-- In GIVETH
GIVETH A Type
GIVETH A BOOLEAN
GIVETH A NUMBER
GIVETH A STRING
GIVETH A DEONTIC
```

---

## Common Type Constructors

```l4
BOOLEAN
NUMBER
STRING
DATE
DEONTIC
LIST OF T
MAYBE T
CustomType  -- User-defined via DECLARE
```
