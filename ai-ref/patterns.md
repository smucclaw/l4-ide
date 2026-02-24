# Canonical L4 Patterns

Copy-paste templates. For struct/record use DECLARE HAS. For enum/union use IS ONE OF. For function use MEANS/DECIDE. For switch/case use CONSIDER/BRANCH. For dot access use 's possessive. For string concat use CONCAT. For optional/nullable use MAYBE.

---

## Product Type (Record / Struct / Class / Interface)

```l4
DECLARE TypeName HAS
    field1 IS A Type1
    field2 IS A Type2
    field3 IS A Type3
```

**Access (dot notation equivalent):** `record's field1`, `record's field2's nested`

---

## Sum Type with Payloads (Enum / Union / Variant / ADT)

```l4
DECLARE TypeName IS ONE OF
    Constructor1
    | Constructor2 ArgType
    | Constructor3 ArgType1 ArgType2
```

**Sum type constructors can have their own fields:**
```l4
DECLARE Status IS ONE OF
    Active
    | Suspended
        HAS reason     IS A STRING
            `start date` IS A DATE
    | Closed
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

## Mixfix Function Definition (Infix / Postfix)

```l4
-- Parameters interspersed with identifier words
GIVEN mom IS A STRING
      dad IS A STRING
      kid IS A STRING
GIVES A STRING
mom and dad `have a baby named` kid MEANS
    CONCAT "mother: ", mom, ", father: ", dad, ", child: ", kid

-- Call: "alice" and "bob" `have a baby named` "carol"
```

---

## String Concatenation

```l4
-- CONCAT joins multiple string arguments
greeting MEANS CONCAT "Hello, ", person's name, "!"

-- With type coercion
summary MEANS CONCAT person's name, " owes ", TOSTRING(amount)
```

---

## Caret Operator (Ditto / Vertical Alignment)

```l4
-- ^ means "same token as previous line in this position"
`tautology` MEANS
        p EQUALS TRUE
    OR  p EQUALS FALSE
   AND  q ^      TRUE    -- ^ = EQUALS
    OR  q ^      FALSE
```

---

## UNLESS (AND NOT)

```l4
-- UNLESS = AND NOT, for natural legal phrasing
DECIDE `is eligible` IF
        `meets age requirement`
    AND `has valid ID`
 UNLESS  `is disqualified`
```

---

## LET / IN (Alternative to WHERE)

```l4
-- Prefix-style local bindings
result MEANS
    LET x MEANS a + b
        y MEANS c * d
    IN  x + y

-- Compare: same logic with WHERE (postfix style)
result MEANS
    x + y
    WHERE
        x MEANS a + b
        y MEANS c * d
```

---

## Section Scoping with §

```l4
§ Definitions
  DECLARE Person HAS
      name IS A STRING
      age IS A NUMBER

§ Eligibility
  GIVEN p IS A Person
  DECIDE `is eligible` IF p's age >= 18

  §§ Special Cases
    -- Nested subsection
    DECIDE `is exempt` IF ...
```

---

## Full Regulative Rule (Loan Contract)

```l4
GIVEN borrower IS A Person
      lender   IS A Person
      capital  IS A NUMBER
      interest IS A NUMBER
      closing  IS A DATE
      repayment IS A DATE
      `conditions precedent are met` IS A BOOLEAN
`Loan Contract` MEANS
  IF     NOT `conditions precedent are met`
  THEN   FULFILLED
  ELSE   PARTY lender
         MUST  EXACTLY send capital to borrower
         BEFORE closing
         HENCE    PARTY  borrower
                  MUST   EXACTLY send accrued to lender
                  BEFORE repayment
  WHERE
    send money to person MEANS
        CONCAT person's name, " receives ", TOSTRING(money)
    accrued MEANS capital PLUS capital * interest
```

---

## Testing Pattern

```l4
-- At end of file: evaluate expression
#EVAL `function name` arg1 arg2 arg3
-- Expected result shown in output

-- Evaluate deontic rule, show obligation trace
#TRACE `deontic rule` context
-- Shows: PARTY X MUST ... HENCE PARTY Y MUST ...
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
BOOLEAN          -- TRUE / FALSE (uppercase!)
NUMBER           -- numeric
STRING           -- text
DATE             -- YYYY-MM-DD (IMPORT daydate)
TIME             -- HH:MM:SS (IMPORT time)
DATETIME         -- ISO-8601 with timezone (IMPORT datetime)
DEONTIC          -- obligation/permission/prohibition effect
LIST OF T        -- homogeneous list (IMPORT prelude for operations)
MAYBE T          -- optional: Nothing or Just value
CustomType       -- user-defined via DECLARE
```

**Coercions:** `TODATE("2024-01-15")`, `TOTIME("14:30:00")`, `TODATETIME("2024-01-15T14:30:00Z")`, `TOSTRING(42)`
