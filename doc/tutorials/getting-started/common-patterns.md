# Common L4 Patterns

Frequently used L4 patterns with working examples.

**Audience:** L4 users at any level  
**Prerequisites:** Basic L4 knowledge  
**Time:** 30 minutes (reference - use as needed)  
**Goal:** Quick reference for common patterns

---

## Type Patterns

### Simple Record

```l4
DECLARE Person
    HAS name IS A STRING
        age IS A NUMBER
        email IS A STRING
```

### Record with Nested Type

```l4
DECLARE Address
    HAS street IS A STRING
        city IS A STRING
        country IS A STRING

DECLARE Person
    HAS name IS A STRING
        address IS A Address
```

### Enumeration (Fixed Choices)

```l4
DECLARE Status IS ONE OF
    Active
    Suspended
    Terminated
```

### Enumeration with Data

```l4
DECLARE PaymentStatus IS ONE OF
    Pending
    Paid HAS date IS A Date
             amount IS A NUMBER
    Rejected HAS reason IS A STRING
```

### Union Type (Either/Or)

```l4
DECLARE LegalEntity IS ONE OF
    Individual HAS person IS A Person
    Corporation HAS company IS A Company
```

---

## Function Patterns

### Simple Boolean Predicate

```l4
GIVEN person IS A Person
GIVETH A BOOLEAN
DECIDE `is adult` IF person's age >= 18
```

### Boolean with Multiple Conditions

```l4
GIVEN person IS A Person
GIVETH A BOOLEAN
DECIDE `is eligible` IF
    person's age >= 18
    AND person's hasValidID
    AND NOT person's isBanned
```

### Computation Function

```l4
GIVEN principal IS A NUMBER
      rate IS A NUMBER
      years IS A NUMBER
GIVETH A NUMBER
`compound interest` MEANS principal * (`power` (1 + rate) years)
```

### Function with WHERE

```l4
GIVEN loan IS A Loan
GIVETH A NUMBER
`monthly payment` MEANS
    payment
    WHERE
        p MEANS loan's principal
        r MEANS loan's rate / 12
        n MEANS loan's months
        payment MEANS p * (r * factor) / (factor - 1)
        factor MEANS `power` (1 + r) n
```

### Pattern Matching Function

```l4
GIVEN status IS A Status
GIVETH A STRING
`describe status` MEANS
    CONSIDER status
    WHEN Active THEN "Currently active"
    WHEN Suspended THEN "Temporarily suspended"
    WHEN Terminated THEN "No longer active"
```

### Pattern Matching with Data Extraction

```l4
GIVEN payment IS A PaymentStatus
GIVETH A STRING
`payment description` MEANS
    CONSIDER payment
    WHEN Pending THEN "Awaiting payment"
    WHEN Paid d a THEN CONCAT "Paid " (SHOW a) " on " (SHOW d)
    WHEN Rejected r THEN CONCAT "Rejected: " r
```

---

## List Patterns

### Create a List

```l4
numbers MEANS LIST 1, 2, 3, 4, 5
names MEANS LIST "Alice", "Bob", "Charlie"
empty MEANS LIST
```

### Check if List is Empty

```l4
GIVEN items IS A LIST OF Item
GIVETH A BOOLEAN
`has items` MEANS NOT null items
```

### Get First Element

```l4
GIVEN items IS A LIST OF Item
GIVETH A MAYBE Item
`first item` MEANS
    CONSIDER items
    WHEN EMPTY THEN NOTHING
    WHEN x FOLLOWED BY rest THEN JUST x
```

### Check if Element is in List

```l4
GIVEN item IS A Item
      items IS A LIST OF Item
GIVETH A BOOLEAN
`contains item` MEANS elem item items
```

### Check All Elements

```l4
GIVEN people IS A LIST OF Person
GIVETH A BOOLEAN
`all adults` MEANS all (GIVEN p YIELD p's age >= 18) people
```

### Check Any Element

```l4
GIVEN people IS A LIST OF Person
GIVETH A BOOLEAN
`any minor` MEANS any (GIVEN p YIELD p's age < 18) people
```

### Transform All Elements

```l4
GIVEN numbers IS A LIST OF NUMBER
GIVETH A LIST OF NUMBER
`double all` MEANS map (GIVEN n YIELD n * 2) numbers
```

### Filter Elements

```l4
GIVEN people IS A LIST OF Person
GIVETH A LIST OF Person
`only adults` MEANS filter (GIVEN p YIELD p's age >= 18) people
```

---

## Conditional Patterns

### Simple If-Then-Else

```l4
GIVEN age IS A NUMBER
GIVETH A STRING
`age category` MEANS
    IF age < 18 THEN "minor"
    ELSE "adult"
```

### Nested Conditions

```l4
GIVEN score IS A NUMBER
GIVETH A STRING
`grade` MEANS
    IF score >= 90 THEN "A"
    ELSE IF score >= 80 THEN "B"
    ELSE IF score >= 70 THEN "C"
    ELSE IF score >= 60 THEN "D"
    ELSE "F"
```

### Conditional in Context

```l4
DECIDE amount IS
    IF isVIP THEN baseAmount * 0.9
    ELSE baseAmount
```

---

## Regulative Rule Patterns

### Simple Obligation

```l4
GIVETH DEONTIC Party Action
`delivery obligation` MEANS
    PARTY Seller
    MUST `deliver goods`
    WITHIN 14
    HENCE FULFILLED
    LEST BREACH
```

### Permission

```l4
GIVETH DEONTIC Party Action
`inspection right` MEANS
    PARTY Buyer
    MAY `inspect goods`
    HENCE FULFILLED
```

### Prohibition

```l4
GIVETH DEONTIC Party Action
`confidentiality clause` MEANS
    PARTY Employee
    SHANT `disclose confidential information`
    HENCE FULFILLED
    LEST BREACH
```

### Chained Obligations

```l4
GIVETH DEONTIC Party Action
`sale contract` MEANS
    PARTY Seller
    MUST `deliver goods`
    WITHIN 14
    HENCE
        PARTY Buyer
        MUST `pay invoice`
        WITHIN 30
        HENCE FULFILLED
        LEST BREACH BY Buyer
    LEST BREACH BY Seller
```

### Conditional Obligation

```l4
GIVETH DEONTIC Party Action
`conditional payment` MEANS
    PARTY Buyer
    MUST `pay` amount PROVIDED amount > 0
    WITHIN 30
    HENCE FULFILLED
    LEST BREACH
```

### Parallel Obligations (RAND)

```l4
GIVETH DEONTIC Party Action
`mutual obligations` MEANS
    (PARTY Seller MUST `deliver` WITHIN 14 HENCE FULFILLED LEST BREACH)
    RAND
    (PARTY Seller MUST `provide documentation` WITHIN 14 HENCE FULFILLED LEST BREACH)
```

### Alternative Paths (ROR)

```l4
GIVETH DEONTIC Party Action
`delivery options` MEANS
    (PARTY Seller MUST `ship goods` WITHIN 14 HENCE FULFILLED)
    ROR
    (PARTY Seller MUST `arrange pickup` WITHIN 7 HENCE FULFILLED)
    LEST BREACH
```

---

## Money and Currency Patterns

### Money Type

```l4
DECLARE Money
    HAS amount IS A NUMBER
        currency IS A STRING
```

### Currency Constructors

```l4
GIVEN n IS A NUMBER
GIVETH A Money
USD MEANS Money n "USD"

GIVEN n IS A NUMBER
GIVETH A Money
EUR MEANS Money n "EUR"
```

### Money Comparison

```l4
GIVEN m1 IS A Money
      m2 IS A Money
GIVETH A BOOLEAN
`money equals` MEANS
    m1's amount = m2's amount
    AND m1's currency EQUALS m2's currency
```

---

## Date Patterns

### Using daydate Library

```l4
IMPORT daydate

-- Create a date
expiryDate MEANS Date 2025 12 31

-- Compare dates
GIVEN d1 IS A Date
      d2 IS A Date
GIVETH A BOOLEAN
`is before` MEANS dateBefore d1 d2
```

### Days Between Dates

```l4
GIVEN start IS A Date
      end IS A Date
GIVETH A NUMBER
`days between` MEANS daysBetween start end
```

---

## Testing Patterns

### Unit Test with #EVAL

```l4
#EVAL `is adult` (Person "Test" 18)  -- TRUE
#EVAL `is adult` (Person "Test" 17)  -- FALSE
```

### Integration Test with #TRACE

```l4
#TRACE `sale contract` AT 0 WITH
    PARTY Seller DOES `deliver goods` AT 10
    PARTY Buyer DOES `pay invoice` AT 25
-- Expected: FULFILLED
```

### Boundary Testing

```l4
-- Test boundaries
#EVAL `is adult` (Person "Edge" 18)   -- TRUE (exactly at boundary)
#EVAL `is adult` (Person "Edge" 17)   -- FALSE (just below)
#EVAL `is adult` (Person "Edge" 19)   -- TRUE (just above)
```

---

## Anti-Patterns (What NOT to Do)

### ❌ Missing Parentheses with Field Access

```l4
-- ❌ Wrong
length charity's governors > 0

-- ✅ Right
length (charity's governors) > 0
```

### ❌ Forgetting ELSE Branch

```l4
-- ❌ Wrong (missing ELSE)
DECIDE result IS IF condition THEN "yes"

-- ✅ Right
DECIDE result IS IF condition THEN "yes" ELSE "no"
```

### ❌ Missing GIVETH

```l4
-- ❌ Wrong
GIVEN n IS A NUMBER
square MEANS n * n

-- ✅ Right
GIVEN n IS A NUMBER
GIVETH A NUMBER
square MEANS n * n
```

### ❌ Inconsistent Indentation

```l4
-- ❌ Wrong (misaligned fields)
DECLARE Person
    HAS name IS A STRING
      age IS A NUMBER
        email IS A STRING

-- ✅ Right (all fields aligned)
DECLARE Person
    HAS name IS A STRING
        age IS A NUMBER
        email IS A STRING
```

---

## Quick Reference

| Pattern                       | When to Use             |
| ----------------------------- | ----------------------- |
| `DECLARE ... HAS`             | Define a record type    |
| `DECLARE ... IS ONE OF`       | Define an enumeration   |
| `GIVEN ... GIVETH ... DECIDE` | Define a function       |
| `CONSIDER ... WHEN`           | Pattern matching        |
| `IF ... THEN ... ELSE`        | Conditional expression  |
| `PARTY ... MUST`              | Create obligation       |
| `PARTY ... MAY`               | Create permission       |
| `PARTY ... SHANT`             | Create prohibition      |
| `RAND`                        | Parallel obligations    |
| `ROR`                         | Alternative obligations |
| `#EVAL`                       | Test expression         |
| `#TRACE`                      | Test regulative rule    |

---

## Next Steps

- [Foundation Course](../../courses/foundation/README.md) - Learn concepts systematically
- [Advanced Course](../../courses/advanced/README.md) - Complex patterns
- [Reference](../../reference/README.md) - Complete language reference
