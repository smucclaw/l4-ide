# Your First L4 File

Create, run, and test a simple L4 program.

**Audience:** Anyone new to L4  
**Prerequisites:** L4 tools installed (VS Code extension or CLI)  
**Time:** 15 minutes  
**Goal:** Create a working L4 file that defines a legal rule and tests it

---

## What You'll Build

A simple eligibility rule: "A person is eligible if they are at least 18 years old and have valid ID."

**Complete example:** [eligibility-example.l4](eligibility-example.l4)

---

## Step 1: Create the File

Create a new file called `eligibility.l4`:

```l4
-- eligibility.l4
-- A simple eligibility check

§ `Eligibility Rule`

-- Define the Person type
DECLARE Person
    HAS name IS A STRING
        age IS A NUMBER
        hasValidID IS A BOOLEAN

-- Define the eligibility rule
GIVEN person IS A Person
GIVETH A BOOLEAN
DECIDE `is eligible` IF
    person's age >= 18
    AND person's hasValidID

-- Test data
alice MEANS Person "Alice" 25 TRUE
bob MEANS Person "Bob" 16 TRUE
charlie MEANS Person "Charlie" 30 FALSE

-- Tests
#EVAL `is eligible` alice    -- Should be TRUE
#EVAL `is eligible` bob      -- Should be FALSE (under 18)
#EVAL `is eligible` charlie  -- Should be FALSE (no valid ID)
```

---

## Step 2: Understand the Code

Let's break down each part:

### Comments

```l4
-- This is a comment
```

Comments start with `--` and are ignored by L4.

### Sections

```l4
§ `Eligibility Rule`
```

Sections organize your code. The `§` symbol starts a section with a title.

### Type Declaration

```l4
DECLARE Person
    HAS name IS A STRING
        age IS A NUMBER
        hasValidID IS A BOOLEAN
```

This defines a `Person` type with three fields: name (text), age (number), and hasValidID (true/false).

### Function Definition

```l4
GIVEN person IS A Person
GIVETH A BOOLEAN
DECIDE `is eligible` IF
    person's age >= 18
    AND person's hasValidID
```

- `GIVEN` declares the input parameter
- `GIVETH A BOOLEAN` says it returns true or false
- `DECIDE ... IF` defines when the result is true

### Test Data

```l4
alice MEANS Person "Alice" 25 TRUE
```

This creates a Person named "Alice" who is 25 years old with valid ID.

### Tests

```l4
#EVAL `is eligible` alice
```

`#EVAL` evaluates an expression and shows the result.

---

## Step 3: Run the File

### Option A: VS Code

1. Open the file in VS Code (with L4 extension installed)
2. Hover over any `#EVAL` line to see the result
3. Errors appear as red underlines with explanations

### Option B: Command Line

```bash
cabal run jl4-cli -- eligibility.l4
```

This validates the file and reports any errors.

### Option C: REPL

```bash
cabal run jl4-repl -- eligibility.l4
```

Then type expressions interactively:

```
> `is eligible` alice
TRUE

> `is eligible` bob
FALSE
```

---

## Step 4: Verify the Results

Your tests should show:

| Expression            | Result | Why                       |
| --------------------- | ------ | ------------------------- |
| `is eligible` alice   | TRUE   | Age 25 ≥ 18, has valid ID |
| `is eligible` bob     | FALSE  | Age 16 < 18               |
| `is eligible` charlie | FALSE  | No valid ID               |

---

## Step 5: Extend the Rule

Let's add another condition: the person must not be banned.

```l4
-- Extended Person type
DECLARE Person
    HAS name IS A STRING
        age IS A NUMBER
        hasValidID IS A BOOLEAN
        isBanned IS A BOOLEAN

-- Extended rule
GIVEN person IS A Person
GIVETH A BOOLEAN
DECIDE `is eligible` IF
    person's age >= 18
    AND person's hasValidID
    AND NOT person's isBanned

-- Updated test data
alice MEANS Person "Alice" 25 TRUE FALSE
bob MEANS Person "Bob" 16 TRUE FALSE
charlie MEANS Person "Charlie" 30 FALSE FALSE
dave MEANS Person "Dave" 40 TRUE TRUE  -- Banned

-- Tests
#EVAL `is eligible` alice    -- TRUE
#EVAL `is eligible` bob      -- FALSE (under 18)
#EVAL `is eligible` charlie  -- FALSE (no valid ID)
#EVAL `is eligible` dave     -- FALSE (banned)
```

---

## Common Mistakes

### Missing Indentation

```l4
-- ❌ Wrong
DECLARE Person
HAS name IS A STRING

-- ✅ Right
DECLARE Person
    HAS name IS A STRING
```

### Missing Apostrophe in Field Access

```l4
-- ❌ Wrong
person age >= 18

-- ✅ Right
person's age >= 18
```

### Missing Backticks in Multi-Word Names

```l4
-- ❌ Wrong
DECIDE is eligible IF ...

-- ✅ Right
DECIDE `is eligible` IF ...
```

---

## What You Learned

- How to create an L4 file
- How to define types with `DECLARE`
- How to define rules with `GIVEN`, `GIVETH`, and `DECIDE`
- How to create test data
- How to test with `#EVAL`

---

## Next Steps

- [Encoding Legislation](encoding-legislation.md) - Turn legal text into L4
- [Common Patterns](common-patterns.md) - Frequently used L4 patterns
- [Foundation Course Module 1](../../courses/foundation/module-1-first-rule.md) - Learn more about legal rules
