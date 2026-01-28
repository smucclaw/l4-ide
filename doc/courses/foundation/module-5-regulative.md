# Module 5: Regulative Rules

In this module, you'll learn how to model legal obligations, permissions, and prohibitions—the core of contract and regulatory law.

## Learning Objectives

By the end of this module, you will be able to:

- Define Actor and Action types
- Create obligations with MUST
- Create permissions with MAY
- Create prohibitions with SHANT
- Chain obligations with HENCE and LEST
- Test regulative rules with #TRACE

---

## What Are Regulative Rules?

**Regulative rules** define what parties must, may, or must not do. They're the building blocks of contracts and regulations:

- **Obligations** (MUST): "The seller must deliver goods"
- **Permissions** (MAY): "The buyer may inspect goods"
- **Prohibitions** (SHANT): "The employee shall not disclose confidential information"
- **Option** (DO): "The employee discloses confidential information"

L4 represents these using the **DEONTIC** type.

---

## The DEONTIC Type

A DEONTIC value represents an obligation, permission, or prohibition. It has:

- **Who** (PARTY): The actor with the duty/permission
- **What** (action): The action to be performed
- **When** (WITHIN): The deadline
- **Consequences** (HENCE/LEST): What happens next

The complete working example:

[module-5-examples.l4](module-5-examples.l4)

### Defining Actor and Action Types

First, define who can act and what actions exist:

```l4
-- Who can act
DECLARE Person IS ONE OF
    Buyer
    Seller

-- What actions exist
DECLARE Action IS ONE OF
    `deliver goods`
    `pay invoice` HAS amount IS A NUMBER
    `inspect goods`
```

---

## Creating Obligations with MUST

```l4
GIVETH A DEONTIC Person Action
`the delivery obligation` MEANS
    PARTY Seller
    MUST `deliver goods`
    WITHIN 14
    HENCE FULFILLED
    LEST BREACH
```

| Part                             | Meaning                                                       |
| -------------------------------- | ------------------------------------------------------------- |
| `GIVETH A DEONTIC Person Action` | Returns a deontic value with Person actors and Action actions |
| `PARTY Seller`                   | The seller has this obligation                                |
| `MUST \`deliver goods\``         | They must deliver goods                                       |
| `WITHIN 14`                      | Within 14 days                                                |
| `HENCE FULFILLED`                | If they do, the obligation is fulfilled                       |
| `LEST BREACH`                    | If they don't, it's a breach                                  |

---

## Creating Permissions with MAY

Permissions don't create breaches if unused:

```l4
GIVETH A DEONTIC Person Action
`the inspection right` MEANS
    PARTY Buyer
    MAY `inspect goods`
    HENCE FULFILLED
```

Note: MAY doesn't need LEST because not exercising a permission isn't a breach.

---

## Creating Prohibitions with SHANT

Prohibitions say what must NOT happen:

```l4
GIVETH A DEONTIC Employee EmployeeAction
`the confidentiality clause` MEANS
    PARTY Employee1
    SHANT `disclose confidential information`
    HENCE FULFILLED
    LEST BREACH
```

`SHANT` is equivalent to "shall not" or "must not."

---

## Chaining Obligations with HENCE

Real contracts have sequences of obligations. Use HENCE to chain them:

```l4
GIVETH A DEONTIC Person Action
`the sale contract` MEANS
    PARTY Seller
    MUST `deliver goods`
    WITHIN 14
    HENCE
        PARTY Buyer
        MUST `pay invoice` 1000
        WITHIN 30
        HENCE FULFILLED
        LEST BREACH
    LEST BREACH
```

This creates a chain:

1. Seller must deliver within 14 days
2. **If delivered**: Buyer must pay within 30 days
3. **If buyer pays**: Contract fulfilled
4. **If either fails**: Breach

---

## Alternative Consequences with LEST

LEST specifies what happens on non-compliance:

```l4
GIVETH A DEONTIC Person Action
`the late payment contract` MEANS
    PARTY Buyer
    MUST `pay invoice` 1000
    WITHIN 30
    HENCE FULFILLED
    LEST
        PARTY Buyer
        MUST `pay invoice` 1100  -- 10% late fee
        WITHIN 14
        HENCE FULFILLED
        LEST BREACH
```

This gives the buyer a second chance with a penalty before reaching breach.

---

## Conditional Obligations with PROVIDED

Add conditions to obligations:

```l4
PARTY Buyer
MUST `pay invoice` amount PROVIDED amount >= 100
WITHIN 30
HENCE FULFILLED
LEST BREACH
```

`PROVIDED` adds a guard condition—the obligation only applies if the condition is true.

---

## Parallel Obligations with RAND

Use `RAND` (regulative AND) for obligations that must all be fulfilled:

```l4
(PARTY Seller MUST `deliver goods` WITHIN 14 HENCE FULFILLED LEST BREACH)
RAND
(PARTY Buyer MUST `pay invoice` 1000 WITHIN 30 HENCE FULFILLED LEST BREACH)
```

Both obligations must be fulfilled for the contract to be fulfilled.

---

## Alternative Paths with ROR

Use `ROR` (regulative OR) when either path fulfills the contract:

```l4
(PARTY Seller MUST `deliver goods` WITHIN 14 HENCE FULFILLED)
ROR
(PARTY Seller MUST `arrange pickup` WITHIN 7 HENCE FULFILLED)
LEST BREACH
```

The seller can choose either option.

---

## Testing with #TRACE

`#TRACE` simulates scenarios to see what happens:

### Basic Trace

```l4
#TRACE `the sale contract` AT 0 WITH
    PARTY Seller DOES `deliver goods` AT 10
    PARTY Buyer DOES `pay invoice` 1000 AT 25
```

This simulates:

- Start at day 0
- Seller delivers at day 10 (within 14-day deadline ✓)
- Buyer pays at day 25 (within 30-day deadline ✓)

Result: `FULFILLED`

### Breach Scenario

```l4
#TRACE `the sale contract` AT 0 WITH
    PARTY Seller DOES `deliver goods` AT 20  -- Late! Deadline was 14
```

Result: `BREACH` (seller delivered late)

---

## Real-World Example: Wedding Vows

See the wedding vows example which demonstrates:

- Mutual exchange of vows
- Prohibitions (fidelity clause)
- Using `BREACH BY ... BECAUSE ...` for clear blame assignment

---

## State Graphs

L4 can visualize regulative rules as state transition diagrams:

```bash
cabal run jl4-cli -- --state-graph mycontract.l4
```

This generates a graph showing:

- **States**: Initial, intermediate, Fulfilled, Breach
- **Transitions**: Actions that move between states
- **Deadlines**: When actions must occur

The graph makes complex contracts easier to understand.

---

## Best Practices

### 1. Define Clear Actors and Actions

```l4
-- ✅ Good: Clear, descriptive types
DECLARE ContractParty IS ONE OF Landlord, Tenant
DECLARE LeaseAction IS ONE OF
    `pay rent` HAS amount IS A NUMBER
    `maintain property`
    `provide access`
    `terminate lease`
```

### 2. Use BREACH BY for Clear Blame

```l4
LEST BREACH BY Seller BECAUSE "failed to deliver on time"
```

### 3. Consider All Paths

Make sure every path leads to either FULFILLED or BREACH:

```l4
-- ✅ Good: All paths handled
MUST action
WITHIN deadline
HENCE FULFILLED
LEST BREACH

-- ❌ Bad: What happens if they don't comply?
MUST action
WITHIN deadline
HENCE FULFILLED
-- Missing LEST!
```

### 4. Test with Multiple Scenarios

```l4
-- Happy path
#TRACE contract AT 0 WITH
    PARTY Seller DOES `deliver` AT 5
    PARTY Buyer DOES `pay` AT 20

-- Late delivery
#TRACE contract AT 0 WITH
    PARTY Seller DOES `deliver` AT 20

-- No action
#TRACE contract AT 0 WITH
    -- Empty: what happens at deadline?
```

---

## Exercises

### Exercise 1: Simple Obligation

Write a regulative rule: "The employee must submit a timesheet within 7 days."

### Exercise 2: Chained Obligations

Write a contract: "Buyer must pay deposit within 7 days. After deposit, seller must deliver within 14 days."

### Exercise 3: Permission and Prohibition

Write rules for: "Tenant may have pets. Tenant shall not smoke indoors."

---

## Summary

| Concept              | Syntax                                      |
| -------------------- | ------------------------------------------- |
| Obligation           | `PARTY actor MUST action`                   |
| Permission           | `PARTY actor MAY action`                    |
| Prohibition          | `PARTY actor SHANT action`                  |
| Deadline             | `WITHIN days`                               |
| Compliance result    | `HENCE nextObligation` or `HENCE FULFILLED` |
| Non-compliance       | `LEST consequence` or `LEST BREACH`         |
| Condition            | `PROVIDED condition`                        |
| Parallel obligations | `obligation1 RAND obligation2`              |
| Alternative paths    | `obligation1 ROR obligation2`               |
| Simulate             | `#TRACE rule AT startTime WITH events`      |
| Event                | `PARTY actor DOES action AT time`           |

---

## What's Next?

In [Module 6: Putting It Together](module-6-capstone.md), you'll build a complete legal model combining everything you've learned.
