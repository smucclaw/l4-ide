# Regulative Rules

How L4 models obligations, permissions, and prohibitions.

---

## What Are Regulative Rules?

**Regulative rules** govern behavior—they tell parties what they must, may, or must not do. They're the foundation of contracts, regulations, and laws.

Compare to **constitutive rules** which define what things are (e.g., DECLARE and DECIDE statements).

### Examples

| Rule Type   | Legal Text                                              | L4 Concept |
| ----------- | ------------------------------------------------------- | ---------- |
| Obligation  | "The seller shall deliver the goods"                    | MUST       |
| Permission  | "The buyer may inspect the goods"                       | MAY        |
| Prohibition | "Employees shall not disclose confidential information" | SHANT      |

---

## The DEONTIC Type

L4 represents regulative rules with the `DEONTIC` type. A DEONTIC value captures:

1. **Who** has the obligation/permission (PARTY)
2. **What** action is required/permitted (MUST/MAY/SHANT)
3. **When** it must occur (WITHIN)
4. **What happens** on compliance (HENCE)
5. **What happens** on breach (LEST)

```l4
GIVETH DEONTIC Actor Action
`delivery obligation` MEANS
    PARTY Seller
    MUST `deliver goods`
    WITHIN 14
    HENCE `payment due`
    LEST BREACH BY Seller
```

---

## Obligations: MUST

An **obligation** requires a party to perform an action.

```l4
PARTY Seller
MUST `deliver goods`
```

### Consequences

If the party performs the action: the obligation is discharged (HENCE).
If the party fails to perform: breach occurs (LEST).

```l4
PARTY Seller
MUST `deliver goods`
WITHIN 14
HENCE FULFILLED          -- If delivered, done
LEST BREACH BY Seller    -- If not delivered, breach
```

### Conditional Obligations

Use `PROVIDED` to add conditions:

```l4
PARTY Buyer
MUST `pay amount` PROVIDED amount > 0
WITHIN 30
HENCE FULFILLED
LEST BREACH
```

The obligation only applies if the condition is true.

---

## Permissions: MAY

A **permission** allows a party to perform an action (but doesn't require it).

```l4
PARTY Buyer
MAY `inspect goods`
HENCE FULFILLED
```

### Key Difference from MUST

Not exercising a permission is **not** a breach:

| Action        | MUST               | MAY             |
| ------------- | ------------------ | --------------- |
| Performed     | HENCE path         | HENCE path      |
| Not performed | LEST path (breach) | Nothing happens |

### Use Cases

- Optional rights ("buyer may return within 30 days")
- Discretionary powers ("commissioner may issue notice")
- Conditional entitlements ("may claim refund if defective")

---

## Prohibitions: SHANT

A **prohibition** forbids a party from performing an action.

```l4
PARTY Employee
SHANT `disclose confidential information`
HENCE FULFILLED
LEST BREACH BY Employee
```

### How Prohibitions Work

- If the party **does not** perform the action: HENCE path (compliance)
- If the party **performs** the prohibited action: LEST path (breach)

This is the opposite of MUST:

|                      | MUST       | SHANT      |
| -------------------- | ---------- | ---------- |
| Action performed     | Compliance | Breach     |
| Action not performed | Breach     | Compliance |

---

## Deadlines: WITHIN

Most obligations have deadlines:

```l4
PARTY Seller
MUST `deliver goods`
WITHIN 14  -- 14 days
```

### What Happens at Deadline?

- If action performed before deadline: HENCE path
- If deadline passes without action: LEST path

### No Deadline

Some obligations have no deadline (ongoing duties):

```l4
PARTY Employee
MUST `maintain confidentiality`
-- No WITHIN: ongoing obligation
HENCE FULFILLED
LEST BREACH
```

---

## Consequences: HENCE and LEST

Every regulative rule specifies consequences:

### HENCE: On Compliance

```l4
HENCE FULFILLED              -- Contract complete
HENCE `next obligation`      -- Chain to another rule
HENCE obligation1 RAND obligation2  -- Multiple follow-ons
```

### LEST: On Breach

```l4
LEST BREACH                  -- Simple breach
LEST BREACH BY Seller        -- Breach by specific party
LEST BREACH BY Seller BECAUSE "late delivery"  -- With reason
LEST `penalty clause`        -- Alternative obligation
```

---

## Chaining Obligations

Real contracts have sequences of obligations. Use HENCE to chain:

```l4
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

This creates a chain:

1. Seller must deliver
2. **If delivered**: Buyer must pay
3. **If not delivered**: Seller is in breach

---

## Parallel Obligations: RAND

When multiple obligations must all be fulfilled:

```l4
(PARTY Seller MUST `deliver goods` WITHIN 14 HENCE FULFILLED LEST BREACH)
RAND
(PARTY Seller MUST `provide warranty` WITHIN 14 HENCE FULFILLED LEST BREACH)
```

- Both obligations run in parallel
- **All** must be fulfilled for the combined obligation to be fulfilled
- **Any** breach causes the combined obligation to breach

---

## Alternative Obligations: ROR

When fulfilling any one obligation is sufficient:

```l4
(PARTY Seller MUST `ship goods` WITHIN 14 HENCE FULFILLED)
ROR
(PARTY Seller MUST `arrange pickup` WITHIN 7 HENCE FULFILLED)
LEST BREACH
```

- Fulfilling **either** one satisfies the combined obligation
- Only if **both** fail does breach occur

---

## The Regulative Rule Lifecycle

```
┌──────────────────────────────────────────────┐
│                  CREATED                      │
│           (rule becomes active)               │
└──────────────────────────────────────────────┘
                      │
                      ▼
          ┌───────────────────────┐
          │    PENDING ACTION     │
          │  (waiting for party   │
          │   to act or deadline) │
          └───────────────────────┘
                      │
        ┌─────────────┼─────────────┐
        │             │             │
        ▼             ▼             ▼
   ┌─────────┐  ┌─────────────┐  ┌──────────┐
   │ ACTION  │  │  DEADLINE   │  │ DIFFERENT│
   │PERFORMED│  │   PASSED    │  │ ACTION   │
   └────┬────┘  └──────┬──────┘  └────┬─────┘
        │              │              │
        ▼              ▼              ▼
   ┌─────────┐  ┌─────────────┐  ┌──────────┐
   │  HENCE  │  │    LEST     │  │   (no    │
   │  path   │  │    path     │  │  effect) │
   └─────────┘  └─────────────┘  └──────────┘
```

---

## Testing with #TRACE

Use `#TRACE` to simulate scenarios:

```l4
-- Happy path
#TRACE `delivery obligation` AT 0 WITH
    PARTY Seller DOES `deliver goods` AT 10
-- Result: HENCE path (FULFILLED)

-- Breach: no delivery
#TRACE `delivery obligation` AT 0 WITH
    -- No actions
-- Result: LEST path (BREACH after day 14)

-- Wrong party acts
#TRACE `delivery obligation` AT 0 WITH
    PARTY Buyer DOES `deliver goods` AT 10
-- Result: No effect (Buyer isn't the obligated party)
```

---

## State Graphs

L4 can visualize regulative rules as state machines:

```bash
cabal run jl4-cli -- --state-graph mycontract.l4
```

This shows:

- **States**: Initial, pending, fulfilled, breach
- **Transitions**: What actions cause state changes
- **Deadlines**: When automatic transitions occur

---

## Best Practices

### 1. Always Handle Both Paths

```l4
-- ✅ Good: Both paths specified
MUST action
HENCE FULFILLED
LEST BREACH

-- ❌ Bad: Missing LEST
MUST action
HENCE FULFILLED
```

### 2. Be Explicit About Who Breaches

```l4
-- ✅ Good: Clear who breaches
LEST BREACH BY Seller BECAUSE "failed to deliver"

-- Less clear
LEST BREACH
```

### 3. Use Meaningful Action Names

```l4
-- ✅ Good: Matches legal text
MUST `deliver goods in accordance with specification`

-- Less clear
MUST deliver
```

---

## Summary

| Concept | Purpose                 | Syntax                 |
| ------- | ----------------------- | ---------------------- |
| MUST    | Obligation              | `PARTY x MUST action`  |
| MAY     | Permission              | `PARTY x MAY action`   |
| SHANT   | Prohibition             | `PARTY x SHANT action` |
| WITHIN  | Deadline                | `WITHIN days`          |
| HENCE   | On compliance           | `HENCE nextRule`       |
| LEST    | On breach               | `LEST consequence`     |
| RAND    | Parallel obligations    | `rule1 RAND rule2`     |
| ROR     | Alternative obligations | `rule1 ROR rule2`      |

---

## Further Reading

- [Foundation Course Module 5](../../courses/foundation/module-5-regulative.md) - Hands-on tutorial
- [Keywords: PARTY](../../reference/regulative/PARTY.md) - Reference for PARTY keyword
- [Keywords: MUST](../../reference/regulative/MUST.md) - Reference for MUST keyword
