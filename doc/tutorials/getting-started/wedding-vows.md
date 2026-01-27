# Wedding Vows Example

A fun introduction to regulative rules using wedding vows.

**Audience:** Anyone new to regulative rules  
**Prerequisites:** Basic L4 knowledge  
**Time:** 20 minutes  
**Goal:** Understand MUST, MAY, SHANT, HENCE, and LEST through a familiar example

---

## What You'll Build

Traditional wedding vows formalized as L4 regulative rules.

**Complete example:** [wedding-vows-example.l4](wedding-vows-example.l4)

The vows:

> "I take you to be my wedded spouse, to have and to hold from this day forward, for better, for worse, for richer, for poorer, in sickness and in health, to love and to cherish, till death do us part."

---

## Step 1: Define Actors and Actions

First, define who can act and what actions exist:

```l4
§ `Wedding Vows`

-- The parties to the marriage
DECLARE Person IS ONE OF Spouse1, Spouse2

-- The actions in the marriage "contract"
DECLARE Action IS ONE OF
    `exchange vows`
    `love and cherish`
    `have and hold`
    `support`
    `care for`
    abandon
    `be unfaithful`
```

### Why IS ONE OF?

Marriage is between two specific parties (Spouse1 and Spouse2). The enumeration ensures we can only refer to these two actors.

---

## Step 2: The Ceremony

The wedding ceremony requires both parties to exchange vows:

```l4
§§ `The Ceremony`

-- The ceremony: exchange of vows
GIVETH DEONTIC Person Action
`wedding ceremony` MEANS
    PARTY Spouse1
    MUST `exchange vows`
    WITHIN 1
    HENCE
        PARTY Spouse2
        MUST `exchange vows`
        WITHIN 1
        HENCE FULFILLED
        LEST BREACH BY Spouse2 BECAUSE "failed to exchange vows"
    LEST BREACH BY Spouse1 BECAUSE "failed to exchange vows"
```

### Breaking It Down

| Code                           | Meaning                                                                  |
| ------------------------------ | ------------------------------------------------------------------------ |
| `GIVETH DEONTIC Person Action` | This returns an obligation/permission involving these actors and actions |
| `PARTY Spouse1`                | Spouse1 is the one with the obligation                                   |
| `MUST \`exchange vows\``       | They must exchange vows                                                  |
| `WITHIN 1`                     | Within 1 day (the wedding day)                                           |
| `HENCE ...`                    | If they do, then Spouse2 must respond                                    |
| `LEST BREACH`                  | If they don't, it's a breach                                             |

---

## Step 3: Core Obligations

"To love and to cherish":

```l4
§§ `Core Obligations`

-- "To love and to cherish" - Spouse1's obligation
GIVETH DEONTIC Person Action
`Spouse1 loves Spouse2` MEANS
    PARTY Spouse1
    MUST `love and cherish`
    HENCE FULFILLED
    LEST BREACH BY Spouse1 BECAUSE "failed to love and cherish"

-- "To love and to cherish" - Spouse2's obligation (mutual)
GIVETH DEONTIC Person Action
`Spouse2 loves Spouse1` MEANS
    PARTY Spouse2
    MUST `love and cherish`
    HENCE FULFILLED
    LEST BREACH BY Spouse2 BECAUSE "failed to love and cherish"

-- "To have and to hold" - mutual obligations
GIVETH DEONTIC Person Action
`Spouse1 holds Spouse2` MEANS
    PARTY Spouse1
    MUST `have and hold`
    HENCE FULFILLED
    LEST BREACH BY Spouse1 BECAUSE "failed to have and hold"

GIVETH DEONTIC Person Action
`Spouse2 holds Spouse1` MEANS
    PARTY Spouse2
    MUST `have and hold`
    HENCE FULFILLED
    LEST BREACH BY Spouse2 BECAUSE "failed to have and hold"
```

---

## Step 4: Conditional Obligations

"For richer, for poorer, in sickness and in health":

```l4
§§ `Conditional Obligations`

-- "For richer or poorer" - support obligation
GIVETH DEONTIC Person Action
`support in all circumstances` MEANS
    PARTY Spouse1
    MUST `support`
    HENCE FULFILLED
    LEST BREACH BECAUSE "failed to support through richer or poorer"

-- "In sickness and in health" - care obligation
GIVETH DEONTIC Person Action
`care in sickness and health` MEANS
    PARTY Spouse1
    MUST `care for`
    HENCE FULFILLED
    LEST BREACH BECAUSE "failed to care in sickness and health"
```

---

## Step 5: Prohibitions

"Forsaking all others" implies prohibitions:

```l4
§§ `Prohibitions`

-- Prohibition on abandonment
GIVETH DEONTIC Person Action
`no abandonment` MEANS
    PARTY Spouse1
    SHANT abandon
    HENCE FULFILLED
    LEST BREACH BY Spouse1 BECAUSE "abandoned spouse"

-- Implied fidelity obligation
GIVETH DEONTIC Person Action
`fidelity` MEANS
    PARTY Spouse1
    SHANT `be unfaithful`
    HENCE FULFILLED
    LEST BREACH BY Spouse1 BECAUSE "was unfaithful"
```

### MUST vs SHANT

| Keyword | Meaning           | Example                 |
| ------- | ----------------- | ----------------------- |
| `MUST`  | Required action   | Must love and cherish   |
| `MAY`   | Permitted action  | May have pets           |
| `SHANT` | Prohibited action | Shall not be unfaithful |

---

## Step 6: Permissions

"Till death do us part" - death releases the vows:

```l4
§§ `Termination`

-- The marriage ends upon death - this is a permission
GIVETH DEONTIC Person Action
`death releases vows` MEANS
    PARTY Spouse1
    MAY `exchange vows`  -- May remarry after death of spouse
    HENCE FULFILLED
```

---

## Step 7: The Complete Contract

Combine all obligations using `RAND` (regulative AND):

```l4
§§ `Combined Marriage Contract`

-- The full marriage as parallel obligations
GIVETH DEONTIC Person Action
`marriage contract` MEANS
    (PARTY Spouse1 MUST `love and cherish` HENCE FULFILLED LEST BREACH)
    RAND
    (PARTY Spouse1 MUST `have and hold` HENCE FULFILLED LEST BREACH)
    RAND
    (PARTY Spouse2 MUST `love and cherish` HENCE FULFILLED LEST BREACH)
    RAND
    (PARTY Spouse2 MUST `have and hold` HENCE FULFILLED LEST BREACH)
    RAND
    (PARTY Spouse1 SHANT `be unfaithful` HENCE FULFILLED LEST BREACH)
    RAND
    (PARTY Spouse2 SHANT `be unfaithful` HENCE FULFILLED LEST BREACH)
```

### What is RAND?

`RAND` means "regulative AND" - **all** obligations must be fulfilled for the contract to be fulfilled. If any one is breached, the whole contract is in breach.

---

## Step 8: Test the Contract

Use `#TRACE` to simulate scenarios:

```l4
§§ `Tests`

-- Happy marriage: both fulfill their vows
#TRACE `wedding ceremony` AT 0 WITH
    PARTY Spouse1 DOES `exchange vows` AT 0
    PARTY Spouse2 DOES `exchange vows` AT 0
-- Result: FULFILLED

-- Sad scenario: Spouse1 doesn't show up
#TRACE `wedding ceremony` AT 0 WITH
    -- No events
-- Result: BREACH BY Spouse1

-- Breach of vows
#TRACE `fidelity` AT 0 WITH
    PARTY Spouse1 DOES `be unfaithful` AT 100
-- Result: BREACH BY Spouse1 BECAUSE "was unfaithful"
```

---

## Complete Example

```l4
§ `Wedding Vows as L4 Regulative Rules`

-- Actors
DECLARE Person IS ONE OF Spouse1, Spouse2

-- Actions
DECLARE Action IS ONE OF
    `exchange vows`
    `love and cherish`
    `have and hold`
    `support`
    `care for`
    abandon
    `be unfaithful`

-- The ceremony
GIVETH DEONTIC Person Action
`wedding ceremony` MEANS
    PARTY Spouse1
    MUST `exchange vows`
    WITHIN 1
    HENCE
        PARTY Spouse2
        MUST `exchange vows`
        WITHIN 1
        HENCE FULFILLED
        LEST BREACH BY Spouse2 BECAUSE "failed to exchange vows"
    LEST BREACH BY Spouse1 BECAUSE "failed to exchange vows"

-- Core vows
GIVETH DEONTIC Person Action
`love obligation` MEANS
    (PARTY Spouse1 MUST `love and cherish` HENCE FULFILLED LEST BREACH)
    RAND
    (PARTY Spouse2 MUST `love and cherish` HENCE FULFILLED LEST BREACH)

-- Prohibitions
GIVETH DEONTIC Person Action
`fidelity clause` MEANS
    (PARTY Spouse1 SHANT `be unfaithful` HENCE FULFILLED LEST BREACH)
    RAND
    (PARTY Spouse2 SHANT `be unfaithful` HENCE FULFILLED LEST BREACH)

-- Test
#TRACE `wedding ceremony` AT 0 WITH
    PARTY Spouse1 DOES `exchange vows` AT 0
    PARTY Spouse2 DOES `exchange vows` AT 0
```

---

## What You Learned

- **MUST** creates obligations
- **MAY** creates permissions
- **SHANT** creates prohibitions
- **HENCE** specifies what happens on compliance
- **LEST** specifies what happens on breach
- **RAND** combines parallel obligations
- **#TRACE** simulates scenarios

---

## Real-World Applications

The same patterns apply to:

- Employment contracts (employee MUST report for work)
- Lease agreements (tenant SHANT sublease without permission)
- Service agreements (provider MUST deliver service WITHIN 30)
- Regulatory compliance (company MUST file report WITHIN 60)

---

## Next Steps

- [Foundation Course Module 5](../../courses/foundation/module-5-regulative.md) - Deep dive on regulative rules
- [Common Patterns](common-patterns.md) - More L4 patterns
- [Encoding Legislation](encoding-legislation.md) - Turn legal text into L4
