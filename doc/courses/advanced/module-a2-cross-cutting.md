# Module A2: Cross-Cutting Concerns

In this module, you'll learn patterns for concerns that span multiple rules: timing, notices, appeals, and escalation.

## Learning Objectives

By the end of this module, you will be able to:

- Model complex timing requirements
- Implement notice-and-cure patterns
- Build appeal procedures
- Create escalation chains

---

## Cross-Cutting Concerns

Some patterns appear repeatedly across legal rules:

| Concern           | Example                                        |
| ----------------- | ---------------------------------------------- |
| **Timing**        | "within 30 days", "as soon as practicable"     |
| **Notices**       | "Commissioner may issue notice", "cure period" |
| **Appeals**       | "may appeal within 21 days"                    |
| **Escalation**    | Warning → Penalty → Suspension → Removal       |
| **Grace Periods** | "10-day grace period before late fee"          |

Rather than copy-paste these patterns, we create reusable abstractions.

---

## Timing Patterns

### Basic Deadlines

```l4
-- Simple deadline: fixed number of days
GIVEN party IS A Actor
      action IS A Action
      deadline IS A NUMBER
GIVETH A DEONTIC Actor Action
`obligation within days` MEANS
    PARTY party
    MUST action
    WITHIN deadline
    HENCE FULFILLED
    LEST BREACH
```

### Relative Deadlines

Deadlines relative to events:

```l4
-- "Within 30 days of receiving notice"
GIVEN party IS A Actor
      action IS A Action
      triggerDate IS A NUMBER  -- Day number when trigger occurred
      dayLimit IS A NUMBER
GIVETH A DEONTIC Actor Action
`obligation after trigger` MEANS
    PARTY party
    MUST action
    WITHIN (triggerDate + dayLimit)
    HENCE FULFILLED
    LEST BREACH
```

### "As Soon As Practicable"

Vague statutory language requires interpretation:

```l4
-- Common interpretations of "as soon as practicable"
DECLARE PracticableDeadline IS ONE OF
    Immediate  -- Within 24 hours
    Prompt     -- Within 7 days
    Reasonable -- Within 14 days
    Extended   -- Within 30 days

GIVEN timing IS A PracticableDeadline
GIVETH A NUMBER
`practicable days` MEANS
    CONSIDER timing
    WHEN Immediate THEN 1
    WHEN Prompt THEN 7
    WHEN Reasonable THEN 14
    WHEN Extended THEN 30
```

### Business Days vs Calendar Days

```l4
-- Rough conversion: 5 business days ≈ 7 calendar days
GIVEN businessDays IS A NUMBER
GIVETH A NUMBER
`to calendar days` MEANS
    (businessDays * 7) / 5

-- 10 business days
deadline MEANS `to calendar days` 10  -- ≈ 14 calendar days
```

---

## Notice-and-Cure Pattern

Many regulations follow a notice-and-cure pattern:

1. **Violation detected**
2. **Notice issued** with deadline
3. **Cure period** to fix the issue
4. **Consequence** if not cured

### Generic Notice-and-Cure

```l4
GIVEN regulator IS A Actor
      regulated IS A Actor
      violation IS A STRING
      curePeriod IS A NUMBER
      consequenceAction IS A Action
GIVETH A DEONTIC Actor Action
`notice and cure` MEANS
    PARTY regulator
    MAY `issue warning notice` violation
    HENCE
        PARTY regulated
        MUST `cure violation`
        WITHIN curePeriod
        HENCE FULFILLED
        LEST
            PARTY regulator
            MAY consequenceAction
            HENCE BREACH BY regulated
```

### Real Example: Required Steps Notice

From the Jersey Charities Law:

```l4
-- Article 27: Required Steps Notice procedure

DECLARE RequiredStepsNotice
    HAS noticeId IS A STRING
        issuedDate IS A NUMBER
        steps IS A LIST OF STRING
        deadline IS A NUMBER

GIVEN charity IS A RegisteredCharity
      notice IS A RequiredStepsNotice
GIVETH A DEONTIC Actor Action
`required steps procedure` MEANS
    -- Commissioner issues notice
    PARTY CommissionerActor
    MUST `serve Required Steps Notice`
    HENCE
        -- Charity must comply
        PARTY CharityActor charity
        MUST `take required steps` notice
        WITHIN notice's deadline
        HENCE
            -- If complied, Commissioner MUST publish
            PARTY CommissionerActor
            MUST `publish compliance`
            WITHIN 14
            HENCE FULFILLED
            LEST BREACH BY CommissionerActor
        LEST
            -- If not complied, Commissioner MAY deregister
            PARTY CommissionerActor
            MAY `deregister charity`
            HENCE FULFILLED
    LEST BREACH BY CommissionerActor BECAUSE "failed to serve notice"
```

---

## Appeal Procedures

Appeals are a special pattern: they suspend the original decision and create a new proceeding.

### Generic Appeal Structure

```l4
-- Standard appeal pattern
DECLARE AppealOutcome IS ONE OF
    AppealAllowed
    AppealDismissed
    AppealPartiallyAllowed HAS modifications IS A STRING

GIVEN appellant IS A Actor
      appealDeadline IS A NUMBER
GIVETH A DEONTIC Actor Action
`right to appeal` MEANS
    PARTY appellant
    MAY `lodge appeal`
    WITHIN appealDeadline
    HENCE
        -- Appeal body must decide
        PARTY AppealBody
        MUST `determine appeal`
        WITHIN 90  -- Typical statutory deadline
        HENCE FULFILLED
        LEST BREACH BY AppealBody
```

### Appeal with Suspension Effect

```l4
-- Appeal suspends the original decision
GIVEN decision IS A Decision
      appellant IS A Actor
      appealDeadline IS A NUMBER
GIVETH A DEONTIC Actor Action
`suspensive appeal` MEANS
    PARTY appellant
    MAY `lodge appeal against` decision
    WITHIN appealDeadline
    HENCE
        -- Original decision is suspended
        `decision suspended` decision
        RAND
        -- Appeal must be determined
        PARTY AppealBody
        MUST `determine appeal against` decision
        WITHIN 90
        HENCE FULFILLED
        LEST BREACH
```

### Real Example: Charity Decision Appeals

```l4
-- Article 33: Appeal to Royal Court

GIVEN charity IS A RegisteredCharity
      decision IS A CommissionerDecision
GIVETH A DEONTIC Actor Action
`appeal commissioner decision` MEANS
    PARTY CharityActor charity
    MAY `appeal to Royal Court` decision
    WITHIN 21  -- 21 days from notification
    HENCE
        PARTY RoyalCourt
        MUST `hear appeal` decision
        -- No statutory deadline for hearing
        HENCE
            CONSIDER `court decision`
            WHEN Upheld THEN
                `original decision takes effect`
            WHEN Quashed THEN
                `decision has no effect`
            WHEN Varied newDecision THEN
                `varied decision takes effect` newDecision
        LEST BREACH
```

---

## Escalation Chains

Many enforcement regimes have graduated responses:

### Generic Escalation

```l4
-- Escalation: Warning → Fine → Suspension → Removal

GIVEN subject IS A Actor
GIVETH A DEONTIC Actor Action
`escalation chain` MEANS
    -- Level 1: Warning
    PARTY Regulator
    MAY `issue warning`
    HENCE
        PARTY subject
        MUST `comply with warning`
        WITHIN 30
        HENCE FULFILLED
        LEST
            -- Level 2: Fine
            PARTY Regulator
            MAY `impose fine`
            HENCE
                PARTY subject
                MUST `pay fine and comply`
                WITHIN 14
                HENCE FULFILLED
                LEST
                    -- Level 3: Suspension
                    PARTY Regulator
                    MAY `suspend registration`
                    HENCE
                        PARTY subject
                        MUST `remedy and apply for reinstatement`
                        WITHIN 60
                        HENCE FULFILLED
                        LEST
                            -- Level 4: Removal
                            PARTY Regulator
                            MUST `remove from register`
                            HENCE BREACH
```

### Escalation with Bypass

Sometimes serious violations skip early steps:

```l4
GIVEN violation IS A Violation
GIVETH A DEONTIC Actor Action
`enforcement response` MEANS
    IF violation's severity EQUALS Critical
    THEN
        -- Immediate suspension
        PARTY Regulator
        MUST `suspend immediately`
        HENCE `remediation required`
    ELSE IF violation's severity EQUALS Serious
    THEN
        -- Skip warning, go to fine
        PARTY Regulator
        MAY `impose fine`
        HENCE `escalation chain level 2`
    ELSE
        -- Start with warning
        `escalation chain`
```

---

## Grace Periods

Grace periods delay consequences:

```l4
-- Payment with grace period
GIVEN debtor IS A Actor
      amount IS A NUMBER
      dueDate IS A NUMBER
      gracePeriod IS A NUMBER
      lateFeePct IS A NUMBER
GIVETH A DEONTIC Actor Action
`payment with grace` MEANS
    PARTY debtor
    MUST `pay` amount
    WITHIN dueDate
    HENCE FULFILLED
    LEST
        -- Grace period before late fee
        PARTY debtor
        MUST `pay with late fee` (amount * (1 + lateFeePct))
        WITHIN (dueDate + gracePeriod)
        HENCE FULFILLED
        LEST BREACH
```

---

## Composition: Combining Patterns

Real procedures combine multiple patterns:

```l4
-- Complete regulatory procedure:
-- 1. Violation detected
-- 2. Warning issued (notice)
-- 3. Cure period
-- 4. If not cured, penalty with grace period
-- 5. If not paid, appeal right
-- 6. If no appeal or appeal dismissed, enforcement

GIVEN subject IS A Actor
      violation IS A Violation
GIVETH A DEONTIC Actor Action
`full enforcement procedure` MEANS
    -- Phase 1: Notice and Cure
    `notice and cure` Regulator subject (violation's description) 30 `impose penalty`
    WHERE
        -- Phase 2: Penalty with Grace
        `impose penalty` MEANS
            PARTY Regulator
            MUST `issue penalty notice` (violation's penalty)
            HENCE
                `payment with grace` subject (violation's penalty) 14 10 0.1
                RAND
                `right to appeal` subject 21
```

---

## Exercise: Model a Procedure

Encode this procedure:

> **Licence Renewal Procedure:**
>
> 1. Licence holder must apply for renewal 30 days before expiry
> 2. If not applied in time, 14-day grace period with late fee
> 3. After grace period, licence suspended
> 4. Suspended licence can be reinstated within 60 days by paying reinstatement fee
> 5. After 60 days, licence is revoked with right to appeal within 21 days

<details>
<summary>Solution</summary>

```l4
DECLARE LicenceHolder IS ONE OF Holder
DECLARE LicenceAction IS ONE OF
    `apply for renewal`
    `apply with late fee`
    `apply for reinstatement`
    `lodge appeal`

GIVEN holder IS A LicenceHolder
      expiryDate IS A NUMBER
GIVETH A DEONTIC LicenceHolder LicenceAction
`licence renewal procedure` MEANS
    PARTY holder
    MUST `apply for renewal`
    WITHIN (expiryDate - 30)  -- 30 days before expiry
    HENCE FULFILLED
    LEST
        -- Grace period with late fee
        PARTY holder
        MUST `apply with late fee`
        WITHIN (expiryDate - 30 + 14)  -- 14 day grace
        HENCE FULFILLED
        LEST
            -- Suspension
            `licence suspended`
            RAND
            -- Reinstatement period
            PARTY holder
            MAY `apply for reinstatement`
            WITHIN 60
            HENCE FULFILLED
            LEST
                -- Revocation with appeal right
                `licence revoked`
                RAND
                PARTY holder
                MAY `lodge appeal`
                WITHIN 21
                HENCE `appeal procedure`
                -- If no appeal, final
```

</details>

---

## Summary

| Pattern             | When to Use                              |
| ------------------- | ---------------------------------------- |
| **Timing**          | Any deadline requirement                 |
| **Notice-and-Cure** | Violations with opportunity to fix       |
| **Appeals**         | Decisions that can be challenged         |
| **Escalation**      | Graduated enforcement responses          |
| **Grace Periods**   | Soft deadlines with delayed consequences |

Key principle: **Abstract common patterns** into reusable functions rather than duplicating code.

---

## What's Next?

In [Module A3: Contracts in Depth](module-a3-contracts.md), you'll learn advanced contract modeling including complex payment terms, recursive obligations, and penalty structures.
