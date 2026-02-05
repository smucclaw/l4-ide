# Module A1: Real Regulatory Schemes

In this module, you'll learn how to model complete legislative frameworks using a systematic three-layer approach. Find a working example at the end.

## Learning Objectives

By the end of this module, you will be able to:

- Apply the three-layer approach to legislative encoding
- Extract type definitions from statutory text
- Model deontic rules with proper actors and triggers
- Track state transitions and register events

---

## The Three-Layer Approach

When encoding legislation, we organize rules into three layers:

| Layer             | What It Contains                       | L4 Constructs     |
| ----------------- | -------------------------------------- | ----------------- |
| **A: Structural** | Definitions, types, enums              | DECLARE, glossary |
| **B: Deontic**    | Obligations, permissions, prohibitions | MUST, MAY, SHANT  |
| **C: Events**     | State transitions, register updates    | Actions, effects  |

This mirrors how legislation is typically structured:

- Early sections define terms
- Middle sections create duties and powers
- Later sections specify procedures and consequences

---

## Case Study: Charity Registration

We'll use the Jersey Charities Law as a running example. This real legislation covers:

- What qualifies as a charity
- Who can be a governor
- Filing requirements
- Enforcement powers

### Starting Point: The Legislation

From the Charities (Jersey) Law 2014:

> **Article 2 - Definitions**
> "charitable purpose" means any of the purposes specified in Schedule 1
> "governor" means any person who is responsible for the control and management of the administration of a registered charity
> "misconduct" includes mismanagement or misapplication of charity property

> **Article 11 - Registration**
> A charity may apply to the Commissioner for registration by providing:
> (a) its constitution
> (b) a statement of its charitable purposes
> (c) evidence of public benefit
> (d) core financial information

---

## Layer A: Structural Definitions

First, encode the definitions and types:

```l4
§ `Structural Layer - Definitions`

-- Article 2(10): "misconduct" includes mismanagement or misapplication
DECLARE MisconductType IS ONE OF
    Mismanagement
    Misapplication
    OtherMisconduct HAS description IS A STRING

-- Schedule 1: Charitable purposes (13 statutory heads)
DECLARE CharitablePurpose IS ONE OF
    `prevention or relief of poverty`
    `advancement of education`
    `advancement of religion`
    `advancement of health or saving of lives`
    `advancement of citizenship or community development`
    `advancement of arts, culture, heritage or science`
    `advancement of amateur sport`
    `advancement of human rights, conflict resolution, reconciliation`
    `advancement of environmental protection or improvement`
    `relief of those in need`
    `advancement of animal welfare`
    `purposes analogous to charitable purposes`
    `other charitable purpose` HAS description IS A STRING

-- Article 2: Governor definition
DECLARE Governor HAS
    name IS A STRING
    dateOfBirth IS A DATE
    address IS A STRING
    isBankrupt IS A BOOLEAN
    convictions IS A LIST OF Conviction

-- Core financial information (Regulation 1, Core Info Regs 2018)
DECLARE CoreFinancialInfo HAS
    income IS A Money
    expenditure IS A Money
    openingAssets IS A Money
    closingAssets IS A Money
    otherAssets IS A LIST OF Asset

-- Register sections
DECLARE RegisterSection IS ONE OF
    GeneralSection
    RestrictedSection

-- Charity status
DECLARE CharityStatus IS ONE OF
    Pending
    Active
    Suspended HAS
        reason IS A STRING
        date IS A DATE
    Deregistered HAS
        reason IS A STRING
        date IS A DATE
        isRetrospective IS A BOOLEAN

-- Main charity record
DECLARE RegisteredCharity HAS
    name IS A STRING
    registrationNumber IS A STRING
    section IS A RegisterSection
    status IS A CharityStatus
    constitution IS A STRING
    purposes IS A LIST OF CharitablePurpose
    publicBenefitStatement IS A STRING
    governors IS A LIST OF Governor
    financials IS A CoreFinancialInfo
    registrationDate IS A DATE
```

### Key Principle: Glossary-First

Notice how we encode the statutory glossary (Article 2) as types. This:

1. **Prevents ambiguity** - `MisconductType` has exactly three variants
2. **Enables validation** - Can only use defined purposes
3. **Documents the source** - Comments link to legislation

---

## Layer B: Deontic Rules

Now encode the obligations, permissions, and prohibitions:

```l4
§ `Deontic Layer - Rules`

-- Actors in the regulatory system
DECLARE Actor IS ONE OF
    CharityActor HAS charity IS A RegisteredCharity
    GovernorActor HAS governor IS A Governor
    CommissionerActor
    ApplicantActor HAS applicant IS A Applicant

-- Actions that can be performed
DECLARE Action IS ONE OF
    -- Charity obligations
    `file annual return`
    `report change of particulars`
    `report reportable matter` HAS
        matter IS A ReportableMatter
    -- Commissioner powers
    `demand information`
    `issue Required Steps Notice`
    `suspend governor` HAS
        reason IS A STRING
    `deregister charity` HAS
        reason IS A STRING
    -- Governor obligations
    `act in best interests`
    `report conviction`
    -- Appeal actions
    `lodge appeal`
```

### Encoding Individual Rules

Each statutory rule becomes a function:

```l4
-- B-AR-01: Annual Return Obligation
-- Article 13(7)-(10) + Timing Order 2019
-- "A registered charity must file an annual return within 2 months of year end"

GIVEN charity IS A RegisteredCharity
GIVETH A DEONTIC Actor Action
`annual return obligation` MEANS
    IF charity's status EQUALS Active
    THEN
        PARTY CharityActor charity
        MUST `file annual return`
        WITHIN 60  -- 2 months ≈ 60 days
        HENCE FULFILLED
        LEST `Commissioner may issue notice` charity
    ELSE FULFILLED

-- B-RSN-01: Commissioner's Power to Issue Notice
-- Article 27(1)-(4)

GIVEN charity IS A RegisteredCharity
GIVETH A DEONTIC Actor Action
`Commissioner may issue notice` MEANS
    PARTY CommissionerActor
    MAY `issue Required Steps Notice`
    HENCE `charity must comply with notice` charity
```

### Rule Identification

Use consistent identifiers for traceability:

| ID       | Type      | Description              |
| -------- | --------- | ------------------------ |
| B-AR-01  | CONDUCT   | Annual return filing     |
| B-AR-02  | CONDUCT   | Commissioner publication |
| B-RSN-01 | PROCEDURE | Required Steps Notice    |
| B-GOV-01 | CONDUCT   | Governor best interests  |

---

## Layer C: Events and State Transitions

Track what happens when actions occur:

```l4
§ `Event Layer - State Transitions`

-- Events that change the register
DECLARE RegisterEvent IS ONE OF
    CharityRegistered HAS
        charity IS A RegisteredCharity
        date IS A DATE
    CharityMovedToRestricted HAS
        charity IS A RegisteredCharity
        date IS A DATE
    CharityDeregistered HAS
        charity IS A RegisteredCharity
        reason IS A STRING
        date IS A DATE
        isRetrospective IS A BOOLEAN
    AnnualReturnFiled HAS
        charity IS A RegisteredCharity
        year IS A NUMBER
        wasLate IS A BOOLEAN
    RequiredStepsNoticeIssued HAS
        charity IS A RegisteredCharity
        noticeId IS A STRING
        deadline IS A DATE
    GovernorSuspended HAS
        governor IS A Governor
        charity IS A RegisteredCharity
        reason IS A STRING
        period IS A NUMBER

-- Effect of events on register
GIVEN event IS A RegisterEvent
GIVETH A STRING  -- Describes the effect
`event effect` MEANS
    CONSIDER event
    WHEN CharityRegistered c d THEN
        "New active entry created in register"
    WHEN CharityDeregistered c r d retro THEN
        IF retro
        THEN "Entry moved to historic; registration void from earlier date"
        ELSE "Entry moved to historic"
    WHEN AnnualReturnFiled c y late THEN
        IF late
        THEN "Annual return logged with late flag"
        ELSE "Annual return logged"
    WHEN RequiredStepsNoticeIssued c nid deadline THEN
        "Notice reference stored under Art 8(3)(k)"
    OTHERWISE "Register updated"
```

---

## Handling Cross-References

Legislation often cross-references between sections. Model these explicitly:

```l4
-- The charity test (Article 5) references:
-- - Schedule 1 (purposes)
-- - Article 7 (public benefit)
-- - Regulations (core financial info)

GIVEN charity IS A RegisteredCharity
GIVETH A BOOLEAN
DECIDE `meets charity test` IF
    `has charitable purposes` charity              -- Schedule 1
    AND `provides public benefit` charity          -- Article 7
    AND `has valid constitution` charity           -- Article 11(2)(a)
    AND `has complete financial info` charity      -- Core Info Regs

-- Article 7: Public benefit factors
GIVEN charity IS A RegisteredCharity
GIVETH A BOOLEAN
DECIDE `provides public benefit` IF
    `has identifiable benefit` charity             -- Art 7(2)(a)
    AND `benefit outweighs detriment` charity      -- Art 7(2)(b)
    AND NOT `unduly restricts beneficiaries` charity  -- Art 7(3)
```

---

## Handling Amendments

Legislation changes over time. Track versions:

```l4
-- Original Law 2014 had 12 charitable purposes
-- R&O 27/2025 added "advancement of animal welfare"

-- Model with effective dates:
DECLARE PurposeWithDate HAS
    purpose IS A CharitablePurpose
    effectiveFrom IS A DATE

-- Check if purpose was valid at a given date
GIVEN purpose IS A CharitablePurpose
      asOfDate IS A DATE
GIVETH A BOOLEAN
DECIDE `purpose valid at date` IF
    -- Animal welfare only valid from 2025
    IF purpose EQUALS `advancement of animal welfare`
    THEN asOfDate >= Date 1 1 2025
    ELSE TRUE  -- Other purposes valid from 2014
```

---

## Exercise: Encode a Rule

Encode this statutory requirement:

> **Article 19(1)**: A governor must notify the Commissioner as soon as practicable if any of the following matters applies to them:
> (a) bankruptcy
> (b) disqualification as a company director
> (c) conviction for an offence involving dishonesty

<details>
<summary>Solution</summary>

```l4
-- Reportable matters under Article 19(1)
DECLARE ReportableMatter IS ONE OF
    Bankruptcy HAS
        date IS A DATE
    DirectorDisqualification HAS
        date IS A DATE
        jurisdiction IS A STRING
    DishonestConviction HAS
        description IS A STRING
        date IS A DATE

-- B-GOV-02: Governor reporting obligation
GIVEN governor IS A Governor
      matter IS A ReportableMatter
GIVETH A DEONTIC Actor Action
`governor reporting obligation` MEANS
    PARTY GovernorActor governor
    MUST `report reportable matter` matter
    WITHIN 14  -- "as soon as practicable" interpreted as 14 days
    HENCE FULFILLED
    LEST `commissioner may suspend` governor
```

</details>

---

## Full Example

[module-a1-regulatory-examples.l4](module-a1-regulatory-examples.l4)

---

## Summary

| Layer             | Purpose     | L4 Approach                 |
| ----------------- | ----------- | --------------------------- |
| **A: Structural** | Definitions | DECLARE types from glossary |
| **B: Deontic**    | Rules       | MUST/MAY/SHANT functions    |
| **C: Events**     | Transitions | Event types + effects       |

Key practices:

- Start with the statutory glossary (definitions section)
- Assign rule IDs for traceability
- Model cross-references explicitly
- Track temporal validity for amendments

---

## What's Next?

In [Module A2: Cross-Cutting Concerns](module-a2-cross-cutting.md), you'll learn patterns for timing, notices, appeals, and other concerns that span multiple rules.
