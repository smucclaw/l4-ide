# Module A3: Contracts in Depth

In this module, you'll learn advanced contract modeling including complex payment terms, recursive obligations, and penalty structures. Find the full working example at the bottom.

## Learning Objectives

By the end of this module, you will be able to:

- Model complex payment schedules
- Implement recursive payment obligations
- Create penalty and interest calculations
- Handle multi-party contracts

---

## Contract Modeling Principles

Well-modeled contracts in L4 follow these principles:

1. **Clear parties**: Who has obligations?
2. **Precise actions**: What must be done?
3. **Explicit conditions**: When do obligations apply?
4. **Complete paths**: Every scenario leads to FULFILLED or BREACH
5. **Testable**: Use #TRACE to verify behavior

---

## Case Study: Promissory Note

We'll build a complete promissory note with:

- Principal and interest
- Monthly installments
- Late payment penalties
- Default provisions

### Step 1: Define Types

```l4
-- Money type with currency
DECLARE Money
    HAS amount IS A NUMBER
        currency IS A STRING

-- Currency constructors
GIVEN n IS A NUMBER
GIVETH A Money
USD MEANS Money WITH amount IS n, currency IS "USD"

GIVEN n IS A NUMBER
GIVETH A Money
SGD MEANS Money WITH amount IS n, currency IS "SGD"

-- Party types
DECLARE Party IS ONE OF
    BorrowerParty HAS `the borrower` IS A Borrower
    LenderParty HAS `the lender` IS A Lender

-- Borrower can be individual or company
DECLARE Borrower IS ONE OF
    IndividualBorrower HAS person IS A Person
    CorporateBorrower HAS company IS A Company

DECLARE Lender IS ONE OF
    IndividualLender HAS person IS A Person
    InstitutionalLender HAS company IS A Company

-- Payment record
DECLARE Payment
    HAS amount IS A Money
        dueDate IS A NUMBER  -- Days from commencement

-- Penalty terms
DECLARE PenaltyTerms
    HAS interestRate IS A NUMBER
        gracePeriodDays IS A NUMBER
```

### Step 2: Define Loan Terms

```l4
DECLARE LoanTerms
    HAS principal IS A Money
        annualInterestRate IS A NUMBER
        numberOfInstallments IS A NUMBER
        defaultAfterDays IS A NUMBER
        penaltyTerms IS A PenaltyTerms

-- Example loan
exampleLoan MEANS LoanTerms WITH
    principal            IS USD 25000
    annualInterestRate   IS 0.15             -- 15% annual interest
    numberOfInstallments IS 12               -- 12 monthly installments
    defaultAfterDays     IS 30               -- Default after 30 days late
    penaltyTerms         IS PenaltyTerms WITH
                              interestRate    IS 0.05   -- 5% penalty
                              gracePeriodDays IS 10
```

### Step 3: Calculate Payment Schedule

```l4
-- Monthly interest rate
GIVEN terms IS A LoanTerms
GIVETH A NUMBER
`monthly rate` MEANS terms's annualInterestRate / 12

-- Power function for calculations
GIVEN base IS A NUMBER
      exp IS A NUMBER
GIVETH A NUMBER
`power` MEANS
    IF exp = 0
    THEN 1
    ELSE IF exp = 1
         THEN base
         ELSE base * `power` base (exp - 1)

-- Monthly payment using amortization formula
-- PMT = P × (r(1+r)^n) / ((1+r)^n - 1)
GIVEN terms IS A LoanTerms
GIVETH A Money
`monthly payment amount` MEANS
    Money WITH amount IS payment, currency IS terms's principal's currency
    WHERE
        p MEANS terms's principal's amount
        r MEANS `monthly rate` terms
        n MEANS terms's numberOfInstallments
        compoundFactor MEANS `power` (1 + r) n
        payment MEANS p * (r * compoundFactor) / (compoundFactor - 1)
```

---

## Recursive Payment Obligations

The key insight: **payment obligations are recursive**. After each payment, there's another payment (until paid off).

```l4
-- Actions for the loan contract
DECLARE LoanAction IS ONE OF
    `pay installment` HAS recipient IS A Lender
                          amount IS A Money

-- Validate payment amount
GIVEN paid IS A Money
      expected IS A Money
GIVETH A BOOLEAN
`is valid payment` MEANS
    paid's currency EQUALS expected's currency
    AND paid's amount >= (expected's amount - 0.05)  -- Allow small rounding

-- The recursive payment obligation
GIVEN terms IS A LoanTerms
      debtorParty IS A Borrower
      creditorParty IS A Lender
      outstanding IS A Money  -- Remaining balance
GIVETH A DEONTIC Party LoanAction
`payment obligation` MEANS
    IF outstanding's amount > 0
    THEN
        PARTY BorrowerParty debtorParty
        MUST `pay installment` EXACTLY creditorParty
                               amountPaid PROVIDED amountPaid's amount >= paymentDue's amount
        WITHIN nextDueDate
        HENCE
            -- Recursive call with reduced balance
            `payment obligation` terms debtorParty creditorParty newOutstanding
        LEST
            -- Late: apply penalty
            `late payment handling` terms debtorParty creditorParty outstanding
    ELSE FULFILLED
    WHERE
        -- Calculate next payment
        paymentDue MEANS `calculate next payment` terms outstanding
        nextDueDate MEANS `calculate due date` terms outstanding
        -- After payment, reduce outstanding (simplified)
        newOutstanding MEANS Money WITH
            amount   IS outstanding's amount - paymentDue's amount
            currency IS outstanding's currency
```

### Late Payment with Penalty

```l4
GIVEN terms IS A LoanTerms
      debtorParty IS A Borrower
      creditorParty IS A Lender
      outstanding IS A Money
GIVETH A DEONTIC Party LoanAction
`late payment handling` MEANS
    -- Grace period with penalty interest
    PARTY BorrowerParty debtorParty
    MUST `pay installment` EXACTLY creditorParty
                           amountPaid PROVIDED amountPaid's amount >= paymentWithPenalty's amount
    WITHIN (nextDueDate + terms's penaltyTerms's gracePeriodDays)
    HENCE
        -- Continue with remaining payments
        `payment obligation` terms debtorParty creditorParty newOutstanding
    LEST
        -- Default: full balance due
        `default handling` terms debtorParty creditorParty outstanding
    WHERE
        basePayment MEANS `calculate next payment` terms outstanding
        penaltyAmount MEANS basePayment's amount * terms's penaltyTerms's interestRate
        paymentWithPenalty MEANS Money WITH
            amount   IS basePayment's amount + penaltyAmount
            currency IS basePayment's currency
        nextDueDate MEANS `calculate due date` terms outstanding
        newOutstanding MEANS Money WITH
            amount   IS outstanding's amount - paymentWithPenalty's amount
            currency IS outstanding's currency
```

### Default: Full Balance Due

```l4
GIVEN terms IS A LoanTerms
      debtorParty IS A Borrower
      creditorParty IS A Lender
      outstanding IS A Money
GIVETH A DEONTIC Party LoanAction
`default handling` MEANS
    PARTY BorrowerParty debtorParty
    MUST `pay installment` EXACTLY creditorParty
                           EXACTLY outstanding  -- Full balance required
    WITHIN terms's defaultAfterDays
    HENCE FULFILLED
    LEST BREACH BY (BorrowerParty debtorParty) BECAUSE "loan default"
```

---

## Testing with #TRACE

### Happy Path: All Payments On Time

```l4
-- Define test parties
aliceBorrower MEANS IndividualBorrower (Person WITH name IS "Alice")
bobLender MEANS IndividualLender (Person WITH name IS "Bob")

#TRACE `payment obligation` exampleLoan aliceBorrower bobLender (USD 25000) AT 0 WITH
    PARTY (BorrowerParty aliceBorrower) DOES `pay installment` bobLender (USD 2256.46) AT 30
    PARTY (BorrowerParty aliceBorrower) DOES `pay installment` bobLender (USD 2256.46) AT 60
    PARTY (BorrowerParty aliceBorrower) DOES `pay installment` bobLender (USD 2256.46) AT 90
    -- ... continue for all 12 payments
```

### Late Payment Scenario

```l4
-- First payment on time, second payment late (in grace period)
#TRACE `payment obligation` exampleLoan aliceBorrower bobLender (USD 25000) AT 0 WITH
    PARTY (BorrowerParty aliceBorrower) DOES `pay installment` bobLender (USD 2256.46) AT 30
    -- Second payment late with penalty
    PARTY (BorrowerParty aliceBorrower) DOES `pay installment` bobLender (USD 2369.28) AT 75
```

### Default Scenario

```l4
-- Borrower stops paying
#TRACE `payment obligation` exampleLoan aliceBorrower bobLender (USD 25000) AT 0 WITH
    PARTY (BorrowerParty aliceBorrower) DOES `pay installment` bobLender (USD 2256.46) AT 30
    -- No more payments... contract should reach BREACH
```

---

## Multi-Party Contracts

Some contracts involve more than two parties:

```l4
-- Escrow arrangement: Buyer, Seller, Escrow Agent
DECLARE EscrowParty IS ONE OF Buyer, Seller, EscrowAgent

DECLARE EscrowAction IS ONE OF
    `deposit funds` HAS amount IS A Money
    `deliver goods`
    `release funds to seller`
    `return funds to buyer`

GIVEN amount IS A Money
GIVETH A DEONTIC EscrowParty EscrowAction
`escrow arrangement` MEANS
    -- Buyer deposits funds
    PARTY Buyer
    MUST `deposit funds` amount
    WITHIN 7
    HENCE
        -- Seller delivers goods
        PARTY Seller
        MUST `deliver goods`
        WITHIN 14
        HENCE
            -- Escrow releases to seller
            PARTY EscrowAgent
            MUST `release funds to seller`
            WITHIN 3
            HENCE FULFILLED
            LEST BREACH BY EscrowAgent BECAUSE "escrow agent failed to release funds"
        LEST
            -- Seller didn't deliver, return funds
            PARTY EscrowAgent
            MUST `return funds to buyer`
            WITHIN 3
            HENCE BREACH BY Seller BECAUSE "seller failed to deliver goods"
            LEST BREACH BY EscrowAgent BECAUSE "escrow agent failed to return funds"
    LEST BREACH BY Buyer BECAUSE "buyer failed to deposit funds"
```

---

## Parallel Obligations with RAND

When multiple obligations must all be fulfilled:

```l4
-- Types for service contract examples
DECLARE ServiceParty IS ONE OF Provider, Client

DECLARE ContractAction IS ONE OF
    `deliver service`
    `provide documentation`
    `ship goods`
    `arrange pickup`
    `make payment`

-- Service contract: Provider must deliver service AND provide documentation
GIVETH A DEONTIC ServiceParty ContractAction
`service delivery` MEANS
    (PARTY Provider MUST `deliver service` WITHIN 30 HENCE FULFILLED LEST BREACH BY Provider BECAUSE "failed to deliver service")
    RAND
    (PARTY Provider MUST `provide documentation` WITHIN 30 HENCE FULFILLED LEST BREACH BY Provider BECAUSE "failed to provide documentation")
```

Both obligations must be fulfilled for the contract to be fulfilled.

---

## Alternative Obligations with ROR

When fulfilling any one obligation is sufficient:

```l4
-- Delivery options: Ship OR pickup
GIVETH A DEONTIC ServiceParty ContractAction
`delivery options` MEANS
    (PARTY Provider MUST `ship goods` WITHIN 14 HENCE FULFILLED LEST BREACH BY Provider BECAUSE "failed to ship")
    ROR
    (PARTY Provider MUST `arrange pickup` WITHIN 7 HENCE FULFILLED LEST BREACH BY Provider BECAUSE "failed to arrange pickup")
```

Either shipping or arranging pickup fulfills the delivery obligation.

---

## Practical Patterns

### Minimum Payment

```l4
-- Credit card style: Pay minimum or full balance
DECLARE CardParty IS ONE OF Cardholder, Bank

DECLARE CardAction IS ONE OF
    `pay` HAS `payment amount` IS A NUMBER

GIVEN balance IS A Money
      minimumPct IS A NUMBER
GIVETH A DEONTIC CardParty CardAction
`credit payment` MEANS
    PARTY Cardholder
    MUST `pay` paidAmount PROVIDED paidAmount >= minimumPayment
    WITHIN 30
    HENCE
        IF paidAmount >= balance's amount
        THEN FULFILLED  -- Paid in full
        ELSE `credit payment` newBalance minimumPct  -- Recurse with new balance
    LEST BREACH BY Cardholder BECAUSE "missed credit card payment"
    WHERE
        minimumPayment MEANS balance's amount * minimumPct
        newBalance MEANS Money WITH amount IS balance's amount - minimumPayment, currency IS balance's currency
```

### Milestone-Based Payment

```l4
DECLARE Milestone IS ONE OF
    Design
    Development
    Testing
    Delivery

DECLARE MilestoneAction IS ONE OF
    `complete milestone` HAS milestone IS A Milestone
    `pay milestone` HAS milestone IS A Milestone

GIVEN milestones IS A LIST OF Milestone
GIVETH A DEONTIC ServiceParty MilestoneAction
`milestone payments` MEANS
    CONSIDER milestones
    WHEN EMPTY THEN FULFILLED
    WHEN m FOLLOWED BY rest THEN
        PARTY Provider
        MUST `complete milestone` m
        WITHIN 30
        HENCE
            PARTY Client
            MUST `pay milestone` m
            WITHIN 14
            HENCE `milestone payments` rest  -- Recurse
            LEST BREACH BY Client BECAUSE "client failed to pay milestone"
        LEST BREACH BY Provider BECAUSE "provider failed to complete milestone"
```

---

### Full Example

[module-a3-contracts-examples.l4](module-a3-contracts-examples.l4)

---

## Summary

| Pattern                   | Use Case                            |
| ------------------------- | ----------------------------------- |
| **Recursive obligations** | Installment payments, subscriptions |
| **Penalty structures**    | Late fees, interest                 |
| **Default acceleration**  | Full balance on default             |
| **Multi-party**           | Escrow, three-way agreements        |
| **RAND**                  | Multiple parallel obligations       |
| **ROR**                   | Alternative ways to fulfill         |

Key insight: Complex contracts are **compositions of simpler patterns**, often with recursion for repeated obligations.

---

## What's Next?

In [Module A4: Production Patterns](module-a4-production.md), you'll learn patterns for organizing large codebases, testing strategies, and integration considerations.
