# Module A3: Contracts in Depth

In this module, you'll learn advanced contract modeling including complex payment terms, recursive obligations, and penalty structures.

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
USD MEANS Money n "USD"

GIVEN n IS A NUMBER
GIVETH A Money
SGD MEANS Money n "SGD"

-- Party types
DECLARE Party IS ONE OF
    BorrowerParty HAS borrower IS A Borrower
    LenderParty HAS lender IS A Lender

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
exampleLoan MEANS LoanTerms
    (USD 25000)          -- Principal
    0.15                 -- 15% annual interest
    12                   -- 12 monthly installments
    30                   -- Default after 30 days late
    (PenaltyTerms 0.05 10)  -- 5% penalty, 10-day grace
```

### Step 3: Calculate Payment Schedule

```l4
-- Monthly interest rate
GIVEN terms IS A LoanTerms
GIVETH A NUMBER
`monthly rate` MEANS terms's annualInterestRate / 12

-- Monthly payment using amortization formula
-- PMT = P × (r(1+r)^n) / ((1+r)^n - 1)
GIVEN terms IS A LoanTerms
GIVETH A Money
`monthly payment amount` MEANS
    Money payment (terms's principal's currency)
    WHERE
        p MEANS terms's principal's amount
        r MEANS `monthly rate` terms
        n MEANS terms's numberOfInstallments
        compoundFactor MEANS `power` (1 + r) n
        payment MEANS p * (r * compoundFactor) / (compoundFactor - 1)

-- Power function
GIVEN base IS A NUMBER
      exp IS A NUMBER
GIVETH A NUMBER
`power` MEANS
    IF exp = 0 THEN 1
    ELSE IF exp = 1 THEN base
    ELSE base * (`power` base (exp - 1))
```

---

## Recursive Payment Obligations

The key insight: **payment obligations are recursive**. After each payment, there's another payment (until paid off).

```l4
-- Actions for the loan contract
DECLARE LoanAction IS ONE OF
    `pay installment` HAS recipient IS A Lender
                          amount IS A Money

-- The recursive payment obligation
GIVEN terms IS A LoanTerms
      borrower IS A Borrower
      lender IS A Lender
      outstanding IS A Money  -- Remaining balance
GIVETH A DEONTIC Party LoanAction
`payment obligation` MEANS
    IF outstanding's amount > 0
    THEN
        PARTY BorrowerParty borrower
        MUST `pay installment` lender paymentDue
            PROVIDED `is valid payment` transferAmount paymentDue
        WITHIN nextDueDate
        HENCE
            -- Recursive call with reduced balance
            `payment obligation` terms borrower lender newOutstanding
        LEST
            -- Late: apply penalty
            `late payment obligation` terms borrower lender outstanding
    ELSE FULFILLED
    WHERE
        -- Calculate next payment
        paymentDue MEANS `calculate next payment` terms outstanding
        nextDueDate MEANS `calculate due date` terms outstanding

        -- After payment, reduce outstanding
        newOutstanding MEANS Money
            (outstanding's amount - transferAmount's amount)
            (outstanding's currency)

        -- Variable from the action
        transferAmount MEANS `Amount Transferred`  -- Bound by PROVIDED

-- Validate payment amount
GIVEN paid IS A Money
      expected IS A Money
GIVETH A BOOLEAN
`is valid payment` MEANS
    paid's currency EQUALS expected's currency
    AND paid's amount >= (expected's amount - 0.05)  -- Allow small rounding
```

### Late Payment with Penalty

```l4
GIVEN terms IS A LoanTerms
      borrower IS A Borrower
      lender IS A Lender
      outstanding IS A Money
GIVETH A DEONTIC Party LoanAction
`late payment obligation` MEANS
    -- Grace period with penalty interest
    PARTY BorrowerParty borrower
    MUST `pay installment` lender paymentWithPenalty
        PROVIDED `is valid payment` transferAmount paymentWithPenalty
    WITHIN (nextDueDate + terms's penaltyTerms's gracePeriodDays)
    HENCE
        -- Continue with remaining payments
        `payment obligation` terms borrower lender newOutstanding
    LEST
        -- Default: full balance due
        `default obligation` terms borrower lender outstanding
    WHERE
        basePayment MEANS `calculate next payment` terms outstanding
        penaltyAmount MEANS basePayment's amount * terms's penaltyTerms's interestRate
        paymentWithPenalty MEANS Money
            (basePayment's amount + penaltyAmount)
            (basePayment's currency)
        nextDueDate MEANS `calculate due date` terms outstanding
```

### Default: Full Balance Due

```l4
GIVEN terms IS A LoanTerms
      borrower IS A Borrower
      lender IS A Lender
      outstanding IS A Money
GIVETH A DEONTIC Party LoanAction
`default obligation` MEANS
    PARTY BorrowerParty borrower
    MUST `pay installment` lender outstanding  -- Full balance
    WITHIN terms's defaultAfterDays
    HENCE FULFILLED
    LEST BREACH BY (BorrowerParty borrower) BECAUSE "loan default"
```

---

## Testing with #TRACE

### Happy Path: All Payments On Time

```l4
#TRACE `payment obligation` exampleLoan theBorrower theLender (USD 25000) AT 0 WITH
    PARTY (BorrowerParty theBorrower) DOES `pay installment` theLender (USD 2256.46) AT 30
    PARTY (BorrowerParty theBorrower) DOES `pay installment` theLender (USD 2256.46) AT 60
    PARTY (BorrowerParty theBorrower) DOES `pay installment` theLender (USD 2256.46) AT 90
    -- ... continue for all 12 payments
```

### Late Payment Scenario

```l4
-- First payment on time, second payment late (in grace period)
#TRACE `payment obligation` exampleLoan theBorrower theLender (USD 25000) AT 0 WITH
    PARTY (BorrowerParty theBorrower) DOES `pay installment` theLender (USD 2256.46) AT 30
    -- Second payment late with penalty
    PARTY (BorrowerParty theBorrower) DOES `pay installment` theLender (USD 2369.28) AT 75
```

### Default Scenario

```l4
-- Borrower stops paying
#TRACE `payment obligation` exampleLoan theBorrower theLender (USD 25000) AT 0 WITH
    PARTY (BorrowerParty theBorrower) DOES `pay installment` theLender (USD 2256.46) AT 30
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
            LEST BREACH BY EscrowAgent
        LEST
            -- Seller didn't deliver, return funds
            PARTY EscrowAgent
            MUST `return funds to buyer`
            WITHIN 3
            HENCE BREACH BY Seller
            LEST BREACH BY EscrowAgent
    LEST BREACH BY Buyer
```

---

## Parallel Obligations with RAND

When multiple obligations must all be fulfilled:

```l4
-- Service contract: Provider must deliver service AND provide documentation
GIVETH A DEONTIC Party ContractAction
`service delivery` MEANS
    (PARTY Provider MUST `deliver service` WITHIN 30 HENCE FULFILLED LEST BREACH)
    RAND
    (PARTY Provider MUST `provide documentation` WITHIN 30 HENCE FULFILLED LEST BREACH)
```

Both obligations must be fulfilled for the contract to be fulfilled.

---

## Alternative Obligations with ROR

When fulfilling any one obligation is sufficient:

```l4
-- Delivery options: Ship OR pickup
GIVETH A DEONTIC Party ContractAction
`delivery options` MEANS
    (PARTY Seller MUST `ship goods` WITHIN 14 HENCE `payment due`)
    ROR
    (PARTY Seller MUST `arrange pickup` WITHIN 7 HENCE `payment due`)
```

Either shipping or arranging pickup fulfills the delivery obligation.

---

## Practical Patterns

### Minimum Payment

```l4
-- Credit card style: Pay minimum or full balance
GIVEN balance IS A Money
      minimumPct IS A NUMBER
GIVETH A DEONTIC Party Action
`credit payment` MEANS
    PARTY Cardholder
    MUST `pay` amount PROVIDED amount >= minimum
    WITHIN 30
    HENCE
        IF amount >= balance's amount
        THEN FULFILLED  -- Paid in full
        ELSE `credit payment` newBalance minimumPct  -- Recurse with new balance
    LEST BREACH
    WHERE
        minimum MEANS balance's amount * minimumPct
        newBalance MEANS Money (balance's amount - amount) (balance's currency)
```

### Milestone-Based Payment

```l4
DECLARE Milestone IS ONE OF
    Design
    Development
    Testing
    Delivery

GIVEN milestones IS A LIST OF Milestone
GIVETH A DEONTIC Party Action
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
            LEST BREACH BY Client
        LEST BREACH BY Provider
```

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
