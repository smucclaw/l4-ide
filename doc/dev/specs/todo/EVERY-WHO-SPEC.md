# EVERY...WHO Specification for Regulative Rules

**Status:** TODO
**Author:** Meng Wong, Claude
**Date:** 2026-01-24

## Summary

This specification extends L4's regulative rule syntax to support universal quantification over entity types with optional preconditions, using the `EVERY PartyType [WHO predicate]` form as an alternative to `PARTY`.

## Motivation

The current regulative rule syntax requires an awkward pattern when expressing rules that apply to all members of a type who satisfy certain conditions:

```l4
GIVEN p IS A Person
PARTY p
   IF `has green hair` p
 MUST `wash hair`
```

This reads unnaturally. The more intuitive phrasing would be:

```l4
EVERY Person p
  WHO `has green hair`
 MUST `wash hair`
```

This mirrors how legal rules are actually written: "Every person who..." or "Any driver who..." rather than "Given a person p, the party p, if p has some property..."

## Legislative Mode vs Contract Mode

L4 supports two distinct styles of regulative rules, corresponding to two different legal paradigms:

### Contract Mode: Explicit State Machines

Contracts are bilateral or multilateral agreements between **named parties**. The obligations form an explicit state machine connected by `HENCE` and `LEST`:

```l4
§ `Sale Agreement`
PARTY buyer
 MUST `pay` purchasePrice
WITHIN 30 days
HENCE PARTY seller
       MUST `deliver` goods
      WITHIN 14 days
      HENCE FULFILLED
      LEST  PARTY buyer MAY `claim refund`
LEST  BREACH
```

**Characteristics:**
- Parties are named and finite (Alice, Bob, Acme Corp)
- Rules are connected via explicit `HENCE`/`LEST` transitions
- Execution follows the state machine: "What happens next?"
- The contract has a lifecycle (pending → active → fulfilled/breached)

### Legislative Mode: Predicate-Based Applicability

Legislation and regulations apply to **open classes of entities** who satisfy predicates. Rules are typically independent - they don't chain to each other via `HENCE`/`LEST`:

```l4
§ `Income Tax Act s.10`
EVERY Person p
  WHO p's annualIncome > 50000
 MUST `file tax return`
WITHIN `April 15 of following year`

§ `Road Traffic Act s.63`
EVERY Driver d
  WHO `exceeds speed limit` d
 MUST `pay fine` (speedingFine d)
WITHIN 30 days
LEST `license suspended`

§ `Education Act s.4`
EVERY Person p
  WHO p's age >= 6 AND p's age <= 16
 MUST `attend school`
```

**Characteristics:**
- Subjects are open classes (any person, any driver, any business)
- Rules fire independently based on predicate satisfaction
- No explicit state machine connecting rules
- Query direction: "What rules apply to me?"
- Rules may have individual `LEST` consequences but rarely chain via `HENCE`

### Comparison

| Aspect | Contract Mode | Legislative Mode |
|--------|---------------|------------------|
| **Parties** | Named, finite | Open class, potentially infinite |
| **Activation** | UPON events, HENCE chains | Predicate satisfaction |
| **State machine** | Explicit transitions | Implicit (rules fire when applicable) |
| **Query direction** | "What happens next?" | "What rules apply to entity X?" |
| **HENCE/LEST** | Connect rules to each other | Usually terminal (BREACH, penalty) |
| **Typical keyword** | `PARTY alice` | `EVERY Person p WHO ...` |

### Mixed Mode

Real-world legal instruments often mix both modes. A regulation might define class-based obligations that, when triggered, create contract-like state machines:

```l4
-- Legislative: applies to a class
§ `Tax Assessment`
EVERY Person p
  WHO `owes back taxes` p
 MUST `pay assessment`
WITHIN 60 days
-- Contract-like continuation for this specific person
HENCE PARTY p
       MAY `request payment plan`
      WITHIN 30 days
      HENCE `payment plan for` p
      LEST  `full payment required`
LEST  `enforcement action against` p
```

The `EVERY...WHO` syntax is primarily designed for legislative mode, but as shown above, it can introduce a named party `p` that then participates in contract-mode `HENCE`/`LEST` chains.

### Execution Semantics

**Contract mode** execution:
1. Start at initial state
2. Wait for events / party actions
3. Evaluate guards, transition via HENCE/LEST
4. Repeat until terminal state (FULFILLED/BREACH)

**Legislative mode** execution:
1. Given an entity X, find all `EVERY` rules where X matches the type
2. Evaluate each rule's `WHO` predicate for X
3. Return list of applicable obligations/permissions/prohibitions
4. No automatic chaining between rules

The evaluator needs to support both modes. Contract mode is trace-based (following a history of events). Legislative mode is query-based (given facts, what applies?).

## Syntax

### Grammar

```ebnf
regulative-rule ::= party-clause action-clause [temporal-clause] [hence-clause] [lest-clause]

party-clause ::= "PARTY" expr
               | every-clause

every-clause ::= "EVERY" type-name identifier [who-clause]

who-clause ::= "WHO" expr

action-clause ::= deontic-modal [pattern] ["PROVIDED" expr]

deontic-modal ::= "MUST" ["NOT"] | "MAY" | "SHANT" | "DO"
```

### Keyword Additions

Two new keywords are required:

| Keyword | Token      | Description                                           |
|---------|------------|-------------------------------------------------------|
| `EVERY` | `TKEvery`  | Introduces universal quantification over a type       |
| `WHO`   | `TKWho`    | Introduces a predicate that restricts the quantifier  |

### Alternative Forms

The `WHO` clause is optional. The following are all valid:

```l4
-- With WHO clause
EVERY Person p
  WHO `is over 18`
 MUST `pay taxes`

-- Without WHO clause (applies to all persons)
EVERY Person p
  MAY `breathe`

-- Traditional PARTY form (still supported)
PARTY alice
 MUST `pay bob` 100
```

## Semantics

### Desugaring

The `EVERY...WHO` form is syntactic sugar that desugars to the standard `GIVEN...PARTY...IF` form:

**Source:**
```l4
EVERY TypeName varName
  WHO predicate
 MUST action
```

**Desugars to:**
```l4
GIVEN varName IS A TypeName
PARTY varName
   IF predicate
 MUST action
```

When `WHO` is omitted, the `IF` clause is simply absent:

**Source:**
```l4
EVERY Person p
  MAY depart
```

**Desugars to:**
```l4
GIVEN p IS A Person
PARTY p
  MAY depart
```

### Predicate Application

The `WHO` predicate is expected to be a boolean expression or a predicate that can be applied to the bound variable. Two forms are supported:

1. **Predicate form** (prefix application):
   ```l4
   EVERY Person p
     WHO `has green hair`  -- implicitly: `has green hair` p
   ```

2. **Expression form** (explicit):
   ```l4
   EVERY Person p
     WHO p's age >= 18
   ```

In the predicate form, if the expression after `WHO` is a function of type `TypeName -> BOOLEAN`, it is automatically applied to the bound variable. Otherwise, the expression is used as-is (and must be of type `BOOLEAN`).

### Interaction with GIVEN

The `EVERY` clause can coexist with `GIVEN` for additional parameters:

```l4
GIVEN amount IS A NUMBER
EVERY Person p
  WHO owes amount
 MUST pay amount
```

This desugars to:

```l4
GIVEN amount IS A NUMBER
      p IS A Person
PARTY p
   IF owes p amount
 MUST pay amount
```

### Interaction with UPON

When combined with `UPON` (trigger events), the structure is:

```l4
UPON `receiving invoice`
EVERY Customer c
  WHO `is premium member`
 MUST `pay within` 30 days
```

The `UPON` clause precedes the `EVERY` clause, consistent with existing syntax.

## Examples

### Basic Usage

```l4
-- Every adult must pay taxes
EVERY Person p
  WHO p's age >= 18
 MUST `pay taxes`

-- Every driver may park in designated areas
EVERY Driver d
  MAY `park in` designatedArea

-- Every student who passes must receive diploma
EVERY Student s
  WHO `has passed all courses`
 MUST `receive diploma`
```

### Legal Rules (MACMA example)

From the MACMA statute formalization:

```l4
GIVEN fc IS A Country
EVERY Person aa
  WHO `is an appropriate authority of` fc
  MAY `make a request`
       that: `evidence be taken in Singapore`
       for:  `criminal proceedings pending in` fc
HENCE PARTY ag
        WHO `is the Attorney-General of` Singapore
        MAY `authorise a Magistrate`
```

### Prohibitions

```l4
-- No minor shall purchase alcohol
EVERY Person p
  WHO p's age < 21
SHANT `purchase alcohol`

-- Alternative phrasing
EVERY Person p
  WHO `is a minor`
 MUST NOT `purchase alcohol`
```

### Complex Conditions

```l4
GIVEN location IS A Location
EVERY Person p
  WHO `is at` location
  AND `has valid ticket for` location
  MAY `enter` location
```

### Freedom of Movement (from regulative-powers.l4)

Current syntax:
```l4
GIVEN l IS A Location
EVERY Person p
   IF person p is at l
  MAY depart l
```

Proposed syntax (equivalent):
```l4
GIVEN l IS A Location
EVERY Person p
  WHO `is at` l
  MAY depart l
```

## Implementation Notes

### AST Changes

The `Deonton` type needs to accommodate the new syntax. Options:

**Option A: Unified representation**
Keep `party :: Expr n` but add a flag indicating whether it came from `EVERY`:

```haskell
data Deonton n = MkDeonton
  { anno :: Anno
  , party :: Expr n
  , partySource :: PartySource  -- New field
  , action :: RAction n
  , due :: Maybe (Expr n)
  , hence :: Maybe (Expr n)
  , lest :: Maybe (Expr n)
  }

data PartySource
  = FromParty              -- PARTY expr
  | FromEvery Name (Type' Name) (Maybe (Expr Name))  -- EVERY Type var [WHO pred]
```

**Option B: Sum type**

```haskell
data PartySpec n
  = ExplicitParty (Expr n)           -- PARTY expr
  | QuantifiedParty
      { partyType :: Type' n         -- The type (e.g., Person)
      , partyVar :: n                -- The bound variable
      , partyPred :: Maybe (Expr n)  -- Optional WHO predicate
      }
```

**Recommendation:** Option B is cleaner and makes the distinction explicit in the type system.

### Parser Changes

1. Add `TKEvery` and `TKWho` to the lexer
2. Extend `obligation` parser to handle both `PARTY` and `EVERY` forms:

```haskell
obligation :: Parser (Deonton Name)
obligation = do
  current <- Lexer.indentLevel
  attachAnno $
    MkDeonton emptyAnno
      <$> (partyClause current <|> everyClause current)
      <*> annoHole (must current)
      <*> optionalWithHole (deadline current)
      <*> optionalWithHole (hence current)
      <*> optionalWithHole (lest current)

partyClause :: Pos -> Parser (PartySpec Name)
partyClause current =
  ExplicitParty
    <$  annoLexeme (spacedKeyword_ TKParty)
    <*> annoHole (indentedExpr current)

everyClause :: Pos -> Parser (PartySpec Name)
everyClause current = do
  annoLexeme (spacedKeyword_ TKEvery)
  ty <- annoHole typeName
  var <- annoHole name
  pred <- optionalWithHole whoClause
  pure $ QuantifiedParty ty var pred
  where
    whoClause =
      annoLexeme (spacedKeyword_ TKWho)
        *> annoHole (indentedExpr current)
```

### Type Checking

The type checker must verify:

1. The type in `EVERY TypeName var` is a valid declared type
2. The bound variable `var` has type `TypeName` within the scope of the rule
3. If `WHO pred` is present:
   - If `pred` is a function `TypeName -> BOOLEAN`, apply it to `var`
   - If `pred` is already `BOOLEAN`, use it directly
   - Otherwise, type error

### Desugaring Phase

A desugaring pass should transform `EVERY...WHO` into the standard `GIVEN...PARTY...IF` form before evaluation, so the evaluator doesn't need modification.

## Test Cases

### Parser Tests (`jl4/ok/`)

```l4
-- every-basic.l4
EVERY Person p
 MUST `exist`

-- every-who.l4
EVERY Person p
  WHO `is adult`
 MUST `pay taxes`

-- every-complex.l4
GIVEN amount IS A NUMBER
EVERY Customer c
  WHO c's balance >= amount
  MAY `withdraw` amount
```

### Parser Error Tests (`jl4/not-ok/`)

```l4
-- every-missing-type.l4
EVERY p        -- Error: expected type name after EVERY
 MUST `exist`

-- every-who-not-boolean.l4
EVERY Person p
  WHO 42       -- Error: WHO clause must be BOOLEAN
 MUST `exist`
```

## Open Questions

1. **Should `WHO` allow pattern matching?**

   ```l4
   EVERY Person p
     WHO Adult _  -- Pattern instead of predicate?
    MUST `pay taxes`
   ```

   Current answer: No, keep it to predicates for simplicity.

2. **Should there be a `WHERE` variant?**

   Some legal texts use "where" instead of "who" for non-human entities:
   ```l4
   EVERY Contract c
    WHERE c's value > 10000
    MUST `be notarized`
   ```

   Current answer: Defer to future work. `WHO` can work for non-human entities in programming contexts.

3. **Multiple bound variables?**

   ```l4
   EVERY Person buyer, Person seller
     WHO buyer wants item AND seller has item
    MAY `transact`
   ```

   Current answer: Out of scope. Use nested `EVERY` or explicit `GIVEN` for multi-party rules.

## Use Cases

### Mutual NDAs: Symmetric Obligations

The `EVERY...WHO` pattern dramatically simplifies mutual agreements where both parties have identical obligations. Instead of writing the same clause twice with party names swapped:

**Without EVERY (verbose, error-prone):**
```l4
§ `Mutual NDA - Traditional`
GIVEN partyA IS A Party
      partyB IS A Party

PARTY partyA
 MUST `keep confidential` partyB's confidentialInfo
LEST BREACH

PARTY partyB
 MUST `keep confidential` partyA's confidentialInfo
LEST BREACH
```

**With EVERY (concise, symmetric):**
```l4
§ `Mutual NDA - With EVERY`
DECLARE SigningParty IS ONE OF PartyA, PartyB

EVERY SigningParty p
 MUST `keep confidential` (otherParty p)'s confidentialInfo
LEST BREACH
WHERE
  otherParty MEANS
    CONSIDER p
      WHEN PartyA THEN PartyB
      WHEN PartyB THEN PartyA
```

Or more elegantly with a relation:
```l4
§ `Mutual NDA - Relational`
GIVEN parties IS A LIST OF Party
EVERY Party p
  WHO p IN parties
 MUST `keep confidential` ALL (q's confidentialInfo FOR q IN parties WHERE q != p)
LEST BREACH
```

### Age-Based Regulations

Laws frequently apply to persons based on age thresholds. `EVERY...WHO` makes these natural to express:

```l4
-- Voting rights
EVERY Person p
  WHO p's age >= 18
  AND p's citizenship EQUALS Singapore
  MAY vote

-- Alcohol purchase prohibition
EVERY Person p
  WHO p's age < 21
SHANT `purchase alcohol`
LEST `commit offense under` LiquorControlAct

-- Mandatory education
EVERY Person p
  WHO p's age >= 6 AND p's age <= 16
 MUST `attend school`
LEST PARTY p's parents MUST `pay truancy fine`

-- Driving eligibility (with graduated licensing)
EVERY Person p
  WHO p's age >= 17
  MAY `apply for` provisionalLicense

EVERY Person p
  WHO p's age >= 18
  AND `has held provisional license for` p >= 1 year
  MAY `apply for` fullLicense

-- Senior benefits
EVERY Person p
  WHO p's age >= 65
  MAY `claim` seniorCitizenDiscount
```

### Employment Law

```l4
-- Minimum wage
EVERY Employer e
 MUST `pay at least` minimumWage TO `each employee of` e

-- Working hours for minors
EVERY Employer e
SHANT `require work exceeding` 8 hours
  FROM ANY (Person p WHO p's age < 18 AND `is employed by` p e)

-- Anti-discrimination
EVERY Employer e
SHANT `discriminate based on` protectedCharacteristics
  IN `hiring decisions`
LEST `commit offense under` EmploymentAct
```

### Tax Law

```l4
-- Progressive taxation thresholds
EVERY Person p
  WHO p's annualIncome > 0 AND p's annualIncome <= 20000
 MUST `pay tax at rate` 0%

EVERY Person p
  WHO p's annualIncome > 20000 AND p's annualIncome <= 40000
 MUST `pay tax at rate` 2%

EVERY Person p
  WHO p's annualIncome > 40000
 MUST `pay tax at rate` (progressiveTaxRate p's annualIncome)

-- GST registration threshold
EVERY Business b
  WHO b's annualRevenue > 1000000
 MUST `register for GST`
WITHIN 30 days OF `exceeding threshold`
LEST `pay penalty` (lateRegistrationPenalty b)
```

## Feature Interactions

### Interaction with DEONTIC Reification

The HOMOICONICITY-SPEC proposes treating deontic positions (obligations, permissions, prohibitions) as first-class runtime values of type `DEONTIC`. This creates important interactions with `EVERY...WHO`:

#### From Syntax to Values

When `EVERY...WHO` is used to define a rule, and DEONTIC reification is in effect, the system must determine how to instantiate the deontic values:

**Static interpretation (compile-time):**
```l4
-- This rule defines a schema, not individual deontics
EVERY Person p
  WHO p's age >= 18
 MUST `pay taxes`
```

**Runtime instantiation (when needed):**
```l4
-- Query: what deontics exist for person alice?
`deontics for` alice MEANS
  IF alice's age >= 18
  THEN [DEONTIC FROM alice TO government DO `pay taxes` BY taxDeadline]
  ELSE []
```

#### The Registry Question

The HOMOICONICITY-SPEC introduces a runtime `DeontRegistry` tracking active deontic positions. With `EVERY...WHO`, we must decide:

**Option A: Lazy/On-Demand Instantiation**
- The registry doesn't pre-populate all possible deontics
- When querying "what must person X do?", the system evaluates applicable `EVERY` rules
- Deontics are instantiated only when relevant

**Option B: Eager/Full Instantiation**
- When a person enters the system, all applicable `EVERY` rules generate deontics
- The registry contains explicit deontic entries for each (person, rule) pair
- Requires knowing the universe of persons

**Recommendation:** Option A (lazy) for unbounded types like `Person`, Option B (eager) for bounded enums like `SigningParty` in an NDA.

#### Quantified Deontics in Higher-Order Operations

The HOMOICONICITY-SPEC's `GRANT`, `REVOKE`, `WAIVE`, and `PROCURE` operators can target quantified deontics:

```l4
-- Power to waive the tax obligation for all seniors
§ `Tax Relief Power`
PARTY parliament MAY
  WAIVE (EVERY Person p WHO p's age >= 65 MUST `pay taxes`)
  PROVIDED `budget permits`

-- This creates a meta-level power: the power to eliminate an entire class of obligations
```

When this power is exercised:
1. Does it modify the rule itself? (removing the EVERY clause)
2. Or does it discharge all instantiated deontics matching the pattern?
3. Or does it add an exception that's checked at query time?

**Recommendation:** Option 3 - add an exception. The original `EVERY` rule remains, but a "waived" flag is set that causes the rule to not generate deontics for matching persons.

#### Netting and Novation with Quantified Obligations

From HOMOICONICITY-SPEC's IFEMA example, obligations can be netted. With `EVERY...WHO`, bilateral netting across a class of parties becomes possible:

```l4
-- Multilateral netting: all parties who owe each other can net
§ `Multilateral Netting`
AUTOMATICALLY
  WHEN `netting cycle detected` parties
  THEN
    FOR EACH pair (p1, p2) IN parties
      NET (EVERY o IN currentObligations
            WHERE o's obligor EQUALS p1 AND o's obligee EQUALS p2)
         WITH
          (EVERY o IN currentObligations
            WHERE o's obligor EQUALS p2 AND o's obligee EQUALS p1)
```

#### PROCURE with EVERY

The `PROCURE` construct from HOMOICONICITY-SPEC can interact with `EVERY`:

```l4
-- Parent company must procure compliance from ALL subsidiaries
§ `Group Compliance`
PARTY parentCompany MUST
  PROCURE STRICT (EVERY Subsidiary s
                    WHO s `is controlled by` parentCompany
                   MUST `comply with` regulations)
  HENCE `group compliant`
  LEST `parent breach` -- parent liable for ANY subsidiary's failure
```

This creates a "quantified procure" - a higher-order obligation over a class of underlying obligations.

### Interaction with HENCE and YIELDS

When an `EVERY` rule has a `HENCE` clause, the continuation can reference the bound variable:

```l4
EVERY Person p
  WHO `has applied for` license
  MAY `take driving test`
  HENCE PARTY p MUST `pay test fee`  -- 'p' is in scope
  LEST `application expired`
```

With `YIELDS` (from HOMOICONICITY-SPEC), a quantified rule can yield individualized contracts:

```l4
EVERY Subscriber s
  WHO s's subscription IS expiring
  MAY `renew subscription`
  YIELDS `Renewal Agreement` WITH
    subscriber IS s
    term IS 1 year
    price IS renewalPrice s
  EXECUTABLE BY s
```

### Interaction with Powers Hierarchy

From `regulative-powers.l4` and HOMOICONICITY-SPEC, powers form hierarchies. `EVERY...WHO` can be used at multiple levels:

```l4
-- Base: every person has freedom of movement
EVERY Person p
  MAY `move freely`

-- First-order power: certain officers can restrict this
EVERY Officer o
  WHO `is authorized` o
  MAY REVOKE (PARTY person MAY `move freely`)
  PROVIDED `lawful grounds exist`

-- Second-order power: legislature can grant/revoke officer powers
PARTY parliament MAY
  GRANT (EVERY Officer o WHO `meets criteria` o
           MAY REVOKE (PARTY person MAY `move freely`))
```

### Implementation Implications

#### AST Extension for Reified Quantified Deontics

When DEONTIC reification is active, the `PartySpec` type needs to support quantification at the value level:

```haskell
data PartySpec n
  = ExplicitParty (Expr n)
  | QuantifiedParty
      { partyType :: Type' n
      , partyVar :: n
      , partyPred :: Maybe (Expr n)
      }
  | QuantifiedDeonticPattern  -- New: for matching in WAIVE, NET, etc.
      { patternType :: Type' n
      , patternVar :: n
      , patternPred :: Maybe (Expr n)
      }
```

#### Query Semantics

When querying "what are person X's obligations?", the evaluator must:

1. Find all `EVERY` rules where `X` satisfies the type constraint
2. Evaluate the `WHO` predicate for `X`
3. For matching rules, instantiate a `DEONTIC` value
4. Check for any `WAIVE` or exception modifiers
5. Return the filtered list

```haskell
obligationsFor :: Party -> EvalM [Deonton]
obligationsFor party = do
  everyRules <- getEveryRules
  forM everyRules $ \rule -> do
    typeMatches <- party `isOfType` rule.partyType
    predSatisfied <- case rule.partyPred of
      Nothing -> pure True
      Just pred -> evalPred pred party
    if typeMatches && predSatisfied
      then do
        waived <- isWaived rule party
        if waived then pure Nothing
        else pure $ Just (instantiateDeont rule party)
      else pure Nothing
```

## Related Work

- **LegalRuleML**: Uses `<Agent>` elements with optional qualifiers
- **Defeasible Deontic Logic**: Universal quantification with applicability conditions
- **CSL (Contract Specification Language)**: Template-based party specifications
- **HOMOICONICITY-SPEC**: Deontic reification enabling higher-order operations on obligations

## References

- `doc/regulative-spec.org` - Main regulative rule specification
- `doc/regulative-examples.md` - MACMA example using WHO syntax
- `doc/dev/specs/todo/HOMOICONICITY-SPEC.md` - Deontic reification and higher-order obligations
- `jl4/experiments/regulative-powers.l4` - Current EVERY usage (with IF instead of WHO)
- `jl4/experiments/deontic-may.l4` - Various deontic examples
