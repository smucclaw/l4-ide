# Specification: MUST NOT (Prohibitions) and BREACH Keywords

**Status:** 📋 DRAFT (December 2025)
**Related:** `doc/regulative.md`, `doc/regulative-spec.org`, `doc/advanced-course-ai/module-a11-regulative-rules.md`

## Executive Summary

Add two new language features to L4's regulative rules:

1. **`MUST NOT` / `SHANT`** — A deontic operator for prohibitions (the inverse of `MUST`)
2. **`BREACH`** — An explicit terminal clause (the counterpart to `FULFILLED`)

These features extend L4 beyond Hvitved's CSL (Contract Specification Language) to provide cleaner syntax for common contract patterns.

## Motivation

### The Problem with Prohibitions in CSL

Hvitved's CSL models prohibitions using an awkward workaround: an external choice that triggers an unfulfillable obligation if the prohibited action occurs:

```csl
-- CSL: Employee must not disclose information for 5 years
if Disclosure(e)
  where e = employee due within 5Y
then
  ⟨employee⟩ Unfulfillable where false due immediately
```

This pattern has several problems:

1. **Readability**: The intent ("don't disclose") is obscured by implementation details
2. **Fragility**: Easy to forget the `where false` or mess up the structure
3. **No static analysis**: A compiler can't distinguish "real" obligations from prohibition-encoding unfulfillable ones

### L4's Current Workaround

L4's current documentation suggests a similar pattern:

```l4
-- Prohibition: Employee must not disclose information for 5 years
IF Disclosure happens within 5 years
   THEN PARTY Employee
        MUST `do impossible action`
             PROVIDED FALSE
        WITHIN 0
   ELSE FULFILLED
```

This is equally awkward and should be replaced with native syntax.

### The Case for Explicit BREACH

Currently, `BREACH` is only an evaluation outcome (`ValBreached` in the evaluator), not something you write in L4 code. A contract breaches implicitly when:

1. A `WITHIN` deadline passes
2. No `LEST` clause handles the failure

However, there are cases where explicit `BREACH` would be clearer:

```l4
-- Current: implicit breach via omitted LEST
PARTY Borrower
MUST pay
WITHIN 30
-- If deadline passes with no LEST, breach occurs implicitly

-- Proposed: explicit BREACH for clarity
PARTY Borrower
MUST pay
WITHIN 30
LEST BREACH  -- Explicit: this contract terminates in breach
```

This parallels how `FULFILLED` works as an explicit terminal.

## Design

### Theoretical Foundation

From `doc/regulative.md`, the deontic operators are sugar over a basic `DO`:

| Deontic Modal | HENCE (default)               | LEST (default)                |
| ------------- | ----------------------------- | ----------------------------- |
| `DO`          | _required_                    | _required_                    |
| `MUST`        | action done → `FULFILLED`     | deadline passed → `BREACH`    |
| `MAY`         | action done → `FULFILLED`     | deadline passed → `FULFILLED` |
| `MUST NOT`    | deadline passed → `FULFILLED` | action done → `BREACH`        |

Key insight: **`MUST NOT` / `SHANT` flips the polarity of HENCE/LEST**:

- `MUST`: HENCE = action done; LEST = deadline passed without action
- `MUST NOT`: HENCE = deadline passed (prohibition respected); LEST = action done (violation!)

This makes the natural language reading intuitive:

- "You SHANT smoke LEST you BREACH" reads naturally as "don't smoke, or you breach"
- Compare to English: "Don't smoke, **lest** you face consequences"

### Syntax Design

#### MUST NOT / SHANT

Two equivalent keywords (user preference):

```l4
-- Option A: MUST NOT (two words, more natural English)
PARTY Employee
MUST NOT `disclose confidential information`
WITHIN 5 years
LEST `Penalty Clause`

-- Option B: SHANT (single word, archaic but concise)
PARTY Employee
SHANT `disclose confidential information`
WITHIN 5 years
LEST `Penalty Clause`
```

**Grammar extension:**

```ebnf
DeonticModal ::= "MUST" | "MAY" | "MUST" "NOT" | "SHANT"
```

**Semantics:**

`MUST NOT action WITHIN d` is equivalent to:

```l4
IF action happens WITHIN d
   THEN BREACH
   ELSE FULFILLED
```

With HENCE/LEST clauses:

```l4
PARTY p
MUST NOT action
WITHIN d
HENCE continuation    -- What happens if deadline passes without action (prohibition respected)
LEST  consequence     -- What happens if action IS done (prohibition violated!)
```

**The polarity is flipped from `MUST`** to match natural English:

| Modal      | HENCE triggers when...         | LEST triggers when...          |
| ---------- | ------------------------------ | ------------------------------ |
| `MUST`     | action is taken                | deadline passes without action |
| `MUST NOT` | deadline passes without action | action is taken (violation!)   |

This reads naturally: "You SHANT disclose secrets LEST you pay damages" means
"don't disclose, **lest** (for fear that) you face the penalty clause."

#### BREACH

A terminal clause, syntactically parallel to `FULFILLED`:

```l4
TerminalClause ::= "FULFILLED" | "BREACH"
```

`BREACH` may optionally specify the breaching party and reason (for explicit blame):

```l4
-- Simple BREACH (party inferred from context)
LEST BREACH

-- BREACH with explicit party (overrides inference)
LEST BREACH BY `The Borrower`

-- BREACH with reason (for audit trail)
LEST BREACH BY `The Borrower` BECAUSE `failed to pay within grace period`
```

The `BY` and `BECAUSE` clauses are optional. If omitted:

- Party is inferred from the nearest enclosing `PARTY` clause
- Reason is generated from the missed deadline context

### Desugaring Rules

#### MUST NOT Desugaring

```l4
-- Source:
PARTY p
MUST NOT action
WITHIN d
HENCE c1   -- runs if prohibition respected (deadline passes, action NOT taken)
LEST c2    -- runs if prohibition violated (action IS taken)

-- Desugars to:
IF action happens WITHIN d
   THEN c2     -- LEST: Prohibition violated (action WAS taken)
   ELSE c1     -- HENCE: Prohibition respected (deadline passed, action was NOT taken)
```

If `HENCE` is omitted, default is `FULFILLED` (prohibition was respected).
If `LEST` is omitted, default is `BREACH BY p` (prohibition was violated).

**Note on polarity:** For MUST NOT, the evaluator swaps HENCE↔LEST internally:

- When action matches → trigger LEST (violation)
- When deadline passes → trigger HENCE (respected)

#### BREACH Desugaring

`BREACH` is a primitive terminal that produces `ValBreached` with:

- The party to blame
- The timestamp of breach
- The action/deadline that caused it (from evaluation context)

```l4
-- Source:
LEST BREACH BY p BECAUSE reason

-- Evaluates to:
ValBreached (ReasonForBreach {
  blamedParty = p,
  breachTime = currentTime,
  reason = reason
})
```

### Static Analysis

#### MUST NOT Requirements

A `MUST NOT` clause MUST have either:

1. An explicit `LEST` clause, OR
2. Implicit `BREACH` as the default consequence

The compiler should WARN if `MUST NOT` has no `LEST`:

```
Warning: MUST NOT without LEST clause will result in immediate BREACH
if the prohibited action occurs. Consider adding an explicit LEST clause
for reparation (e.g., penalty payment).
```

#### BREACH Party Inference

The compiler should ERROR if `BREACH` appears without a determinable party:

```l4
-- ERROR: No party in scope
IF some_condition
   THEN BREACH  -- Error: Cannot determine party to blame

-- OK: Party from enclosing clause
PARTY Borrower
MUST pay
WITHIN 30
LEST BREACH    -- OK: Borrower is blamed

-- OK: Explicit party
LEST BREACH BY Borrower  -- OK: Explicit
```

#### Explicit Party Disagreement Warning

If the user specifies an explicit `BREACH BY party` that disagrees with the computed blame assignment (based on the enclosing `PARTY` clause), the compiler should WARN:

```l4
PARTY Borrower
MUST pay
WITHIN 30
LEST BREACH BY Lender  -- Warning: Explicit party 'Lender' differs from
                       -- computed blame 'Borrower' (from enclosing PARTY clause)
```

This warning helps catch potential drafting errors where the wrong party is blamed. The explicit party OVERRIDES the computed blame (intentional blame redirection is valid), but the warning ensures the drafter is aware of the discrepancy.

To suppress the warning, the drafter can acknowledge the override with a comment or annotation:

```l4
PARTY Borrower
MUST pay
WITHIN 30
LEST BREACH BY Lender  -- @desc Lender bears risk if Borrower defaults
```

#### Unique Blame in OR Compositions

CSL requires that in `c1 OR c2`, if both branches can breach, they must blame the same party. This ensures deterministic blame assignment.

For `MUST NOT`:

```l4
-- OK: Same party blamed in both branches
PARTY Employee
MUST NOT disclose
WITHIN 5 years
OR
PARTY Employee
MUST NOT compete
WITHIN 2 years

-- ERROR: Different parties could be blamed
PARTY Employee
MUST NOT disclose
WITHIN 5 years
OR
PARTY Employer
MUST pay severance
WITHIN 30 days
-- Error: Disjunction branches blame different parties (Employee vs Employer)
```

## Implementation Plan

### Phase 1: Lexer

**File:** `jl4-core/src/L4/Lexer.hs`

Add keywords:

```haskell
data TKeywords
  = ...
  | TKShant      -- NEW: SHANT keyword
  ...

keywords :: Map Text TKeywords
keywords = Map.fromList
  [ ...
  , ("SHANT"     , TKShant     )  -- NEW
  ...
  ]
```

Note: `MUST NOT` is lexed as two tokens (`TKMust`, `TKNot`), parsed together.

### Phase 2: Parser

**File:** `jl4-core/src/L4/Parser.hs`

Extend the deontic modal parser:

```haskell
data DeonticModal = DMust | DMay | DMustNot | DShant
  deriving (Eq, Show)

parseDeonticModal :: Parser DeonticModal
parseDeonticModal = choice
  [ DMustNot <$ (keyword TKMust *> keyword TKNot)  -- MUST NOT (two tokens)
  , DShant   <$ keyword TKShant                    -- SHANT
  , DMust    <$ keyword TKMust
  , DMay     <$ keyword TKMay
  ]
```

Parse BREACH as a terminal:

```haskell
parseTerminal :: Parser (Clause n)
parseTerminal = choice
  [ Fulfilled <$ keyword TKFulfilled
  , Breach    <$> parseBreachClause  -- NEW
  ]

parseBreachClause :: Parser (BreachInfo n)
parseBreachClause = do
  keyword TKBreach
  party  <- optional (keyword TKBy *> parsePartyExpr)
  reason <- optional (keyword TKBecause *> parseStringLiteral)
  pure MkBreachInfo { biParty = party, biReason = reason }
```

### Phase 3: AST Extension

**File:** `jl4-core/src/L4/Syntax.hs`

```haskell
-- Extend DeonticModal
data DeonticModal = Must | May | MustNot
  deriving (Eq, Show, Generic)

-- Add BREACH clause
data Clause n
  = Fulfilled
  | Breach (BreachInfo n)  -- NEW
  | Obligation ...
  | ...

data BreachInfo n = MkBreachInfo
  { biAnno   :: Anno
  , biParty  :: Maybe (Expr n)     -- Optional explicit party
  , biReason :: Maybe Text         -- Optional reason string
  }
  deriving (Eq, Show, Generic)
```

### Phase 4: Desugaring

**File:** `jl4-core/src/L4/Desugar.hs` (new or existing)

Transform `MUST NOT` to the equivalent external choice:

```haskell
desugarMustNot :: Obligation n -> Clause n
desugarMustNot obl@MkObligation{..} =
  ExternalChoice
    { ecAction    = oblAction
    , ecCondition = oblCondition
    , ecDeadline  = oblDeadline
    , ecThen      = oblLest `orDefault` Breach (inferParty obl)  -- If done: LEST
    , ecElse      = oblHence `orDefault` Fulfilled               -- If not done: HENCE
    }
```

Note the swap: for `MUST NOT`, the "then" branch (action taken) gets LEST semantics, and "else" branch (action not taken) gets HENCE semantics.

### Phase 5: Type Checking

**File:** `jl4-core/src/L4/TypeCheck.hs`

Add checks:

```haskell
-- Check BREACH has determinable party
checkBreach :: BreachInfo Resolved -> TC ()
checkBreach bi = case biParty bi of
  Just _  -> pure ()  -- Explicit party provided
  Nothing -> do
    ctx <- getPartyContext
    when (isNothing ctx) $
      throwError "BREACH requires a party (use 'BREACH BY party' or ensure enclosing PARTY clause)"

-- Check MUST NOT in OR has same blame
checkOrBlame :: Clause Resolved -> Clause Resolved -> TC ()
checkOrBlame c1 c2 = do
  parties1 <- getBlameParties c1
  parties2 <- getBlameParties c2
  unless (parties1 == parties2 || null parties1 || null parties2) $
    throwError "Disjunction branches must blame the same party for deterministic blame assignment"
```

### Phase 6: Evaluator

**File:** `jl4-core/src/L4/Evaluate/...`

`BREACH` evaluates to `ValBreached`:

```haskell
evalClause (Breach bi) = do
  party <- case biParty bi of
    Just p  -> evalPartyExpr p
    Nothing -> getContextParty
  time <- getCurrentTime
  reason <- case biReason bi of
    Just r  -> pure r
    Nothing -> generateBreachReason
  pure $ ValBreached $ MkReasonForBreach party time reason
```

### Phase 7: Pretty Printing & IDE Support

**Files:** `jl4-core/src/L4/Print.hs`, `jl4-lsp/...`

- Print `MUST NOT` and `SHANT` correctly
- Syntax highlighting for new keywords
- Hover info explaining prohibition semantics
- Autocomplete suggestions

## Test Cases

### Parser Tests

```haskell
describe "MUST NOT parsing" $ do
  it "parses MUST NOT with two tokens" $ do
    parse "PARTY p MUST NOT action WITHIN 30"
      `shouldParseTo` Obligation { oblModal = MustNot, ... }

  it "parses SHANT as equivalent" $ do
    parse "PARTY p SHANT action WITHIN 30"
      `shouldParseTo` Obligation { oblModal = MustNot, ... }

  it "parses BREACH as terminal" $ do
    parse "LEST BREACH"
      `shouldParseTo` Breach (MkBreachInfo Nothing Nothing)

  it "parses BREACH BY party" $ do
    parse "LEST BREACH BY Borrower"
      `shouldParseTo` Breach (MkBreachInfo (Just "Borrower") Nothing)

  it "parses BREACH BY party BECAUSE reason" $ do
    parse "LEST BREACH BY Borrower BECAUSE \"failed to pay\""
      `shouldParseTo` Breach (MkBreachInfo (Just "Borrower") (Just "failed to pay"))
```

### Semantic Tests

```haskell
describe "MUST NOT semantics" $ do
  it "FULFILLED when prohibition respected (action NOT taken)" $ do
    let contract = [l4|
      PARTY Employee
      MUST NOT disclose
      WITHIN 30
      |]
    -- No disclosure event within 30 days
    evaluate contract [Day 31] `shouldBe` Fulfilled

  it "BREACH when prohibition violated (action IS taken)" $ do
    let contract = [l4|
      PARTY Employee
      MUST NOT disclose
      WITHIN 30
      |]
    -- Disclosure event at day 15
    evaluate contract [(Day 15, Disclose)] `shouldBe` Breached "Employee"

  it "LEST clause triggered on violation" $ do
    let contract = [l4|
      PARTY Employee
      MUST NOT disclose
      WITHIN 30
      LEST PARTY Employee
           MUST `pay penalty`
           WITHIN 7
      |]
    -- Disclosure triggers penalty obligation
    let result = evaluate contract [(Day 15, Disclose)]
    result `shouldSatisfy` isObligation "pay penalty"
```

### Integration with #TRACE

```haskell
describe "#TRACE with MUST NOT" $ do
  it "shows FULFILLED when prohibition respected" $ do
    let contract = [l4|
      `NDA` MEANS
        PARTY Employee
        MUST NOT `disclose information`
        WITHIN 365

      #TRACE `NDA` AT Day 0 WITH
        -- No events (prohibition respected)
      |]
    traceResult `shouldBe` "FULFILLED"

  it "shows BREACH when prohibition violated" $ do
    let contract = [l4|
      `NDA` MEANS
        PARTY Employee
        MUST NOT `disclose information`
        WITHIN 365

      #TRACE `NDA` AT Day 0 WITH
        PARTY Employee DOES `disclose information` AT Day 100
      |]
    traceResult `shouldContain` "BREACH BY Employee"
```

## Edge Cases

### 1. MUST NOT with Immediate Deadline

```l4
PARTY p
MUST NOT action
WITHIN 0  -- Immediate: breach if action EVER happens at time 0
```

If an action occurs at the exact start time, is it a violation? **Yes** — `WITHIN 0` means "within the first instant."

### 2. MUST NOT with HENCE and LEST

Both can be specified. Note the natural reading:

```l4
PARTY Employee
MUST NOT disclose
WITHIN 365
HENCE PARTY Employer        -- If prohibition respected (365 days pass, no disclosure)
      MUST `pay bonus`      -- "hence you get a bonus"
      WITHIN 30
LEST  PARTY Employee        -- If prohibition violated (disclosure happened)
      MUST `pay damages`    -- "lest you pay damages"
      WITHIN 14
```

This reads naturally: "Employee must not disclose for 365 days, **hence** employer pays bonus, **lest** employee pays damages."

### 3. Nested MUST NOT in AND/OR

```l4
-- AND: Both prohibitions must be respected
PARTY Employee
MUST NOT disclose
WITHIN 365
AND
PARTY Employee
MUST NOT compete
WITHIN 730

-- OR: Respecting either prohibition suffices
PARTY Employee
MUST NOT disclose
WITHIN 365
LEST PARTY Employee
     MUST `pay fine` WITHIN 7
OR
PARTY Employee
MUST NOT compete
WITHIN 365
LEST PARTY Employee
     MUST `pay fine` WITHIN 7
```

### 4. BREACH in Non-LEST Position

Should `BREACH` be allowed only in `LEST` clauses?

**Recommendation:** Allow `BREACH` anywhere a terminal is allowed, for flexibility:

```l4
-- OK: In LEST
LEST BREACH

-- OK: In HENCE (unusual but valid - "breach even on success")
HENCE BREACH

-- OK: As standalone clause (immediate breach)
IF some_impossible_condition
   THEN BREACH BY Contractor
   ELSE FULFILLED
```

### 5. BREACH Without Party in Template

```l4
-- Template: party is parametric
GIVEN p IS A Party
`Prohibition Template` MEANS
  PARTY p
  MUST NOT action
  WITHIN 30
  LEST BREACH  -- Party inferred from enclosing PARTY (p)
```

The evaluator should resolve `p` to the actual party at instantiation time.

## Differences from CSL

| Feature                | CSL                                        | L4 (This Spec)               |
| ---------------------- | ------------------------------------------ | ---------------------------- |
| Prohibition syntax     | External choice + unfulfillable obligation | Native `MUST NOT` / `SHANT`  |
| Explicit BREACH        | No (only `fulfilment` is terminal)         | Yes (`BREACH` is terminal)   |
| Blame in BREACH        | Implicit from obligation                   | Explicit `BY party` optional |
| Prohibition HENCE/LEST | N/A (workaround only)                      | Full support                 |

## Migration Path

### Backward Compatibility

Both features are purely additive:

- Existing L4 code without `MUST NOT` or `BREACH` continues to work
- The workaround pattern (`IF action THEN unfulfillable ELSE FULFILLED`) remains valid

### Deprecation of Workaround

Consider adding a lint warning:

```
Warning: This pattern appears to encode a prohibition using an unfulfillable
obligation. Consider using `MUST NOT` for clarity:

  Before:
    IF disclose THEN PARTY p MUST impossible PROVIDED FALSE WITHIN 0
    ELSE FULFILLED

  After:
    PARTY p MUST NOT disclose WITHIN deadline
```

## References

- Hvitved, Tom. "Contract Formalisation and Modular Implementation of Domain-Specific Languages." PhD thesis, IT University of Copenhagen, Chapter 2.
- `doc/regulative.md` — L4's design notes on deontic operators
- `doc/regulative-spec.org` — Technical specification for regulative rules
- `doc/advanced-course-ai/module-a11-regulative-rules.md` — User documentation

## Open Questions

1. **Keyword choice:** Should we support both `MUST NOT` (two words) and `SHANT` (one word), or pick one?

   - Recommendation: Support both, with `MUST NOT` as canonical in docs.

2. **BECAUSE clause:** Is the reason string sufficient, or should it support expressions?

   - Recommendation: Start with string literals, extend later if needed.

3. **Visualization:** How should `MUST NOT` appear in Graphviz contract diagrams?

   - Recommendation: Show as a "prohibition node" with distinctive styling (e.g., red border, "⊘" symbol).

4. **BREACH propagation:** In `c1 AND c2`, if `c1` is `BREACH`, should evaluation short-circuit?
   - Recommendation: Yes, following CSL's "earliest breach wins" for conjunction.
