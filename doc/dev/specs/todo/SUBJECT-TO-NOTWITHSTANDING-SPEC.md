# The Semantics of "Notwithstanding", "Subject to", and "Despite"

**Survey, Examples, Logical Theory, Modifiers, and an Evaluation Pipeline for L4**

**Status:** Draft (Revised with Evaluation Pipeline)
**Date:** 2025-01-27
**Branch:** mengwong/spec-notwithstanding

---

## 1. Introduction and Motivation

Legal and regulatory texts routinely use phrases such as "subject to", "notwithstanding", and "despite" to structure how rules operate. These phrases are deceptively uniform in surface form but express multiple, distinct semantic mechanisms.

Early interpretive canons correctly identify some of these mechanisms as priority relations among norms. However, real-world legal reasoning reveals that priority is only one—and often not the dominant—semantic role these phrases play.

Instead, they frequently express:
- procedural constraints,
- conditional applicability,
- value transformations,
- and even temporary reinterpretations of a rule's own vocabulary.

For L4, whose purpose is to encode legal rules as executable logic, faithfully capturing these distinctions is essential.

### 1.1 Key Insight: Linguistic Signals, Not Semantic Operators

**"Subject to", "notwithstanding", and "despite" are linguistic signals, not semantic operators.**

They may introduce:
- guards,
- filters,
- modifiers,
- interpretive rewrites,
- or priority relations,

depending on what they range over and how they compose. The same surface phrase can realize entirely different semantic mechanisms.

### 1.2 Directionality in Document Structure

| Keyword | Appears In | Points To | Typical Effect |
|---------|-----------|-----------|----------------|
| `SUBJECT TO` | Main/subordinate clause | Modifying clause | "I am modified/constrained by that" |
| `NOTWITHSTANDING` | Prevailing clause | Overridden clause | "I override that provision" |
| `DESPITE` | Prevailing clause | Overridden clause | Same as NOTWITHSTANDING |

As noted in drafting literature: "notwithstanding looks back whilst subject to looks forward."

---

## 2. Survey of Scholarly and Legal Literature (Summary)

Interpretive and drafting literature converges on the following points:

1. "Notwithstanding" and "despite" are consistently treated as superordinating (priority-asserting) language.
2. "Subject to" is treated as subordinating language when it ranges over other provisions.
3. Courts and drafters implicitly—but systematically—treat:
   - "subject to consent/approval/compliance" as **conditions precedent**, not priority.
   - "subject to Section X" as **priority**.
4. Tax, pricing, and regulatory schemes routinely use "subject to" to express **conditional modifications**, not overrides.
5. Some regimes go further, using "subject to" to **temporarily redefine classifications** that other rules depend on.

The literature does not unify these phenomena under a single operator, but practice consistently distinguishes them.

---

## 3. A Refined Taxonomy of Semantic Roles

The same surface phrases can realize distinct semantic roles. These roles must be separated in any faithful formalization.

### 3.1 Semantic Roles

| Role | Description | Conflict Required? |
|------|-------------|-------------------|
| **Guard / Precondition** | Enables or disables an action or rule | No |
| **Input Filter** | Restricts the domain where a rule applies | No |
| **Input Transformer** (Homoiconic) | Rewrites how inputs or predicates are interpreted | No |
| **Output Modifier** | Transforms the result of a rule | No |
| **Priority / Override** | Resolves inconsistency among rules | **Yes** |

### 3.2 Selection vs Composition

A fundamental distinction:
- **Priority** is *selection* among competing rules.
- **Guards, filters, transformers, and modifiers** are *composition* with a rule.

**Only priority presupposes inconsistency.**

### 3.3 Two Levels of Legal Meaning

Legal texts give rise to:

1. **Object-level rule behavior**
   - actions, values, classifications, applicability
2. **Meta-level rule interaction**
   - conflict detection and resolution

Only the second level involves priority.

---

## 4. Detailed Semantics of Each Role

### 4.1 Guards (Enablement / Precondition)

**Function:** Prevent an action or rule from being available until a condition is satisfied.

**Typical signals:** consent, approval, authorization, compliance

**Example:**
```
"The tenant may assign the lease, subject to the landlord's consent."
```

**Reasoning:**
- Assignment is unavailable unless consent is obtained.
- No conflict with any other rule is asserted.
- This constrains the state transition graph of lawful actions.

**Computational model:**
```haskell
action_available(x) = guard_satisfied(x)
-- If guard fails, action is not in the set of available actions
```

---

### 4.2 Input Filters (Domain Restriction)

**Function:** Narrow the situations in which a rule applies. Do not transform the rule or its outputs.

**Example:**
```
"This Part applies to all employees, subject to the exclusions in Schedule 2."
```

**Reasoning:**
- The rule's domain is restricted to employees not listed in Schedule 2.
- The rule itself is unchanged; it simply doesn't fire for excluded cases.

**Computational model:**
```haskell
rule_applies(x) = in_base_domain(x) && not(excluded(x))
-- Filter happens before rule evaluation
```

---

### 4.3 Input Transformers (Homoiconic Predicate Rewriting)

**Function:** Rewrite classifications or predicates that a rule depends on. Temporarily alter the rule's interpretive environment. Affects all downstream logic.

**This is common in tax, regulatory, and emergency regimes.**

**Example:**
```
"GST/VAT taxes operate as usual, subject to an exception between
December 25 and January 1 that all beverage items will qualify
to be taxed as food items."
```

**Reasoning:**
- The tax rules remain unchanged.
- During a specified period, the classification predicates those rules rely on are altered.
- Beverages are treated as if they satisfy the "food" predicate.

**This is neither filtering nor output adjustment—it is a temporary reinterpretation of the rule's ontology.**

**Computational model:**
```haskell
-- Normal interpretation
is_food(x) = x.category == Food

-- Transformed interpretation (Dec 25 - Jan 1)
is_food(x) = x.category == Food || x.category == Beverage

-- The tax rule itself is unchanged:
tax_rate(x) = if is_food(x) then 0% else 10%
```

**Test case:**
- Dec 24 beverage → 10% (normal interpretation)
- Dec 26 beverage → 0% (transformed interpretation)
- Jan 2 beverage → 10% (normal interpretation restored)

---

### 4.4 Output Modifiers (Result Transformation)

**Function:** Transform results (prices, rates, quantities, penalties) after rule evaluation. Often conditional. Do not alter applicability or interpretation.

**Example:**
```
"Subject to a 50% discount on public holidays, parking costs
$4 per hour on weekends and $6 per hour on weekdays."
```

**Reasoning:**
1. Compute the base rate (weekday/weekend).
2. If the day is a public holiday, apply a discount.

The base rule is not defeated or replaced; its output is conditionally transformed.

**Computational model:**
```haskell
base_rate(day) = if is_weekend(day) then 4 else 6

final_rate(day) =
  let base = base_rate(day)
  in if is_holiday(day)
     then base * 0.5  -- output modifier
     else base
```

**Test case:**
- Weekend + holiday → $2
- Weekday + holiday → $3
- Weekend, no holiday → $4

---

### 4.5 Priority / Override (Conflict Resolution)

**Function:** Resolves inconsistency among rules. Applies only when multiple rules produce incompatible outcomes.

**Example:**
```
"Notwithstanding Section 5, the landlord may terminate
the lease for non-payment."
```

**Reasoning:**
- Two applicable rules conflict.
- This clause specifies which one prevails.

**Computational model:**
```haskell
-- Rule A and Rule B both apply and produce incompatible results
-- Priority declaration: B notwithstanding A
final_result = if conflict(result_A, result_B)
               then result_B  -- B wins
               else combine(result_A, result_B)
```

**Key invariant:** Priority never changes the meaning of a rule—only which result survives.

---

## 5. Evaluation Pipeline

To accommodate all of the above, rule evaluation must follow a disciplined pipeline. This pipeline is conceptual and does not presuppose types or syntax.

### 5.1 Evaluation Stages

```
┌─────────────────────────────────────────────────────────────────┐
│  1. INTERPRETATION LAYER                                        │
│     Apply input transformers                                    │
│     Rewrite classifications, predicates, or meanings locally    │
├─────────────────────────────────────────────────────────────────┤
│  2. APPLICABILITY LAYER                                         │
│     Apply input filters                                         │
│     Determine whether the rule applies to the situation         │
├─────────────────────────────────────────────────────────────────┤
│  3. ENABLEMENT LAYER                                            │
│     Apply guards / preconditions                                │
│     Determine whether the action or effect is available         │
├─────────────────────────────────────────────────────────────────┤
│  4. BASE RULE EVALUATION                                        │
│     Compute obligations, permissions, values, or outcomes       │
├─────────────────────────────────────────────────────────────────┤
│  5. OUTPUT TRANSFORMATION                                       │
│     Apply output modifiers                                      │
│     Adjust results (discounts, caps, multipliers, etc.)         │
├─────────────────────────────────────────────────────────────────┤
│  6. CONFLICT DETECTION                                          │
│     Identify incompatible results from multiple rules           │
├─────────────────────────────────────────────────────────────────┤
│  7. PRIORITY RESOLUTION                                         │
│     Apply "notwithstanding / subject to / despite" priority     │
│     Select surviving outcomes                                   │
└─────────────────────────────────────────────────────────────────┘
```

### 5.2 Key Invariants

1. **Priority never precedes object-level evaluation.**
2. **Modifiers never resolve conflicts.**
3. **Input transformers affect all downstream stages.**
4. **Guards and filters do not rewrite rule meaning.**
5. **Output modifiers do not affect applicability.**

### 5.3 Pipeline as Function Composition

```haskell
evaluate :: Context -> Rule -> Input -> Maybe Result
evaluate ctx rule input =
  input
    |> applyTransformers ctx      -- Stage 1: rewrite predicates
    |> applyFilters rule          -- Stage 2: check applicability
    |> applyGuards rule           -- Stage 3: check enablement
    |> evaluateBase rule          -- Stage 4: compute result
    |> applyOutputModifiers ctx   -- Stage 5: transform result

resolveConflicts :: [Result] -> Result
resolveConflicts results =
  results
    |> detectConflicts            -- Stage 6
    |> applyPriority              -- Stage 7
```

---

## 6. Corpus of Examples (Reclassified)

We reclassify the corpus using the refined taxonomy.

### Classification Key

| Code | Role |
|------|------|
| **G** | Guard / Precondition |
| **F** | Input Filter |
| **T** | Input Transformer (Homoiconic) |
| **M** | Output Modifier |
| **P** | Priority / Override |

---

### 6.1 Constitutional and Charter Examples

| # | Source | Text | Role | Notes |
|---|--------|------|------|-------|
| 1 | Canadian Charter s.33 | "Parliament may declare that the Act shall operate NOTWITHSTANDING sections 2 or 7-15" | **P** | Legislature overrides Charter rights |
| 2 | Canadian Charter s.1 | "Rights guaranteed SUBJECT TO reasonable limits as can be demonstrably justified" | **F** | Rights apply only within "reasonable limits" domain |
| 3 | Indian Constitution Art.31B | "NOTWITHSTANDING anything in this Part, Ninth Schedule Acts not void" | **P** | Override fundamental rights challenges |
| 4 | US Constitution Art.VI | "Supreme Law of the Land... any State law to the Contrary NOTWITHSTANDING" | **P** | Federal supremacy |

---

### 6.2 Corporate Law Examples

| # | Source | Text | Role | Notes |
|---|--------|------|------|-------|
| 5 | UK Companies Act s.168 | "Remove director NOTWITHSTANDING anything in constitution or agreement" | **P** | Statutory right overrides contract |
| 6 | Singapore Companies Act | "Remove director SUBJECT TO contrary provision in articles" | **F** | Removal right filtered by articles |
| 7 | Companies Act | "SUBJECT TO sections 549, 551, 559, company may issue debentures" | **F** | Power filtered by other sections |
| 8 | Singapore s.25C | "Transaction voidable NOTWITHSTANDING section 25B" | **P** | Voidability overrides validity |

---

### 6.3 Contract Clause Examples

| # | Source | Text | Role | Notes |
|---|--------|------|------|-------|
| 9 | Assignment | "Assignment prohibited. NOTWITHSTANDING, affiliates permitted" | **P** | Exception overrides prohibition |
| 10 | Termination | "30-day notice. NOTWITHSTANDING, immediate for breach" | **P** | Breach exception overrides notice |
| 11 | Lease assignment | "Tenant may assign SUBJECT TO landlord's consent" | **G** | Consent is guard/precondition |
| 12 | Confidentiality | "NOTWITHSTANDING termination, confidentiality survives 5 years" | **P** | Survival overrides termination |
| 13 | Liability cap | "NOTWITHSTANDING anything contrary, liability ≤ $50M" | **P** + **M** | Priority + output cap |
| 14 | Security deposit | "Repay deposit SUBJECT TO proper deductions" | **M** | Deductions modify output |

---

### 6.4 Real Estate Examples

| # | Source | Text | Role | Notes |
|---|--------|------|------|-------|
| 15 | Purchase | "Complete SUBJECT TO satisfactory survey" | **G** | Survey is precondition |
| 16 | Purchase | "SUBJECT TO buyer obtaining financing" | **G** | Financing is precondition |
| 17 | Purchase | "SUBJECT TO buyer selling current home by Jan 1" | **G** | Sale is precondition |
| 18 | Lease holdover | "NOTWITHSTANDING any provision, holdover = default" | **P** | Override other provisions |
| 19 | Lease premises | "NOTWITHSTANDING s.1.2, 8 acres excluded" | **F** | Filter premises domain |

---

### 6.5 Tax and Pricing Examples

| # | Source | Text | Role | Notes |
|---|--------|------|------|-------|
| 20 | Parking | "SUBJECT TO 50% holiday discount, $4 weekend / $6 weekday" | **M** | Discount modifies output |
| 21 | GST/VAT | "SUBJECT TO Dec 25-Jan 1 exception: beverages taxed as food" | **T** | Homoiconic transformer |
| 22 | IRC §527 | "NOTWITHSTANDING any other provision, include exempt function amounts" | **P** | Override exclusions |
| 23 | IRC §165 | "NOTWITHSTANDING (a), disaster loss may be taken in preceding year" | **M** | Timing modifier |
| 24 | IRC §139 | "NOTWITHSTANDING any provision, no deduction if already excluded" | **P** | No-double-benefit override |

---

### 6.6 Employment Examples

| # | Source | Text | Role | Notes |
|---|--------|------|------|-------|
| 25 | At-will | "NOTWITHSTANDING anything, either party may terminate with 60 days" | **P** | Override other restrictions |
| 26 | Cause | "NOTWITHSTANDING foregoing, not Cause unless Board resolution" | **F** | "Cause" filtered by Board |
| 27 | Severance | "Severance SUBJECT TO signing release" | **G** | Release is precondition |

---

### 6.7 Trade and International Law

| # | Source | Text | Role | Notes |
|---|--------|------|------|-------|
| 28 | GATT XII | "NOTWITHSTANDING Art.XI, may restrict imports, SUBJECT TO following" | **P** + **F** | Override + filter |
| 29 | WTO TRIMs | "NOTWITHSTANDING Art.2, may apply TRIM during transition" | **P** | Transition override |
| 30 | WTO Enabling | "NOTWITHSTANDING Art.I, preferential treatment for developing countries" | **P** | Override MFN |
| 31 | GDPR Art.21(5) | "NOTWITHSTANDING Directive 2002/58/EC, object by automated means" | **P** | GDPR overrides e-Privacy |

---

### 6.8 Data Protection

| # | Source | Text | Role | Notes |
|---|--------|------|------|-------|
| 32 | Singapore PDPA | "Duty to respond SUBJECT TO exceptions in s.21(2)-(4)" | **F** | Duty filtered by exceptions |
| 33 | GDPR Art.9 | "Special categories SUBJECT TO additional protections" | **M** | Additional constraints modify processing |

---

### 6.9 Partnership and Business

| # | Source | Text | Role | Notes |
|---|--------|------|------|-------|
| 34 | Indian Partnership s.11 | "SUBJECT TO this Act, rights may be determined by contract" | **F** | Contractual freedom filtered |
| 35 | Indian Partnership s.11(2) | "NOTWITHSTANDING s.27 Contract Act, non-compete valid" | **P** | Override general invalidity |

---

## 7. Corpus Analysis (Revised)

### 7.1 Category Distribution

| Role | Count | Percentage |
|------|-------|------------|
| **P** Priority/Override | 22 | 63% |
| **G** Guard/Precondition | 7 | 20% |
| **F** Input Filter | 11 | 31% |
| **T** Input Transformer | 1 | 3% |
| **M** Output Modifier | 6 | 17% |

*Note: Some examples combine roles (e.g., P+F, P+M), so percentages exceed 100%.*

### 7.2 Key Observations

1. **Priority remains dominant** (63%), but the revised taxonomy reveals it's not the only pattern.

2. **Guards are distinct from Filters**: "Subject to consent" (guard) vs "subject to exceptions in Schedule 2" (filter). Guards control action availability; filters control rule applicability.

3. **Input Transformers are rare but real**: The GST/beverage-as-food example is unusual but demonstrates a genuine semantic pattern—homoiconic predicate rewriting.

4. **Output Modifiers are underrepresented in statutes**: More common in pricing/commercial contexts than in legislation.

5. **Combined patterns remain common**: Real-world provisions often combine roles (e.g., "NOTWITHSTANDING X, but SUBJECT TO Y" = Priority + Filter).

### 7.3 Disambiguation Heuristics

| "Subject to" ranges over... | Likely Role |
|----------------------------|-------------|
| Another section/provision | Priority (subordination) |
| Consent/approval/authorization | Guard |
| Exceptions/exclusions | Filter |
| A discount/rate/adjustment | Output Modifier |
| A reclassification of inputs | Input Transformer |

---

## 8. Problematic Patterns

### 8.1 Scope Ambiguity

```
"NOTWITHSTANDING anything to the contrary in this Agreement..."
```

What does "anything" include? The drafter may not have enumerated overridden provisions.

**Recommendation for L4:** Require explicit references.

### 8.2 Circular Priority

```
Section A: "NOTWITHSTANDING Section B, ..."
Section B: "NOTWITHSTANDING Section A, ..."
```

Creates a paradox.

**Computational solution:** Detect cycles in priority graph; flag as error.

### 8.3 Mixed Roles in Single Clause

```
"NOTWITHSTANDING the foregoing, but SUBJECT TO Section 5.04,
Landlord shall reinstate utilities."
```

Requires parsing:
- Priority (over "the foregoing")
- Filter or Guard (by Section 5.04)

### 8.4 Distinguishing Guards from Filters

Both use "subject to", but:
- **Guard**: "may do X subject to consent" → X unavailable without consent
- **Filter**: "rule applies subject to exceptions" → rule doesn't fire for excepted cases

The distinction matters for evaluation order (Guards at Stage 3, Filters at Stage 2).

---

## 9. Implications for L4

### 9.1 Proposed Constructs

| Role | Possible L4 Syntax |
|------|-------------------|
| Guard | `ACTION X REQUIRES consent` or `X GIVEN THAT guard` |
| Filter | `RULE X APPLIES WHEN condition` or `EXCEPT WHEN condition` |
| Transformer | `TREATING beverages AS food DURING period` |
| Output Modifier | `RESULT MODIFIED BY discount WHEN holiday` |
| Priority | `RULE X NOTWITHSTANDING Y` or `RULE X HAS PRIORITY OVER Y` |

### 9.2 Composition with Existing L4 Features

L4 already has:
- `CONSIDER` / `WHEN` for conditional logic (can express guards, filters)
- `MEANS` / `DECIDE` for definitions
- `OTHERWISE` for defaults

New constructs needed:
- **Priority declarations** between rules
- **Input transformers** for homoiconic rewriting
- **Output modifiers** for result transformation

### 9.3 Static Analysis Opportunities

- Detect circular priority declarations
- Verify priority graph is acyclic
- Flag "anything to the contrary" as ambiguous
- Check for unreachable rules (always overridden)
- Validate transformer scopes don't overlap inconsistently

---

## 10. Conclusion

"Subject to", "notwithstanding", and "despite" are **linguistic signals, not semantic operators**. They may introduce guards, filters, modifiers, interpretive rewrites, or priority relations, depending on what they range over and how they compose.

For L4, the correct foundation is:
- **Rich object-level composition** (guards, filters, transformers, modifiers)
- **Explicit interpretive control** (homoiconic transformers)
- **Priority as a last-stage, conflict-only mechanism**

With this clarified, we are now in a position to discuss types and operators without semantic confusion.

---

## 11. L4 Syntax Proposals

This section explores concrete syntax options compatible with existing L4 constructs.

### 11.1 Current L4 Syntax Summary

L4 rules are expressed as functions with:
```l4
GIVEN param1 IS A Type1, param2 IS A Type2
GIVETH A ReturnType
DECIDE `function name` param1 param2 IS
  expression
```

Key existing constructs:
- `DECIDE ... IS/IF/MEANS` - function definition
- `CONSIDER ... WHEN ... THEN` - pattern matching
- `WHERE` / `LET ... IN` - local bindings
- `AND`, `OR`, `NOT` - boolean operators
- `IF ... THEN ... ELSE` - conditionals
- Sections (`§`) for organizing rules
- Regulative modals: `MUST`, `MAY`, `MUST NOT`

### 11.2 Design Principles

1. **Declarative over imperative**: Express relations, not evaluation order
2. **Isomorphic to legal text**: Syntax should read like the source document
3. **Composable**: New constructs should compose with existing ones
4. **Explicit references**: Avoid "anything to the contrary" ambiguity
5. **Statically analyzable**: Priority graphs, transformer scopes should be checkable

### 11.3 Syntax Proposals by Semantic Role

---

#### 11.3.1 Guards (Preconditions)

**Context:** "Tenant may assign lease, subject to landlord's consent"

**Option A: REQUIRES clause on actions**
```l4
GIVEN tenant IS A Tenant, lease IS A Lease
DECIDE `tenant may assign lease` tenant lease
  REQUIRES `landlord consent obtained` tenant lease
  IS TRUE
```

**Option B: GIVEN THAT as guard**
```l4
GIVEN tenant IS A Tenant, lease IS A Lease
GIVEN THAT `landlord consent obtained` tenant lease
DECIDE `tenant may assign lease` tenant lease IS TRUE
```

**Option C: SUBJECT TO as explicit guard keyword**
```l4
GIVEN tenant IS A Tenant, lease IS A Lease
DECIDE `tenant may assign lease` tenant lease
  SUBJECT TO `landlord consent obtained` tenant lease
  IS TRUE
```

**Recommendation:** Option C is most isomorphic to legal text but requires parsing "SUBJECT TO" differently based on what follows (a condition vs a section reference).

---

#### 11.3.2 Input Filters (Domain Restriction)

**Context:** "This Part applies to all employees, subject to exclusions in Schedule 2"

**Option A: APPLIES WHEN / EXCEPT WHEN clauses**
```l4
§ `Part 3: Annual Leave`

@applies WHEN `is employee` person AND NOT `excluded under Schedule 2` person

GIVEN person IS A Person
DECIDE `annual leave entitlement` person IS 20
```

**Option B: GIVEN with filter predicate**
```l4
GIVEN person IS A Person
  WHERE `is employee` person
  EXCEPT WHEN `excluded under Schedule 2` person
DECIDE `annual leave entitlement` person IS 20
```

**Option C: Section-level SCOPE declaration**
```l4
§ `Part 3: Annual Leave`
  SCOPE `is employee` person
  EXCEPT `excluded under Schedule 2` person

DECIDE `annual leave entitlement` person IS 20
```

**Recommendation:** Option C for section-level filters; Option B for rule-level filters.

---

#### 11.3.3 Input Transformers (Homoiconic Predicate Rewriting)

**Context:** "Subject to Dec 25-Jan 1 exception: beverages taxed as food"

This is the most novel construct. It rewrites how predicates are interpreted.

**Option A: TREATING ... AS ... DURING**
```l4
§ `Holiday Tax Exception`
  TREATING `is beverage` AS `is food`
  DURING `december 25` TO `january 1`

-- The tax rules themselves are unchanged:
GIVEN item IS AN Item
DECIDE `tax rate` item IS
  IF `is food` item THEN 0%
  ELSE 10%
```

**Option B: REINTERPRET block**
```l4
§ `Holiday Tax Exception`
  REINTERPRET DURING `december 25` TO `january 1`
    `is food` item MEANS `is food` item OR `is beverage` item
```

**Option C: Context-scoped predicate override**
```l4
§ `Holiday Tax Exception`

GIVEN item IS AN Item, date IS A Date
DECIDE `is food for tax purposes` item date IS
  `is food` item
  OR (`is beverage` item AND `is holiday period` date)

-- Tax rule uses the context-aware predicate:
DECIDE `tax rate` item date IS
  IF `is food for tax purposes` item date THEN 0%
  ELSE 10%
```

**Recommendation:** Option C is most explicit and doesn't require new syntax, but loses the "homoiconic" quality. Option A is most readable but requires temporal scoping infrastructure.

---

#### 11.3.4 Output Modifiers (Result Transformation)

**Context:** "Parking costs $4/weekend, $6/weekday, subject to 50% holiday discount"

**Option A: MODIFIED BY clause**
```l4
GIVEN day IS A Day
DECIDE `parking rate` day IS
  IF `is weekend` day THEN 4 ELSE 6
  MODIFIED BY `holiday discount` day

GIVEN day IS A Day, base IS A NUMBER
DECIDE `holiday discount` day base IS
  IF `is holiday` day THEN base * 0.5 ELSE base
```

**Option B: Pipeline with THEN APPLY**
```l4
GIVEN day IS A Day
DECIDE `parking rate` day IS
  (IF `is weekend` day THEN 4 ELSE 6)
  THEN APPLY `holiday discount` day
```

**Option C: Explicit base + modifier pattern**
```l4
GIVEN day IS A Day
DECIDE `base parking rate` day IS
  IF `is weekend` day THEN 4 ELSE 6

GIVEN day IS A Day
DECIDE `parking rate` day IS
  LET base = `base parking rate` day
  IN IF `is holiday` day THEN base * 0.5 ELSE base
```

**Recommendation:** Option C is already expressible in L4 and most explicit. Options A/B add syntactic sugar but may obscure the computation.

---

#### 11.3.5 Priority / Override (Conflict Resolution)

**Context:** "Notwithstanding Section 5, landlord may terminate for non-payment"

This is the key new construct - declaring which rule wins when both fire.

**Option A: NOTWITHSTANDING as rule modifier**
```l4
§ `Section 5: Termination Rights`

DECIDE `landlord may terminate` lease IS
  `notice period satisfied` lease

§ `Section 6: Non-Payment Exception`

DECIDE `landlord may terminate` lease
  NOTWITHSTANDING `Section 5`
  IS `tenant in arrears` lease
```

**Option B: PRIORITY declaration (separate from rules)**
```l4
§ `Section 5: Termination Rights`
DECIDE `s5 termination right` lease IS `notice period satisfied` lease

§ `Section 6: Non-Payment Exception`
DECIDE `s6 termination right` lease IS `tenant in arrears` lease

§ `Priority`
PRIORITY `s6 termination right` OVER `s5 termination right`
  -- or: `s6 termination right` NOTWITHSTANDING `s5 termination right`

-- Final combined rule:
DECIDE `landlord may terminate` lease IS
  `s5 termination right` lease OR `s6 termination right` lease
```

**Option C: Explicit conflict resolution function**
```l4
DECIDE `landlord may terminate` lease IS
  RESOLVE
    `s5 termination right` lease
    `s6 termination right` lease
  WITH PRIORITY `s6 termination right`
```

**Option D: Section-level override declaration**
```l4
§ `Section 6: Non-Payment Exception`
  NOTWITHSTANDING `Section 5`

DECIDE `landlord may terminate` lease IS
  `tenant in arrears` lease
```

**Recommendation:** Option D for section-level overrides (most common pattern); Option A for rule-level overrides.

---

### 11.4 Composite Example

A complete example combining multiple roles:

```l4
§ `Alcohol Sales Regulations`

-- Type declarations
DECLARE Seller HAS
  `is body corporate` IS A BOOLEAN
  `is public house` IS A BOOLEAN
  `has conviction` IS A BOOLEAN

DECLARE Day HAS
  `is public holiday` IS A BOOLEAN
  `is weekend` IS A BOOLEAN

-- Section 1: General prohibition (base rule)
§ `Section 1: Prohibition`
  SCOPE `is body corporate` seller

GIVEN seller IS A Seller
DECIDE `may sell alcohol` seller IS FALSE

-- Section 2: Licensed premises exception (override)
§ `Section 2: Licensed Premises`
  NOTWITHSTANDING `Section 1`
  SCOPE `is public house` seller OR `is hotel` seller

GIVEN seller IS A Seller
DECIDE `may sell alcohol` seller IS TRUE

-- Section 3: Conviction disqualification (override of override)
§ `Section 3: Disqualification`
  NOTWITHSTANDING `Section 2`

GIVEN seller IS A Seller
DECIDE `may sell alcohol` seller
  REQUIRES NOT `has conviction` seller
  IS TRUE

-- Section 4: Holiday pricing (output modifier)
§ `Section 4: Pricing`

GIVEN day IS A Day
DECIDE `alcohol duty rate` day IS
  LET base = IF `is weekend` day THEN 0.15 ELSE 0.20
  IN IF `is public holiday` day THEN base * 0.5 ELSE base
```

### 11.5 Static Analysis Implications

The proposed syntax enables:

1. **Priority graph construction**: Extract all `NOTWITHSTANDING` declarations and build a directed graph. Check for cycles.

2. **Scope coverage verification**: For sections with `SCOPE` declarations, verify all rules in the section respect the scope.

3. **Transformer conflict detection**: If two transformers affect the same predicate in overlapping time periods, flag a warning.

4. **Guard completeness**: For rules with `REQUIRES`, verify the guard condition is satisfiable.

5. **Unreachable rule detection**: If rule A is always overridden by rule B (same inputs, B always wins), flag A as potentially dead code.

### 11.6 Open Questions

1. **Syntactic disambiguation**: How to distinguish "SUBJECT TO Section 5" (priority) from "SUBJECT TO consent obtained" (guard)?
   - Option: Different keywords (`SUBORDINATE TO` vs `REQUIRES`)
   - Option: Type-based disambiguation (section reference vs boolean expression)

2. **Scope of transformers**: Should transformers affect only the current section, or propagate to subsections?

3. **Priority transitivity**: If A > B and B > C, is A > C implicit?

4. **Conflict semantics**: What happens when rules don't conflict but priority is declared anyway?

5. **Evaluation traces**: How should the trace display priority resolution and modifier application?

---

## 12. Constitutive vs Regulative: A 5×2 Matrix

The five semantic roles interact differently with **constitutive rules** (DECIDE/MEANS - what something IS) versus **regulative rules** (MUST/MAY/MUST NOT - what actors SHALL do). This section explores each combination.

### 12.1 The Matrix

| Role | Constitutive (IS) | Regulative (MUST/MAY) |
|------|-------------------|----------------------|
| **Guard** | Classification requires precondition | Action requires precondition |
| **Filter** | Definition has restricted domain | Duty applies to subset |
| **Transformer** | Redefines classification | Redefines trigger conditions |
| **Modifier** | Adjusts computed value | Adjusts the duty/remedy |
| **Priority** | Definitional conflict resolution | Deontic conflict resolution |

---

### 12.2 Guards × Constitutive/Regulative

#### Guard + Constitutive
The classification itself depends on a precondition being satisfied.

**Real-world examples:**

| Source | Text | Analysis |
|--------|------|----------|
| US Voter Registration | "A person IS a qualified voter SUBJECT TO having registered with the Electoral Commission" | Citizenship + age define the class; registration is the guard |
| Securities Act | "A security IS exempt under Regulation D SUBJECT TO the issuer having filed Form D" | The exemption classification requires the filing precondition |
| Professional Licensing | "A person IS a licensed attorney SUBJECT TO having passed the bar examination and character review" | The classification depends on multiple guards |
| UK Charities Act | "An organization IS a registered charity SUBJECT TO registration with the Charity Commission" | Registration is guard on the constitutive status |

**L4 Syntax:**
```l4
GIVEN person IS A Person
DECIDE `is qualified voter` person
  SUBJECT TO `is registered with electoral commission` person
  IS `meets age requirement` person AND `is citizen` person
```

**Semantics:** The constitutive rule only produces a result if the guard is satisfied. Otherwise, the classification is undefined/UNKNOWN (not FALSE).

#### Guard + Regulative
The action/duty is only available if precondition is met.

**Real-world examples:**

| Source | Text | Analysis |
|--------|------|----------|
| Standard Lease | "Tenant MAY assign this lease SUBJECT TO the landlord's prior written consent" | Consent is guard on the permission |
| Building Code | "Owner MAY demolish a listed structure SUBJECT TO approval from the Heritage Commission" | Approval guards the permission |
| Singapore Companies Act s.403 | "A company MAY declare dividends SUBJECT TO the solvency test being satisfied" | Solvency is guard on the corporate power |
| Employment Contract | "Employee MAY work remotely SUBJECT TO manager approval for each instance" | Approval guards each exercise of permission |

**L4 Syntax:**
```l4
GIVEN tenant IS A Tenant, lease IS A Lease
tenant MAY `assign` lease
  SUBJECT TO `landlord written consent obtained` tenant lease
```

**Semantics:** The permission is not exercisable until the guard is satisfied. The permission exists in principle, but is "locked" without the precondition.

**Key Difference:** For constitutive rules, a failed guard means the classification is undefined. For regulative rules, a failed guard means the action is unavailable but the rule is still "there."

#### Why Guards Aren't Just Conjuncts

A natural question: why not flatten `X SUBJECT TO Y` into `X AND Y`? In software, separating these might be "overengineering." But there are real semantic and pragmatic differences:

| Dimension | Inlined Conjunct (`X AND Y`) | Lifted Guard (`X SUBJECT TO Y`) |
|-----------|------------------------------|--------------------------------|
| **Epistemic status** | `Y = false` → result is FALSE | `Y = false` → result is UNKNOWN |
| **Evaluation order** | Unspecified (short-circuit) | Y must be checked first |
| **Explanation** | "Criteria not met" | "Precondition not satisfied" |
| **Remediation** | You're ineligible | Go obtain Y, then try again |
| **Responsibility** | Same actor evaluates all | Different actors may handle guard vs. substance |
| **Temporal structure** | Simultaneous | Sequential (Y before X) |

**Example:** "May assign lease subject to landlord consent"
- **As conjunct:** `can_assign = landlord_consented AND meets_lease_terms AND ...`
  - Consent is just another checkbox; if FALSE, tenant is "not permitted"
- **As guard:** `SUBJECT TO landlord_consent THEN (meets_lease_terms AND ...)`
  - Consent is a gating step; if not obtained, assignment is "not yet available"
  - Different next step: get consent vs. give up

**Legal drafters lift guards because:**
1. **Cognitive chunking**: Humans process "preconditions" separately from "substance" (Miller's 7±2)
2. **Document structure**: Preconditions often come from different sections/schedules
3. **Actor separation**: Procurement gets quotes; Legal checks terms; Board approves
4. **Audit trails**: "Blocked at consent stage" vs "failed eligibility check" are different findings
5. **Defeasibility**: Guards can be "obtained" or "waived"; criteria are fixed

**For L4:** We preserve the guard/conjunct distinction because it enables richer:
- Evaluation traces (which stage failed?)
- Query planning (what do we need to unlock this action?)
- Explanation generation (why is this unavailable vs. why is this false?)

---

### 12.3 Filters × Constitutive/Regulative

#### Filter + Constitutive
The definition applies only within a restricted domain.

**Real-world examples:**

| Source | Text | Analysis |
|--------|------|----------|
| UK Employment Rights Act | "'Employee' MEANS an individual who has entered into or works under a contract of employment... This Act does not apply to share fishermen or the police service" | Definition filtered by exclusions |
| IRC §401(k) | "'Highly compensated employee' MEANS any employee who... EXCEPT that the term does not include any employee described in subparagraph (B)" | Tax classification with carve-outs |
| GDPR Art.2 | "'Personal data' MEANS any information relating to an identified person... This Regulation does not apply to processing by natural persons in purely personal activities" | Definition scoped by domain filter |
| Singapore PDPA | "'Personal data' MEANS data about an individual... EXCEPT business contact information" | Definition with explicit exclusion |

**L4 Syntax:**
```l4
§ `Part 3: Employment Definitions`
  SCOPE NOT `excluded under Schedule 2` person

GIVEN person IS A Person
DECIDE `is employee` person IS
  `engaged under contract of service` person
```

**Semantics:** Outside the filtered domain, the definition simply doesn't apply (neither true nor false for those cases).

#### Filter + Regulative
The duty applies only to entities within the filtered domain.

**Real-world examples:**

| Source | Text | Analysis |
|--------|------|----------|
| US FMLA 29 USC §2611 | "Employer MUST provide 12 weeks unpaid leave... This title applies only to employers with 50 or more employees for at least 20 workweeks" | Duty filtered by employer size |
| Title VII | "Employer MUST NOT discriminate... This title does not apply to employers with fewer than 15 employees" | Prohibition filtered by threshold |
| ADA Title I | "Employer MUST provide reasonable accommodations... applies to employers with 15 or more employees" | Duty filtered by size |
| Singapore Employment Act | "Employer MUST pay overtime at 1.5x... This Part applies only to workmen earning ≤$4,500/month" | Duty filtered by salary cap |
| EU Working Time Directive | "Employer MUST limit work to 48 hours/week... Member States may derogate for managing executives" | Duty filtered by role |

**L4 Syntax:**
```l4
§ `Section 5: Annual Leave`
  SCOPE `employer with 50 or more employees` employer

GIVEN employer IS AN Employer, employee IS AN Employee
employer MUST `provide annual leave of` 20 `days to` employee
```

**Semantics:** Small employers are not bound by the obligation at all - it's not that they're permitted to violate it, but that the obligation never attaches to them.

**Key Difference:** Filter + Constitutive affects what IS; Filter + Regulative affects who is BOUND.

---

### 12.4 Transformers × Constitutive/Regulative

#### Transformer + Constitutive
Temporarily redefines what satisfies a classification.

**Real-world examples:**

| Source | Text | Analysis |
|--------|------|----------|
| IRC §121(d)(3) | "For purposes of this section, the destruction, theft, seizure, requisition, or condemnation of property SHALL BE TREATED AS the sale of such property" | Involuntary conversion treated as sale for gain exclusion |
| IRC §121(d)(3)(B) | "Solely for purposes of this section, an individual SHALL BE TREATED AS using property as principal residence during any period while spouse is granted use under divorce instrument" | Constructive use via spouse for tax purposes |
| UK VAT Act | "For purposes of zero-rating, children's clothing up to specified sizes SHALL BE TREATED AS exempt goods" | Reclassification for tax treatment |
| Singapore GST (Holiday) | "For purposes of GST during the specified period, prepared beverages SHALL BE TREATED AS food items" | Temporary category shift |
| Transition Provisions | "References to the repealed Act SHALL BE CONSTRUED AS references to the corresponding provisions of this Act" | Interpretive transformer across statutes |

**L4 Syntax:**
```l4
§ `Holiday Tax Exception`
  TREATING `is beverage` AS `is food`
  DURING `december 25` TO `january 1`

-- The constitutive rule is unchanged:
GIVEN item IS AN Item
DECIDE `is food` item IS item.category == Food
```

**Semantics:** The classification predicate is temporarily extended. Beverages now satisfy `is food` during the specified period.

#### Transformer + Regulative
Temporarily redefines what triggers an obligation.

**Real-world examples:**

| Source | Text | Analysis |
|--------|------|----------|
| COVID-19 Regulations (2020) | "For purposes of meeting attendance requirements, participation by video conference SHALL BE TREATED AS attendance in person" | Transforms trigger condition for attendance duties |
| UK Sunday Trading Act | "For purposes of closing hour restrictions, Christmas Day SHALL BE TREATED AS a Sunday" | Holiday treated as Sunday for closing duties |
| Emergency Powers | "For purposes of procurement rules during a declared emergency, competitive bidding requirements SHALL BE TREATED AS satisfied by three informal quotes" | Relaxes what satisfies the duty trigger |
| Singapore COVID Regulations | "For purposes of safe distancing measures, outdoor spaces SHALL BE TREATED AS indoor spaces during the specified period" | Extends scope of distancing duties |
| Force Majeure Clauses | "For purposes of delivery obligations, the period of force majeure SHALL BE TREATED AS excluded from the delivery timeline" | Transforms what triggers breach |

**L4 Syntax:**
```l4
§ `Holiday Closing Hours`
  TREATING `is public holiday` AS `is Sunday`

GIVEN shop IS A Shop, day IS A Day
shop MUST `close by 10pm on` day
  IF `is Sunday` day
```

**Semantics:** The trigger condition for the duty is temporarily expanded. The obligation now applies on public holidays too.

**Key Difference:** Constitutive transformers change what things ARE; regulative transformers change when duties ARISE.

---

### 12.5 Modifiers × Constitutive/Regulative

#### Modifier + Constitutive
Adjusts the computed value of a constitutive rule.

**Real-world examples:**

| Source | Text | Analysis |
|--------|------|----------|
| Parking Regulations | "The parking fee IS $6/hour on weekdays, $4/hour on weekends, REDUCED BY 50% on public holidays" | Base rate modified by holiday discount |
| IRC §165(i) | "Disaster losses MAY BE TREATED AS occurring in the preceding taxable year" | Timing modifier on when the loss "is" recognized |
| Insurance Policy | "The deductible IS $500, REDUCED TO $250 for policyholders with no claims in 3 years" | Value modified by claims history |
| Rent Control | "Maximum rent increase IS 5% annually, CAPPED AT 3% for senior tenants" | Rate modified by tenant status |
| Interest Calculation | "Interest IS calculated at prime + 2%, SUBJECT TO a floor of 4% and ceiling of 12%" | Value bounded by modifiers |

**L4 Syntax:**
```l4
GIVEN item IS AN Item, day IS A Day
DECIDE `tax rate` item day IS
  LET base = 0.10
  IN IF `is public holiday` day THEN base * 0.5 ELSE base
```

**Semantics:** The constitutive rule produces a base value; the modifier transforms it.

#### Modifier + Regulative
Adjusts the content of the duty or its remedy.

**Real-world examples:**

| Source | Text | Analysis |
|--------|------|----------|
| Termination Clause | "Landlord MUST provide 30 days notice to terminate, REDUCED TO 7 days for non-payment of rent" | Notice period (duty content) modified by circumstance |
| Traffic Code | "Speeding fine IS $100, REDUCED TO $50 for first-time offenders" | Penalty (remedy) modified by offender history |
| Employment Law | "Employer MUST provide 2 weeks severance per year of service, INCREASED TO 3 weeks for employees over 50" | Duty quantum modified by employee status |
| Limitation Periods | "Claimant MUST file within 2 years, EXTENDED TO 3 years if defendant was outside jurisdiction" | Time limit (duty parameter) modified by circumstance |
| Contract Damages | "Breaching party MUST pay liquidated damages of $10,000, REDUCED proportionally if partial performance rendered" | Remedy modified by mitigation |

**L4 Syntax:**
```l4
GIVEN landlord IS A Landlord, tenant IS A Tenant
landlord MUST `provide notice of`
  (IF `tenant in arrears` tenant THEN 7 ELSE 30) `days`
```

Or for remedy modification:
```l4
GIVEN violator IS A Person
DECIDE `parking fine` violator IS
  IF `is first offense` violator THEN 50 ELSE 100
```

**Semantics:** The duty's parameters or the remedy's magnitude are adjusted.

**Key Difference:** Constitutive modifiers change computed VALUES; regulative modifiers change duty PARAMETERS or REMEDIES.

---

### 12.6 Priority × Constitutive/Regulative

This is where the distinction becomes most interesting.

#### Priority + Constitutive
One definition prevails over another.

**Real-world examples:**

| Source | Text | Analysis |
|--------|------|----------|
| IRC §162 vs §280A | "Business expenses are deductible... NOTWITHSTANDING §162, home office expenses are limited per §280A" | Special definition overrides general |
| California Civil Code §1646 | "NOTWITHSTANDING Section 1646, parties to contracts ≥$250,000 MAY agree California law governs" | Override of default choice-of-law rule |
| GDPR Art.9 vs Art.6 | "NOTWITHSTANDING Article 6, special categories of data require explicit consent" | Stricter definition prevails |
| Interpretation Acts | "NOTWITHSTANDING any definition in other legislation, 'person' in this Act includes corporations" | Act-specific definition overrides general |
| Tax Treaty Override | "NOTWITHSTANDING the domestic definition of 'resident', the treaty definition shall apply" | International definition prevails |

**L4 Syntax:**
```l4
§ `Section 2: General Definition`
DECIDE `income` taxpayer IS `gross receipts` taxpayer

§ `Part 5: Special Provisions`
  NOTWITHSTANDING `Section 2`

DECIDE `income` taxpayer IS
  `gross receipts` taxpayer - `allowable deductions` taxpayer
```

**Semantics:** When both definitions could apply, the Part 5 definition prevails. This is pure definitional override.

#### Priority + Regulative (Deontic Conflicts)

This is more complex because obligations, permissions, and prohibitions can conflict in different ways.

**Real-world examples:**

| Source | Text | Deontic Pattern | Analysis |
|--------|------|-----------------|----------|
| Singapore Companies Act s.152 | "Public company MAY remove director by ordinary resolution, NOTWITHSTANDING anything in its constitution or any agreement" | MAY overrides MUST NOT | Statutory permission overrides contractual prohibition |
| Standard Lease | "Tenant MUST NOT sublease. NOTWITHSTANDING the foregoing, tenant MAY sublease to immediate family" | MAY overrides MUST NOT | Exception carves out from prohibition |
| Canadian Charter s.33 | "Parliament MAY declare Act operates NOTWITHSTANDING sections 2 or 7-15" | MAY overrides MUST NOT | Legislature overrides Charter rights |
| UK Companies Act s.168 | "Company MAY remove director NOTWITHSTANDING any provision in articles or agreement" | MAY overrides MUST NOT | Statutory power overrides contract |
| Employment Contract | "Employee MAY take lunch anytime. NOTWITHSTANDING this, during peak season employee MUST take lunch 12-2pm" | MUST overrides MAY | Obligation restricts permission |
| Traffic Law | "Driver MUST yield to pedestrians. NOTWITHSTANDING this, driver MUST NOT yield when police directs otherwise" | MUST NOT overrides MUST | Prohibition creates exception to duty |

**Case A: Permission overrides Prohibition (MAY > MUST NOT)**
```l4
§ `Section 5: Sublease Prohibition`
GIVEN tenant IS A Tenant, sublessee IS A Person
tenant MUST NOT `sublease to` sublessee

§ `Section 6: Family Exception`
  NOTWITHSTANDING `Section 5`

GIVEN tenant IS A Tenant, sublessee IS A Person
tenant MAY `sublease to` sublessee
  IF `is immediate family of` sublessee tenant
```

**Case B: Obligation overrides Permission (MUST > MAY)**
```l4
§ `Section 3: Flexible Lunch`
employee MAY `take lunch at` any_time

§ `Section 4: Peak Season`
  NOTWITHSTANDING `Section 3`

employee MUST `take lunch at` time
  IF `is peak season` date AND time >= 12:00 AND time <= 14:00
```

**Case C: Prohibition overrides Obligation (MUST NOT > MUST)**
```l4
§ `Section 7: Yield Rule`
driver MUST `yield to pedestrians at` crossing

§ `Section 8: Police Override`
  NOTWITHSTANDING `Section 7`

driver MUST NOT `yield at` crossing
  IF `police officer directing otherwise` crossing
```

**Semantics:** Deontic priority requires careful handling:
- MAY can override MUST NOT (create an exception to prohibition)
- MUST can override MAY (restrict a permission to a duty)
- MUST NOT can override MUST (create an exception to duty)

**Key Difference:** Constitutive priority is about which definition wins. Regulative priority involves deontic logic - the interaction of obligation, permission, and prohibition modalities.

---

### 12.7 Evaluation Pipeline Implications

The constitutive/regulative distinction affects the evaluation pipeline:

```
┌─────────────────────────────────────────────────────────────────┐
│  For CONSTITUTIVE rules:                                        │
│                                                                 │
│  1. Transform (rewrite classifications)                         │
│  2. Filter (check domain applicability)                         │
│  3. Guard (check preconditions → undefined if fails)            │
│  4. Evaluate (compute value/classification)                     │
│  5. Modify (transform result)                                   │
│  6. Detect conflicts (multiple definitions)                     │
│  7. Resolve priority (definitional override)                    │
└─────────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────────┐
│  For REGULATIVE rules:                                          │
│                                                                 │
│  1. Transform (rewrite trigger conditions)                      │
│  2. Filter (check if actor/situation is bound)                  │
│  3. Guard (check if action is available)                        │
│  4. Evaluate (determine if duty/permission active)              │
│  5. Modify (adjust duty parameters/remedies)                    │
│  6. Detect deontic conflicts (O/P/F clash)                      │
│  7. Resolve priority (deontic override, modal hierarchy)        │
└─────────────────────────────────────────────────────────────────┘
```

Stage 6 and 7 differ significantly:
- Constitutive conflicts: Same predicate, different values
- Regulative conflicts: Same action, different modalities (must vs may vs must-not)

---

### 12.8 L4 Syntax Implications

**For constitutive rules**, the existing proposals work:
```l4
GIVEN x IS A Type
DECIDE `predicate` x
  SUBJECT TO guard
  IS expression
```

**For regulative rules**, we need modal-aware syntax:
```l4
GIVEN actor IS A Type, action IS A Type
actor MUST|MAY|MUST NOT action
  SUBJECT TO guard
  EXCEPT WHEN filter
  NOTWITHSTANDING `other section`
```

**Deontic override rules:**
```l4
§ `Priority Declarations`

-- Permission overrides prohibition
`Section 6` (MAY) NOTWITHSTANDING `Section 5` (MUST NOT)

-- Or implicit: MAY always overrides MUST NOT when declared
-- Or: require explicit override strength declaration
```

---

### 12.9 Open Questions

1. **Deontic conflict semantics**: When a permission and prohibition both apply (without explicit priority), what's the default?
   - Option A: Prohibition wins (conservative)
   - Option B: Permission wins (liberal)
   - Option C: Error - require explicit resolution

2. **Guard failure semantics**:
   - Constitutive: classification is UNKNOWN vs FALSE?
   - Regulative: action unavailable vs forbidden?

3. **Modifier on regulative**: Does modifying the content of a duty (e.g., notice period) change the duty itself, or create a new duty?

4. **Transformer scope for regulative**: If we "treat Sunday as Saturday", does this affect all duties that reference days, or only specific ones?

5. **Cross-type priority**: Can a constitutive rule have priority over a regulative rule or vice versa? (e.g., "NOTWITHSTANDING the definition of 'employee', all workers MUST receive minimum wage")

---

## 13. References

### Legal Drafting
- [Weagree: Notwithstanding in Contracts](https://weagree.com/clm/contracts/contract-wording/notwithstanding/)
- [UpCounsel: Notwithstanding Meaning in Law](https://www.upcounsel.com/notwithstanding-legal-use)
- [LawProse Lesson #196](https://lawprose.org/lawprose-lesson-196-notwithstanding/)
- [Adams on Contract Drafting](https://www.adamsdrafting.com/the-foregoing/)

### Statutory Interpretation
- [CRS: Notwithstanding Clauses (PDF)](https://sgp.fas.org/crs/misc/notwith.pdf)
- [Capitol Weekly: California Notwithstanding Clauses](https://capitolweekly.net/the-use-of-notwithstanding-clauses-in-california-legislation/)
- [SCC Times: Non Obstante Clauses](https://www.scconline.com/blog/post/2023/06/16/circumscribing-non-obstante-clauses-tracing-the-new-jurisprudence/)
- [iPleaders: Non-Obstante Clause](https://blog.ipleaders.in/all-you-need-to-know-about-non-obstante-clause/)

### Defeasibility and Formal Methods
- [Stanford Encyclopedia: Defeasible Reasoning](https://plato.stanford.edu/entries/reasoning-defeasible/)
- [Prakken: Three Faces of Defeasibility](https://onlinelibrary.wiley.com/doi/abs/10.1111/j.0952-1917.2004.00259.x)

### Contract Examples
- [Law Insider: Clause Database](https://www.lawinsider.com/)
- [Afterpattern: Contract Clauses](https://afterpattern.com/clauses/)
