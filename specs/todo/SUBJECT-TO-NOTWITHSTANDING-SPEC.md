# SUBJECT TO / DESPITE / NOTWITHSTANDING: Taxonomic Analysis and Specification

**Status:** Draft
**Author:** Research compilation for L4 language design
**Date:** 2025-01-23
**Branch:** mengwong/spec-notwithstanding

---

## 1. Introduction

This document catalogues the various uses of `SUBJECT TO`, `DESPITE`, and `NOTWITHSTANDING` in legal texts before proposing a formal semantics for L4. Legal drafters use these terms to express priority relations, exceptions, overrides, and conditional applicability. However, careful examination reveals these keywords serve multiple distinct semantic functions that must be disambiguated for computational purposes.

### 1.1 Motivation

L4 aims to formalize legal rules with mathematical precision. The keywords "subject to," "despite," and "notwithstanding" appear frequently in statutes and contracts, but their semantics are surprisingly complex:

- They establish **priority relations** between conflicting provisions
- They act as **input filters** that modify which cases a rule applies to
- They act as **output modifiers** that transform results under certain conditions
- They create **exception carve-outs** from general rules
- They signal **defeasibility** of conclusions

Without formal treatment, these constructs remain as informal comments in L4 code (as seen in the Singapore Data Protection Act example: `Subject to exceptions` IS A BOOLEAN`). This specification aims to promote them to first-class language constructs.

### 1.2 Key Insight: Directionality

The fundamental distinction between these keywords is **directionality** in document structure:

| Keyword           | Appears In                  | Points To        | Effect                      |
| ----------------- | --------------------------- | ---------------- | --------------------------- |
| `SUBJECT TO`      | Main/subordinate clause     | Exception clause | "I yield to that provision" |
| `NOTWITHSTANDING` | Exception/prevailing clause | Main clause      | "I override that provision" |
| `DESPITE`         | Exception/prevailing clause | Main clause      | Same as NOTWITHSTANDING     |

As noted in drafting literature: "notwithstanding looks back whilst subject to looks forward."

---

## 2. Taxonomy of Usage Patterns

Based on extensive analysis of legal texts, we identify **seven distinct semantic functions** these keywords serve. Each has different computational implications.

### 2.1 Priority Declaration (Pure Precedence)

**Pattern:** Establishing which of two potentially conflicting provisions prevails.

**Examples:**

```
-- SUBJECT TO form (in subordinate clause)
"The licensee may operate during normal business hours,
 SUBJECT TO the restrictions in Section 5."

-- NOTWITHSTANDING form (in prevailing clause)
"NOTWITHSTANDING Section 3, the Director may grant
 extensions in exceptional circumstances."
```

**Semantics:** When provisions A and B both apply to the same facts and yield conflicting conclusions, the priority declaration determines which conclusion holds.

**Computational model:**

```
priority(A, B) = B  -- "A SUBJECT TO B" means B wins conflicts
priority(A, B) = A  -- "A NOTWITHSTANDING B" means A wins conflicts
```

**Legal sources:**

- U.S. Supreme Court in _Cisneros v. Alpine Ridge Group_ (1993): "a 'notwithstanding' clause signals the drafter's intention that the provisions of the 'notwithstanding' section override conflicting provisions."
- Indian courts: Non obstante clauses "perform the function of removing impediments created by other provisions."

### 2.2 Exception/Carve-Out (Scope Limitation)

**Pattern:** Excluding certain cases from the scope of a general rule.

**Examples:**

```
"All employees are entitled to 20 days annual leave,
 SUBJECT TO the exclusions for part-time workers in Schedule 2."

"Vehicles must not exceed 30 mph,
 NOTWITHSTANDING which, emergency vehicles responding to calls
 may exceed this limit."
```

**Semantics:** The exception clause defines a subset of inputs for which the main rule does not apply (or applies differently).

**Computational model:**

```haskell
-- Main rule with exception
rule(x) =
  if exception_applies(x)
  then exception_result(x)  -- or: rule_does_not_apply
  else main_result(x)
```

**Key distinction from Priority:** Priority resolves conflicts between two independently applicable rules. Exception carve-outs prevent the main rule from applying at all to certain cases.

### 2.3 Condition Precedent (Triggering Condition)

**Pattern:** Making rule applicability contingent on another condition being satisfied.

**Examples:**

```
"Payment shall be made within 30 days,
 SUBJECT TO the goods having passed inspection."

"The agreement becomes effective on the Closing Date,
 SUBJECT TO all regulatory approvals having been obtained."
```

**Semantics:** The main obligation only arises if the condition is met. This is not an exception but a prerequisite.

**Computational model:**

```haskell
-- Condition precedent
obligation_exists(x) =
  condition_satisfied(x) && base_obligation_would_apply(x)
```

**Legal doctrine:** Courts distinguish conditions precedent (must happen before duty arises) from conditions subsequent (terminate existing duty). "Subject to" can signal either.

### 2.4 Proviso/Qualification (Output Modification)

**Pattern:** Modifying the conclusion or output of a rule rather than its applicability.

**Examples:**

```
"The tenant may make improvements to the property,
 SUBJECT TO obtaining landlord's written consent."

-- The right exists, but is qualified/constrained

"Benefits shall be paid monthly,
 NOTWITHSTANDING WHICH, the first payment may be pro-rated."
```

**Semantics:** The rule applies and produces a base result, which is then modified by the qualifying clause.

**Computational model:**

```haskell
-- Output modification
final_result(x) = modify(base_result(x), qualification(x))

-- Example: permission with constraint
may_improve(tenant) = Permission {
  action = Improve,
  constraint = RequiresConsent(landlord)
}
```

**Key distinction:** The rule applies; its output is transformed. Compare to Exception (rule doesn't apply) and Priority (rule's output is replaced).

### 2.5 Savings/Preservation Clause (Non-Interference)

**Pattern:** Declaring that a new provision does not affect existing rights or other provisions.

**Examples:**

```
"This Section 12 is SUBJECT TO and shall not limit or restrict
 the rights granted under Section 8."

"NOTWITHSTANDING the foregoing, nothing in this Agreement shall
 be construed to waive Party A's rights under the Master Agreement."
```

**Semantics:** An explicit declaration of non-conflict, preserving co-existence of provisions.

**Computational model:**

```haskell
-- Preservation: both provisions remain in force
-- No priority determination needed; they don't conflict
applies(section_12, x) = ... -- unchanged
applies(section_8, x) = ...  -- also unchanged
```

**Drafting note:** This is often defensive drafting to prevent unintended implied repeal.

### 2.6 Scope/Domain Restriction (Input Filter)

**Pattern:** Narrowing the domain of inputs to which a rule applies.

**Examples:**

```
"For the purposes of this Part,
 'employee' means any person employed under a contract of service,
 SUBJECT TO the exclusions in paragraph (b)."

"This regulation applies to all data controllers,
 NOTWITHSTANDING WHICH, controllers processing fewer than
 5000 records annually are exempt from Section 4."
```

**Semantics:** The rule's input domain is filtered before evaluation.

**Computational model:**

```haskell
-- Domain restriction
rule_applies_to(x) = in_base_domain(x) && not(excluded(x))

-- Or equivalently as a filter:
applicable_inputs = filter (not . excluded) base_domain
```

**Key distinction from Exception:** Input filtering happens before rule evaluation; exceptions can reference the rule's intermediate computations.

### 2.7 Defeasibility Marker (Rebuttable Conclusion)

**Pattern:** Signaling that a conclusion is provisional and may be defeated by new information.

**Examples:**

```
"A child born in the UK to British parents is British,
 SUBJECT TO any determination to the contrary under Section 40."

"The contract shall be deemed valid,
 NOTWITHSTANDING WHICH, either party may challenge validity
 on grounds of fraud or duress."
```

**Semantics:** The conclusion holds by default but can be overridden by defeating conditions discovered later.

**Computational model:**

```haskell
-- Defeasible rule
conclusion(x) =
  if defeating_condition(x)
  then defeated_result(x)
  else default_result(x)

-- With explicit uncertainty
conclusion(x) = Defeasible {
  default = default_result(x),
  defeaters = [defeating_condition_1, defeating_condition_2, ...],
  confidence = provisional
}
```

**Connection to default logic:** This aligns with Reiter's default logic and the notion of non-monotonic reasoning. See L4's existing `doc/default-logic.md`.

---

## 3. Related Legal Principles

### 3.1 Lex Specialis Derogat Generali

The principle that specific law overrides general law. When both apply to the same facts:

```
General rule: All vehicles must stop at red lights.
Specific rule: Emergency vehicles may proceed through red lights when responding.
```

This is an implicit priority relation. `NOTWITHSTANDING` makes it explicit.

### 3.2 Lex Posterior Derogat Priori

Later law overrides earlier law. Relevant when:

- Amendment acts modify existing statutes
- Contract amendments override original terms

### 3.3 Non Obstante (Latin Root)

The Latin term "non obstante" (notwithstanding) has a rich history in statutory interpretation, particularly in Indian and Commonwealth jurisdictions:

- Creates "overriding effect" over conflicting provisions
- Does NOT repeal conflicting provisions; merely displaces them for specific cases
- Must be interpreted in context of statutory purpose
- Cannot "whittle down" the principal provision it modifies

### 3.4 Provisos vs. Exceptions

Legal drafting distinguishes:

| Construct         | Function                     | Position                      |
| ----------------- | ---------------------------- | ----------------------------- |
| **Proviso**       | Qualifies the main provision | Introduced by "provided that" |
| **Exception**     | Carves out cases from scope  | Can appear anywhere           |
| **Saving clause** | Preserves existing rights    | Usually at end                |

A proviso "must be read and understood in conjunction with the main provision" while an exception "operates independently."

---

## 4. Problematic Patterns and Ambiguities

### 4.1 Scope Ambiguity

```
"NOTWITHSTANDING anything to the contrary in this Agreement..."
```

What does "anything" include? All provisions? Only provisions in the same Part? The drafter may not have fully enumerated the provisions being overridden.

**Recommendation:** Require explicit references: `NOTWITHSTANDING Section 5.2`

### 4.2 Circular Priority

```
Section A: "NOTWITHSTANDING Section B, ..."
Section B: "NOTWITHSTANDING Section A, ..."
```

Two mutually-overriding provisions create a paradox.

**Computational solution:** Detect cycles in priority graph; flag as error.

### 4.3 Layered Exceptions

```
Rule: All employees get 20 days leave
  Exception 1: Part-time workers get 10 days
    Exception to Exception 1: Part-time workers with 10+ years get 20 days
      Exception to Exception to Exception 1: Unless they declined in writing
```

Deep nesting creates comprehension problems.

**Computational solution:** Flatten to decision tree; verify completeness.

### 4.4 Implicit vs. Explicit Override

Sometimes override is implied by document structure (later provision implicitly overrides earlier). Other times it's explicit. Mixing modes in the same document creates confusion.

### 4.5 The "Despite" Alternative

"Despite" is semantically equivalent to "notwithstanding" but:

- Less formal/legalistic
- Fewer ambiguity issues (not confused with "subject to")
- Preferred by plain language advocates

---

## 5. Comparison with Other Formalisms

### 5.1 Catala

The Catala language (catala-lang.org) handles exceptions with explicit scope labels:

```catala
scope IncomeTax:
  definition tax equals income * 0.20

  exception definition tax under condition
    income < 10000
  equals 0
```

Exceptions are tied to specific definitions, with explicit conditions.

### 5.2 Defeasible Logic

Formal defeasible logic systems (Prakken, Sartor) use:

- Strict rules: `A -> B` (cannot be defeated)
- Defeasible rules: `A => B` (can be defeated)
- Defeaters: `A ~> ~B` (attacks but doesn't establish)
- Priority ordering on rules

### 5.3 Answer Set Programming

ASP handles exceptions through negation as failure:

```prolog
flies(X) :- bird(X), not abnormal(X).
abnormal(X) :- penguin(X).
```

### 5.4 Contract-Specific Languages (CSL, etc.)

Contract specification languages like CSL use temporal operators and explicit breach/fulfillment states rather than override semantics.

---

## 6. Observations for L4 Language Design

### 6.1 Multiple Constructs Needed

The seven semantic functions identified suggest L4 needs multiple distinct constructs, not a single overloaded keyword:

| Function             | Suggested L4 Construct                                |
| -------------------- | ----------------------------------------------------- |
| Priority declaration | `SUBJECT TO` / `NOTWITHSTANDING` as explicit priority |
| Exception carve-out  | `EXCEPT WHEN` clause                                  |
| Condition precedent  | `REQUIRES` or `GIVEN THAT`                            |
| Output modification  | `QUALIFIED BY` or modifier syntax                     |
| Preservation         | `WITHOUT AFFECTING`                                   |
| Domain restriction   | Input type constraints                                |
| Defeasibility        | `UNLESS` with defeater semantics                      |

### 6.2 Reader-Friendliness

Following drafting best practices:

- Prefer `SUBJECT TO` in the subordinate clause (alerts reader to exception)
- Use explicit section references, not "anything to the contrary"
- Consider whether exception or condition precedent is the right model

### 6.3 Composition with Existing L4 Features

L4 already has:

- `CONSIDER` / `WHEN` for conditional logic
- `MEANS` / `DECIDE` for definitions
- Optional/Maybe types for missing information
- `OTHERWISE` for defaults

New constructs should compose naturally with these.

### 6.4 Type-Level Considerations

Override semantics may need type-level representation:

- A "defeasible Boolean" that can be defeated
- A "provisional result" that may be superseded
- Priority annotations on rules

---

## 7. Open Questions

1. **Granularity:** Should override apply to entire rules or individual conclusions?

2. **Temporal dynamics:** Can priority change over time? (e.g., grace periods)

3. **Procedural vs. substantive:** Do these keywords affect how rules are evaluated (procedure) or what they mean (substance)?

4. **Explanation generation:** How should evaluation traces explain override decisions?

5. **Verification:** Can we statically verify that override relations are acyclic and complete?

6. **Interoperability:** How do these map to other legal formalization languages?

---

## 8. Next Steps

1. **Gather corpus examples:** Collect real statutory and contract examples using each pattern
2. **Propose concrete syntax:** Design L4 keyword syntax for each semantic function
3. **Define formal semantics:** Specify evaluation rules precisely
4. **Implement prototype:** Add to L4 parser and evaluator
5. **Test with real documents:** Validate against British Nationality Act, PDPA, etc.

---

## References

### Legal Drafting

- Adams, K. "A Notwithstanding Sideshow." Adams on Contract Drafting. [Link](https://www.adamsdrafting.com/a-notwithstanding-sideshow/)
- Weagree. "Notwithstanding in Contracts." [Link](https://weagree.com/clm/contracts/contract-wording/notwithstanding/)
- UpCounsel. "Notwithstanding Meaning in Law." [Link](https://www.upcounsel.com/notwithstanding-legal-use)
- LawProse. "Lesson #196: Notwithstanding." [Link](https://lawprose.org/lawprose-lesson-196-notwithstanding/)

### Statutory Interpretation

- Congressional Research Service. "Notwithstanding Clauses." [PDF](https://sgp.fas.org/crs/misc/notwith.pdf)
- Capitol Weekly. "The Use of Notwithstanding Clauses in California Legislation." [Link](https://capitolweekly.net/the-use-of-notwithstanding-clauses-in-california-legislation/)
- SCC Times. "Circumscribing Non Obstante Clauses." [Link](https://www.scconline.com/blog/post/2023/06/16/circumscribing-non-obstante-clauses-tracing-the-new-jurisprudence/)
- LawCrust. "Non Obstante Clause in Interpretation of Statutes." [Link](https://lawcrust.com/non-obstante-clause/)

### Defeasibility and Formal Methods

- Stanford Encyclopedia of Philosophy. "Defeasible Reasoning." [Link](https://plato.stanford.edu/entries/reasoning-defeasible/)
- Prakken, H. "The Three Faces of Defeasibility in the Law." _Ratio Juris_ (2004). [Link](https://onlinelibrary.wiley.com/doi/abs/10.1111/j.0952-1917.2004.00259.x)
- NDPR. "Allowing for Exceptions: A Theory of Defences and Defeasibility in Law." [Link](https://ndpr.nd.edu/reviews/allowing-for-exceptions-a-theory-of-defences-and-defeasibility-in-law/)
- Schauer, F. "Is Defeasibility an Essential Property of Law?" [PDF](http://www.horty.umiacs.io/courses/readings/schauer-defeasibility.pdf)

### Legal Principles

- US Legal Forms. "Lex Specialis Derogat Generali." [Link](https://legal-resources.uslegalforms.com/l/lex-specialis-derogat-generali)
- iPleaders. "Non-Obstante Clause." [Link](https://blog.ipleaders.in/all-you-need-to-know-about-non-obstante-clause/)
- LII. "Condition Precedent." [Link](https://www.law.cornell.edu/wex/condition_precedent)

### Related L4 Documentation

- `doc/default-logic.md` - L4's treatment of default reasoning
- `doc/regulative.md` - Regulative rule semantics
- `jl4/experiments/Singapore-Data-Protection-Act.l4` - Example using "subject to" informally

---

## Appendix A: Example Corpus (To Be Expanded)

### A.1 Priority Declaration Examples

```
-- From securities regulations
"NOTWITHSTANDING Rule 144, restricted securities may be sold
 pursuant to an effective registration statement."

-- From employment law
"Annual leave entitlement is SUBJECT TO the maximum accrual
 limits in Company Policy 4.2."
```

### A.2 Exception Examples

```
-- From tax code
"All income is taxable, SUBJECT TO the exemptions in Schedule A."

-- From data protection
"Personal data shall not be processed, NOTWITHSTANDING WHICH,
 processing is permitted for the purposes listed in Article 6."
```

### A.3 Condition Precedent Examples

```
-- From contract law
"The purchase shall complete SUBJECT TO satisfactory survey."

-- From regulatory approval
"The merger is SUBJECT TO approval by the Competition Authority."
```

### A.4 Canadian Charter Section 33

The canonical constitutional example:

```
"Parliament or the legislature of a province may expressly declare
 in an Act of Parliament or of the legislature, as the case may be,
 that the Act or a provision thereof shall operate NOTWITHSTANDING
 a provision included in section 2 or sections 7 to 15 of this Charter."
```

This allows legislatures to override certain Charter rights for renewable 5-year periods.
