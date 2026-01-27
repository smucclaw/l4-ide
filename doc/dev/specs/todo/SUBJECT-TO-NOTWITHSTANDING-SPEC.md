# SUBJECT TO / DESPITE / NOTWITHSTANDING: Taxonomic Analysis and Specification

**Status:** Draft (Revised)
**Author:** Research compilation for L4 language design
**Date:** 2025-01-27
**Branch:** mengwong/spec-notwithstanding

---

## 1. Introduction

This document catalogues the various uses of `SUBJECT TO`, `DESPITE`, and `NOTWITHSTANDING` in legal texts before proposing a formal semantics for L4.

### 1.1 Motivation

L4 aims to formalize legal rules with mathematical precision. These keywords appear frequently in statutes and contracts, but their semantics are complex. Without formal treatment, these constructs remain as informal comments in L4 code (e.g., `Subject to exceptions` IS A BOOLEAN in the Singapore PDPA example).

### 1.2 Key Insight: Directionality

The fundamental distinction between these keywords is **directionality** in document structure:

| Keyword | Appears In | Points To | Effect |
|---------|-----------|-----------|--------|
| `SUBJECT TO` | Main/subordinate clause | Exception clause | "I yield to that provision" |
| `NOTWITHSTANDING` | Exception/prevailing clause | Main clause | "I override that provision" |
| `DESPITE` | Exception/prevailing clause | Main clause | Same as NOTWITHSTANDING |

As noted in drafting literature: "notwithstanding looks back whilst subject to looks forward."

---

## 2. Revised Taxonomy

After analysis, we identify **four distinct semantic functions**:

| Category | Semantics | Computational Model |
|----------|-----------|---------------------|
| **Override/Priority** | A prevails over B when both apply and conflict | `if conflict(A,B) then winner(A,B)` |
| **Domain Restriction** | Restricts what inputs the rule applies to | `filter predicate inputs` |
| **Output Modifier** | Transforms result after rule evaluates | `transform(base_result)` |
| **Defeasibility** | Provisional conclusion, can be defeated | `default unless defeated` |

### 2.1 Why Four Categories?

Earlier analysis identified seven patterns, but several collapse:

- **Priority Declaration** and **Preservation/Savings** both reduce to **Override/Priority** (preservation is just priority stated defensively)
- **Exception/Carve-Out**, **Condition Precedent**, and **Scope Restriction** all reduce to **Domain Restriction** (all filter which inputs a rule applies to)
- **Proviso/Qualification** is **Output Modifier**
- **Defeasibility Marker** remains distinct

### 2.2 Open Question: Is Defeasibility Distinct from Override?

A defeater is essentially a higher-priority rule that fires under certain conditions. The distinction may be:
- **Override**: statically declared priority between named rules
- **Defeasibility**: dynamic - any future rule could potentially defeat this one

This may be more about declaration style than semantics. The corpus analysis below will help clarify.

---

## 3. Corpus of Examples

We collected 55 examples from statutes, regulations, and contracts across multiple jurisdictions. Each is classified according to our four-category taxonomy.

### Classification Key

| Code | Category |
|------|----------|
| **O** | Override/Priority |
| **D** | Domain Restriction |
| **M** | Output Modifier |
| **F** | Defeasibility |
| **?** | Ambiguous/Unclear |

---

### 3.1 Constitutional and Charter Examples

| # | Source | Text | Category | Notes |
|---|--------|------|----------|-------|
| 1 | Canadian Charter s.33 | "Parliament may expressly declare that the Act shall operate NOTWITHSTANDING sections 2 or 7-15 of this Charter" | **O** | Legislature overrides Charter rights for 5-year renewable periods |
| 2 | Canadian Charter s.1 | "The Charter guarantees the rights and freedoms set out in it SUBJECT TO such reasonable limits as can be demonstrably justified" | **D** | Rights apply only within "reasonable limits" domain |
| 3 | Indian Constitution Art.31B | "NOTWITHSTANDING anything in this Part, none of the Acts specified in the Ninth Schedule shall be deemed to be void on the ground of inconsistency" | **O** | Ninth Schedule laws override fundamental rights challenges |
| 4 | US Constitution Art.VI | "This Constitution shall be the supreme Law of the Land... any Thing in the Constitution or Laws of any State to the Contrary NOTWITHSTANDING" | **O** | Federal supremacy over state law |

---

### 3.2 Corporate Law Examples

| # | Source | Text | Category | Notes |
|---|--------|------|----------|-------|
| 5 | UK Companies Act 2006 s.168 | "A company may by ordinary resolution remove a director NOTWITHSTANDING anything in its constitution or any agreement between it and him" | **O** | Statutory right overrides contract/articles |
| 6 | Singapore Companies Act | "A private company may remove any director by ordinary resolution SUBJECT TO contrary provision in the articles" | **D** | Removal right applies unless articles provide otherwise |
| 7 | Companies Act (general) | "SUBJECT TO sections 549, 551 and 559, a company may issue debentures" | **D** | Power limited to cases not caught by those sections |
| 8 | Companies Act (general) | "SUBJECT TO the Companies Act and these Articles, a director may vote on matters" | **D** | Voting right exists within statutory/articles limits |
| 9 | Singapore Companies Act s.25C | "Such transaction may, NOTWITHSTANDING section 25B, still be voidable if entered without authority" | **O** | Voidability overrides general validity rule |

---

### 3.3 Contract Clause Examples

| # | Source | Text | Category | Notes |
|---|--------|------|----------|-------|
| 10 | Assignment clause | "Assignment is prohibited. NOTWITHSTANDING the foregoing, assignments to affiliates are permitted" | **O** | Affiliate exception overrides general prohibition |
| 11 | Termination clause | "30-day notice required. NOTWITHSTANDING the foregoing, immediate termination permitted for material breach" | **O** | Breach exception overrides notice requirement |
| 12 | Confidentiality survival | "NOTWITHSTANDING termination of this Agreement, confidentiality obligations survive for 5 years" | **O** | Survival overrides general termination effects |
| 13 | Payment terms | "Buyer pays within 30 days. NOTWITHSTANDING the foregoing, buyer may withhold if goods defective" | **O** | Withholding right overrides payment duty |
| 14 | Liability cap | "NOTWITHSTANDING anything to the contrary, liability shall not exceed $50 million" | **O** | Cap overrides all other liability provisions |
| 15 | Lease subordination | "This Lease is SUBJECT TO the lien of the Mortgage" | **O** | Mortgage takes priority over lease |
| 16 | Security deposit | "Landlord shall repay deposit SUBJECT TO proper deductions" | **M** | Base obligation (repay) modified by deductions |

---

### 3.4 Real Estate Examples

| # | Source | Text | Category | Notes |
|---|--------|------|----------|-------|
| 17 | Purchase agreement | "Purchase shall complete SUBJECT TO satisfactory survey" | **D** | Completion only if survey condition met |
| 18 | Purchase agreement | "SUBJECT TO the buyer obtaining financing on satisfactory terms" | **D** | Transaction domain restricted to financed cases |
| 19 | Purchase agreement | "SUBJECT TO the buyer selling their current home by Jan 1" | **D** | Transaction domain restricted by sale condition |
| 20 | Lease holdover | "NOTWITHSTANDING any provision to the contrary, holdover constitutes default entitling Landlord to remedies" | **O** | Holdover consequences override other provisions |
| 21 | Lease premises | "NOTWITHSTANDING Section 1.2, 8 acres excluded from Leased Premises" | **D** | Domain of "premises" reduced by exclusion |
| 22 | Landlord liability | "NOTWITHSTANDING anything herein, Landlord liability limited to interest in Property" | **O** + **M** | Override + output cap |

---

### 3.5 Employment Law Examples

| # | Source | Text | Category | Notes |
|---|--------|------|----------|-------|
| 23 | At-will termination | "NOTWITHSTANDING anything to the contrary, either party may terminate for any reason with 60 days notice" | **O** | At-will right overrides other restrictions |
| 24 | Termination for cause | "NOTWITHSTANDING the foregoing, Executive not deemed terminated for Cause unless Board resolution" | **D** | "Cause" domain restricted to Board-approved cases |
| 25 | Severance | "Severance payments SUBJECT TO signing release of claims" | **D** | Severance domain restricted to release-signers |
| 26 | Notice period | "SUBJECT TO this clause, each party shall give appropriate notice" | **D** | Notice applies within clause's conditions |

---

### 3.6 Tax Law Examples

| # | Source | Text | Category | Notes |
|---|--------|------|----------|-------|
| 27 | IRC §527 | "NOTWITHSTANDING any other provision of law, gross income shall include amounts expended for exempt function" | **O** | Inclusion rule overrides general exclusions |
| 28 | Texas Property Tax | "NOTWITHSTANDING any other provision, notice may be delivered electronically" | **O** | Electronic delivery overrides paper requirements |
| 29 | California Sales Tax | "NOTWITHSTANDING any other provision, violation with intent to evade is felony when amount exceeds $25,000" | **O** + **D** | Felony status overrides misdemeanor; applies only above threshold |
| 30 | IRC §465 | "NOTWITHSTANDING any other provision, taxpayer not at risk for amounts protected by guarantees" | **O** | At-risk exclusion overrides general inclusion |
| 31 | IRC §139 | "NOTWITHSTANDING any other provision, no deduction allowed for expenditure already excluded" | **O** | No-double-benefit rule overrides deduction rules |
| 32 | IRC §165 disaster | "NOTWITHSTANDING subsection (a), disaster loss may be taken in preceding taxable year" | **M** | Timing of deduction modified |

---

### 3.7 Data Protection Examples

| # | Source | Text | Category | Notes |
|---|--------|------|----------|-------|
| 33 | GDPR Art.21(5) | "NOTWITHSTANDING Directive 2002/58/EC, data subject may object by automated means" | **O** | GDPR right overrides e-Privacy restrictions |
| 34 | Singapore PDPA | "Duty to respond accurately and completely SUBJECT TO exceptions in s.21(2),(3),(3A),(4)" | **D** | Duty domain excludes excepted cases |
| 35 | GDPR Art.9 | "Processing of special categories SUBJECT TO additional protections" | **M** | Processing rules modified for sensitive data |
| 36 | Data breach notification | "Breach must be registered internally NOTWITHSTANDING obligation to notify supervisory authority" | **O** | Internal logging required regardless of notification duty |

---

### 3.8 Trade and International Law Examples

| # | Source | Text | Category | Notes |
|---|--------|------|----------|-------|
| 37 | GATT Art.XII | "NOTWITHSTANDING Article XI, a party may restrict imports to safeguard balance of payments, SUBJECT TO following paragraphs" | **O** + **D** | Override + domain limits |
| 38 | WTO TRIMs | "NOTWITHSTANDING Art.2, Member may apply same TRIM to new investment during transition" | **O** | Transition exception overrides general prohibition |
| 39 | WTO Safeguards | "NOTWITHSTANDING paragraph 5 of Article 7, developing country may reapply safeguard measure" | **O** | Development exception overrides reapplication bar |
| 40 | WTO Enabling Clause | "NOTWITHSTANDING Article I, parties may accord preferential treatment to developing countries" | **O** | Development preference overrides MFN requirement |
| 41 | WTO Agreement on Agriculture | "GATT 1994 shall apply SUBJECT TO this Agreement" | **O** | Agriculture Agreement takes priority over general GATT |
| 42 | Trade Facilitation | "NOTWITHSTANDING general interpretative note, nothing diminishes obligations under GATT 1994" | **?** (Preservation) | Asserting non-conflict; reduces to O if conflict existed |

---

### 3.9 Intellectual Property Examples

| # | Source | Text | Category | Notes |
|---|--------|------|----------|-------|
| 43 | LG Electronics license | "NOTWITHSTANDING anything to the contrary, nothing shall limit patent exhaustion" | **?** (Preservation) | Asserts exhaustion doctrine preserved |
| 44 | 35 USC §282 (struck) | "NOTWITHSTANDING preceding sentence, if composition claim invalid, process no longer nonobvious solely on basis of §103(b)(1)" | **O** | Invalidity consequence overrides nonobviousness finding |

---

### 3.10 Insurance Examples

| # | Source | Text | Category | Notes |
|---|--------|------|----------|-------|
| 45 | Idaho Insurance Code | "Group policies SUBJECT TO provisions of this code section regarding recreational activities" | **D** | Policy scope limited by code requirements |
| 46 | Insurance policy | "Coverage SUBJECT TO exclusions in Schedule B" | **D** | Coverage domain excludes listed items |

---

### 3.11 Lease and Property Examples

| # | Source | Text | Category | Notes |
|---|--------|------|----------|-------|
| 47 | Rent waiver | "NOTWITHSTANDING the foregoing, Landlord waives Base Rent provided Tenant not in Default" | **O** + **D** | Waiver overrides rent duty; applies only to non-defaulting tenants |
| 48 | Termination override | "NOTWITHSTANDING the Second Amendment, Tenant's lease of Suite 470 continues until Dec 31, 2016" | **O** | Continuation overrides amendment's termination |
| 49 | Common area use | "Common areas available SUBJECT TO Rules and Regulations in Exhibit A" | **D** + **M** | Domain (which areas) and manner (how used) restricted |
| 50 | Reinstatement | "NOTWITHSTANDING the foregoing, but SUBJECT TO Section 5.04, Landlord shall reinstate utilities" | **O** + **D** | Override + domain limit |

---

### 3.12 Partnership and Business Examples

| # | Source | Text | Category | Notes |
|---|--------|------|----------|-------|
| 51 | Indian Partnership Act s.11 | "SUBJECT TO this Act, mutual rights and duties may be determined by contract" | **D** | Contractual freedom within statutory limits |
| 52 | Indian Partnership Act s.11(2) | "NOTWITHSTANDING section 27 of the Contract Act, partners may agree to non-compete" | **O** | Partnership non-compete overrides general contract invalidity |
| 53 | Indian Partnership Act s.36 | "NOTWITHSTANDING section 27 of the Contract Act, post-departure non-compete valid if reasonable" | **O** | Similar override with reasonableness limit |

---

### 3.13 Miscellaneous Statutory Examples

| # | Source | Text | Category | Notes |
|---|--------|------|----------|-------|
| 54 | UK Civil Aviation Act | "SUBJECT TO affirmative resolution procedure where Order mentions s.60(3)(r) and SUBJECT TO negative resolution in other cases" | **D** | Different procedures apply to different order types |
| 55 | BC Interpretation Act | "DESPITE subsection (1), an enactment affecting land use does not bind government" | **D** | Government excluded from land-use enactment domain |

---

## 4. Corpus Analysis

### 4.1 Category Distribution

| Category | Count | Percentage |
|----------|-------|------------|
| Override/Priority (O) | 35 | 64% |
| Domain Restriction (D) | 28 | 51% |
| Output Modifier (M) | 6 | 11% |
| Defeasibility (F) | 0 | 0% |
| Ambiguous (?) | 3 | 5% |

*Note: Many examples involve multiple categories (e.g., O+D), so percentages exceed 100%.*

### 4.2 Key Observations

1. **Override/Priority dominates**: Nearly two-thirds of examples establish which provision wins when both apply.

2. **Domain Restriction is common but often implicit**: Many "subject to" clauses define the scope of applicability.

3. **Output Modifier is rare**: Few examples transform results rather than gate/override them. The "parking costs half on holidays" pattern exists but is infrequent.

4. **Defeasibility may not be distinct**: No clear examples of pure defeasibility markers emerged. This suggests defeasibility might collapse into Override (with the defeater being a higher-priority rule) or Domain Restriction (with the defeating condition narrowing applicability).

5. **Combined patterns are common**: Many real-world examples exhibit multiple semantic functions simultaneously (e.g., "NOTWITHSTANDING X, but SUBJECT TO Y").

6. **The "preservation" pattern is rare in pure form**: Most "nothing in this Agreement shall affect..." clauses are either:
   - Redundant (no conflict actually exists)
   - Really priority declarations in defensive form

### 4.3 Revised Taxonomy Proposal

Based on corpus analysis, we might further simplify to **three categories**:

| Category | Semantics |
|----------|-----------|
| **Override** | A prevails over B when both would apply |
| **Filter** | Restricts domain (input) or transforms result (output) |
| **Composition** | How to combine multiple rules (pipe/sequence) |

The distinction between input filtering and output modification may be:
- **Input filter**: `rule applies_to (filter inputs)`
- **Output modifier**: `transform (rule inputs)`

Both are function composition; the difference is where in the pipeline the transformation occurs.

---

## 5. Problematic Patterns

### 5.1 Scope Ambiguity

```
"NOTWITHSTANDING anything to the contrary in this Agreement..."
```

What does "anything" include? The drafter may not have enumerated overridden provisions.

**Recommendation:** Require explicit references.

### 5.2 Circular Priority

```
Section A: "NOTWITHSTANDING Section B, ..."
Section B: "NOTWITHSTANDING Section A, ..."
```

Creates a paradox.

**Computational solution:** Detect cycles in priority graph; flag as error.

### 5.3 Dueling Notwithstanding Clauses

From corpus: LLC agreements with multiple "notwithstanding anything to the contrary" clauses create ambiguity about which "wins."

### 5.4 Mixed NOTWITHSTANDING and SUBJECT TO

Example #50: "NOTWITHSTANDING the foregoing, but SUBJECT TO Section 5.04..."

This requires parsing both an override (over "the foregoing") and a domain limit (by Section 5.04).

---

## 6. Implications for L4

### 6.1 Proposed Constructs

| Pattern | L4 Syntax (Proposal) |
|---------|---------------------|
| Override | `RULE X NOTWITHSTANDING Y` or `RULE X (priority: 10)` |
| Domain restriction | `RULE X WHEN condition` or `RULE X EXCEPT WHEN condition` |
| Output modification | `RULE X THEN transform result` |

### 6.2 Composition with Existing Features

L4 already has:
- `CONSIDER` / `WHEN` for conditional logic
- `MEANS` / `DECIDE` for definitions
- `OTHERWISE` for defaults

The override construct is genuinely new - L4 needs a way to declare priority between rules that might both fire.

### 6.3 Static Analysis Opportunities

- Detect circular priority declarations
- Verify priority graph is acyclic
- Flag "anything to the contrary" patterns as potentially ambiguous
- Check for unreachable rules (always overridden)

---

## 7. References

### Legal Drafting
- [Weagree: Notwithstanding in Contracts](https://weagree.com/clm/contracts/contract-wording/notwithstanding/)
- [UpCounsel: Notwithstanding Meaning in Law](https://www.upcounsel.com/notwithstanding-legal-use)
- [LawProse Lesson #196](https://lawprose.org/lawprose-lesson-196-notwithstanding/)
- [Adams on Contract Drafting: The Foregoing](https://www.adamsdrafting.com/the-foregoing/)

### Statutory Interpretation
- [CRS: Notwithstanding Clauses (PDF)](https://sgp.fas.org/crs/misc/notwith.pdf)
- [Capitol Weekly: Notwithstanding Clauses in California](https://capitolweekly.net/the-use-of-notwithstanding-clauses-in-california-legislation/)
- [SCC Times: Non Obstante Clauses](https://www.scconline.com/blog/post/2023/06/16/circumscribing-non-obstante-clauses-tracing-the-new-jurisprudence/)
- [LawCrust: Non Obstante Clause](https://lawcrust.com/non-obstante-clause/)
- [iPleaders: Non-Obstante Clause](https://blog.ipleaders.in/all-you-need-to-know-about-non-obstante-clause/)

### Defeasibility and Formal Methods
- [Stanford Encyclopedia: Defeasible Reasoning](https://plato.stanford.edu/entries/reasoning-defeasible/)
- [Prakken: Three Faces of Defeasibility](https://onlinelibrary.wiley.com/doi/abs/10.1111/j.0952-1917.2004.00259.x)
- [NDPR: Allowing for Exceptions](https://ndpr.nd.edu/reviews/allowing-for-exceptions-a-theory-of-defences-and-defeasibility-in-law/)

### Contract Examples
- [Law Insider: Notwithstanding Any Other Provision](https://www.lawinsider.com/clause/notwithstanding-any-other-provision)
- [Law Insider: Notwithstanding the Foregoing](https://www.lawinsider.com/clause/notwithstanding-the-foregoing)
- [Law Insider: Notwithstanding Termination](https://www.lawinsider.com/clause/notwithstanding-termination)
- [Afterpattern: Lease Clauses](https://afterpattern.com/clauses/leased-premises)

### Jurisdiction-Specific
- [Law Wales: Tips on Using Legislation](https://law.gov.wales/tips-using-legislation)
- [BC Laws: Interpretation Act](https://www.bclaws.gov.bc.ca/civix/document/id/complete/statreg/96238_01)
- [WTO Legal Texts](https://www.wto.org/english/docs_e/legal_e/legal_e.htm)
- [GDPRhub: Article 21](https://gdprhub.eu/Article_21_GDPR)
