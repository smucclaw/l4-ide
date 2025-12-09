# Temporal EVAL Specification

## Overview

`EVAL … DO …` is a runtime meta-evaluation construct for L4. Unlike the compile-
time `#EVAL` directive, runtime `EVAL` captures an L4 thunk, temporarily rewires
the temporal/rule context (including Git commits), evaluates the thunk, and then
restores the original context. This lets authors answer questions like “would
`may purchase alcohol` have been true under rules effective on 1 Jan 2010 using
the encoding from commit abc123?” without diving into Haskell.

## Motivation

Multi-temporal reasoning needs more than valid-time parameters. We want to:

1. Evaluate predicates at arbitrary valid times (`tcValidTime`).
2. Re-run logic under historical rule versions / encodings (`tcRuleVersionTime`,
   `tcRuleEncodingTime`) without rebuilding the evaluator for each axis.
3. Slice by system knowledge (“as at registry snapshot dated T”).

Git already tracks rule versions and encoding history, so `EVAL` treats commits
as first-class snapshots, akin to VAX/VMS-style file versioning. L4 code can now
ask “what did my program conclude eight commits ago?” entirely from userland.

## Syntax

Canonical block form:

```l4
EVAL
  UNDER VALID TIME January 1 2010
  UNDER RULES EFFECTIVE AT January 1 2010
  UNDER COMMIT "abc123"
  AS OF SYSTEM TIME July 1 2015
  DO `may purchase alcohol` applicant
```

Mixfix sugar (preferred for readability):

```l4
EVAL `retroactive to` January 1 2010
     `under commit` "abc123"
     `as of system time` July 1 2015
     `evaluate` (`may purchase alcohol` applicant)
```

Each clause is optional; absent clauses inherit the caller’s context.

## Semantics

Given current context Γ = (`tcValidTime`, `tcSystemTime`, `tcRuleVersionTime`,
`tcRuleValidTime`, `tcRuleEncodingTime`, `tcDecisionTime`):

1. Save Γ plus the current Git checkout / compiled rule set.
2. Apply clauses in order:
   - `UNDER VALID TIME t` → `tcValidTime := t`.
   - `AS OF SYSTEM TIME t` → `tcSystemTime := t`.
   - `UNDER RULES EFFECTIVE AT d` → resolve commit tagged for `d`, update
     `tcRuleVersionTime`, `tcRuleValidTime`, `tcRuleEncodingTime`.
   - `UNDER COMMIT c` → load rules from commit `c`, update
     `tcRuleVersionTime`, `tcRuleEncodingTime`.
   - `RETROACTIVE TO d` → shorthand for “under rules effective at d” and
     “as of system time d”.
3. Evaluate the thunk (`DO expr`) under the modified context.
4. Restore Γ and the previous checkout/cache state.
5. Return the result of `expr`.

Errors (missing commit, invalid date) abort evaluation and restore Γ.

## Runtime Implementation

Add a helper:

```haskell
withEvalContext :: [EvalClause] -> L4Temporal a -> L4Temporal a
```

`EvalClause` is an ADT covering the clauses above. `withEvalContext` pushes the
current `TemporalContext`/git state on a stack, applies clauses, runs the
computation, then pops the stack. Nested `EVAL`s just grow this stack.

Key reuse points:

- `getRulesAtCommit` already caches compiled rule sets.
- `AuditEvent` should gain an `AuditEval` variant recording requested clauses.
- Use libgit2 APIs (or a detached worktree) to avoid trashing the caller’s
  working tree when checking out historical commits.

## Interaction with Builtins and Prelude

- Builtins: provide uppercase forms (`EVAL`, `UNDER RULES EFFECTIVE AT`,
  `UNDER COMMIT`). The runtime enforces context switching and caching.
- Prelude: add mixfix wrappers so authors can write
  ``EVAL `retroactive to` d `evaluate` expr``. These wrappers just build the
  appropriate `EvalClause` list and call into the builtin.
- Libraries: optional—domain kits can expose constants (e.g.,
  `` `T_BCIA_Commencement` ``) so `EVAL` clauses reference symbolic dates.

## Examples

### 1. Historic eligibility check

```l4
DECIDE `historic injustice made good` applicant MEANS
  EVAL `retroactive to` January 13 2010
       `evaluate` (`is eligible` applicant)
```

### 2. Compare current vs previous regime

```l4
DECIDE `decision changed since commit` commitHash applicant MEANS
  LET oldResult IS EVAL `under commit` commitHash `evaluate`
                      (`is eligible` applicant)
      currentResult IS `is eligible` applicant
  IN oldResult NOT EQUALS currentResult
```

### 3. Nested eval (retroactive counterfactual)

```l4
EVAL `retroactive to` January 1 2005 `evaluate`
  (EVAL `under commit` "windrush_fix"
        `evaluate` (`grant compensation` applicant))
```

## Testing Strategy

1. **Unit tests** in Haskell to verify `withEvalContext` restores contexts even
   on exceptions, and that caching prevents redundant compilation.
2. **Integration tests** in L4 verifying nested `EVAL`s, commit selection, and
   audit logging. Use a fixture repo with two commits whose rules differ.
3. **Performance**: stress-test repeated `EVAL` calls across many commits to
   validate cache hit rates and absence of stale working-tree state.

## Future Work

- Expose read-only views of git blame/history to L4 (`EVAL HISTORY OF rule`?).
- Allow `EVAL` to target tagged states ("latest in-force version") rather than
  explicit commits.
- Combine `EVAL` with streaming evidence updates: `AS OF SYSTEM TIME t` +
  `WITH FACTS AS OF t`.

---

## Appendix: Jurisprudential Perspectives on Temporal Evaluation

> **Note**: This appendix documents how legal scholars and practitioners from different
> traditions would view L4's approach to temporal reasoning, rule versioning, and the
> spirit/letter distinction. It emerged from design discussions in December 2025 and is
> intended to help future contributors understand why certain design decisions matter
> beyond the technical. See also `opm2l4/docs/OPM_RULE_CHANGE.md` §9 for related discussion.

---

### The Legal Academy (Law Schools)

#### Analytical Jurisprudence (Hart, Raz)

**H.L.A. Hart** would recognize the "open texture" problem immediately:

> "Your distinction between letter and spirit is what I called the **core** and
> **penumbra** of legal concepts. In the core, application is clear. In the penumbra,
> we must make choices that the original text does not determine. Your 'semantic
> transformations' are attempts to extend the core into new penumbral cases."

Hart would likely **approve** of making the gap explicit rather than pretending
mechanical application suffices. But he'd warn:

> "The penumbra cannot be eliminated by more precise drafting or better transformations.
> It is inherent in the nature of language applied to an open future."

#### Dworkinian Perspective

**Ronald Dworkin** would see this as vindication of his critique of legal positivism:

> "You've discovered that law is not just rules but also **principles**. Your 'spirit'
> is what I call the **best constructive interpretation** of the legal materials. But
> this cannot be mechanized — it requires moral reasoning about what interpretation
> makes the law **the best it can be**."

He might critique the transformation approach:

> "You allow humans to declare transformations, but you provide no theory of which
> transformations are *correct*. Without a theory of political morality underlying
> the law, you're just delegating the hard question."

#### Legal Realism (Holmes, Llewellyn)

**Oliver Wendell Holmes** would be characteristically blunt:

> "The life of the law has not been logic; it has been experience. Your formal system
> captures the logic admirably, but the experience — the felt necessities, the
> prevalent moral and political theories — that's where the real law lives. Your
> 'semantic transformations' are where the actual lawmaking happens, dressed up as
> interpretation."

**Karl Llewellyn** might add:

> "For every canon of construction, there is an equal and opposite canon. 'Follow
> the plain meaning' vs 'avoid absurd results.' Your system must choose, and the
> choice is not determined by logic."

---

### The Legislative Realm (Drafters)

#### Parliamentary Counsel

A seasoned **legislative drafter** would nod knowingly:

> "We've been dealing with this for centuries. That's why we have **Interpretation
> Acts** — they provide exactly the kind of standing transformations you describe.
> 'Words importing the masculine gender include the feminine.' 'Person includes
> corporation.' You're reinventing a wheel we've been turning since 1850."

They might point to specific mechanisms:

| Legislative Device | L4 Equivalent |
|-------------------|---------------|
| Interpretation Acts | Standing SemanticTransformations |
| "For the purposes of this Act" definitions | `tcInterpretivePurpose` |
| "Notwithstanding any other provision" | Override clauses in EVAL |
| "Subject to section X" | Dependency/priority ordering |
| Henry VIII clauses | Runtime transformation authority |

But they'd also warn:

> "We deliberately leave **constructive ambiguity** in legislation. Sometimes the
> political compromise requires not deciding a question — leaving it to courts.
> Your system assumes someone must specify the transformation in advance. Sometimes
> that's not possible or desirable."

#### Rules-as-Code Advocates (OECD, NZ Better Rules)

This camp would be **enthusiastic but cautious**:

> "This is exactly what we've been advocating — making the interpretation layer
> explicit, auditable, traceable. But you've identified a crucial gap: the
> transformation layer is where policy discretion lives. We need governance
> frameworks for who can declare transformations and under what authority."

---

### The Judicial Realm

#### Common Law Judges

A **senior appellate judge** might say:

> "You've systematized what we do every day. When I interpret a statute, I consider:
> the text (your 'letter'), the purpose (your 'spirit'), the context, the consequences,
> the legal system's coherence. Your 'EvaluationContext' is a reasonable first
> approximation."

But they'd raise concerns:

> "The transformation must have **authority**. I can declare that 'spouse' includes
> same-sex partners because *Parliament* said so in the Marriage Act, or because
> the *Supreme Court* said so in a binding precedent. Your system tracks which
> transformation was applied, but does it verify the transformation is *legally
> authorized* in this jurisdiction, for this purpose, at this time?"

#### Canons of Construction

They'd recognize the "interpretive purpose" pattern:

> "We call this **purposive interpretation** or the **mischief rule** — what problem
> was Parliament trying to solve? But we also have competing canons:
> - *Expressio unius est exclusio alterius* (expressing one thing excludes others)
> - *Ejusdem generis* (general words limited by specific ones)
> - *In pari materia* (statutes on same subject interpreted together)
>
> Your system assumes a single 'purpose' in the evaluation context. But canons
> often conflict, and choosing between them *is* the judicial function."

#### The Living Tree vs Originalism Debate

A **Canadian jurist** might invoke the "living tree" doctrine:

> "The constitution must be interpreted as a 'living tree capable of growth within
> its natural limits.' Your transformations formalize this — we *expect* meaning
> to evolve. The question is whether growth is within 'natural limits.'"

An **American originalist** might object:

> "Original public meaning should constrain interpretation. Your 'spirit' talk is
> dangerous — it invites judges to import their own preferences. The virtue of
> the 'letter' is that it constrains. Your system should default to letter-only
> evaluation and require explicit, democratically-authorized transformations."

---

### Critical Perspectives

#### Critical Legal Studies

**Duncan Kennedy** or **Roberto Unger** might say:

> "The choice of transformation is always **political**. You've created a system
> where someone must decide that 'spouse' includes same-sex partners. That decision
> has winners and losers. By calling it a 'semantic transformation' you've mystified
> what is actually a **distribution of power**. Who controls the transformation
> registry controls the law."

#### Feminist Legal Theory

**Catharine MacKinnon** might observe:

> "The 'husband/wife' example is telling. The 'spirit' of those old rules often
> *was* to exclude women from economic independence. Retrofitting 'spouse' may
> perpetuate structural biases that were *features*, not bugs, of the original
> design. Sometimes the correct transformation is to **reject** the old rule's
> spirit entirely, not to extend it."

#### Postcolonial Legal Theory

A scholar from this tradition might add:

> "When you 'grandfather' rules across eras, whose history counts? Colonial laws
> were designed to exclude colonized peoples. Should we transform 'British subject'
> to include those the Empire exploited? The 'spirit' of those laws was often
> explicitly racist. Your framework assumes the original spirit was benign but
> poorly expressed. Sometimes it was malign and precisely expressed."

---

### A Synthesis

If we brought these perspectives together, they might agree on several points:

#### Points of Consensus

1. **The gap is real**: Letter and spirit genuinely diverge, and pretending otherwise
   is naive.

2. **Mechanization has limits**: Formal systems can *apply* transformations but cannot
   *generate* or *validate* them without normative input.

3. **Authority matters**: A transformation must be legally authorized — by statute,
   precedent, or legitimate interpretive method.

4. **Context is irreducible**: The "interpretive purpose" is not just a parameter but
   often the central contested question.

5. **Audit is valuable**: Making the interpretation explicit and traceable is a genuine
   improvement over hidden judicial discretion.

#### Points of Disagreement

| Issue | Progressive View | Conservative View |
|-------|-----------------|-------------------|
| Default behavior | Apply spirit-preserving transformations | Stick to letter; require explicit authorization |
| Who decides transformations | Courts, evolving with society | Legislature, democratically accountable |
| Historical rules | Interpret charitably | Apply as written; let legislature fix |
| Ambiguity | Resolve in favor of purpose | Resolve in favor of rule-of-law predictability |

---

### What L4 Should Take From This

The jurisprudential perspectives suggest L4 should:

1. **Not claim neutrality**: The system embeds choices. Document them.

2. **Require transformation authority**: Not just "what transformation" but "under
   what legal authority."

3. **Support multiple interpretive stances**: Allow evaluation with "strict letter,"
   "purposive," "living tree," etc., and compare results.

4. **Acknowledge political stakes**: The transformation registry is not merely
   technical infrastructure — it's a site of legal power.

5. **Enable contestation**: When letter and spirit diverge, surface this for human
   decision rather than resolving silently.

---

### References for Further Reading

- Hart, H.L.A. (1961). *The Concept of Law*. Oxford University Press. (esp. Ch. VII on open texture)
- Dworkin, R. (1986). *Law's Empire*. Harvard University Press. (constructive interpretation)
- Llewellyn, K. (1950). "Remarks on the Theory of Appellate Decision." *Vanderbilt Law Review*.
- Fuller, L. (1958). "Positivism and Fidelity to Law." *Harvard Law Review*.
- Scalia, A. (1997). *A Matter of Interpretation*. Princeton University Press. (textualism)
- Kennedy, D. (1976). "Form and Substance in Private Law Adjudication." *Harvard Law Review*.
- Eskridge, W. (1994). *Dynamic Statutory Interpretation*. Harvard University Press.
