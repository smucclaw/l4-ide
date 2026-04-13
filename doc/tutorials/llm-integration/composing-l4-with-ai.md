# Composing L4 with AI

Go from a natural-language policy or contract to draft L4 with Claude Code.

**Audience:** Legal engineers, policy drafters, contract authors
**Prerequisites:** [Your First L4 File](../getting-started/first-l4-file.md), Claude Code installed
**Time:** 30 minutes
**Goal:** Use Claude Code to turn prose rules into type-checked L4

---

## Why Claude Code?

Claude Code is Anthropic's agentic CLI. It edits files, runs the L4 type checker, reads error output, and iterates — all inside your repository. That loop matters for L4: drafting rules is easy, but making them type-check against realistic data is where most of the work is.

Paired with the `writing-l4-rules` skill, Claude Code knows L4 syntax, idioms, and the common pitfalls a generic chat model stumbles on.

---

## Setup

1. **Install Claude Code.** See [claude.com/claude-code](https://claude.com/claude-code).
2. **Open your L4 project.** Run `claude` inside the directory where your `.l4` files live.
3. **Confirm the skill is available.** Ask: _"What L4 skills do you have access to?"_ Claude should mention `writing-l4-rules`.

The skill bundles syntax references, validation steps, and deployment guidance, so you do not need to paste a cheat sheet into every prompt.

---

## The Workflow

```
   Natural language        Claude Code drafts L4        Type-checker runs
   policy / contract   ──▶   using writing-l4-rules  ──▶  against sample data
                                                                │
                             Human review & edits ◀────────── Iterate on errors
```

Four phases. The human owns the first and last; Claude Code does the heavy lifting in between.

---

## Phase 1: Frame the Source Text

Before prompting, decide three things:

1. **Scope** — one clause, one section, or the whole contract? Smaller scopes produce cleaner drafts.
2. **Decision output** — what question should the L4 rule answer? (`is eligible`, `amount owed`, `is in breach`.)
3. **Known inputs** — what facts will a caller provide? A `Person`, an `Order`, a `Claim`?

Write these down as a short brief. You will paste it into your first prompt.

### Example brief

> **Source:** Section 4 of our refund policy (pasted below).
> **Decision:** `is eligible for refund` returning a BOOLEAN.
> **Inputs:** an `Order` with purchase date, amount, and product category; the current date.

---

## Phase 2: Ask Claude Code to Draft

Open Claude Code and paste your brief plus the source text. A good opening prompt:

```
Using the writing-l4-rules skill, draft L4 for the refund policy below.

Brief:
- Decision: `is eligible for refund` returning BOOLEAN
- Input: an Order record
- Put the result in rules/refund.l4

Source (Section 4):
"""
A customer may request a refund within 30 days of purchase, provided
the product has not been opened. Digital goods are non-refundable
except where required by law. Orders over $500 require manager approval
before a refund is issued.
"""

Start by proposing the type declarations, then the decision rule.
Annotate each clause with a comment citing the source.
```

Claude Code will typically:

- declare `Order`, any enums (e.g. `ProductCategory`), and supporting records;
- write the top-level `DECIDE` rule;
- break sub-conditions into named helpers so the rule reads like the prose;
- add `#EVAL` examples at the bottom.

---

## Phase 3: Let It Type-Check and Iterate

Ask Claude Code to run the checker and fix what it finds:

```
Run `l4 check rules/refund.l4` and fix any errors. Keep going until it
type-checks clean, then show me the final file.
```

Claude Code will loop on the type-checker output — missing `GIVETH`, wrong field access, list vs. scalar confusions — until the file compiles. This is the phase where an agent beats a chat transcript: it reads the actual error, edits the actual file, and re-runs.

Then add sanity checks:

```
Add three #EVAL blocks:
1. An opened digital good purchased yesterday — expect FALSE.
2. A $200 unopened physical good purchased 10 days ago — expect TRUE.
3. A $900 unopened physical good purchased 10 days ago — expect
   "requires manager approval" handling.
```

If the evaluations contradict your intent, that is a finding. Either the prose was ambiguous, the draft misread it, or your mental model was wrong. All three are valuable.

---

## Phase 4: Human Review

The draft is not done when it compiles. Review for:

- **Fidelity** — does each clause of the source map to something in the L4?
- **Silent assumptions** — look for defaults Claude picked without asking (e.g. what "opened" means, whether store credit counts).
- **Cross-references** — if the policy cites other sections, are they stubbed with `ASSUME` or genuinely encoded?
- **Edge cases** — day-31 purchases, zero-dollar orders, refunds in foreign currency.

Ask Claude Code to annotate its own assumptions:

```
List every assumption you made that is not explicit in the source text,
with a one-line justification each.
```

This surfaces the interpretive choices so a lawyer or policy owner can sign off.

---

## A Complete Session

```
You: Using writing-l4-rules, draft L4 in rules/late-fee.l4 for:
     "A late fee of 5% of the outstanding balance applies to any
     payment received more than 14 days after the due date. The fee
     is waived for first-time late payments."
     Decision: `late fee owed` returning a NUMBER. Input: a Payment.

Claude: [writes rules/late-fee.l4 with Payment record, helper rules,
         and #EVAL examples]

You: Run the type checker and fix anything that breaks.

Claude: [runs `l4 check`, fixes a missing GIVETH, re-runs, clean]

You: Add an #EVAL for a payment 20 days late where the customer has
     two prior late payments — expect 5% of balance.

Claude: [adds the block, re-runs, confirms]

You: List your assumptions.

Claude: 1. "First-time" means no prior late payments ever, not
           "first this year" — source is silent.
        2. Days late is calendar days, not business days.
        3. The 5% applies to outstanding balance at the due date,
           not at the payment date.
```

Three assumptions the policy owner now has to confirm. That's the point.

---

## Tips

- **One clause per prompt** when the source is dense. Aggregate later.
- **Keep the brief in a file** (e.g. `rules/_brief.md`) and reference it — saves retyping.
- **Ask for alternatives** when a clause is ambiguous: _"show two encodings and explain the difference"_.
- **Let Claude Code deploy** — once the file is clean, ask it to mark the decision `@export` and walk you through deployment. See [Exporting Functions for Deployment](../deploying-functions/exporting-functions-for-deployment.md).

---

## Limitations

- Claude Code can misread novel legal structures. Always diff the draft against the source, clause by clause.
- Generated `#EVAL` cases reflect Claude's interpretation, not ground truth. Write your own before trusting the rule.
- The agent will happily type-check a semantically wrong rule. Type safety is not correctness.

---

## What You Learned

- A four-phase workflow: frame → draft → iterate → review
- How to brief Claude Code so drafts are usable
- How to surface interpretive assumptions before they ship

---

## Next Steps

- [Exporting Functions for Deployment](../deploying-functions/exporting-functions-for-deployment.md) — publish the rule as an API
- [Common Patterns](../getting-started/common-patterns.md) — idioms worth knowing when reviewing Claude's output
- [Legislative Ingestion](legislative-ingestion.md) — deeper workflow for statute-scale text
