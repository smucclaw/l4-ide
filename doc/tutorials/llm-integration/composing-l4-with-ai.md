# Composing L4 with AI

Go from a natural-language policy or contract to draft L4 with Legalese AI.

**Audience:** Legal engineers, policy drafters, contract authors
**Prerequisites:** [Your First L4 File](../getting-started/first-l4-file.md), Legalese AI installed
**Time:** 30 minutes
**Goal:** Use Legalese AI to turn prose rules into type-checked L4

---

## Setup

1. **Install L4 VS Code extension.** See [Visual Studio Marketplace](https://marketplace.visualstudio.com/items?itemName=Legalese.l4-vscode).
2. **Open your L4 project.** Create a VS Code workspace in a folder of your choice. Create a new repo using `git` if you like.
3. **Switch to the Legalese AI tab.** Say "Hello" and see what it responds.

Legalese AI bundles syntax references, validation steps, and deployment guidance, so you do not need to paste a cheat sheet into every prompt.

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

## Phase 2: Ask Legalese AI to Draft

Open Legalese AI and paste your brief plus the source text. A good opening prompt:

```
Draft L4 for the refund policy below.

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

Legalese AI will typically:

- declare `Order`, any enums (e.g. `ProductCategory`), and supporting records;
- write the top-level `DECIDE` rule;
- break sub-conditions into named helpers so the rule reads like the prose;
- add `#EVAL` examples at the bottom.

---

## Phase 3: Let It Type-Check and Iterate

Ask Legalese AI to run the checker and fix what it finds:

```
Run `l4 check rules/refund.l4` and fix any errors. Keep going until it
type-checks clean, then show me the final file.
```

Legalese AI will loop on the type-checker output — missing `GIVETH`, wrong field access, list vs. scalar confusions — until the file compiles. This is the phase where an agent beats a chat transcript: it reads the actual error, edits the actual file, and re-runs.

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
- **Silent assumptions** — look for defaults Legalese AI picked without asking (e.g. what "opened" means, whether store credit counts).
- **Cross-references** — if the policy cites other sections, are they stubbed with `ASSUME` or genuinely encoded?
- **Edge cases** — day-31 purchases, zero-dollar orders, refunds in foreign currency.

Ask Legalese AI to annotate its own assumptions:

```
List every assumption you made that is not explicit in the source text,
with a one-line justification each.
```

This surfaces the interpretive choices so a lawyer or policy owner can sign off.

---

## A Complete Session

```
You: Draft L4 in rules/late-fee.l4 for:
     "A late fee of 5% of the outstanding balance applies to any
     payment received more than 14 days after the due date. The fee
     is waived for first-time late payments."
     Decision: `late fee owed` returning a NUMBER. Input: a Payment.

AI: [writes rules/late-fee.l4 with Payment record, helper rules,
         and #EVAL examples]

You: Run the type checker and fix anything that breaks.

AI: [runs `L4 Check`, fixes a missing GIVETH, re-runs, clean]

You: Add an #EVAL for a payment 20 days late where the customer has
     two prior late payments — expect 5% of balance.

AI: [adds the block, re-runs, confirms]

You: List your assumptions.

AI: 1. "First-time" means no prior late payments ever, not
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
- **Let Legalese AI prepare for deployment** — once the file is clean, ask it to mark the decision `@export` and walk you through deployment. See [Exporting Functions for Deployment](../deploying-functions/exporting-functions-for-deployment.md).

---

## Limitations

- Legalese AI can misread novel legal structures. Always diff the draft against the source, clause by clause.
- Generated `#EVAL` cases reflect AI's interpretation, not ground truth. Write your own before trusting the rule.
- The agent will happily type-check a semantically wrong rule. Type safety is not correctness.

---

## What You Learned

- A four-phase workflow: frame → draft → iterate → review
- How to brief Legalese AI so drafts are usable
- How to surface interpretive assumptions before they ship

---

## Next Steps

- [Exporting Functions for Deployment](../deploying-functions/exporting-functions-for-deployment.md) — publish the rule as an API
- [Common Patterns](../getting-started/common-patterns.md) — idioms worth knowing when reviewing AI's output
- [Legislative Ingestion](legislative-ingestion.md) — deeper workflow for statute-scale text
