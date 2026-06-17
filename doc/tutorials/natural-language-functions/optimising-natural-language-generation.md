# Optimising for Natural Language Document Generation with `@nlg`

L4 can render your rules back into formatted English prose — in the VS Code
**Render** tab, or with `l4 render`. The renderer is **deterministic**: it walks
your code and turns each construct into a sentence or an outline. That means the
quality of the generated prose is mostly in your hands. Well-named, well-shaped
rules read almost like professionally drafted legal writing with no extra effort; awkward ones read like a
transcript of an algorithm.

This tutorial shows how to get transparent English prose out of the renderer, using three increasingly powerful levers:

1. **Names** — backticked identifiers and parameter names.
2. **Shape** — mixfix word order, control flow, section titles, and arithmetic.
3. **`@nlg`** — an authored sentence that overrides the structural rendering.

It finishes with how **Legalese AI** can then apply drafting policies to refine
the result further.

## Prerequisites

- Basic L4 functions (see [Your First L4 File](../getting-started/first-l4-file.md))
- [Infix, Postfix, and Mixfix Functions](natural-language-functions.md) — the
  calling-syntax foundation this tutorial builds on

---

## Lever 1 — Names do most of the work

The renderer prints identifiers and parameter names **verbatim**. Good names are
the single highest-leverage thing you can do.

### Name rules as the phrase you want to read

A backticked identifier can contain spaces, so name a rule as the noun phrase or
clause it represents:

```l4
-- Renders: "Monthly property tax means ..."
`monthly property tax` MEANS ...

-- Renders: "Mpt means ..."
mpt MEANS ...
```

> [!NOTE]
> Beginner programmers are routinely and pointedly reminded to use the first form whenever they reach for the latter.

### Name parameters as nouns, not letters

Parameters appear in the rendered prose and in every `@nlg` slot. Name them the
way they should read:

```l4
-- Renders: "... the buyer ... the seller ..."
GIVEN `the buyer` IS A Person
      `the seller` IS A Person

-- Renders: "... p ... q ..."
GIVEN p IS A Person
      q IS A Person
```

Sometimes existing legal writing will deliberately adopt this form: "A person (A) discriminates against another (B) if ..." (Equality Act 2010, s.13). In that situation the renderer will aim to obey the "legislative variable" style.

If a parameter has a record type, the renderer promotes the type name into a noun phrase: ``GIVEN claim IS A `Payment Claim` `` renders as "the payment claim". This works cleanly only when one parameter has that type — two `Payment Claim` parameters would collide on the same phrase.

### Name record fields readably

Projections render as `X's field`, so field names carry straight into the prose:

```l4
DECLARE `Property Details` HAS
    `market value`         IS A NUMBER
    `monthly property tax` IS A NUMBER
-- "the property's market value", "the property's monthly property tax"
```

---

## Lever 2 — Shape the code so it reads in order

### Use mixfix so calls read as sentences

Put the words and the argument holes where they belong in the sentence. (See the
[mixfix tutorial](natural-language-functions.md) for the full mechanics.)

```l4
GIVEN `the applicant` IS A Person
      `the programme` IS A Programme
      `application date` IS A DATE
GIVETH A BOOLEAN
DECIDE `as at an` `application date` `the applicant` `is eligible for` `the programme` IF ...
  
```

In a conventional programming language, this would be a function taking three arguments: `eligibility(applicant, programme, date)`.

In L4, it is also a function taking three arguments, but the arguments are intermingled across the function name for the sake of readability.

### End helper names in a preposition

A function whose name ends in a preposition (`of`, `for`, `to`, `between`, …)
gets its arguments joined naturally, without a stray "with":

```l4
`the later of` x y MEANS IF x >= y THEN x ELSE y
-- "the later of the start date and the end date"
```

### Let control flow stay structured

`CONSIDER`, `IF`/`THEN`/`ELSE`, and `AND`/`OR` render as indented outlines, not
run-on sentences. Keep operative logic where the renderer can see it as
structure rather than burying it inside an unrelated expression:

```l4
CONSIDER claim's status
WHEN Paid    THEN ...
WHEN Overdue THEN ...
```

renders as

```
depending on the claim's status:
- if it is Paid: ...
- if it is Overdue: ...
```

### Keep arithmetic as arithmetic

Numeric expressions render in **formula mode** — with `+ − × ÷` and parentheses —
not as nested "the sum of the product of …". Write the maths directly instead of
wrapping it in prose helpers:

```l4
(`base rent` PLUS `service charge`) TIMES `months` PLUS `deposit`
-- "(base rent + service charge) × months + deposit"
```

### Group rules into titled sections

Section markers organise a document into titled, numbered sections — they become
headings in the rendered output and entries in the table of contents.

- `` § `Section Name` `` starts a top-level section.
- `` §§ `Subsection Name` `` nests one level deeper (`§§§` deeper still).

The name is backtick-quoted, so it can be a full phrase. Every declaration after
a marker belongs to that section until the next marker:

```l4
§ `Eligibility`

GIVEN `the applicant` IS A Person
`the applicant` `qualifies for EP` IF ...

§§ `Age requirements`

GIVEN `the applicant` IS A Person
`the applicant` `is of working age` IF ...
```

renders as

```
§ 1  Eligibility
    • The applicant qualifies for EP if ...
    1.1  Age requirements
        • The applicant is of working age if ...
```

The `§` numbers appear when **Number sections** is enabled in the Render tab
(or `--number-sections` on the CLI); the headings and table-of-contents entries
appear either way.

A flat file with no markers still gets sensible structure — its type
definitions and rules are grouped into automatic **Definitions** and
**Provisions** sections — but explicit `§`/`§§` markers let you name and order
the parts the way a reader of the contract or statute would expect. Imported
modules that carry their own section titles keep them, rendering under their own
heading rather than a generic one.

---

## Lever 3 — `@nlg`: author the exact sentence

When structure and naming aren't enough — a recursive helper, domain jargon, or
a formula you'd rather state in words — attach an `@nlg` annotation. It is the
authoritative natural-language form of that definition.

### Where it goes: end of the line

Write `@nlg` at the **end of the construct's line**, trailing the signature, with
the body on the next line:

```l4
GIVEN x IS A NUMBER, y IS A NUMBER
GIVETH A NUMBER
`the greater of` x y @nlg the greater of %x% and %y%
  MEANS IF x >= y THEN x ELSE y
```

### `%param%` slots

Inside the sentence, `%name%` refers to a parameter. The renderer fills each slot
with:

- the **parameter name** when it shows the definition itself, and
- the **actual argument** at each call site.

So the rule above renders as _"The greater of means the greater of x and y"_ in
its own definition, and a call `` `the greater of` `start date` `end date` ``
renders as _"the greater of the start date and the end date"_.

### It replaces the implementation

A function with an `@nlg` renders **as its sentence**, not as its body. This is
what makes recursive library functions readable — for example `filter` ships
with:

```l4
filter f list @nlg the items of %list% for which %f% holds
  MEANS ...
```

so ``filter `is eligible` applicants`` reads _"the items of applicants for
which is eligible holds"_ instead of exposing the recursion.

### When to reach for it

| Situation                                             | Why `@nlg` helps                                 |
| ----------------------------------------------------- | ------------------------------------------------ |
| Recursive / higher-order helpers                      | Hide the implementation behind a description     |
| Math you'd rather phrase in words                     | "the pro-rated premium" instead of the formula   |
| Domain terms of art                                   | Match the exact statutory or contractual wording |
| A name that can't be both valid code _and_ good prose | Decouple the two                                 |

### Tips

- Keep slots to the function's own parameters; the sentence should make sense
  with each slot read as a noun phrase.
- Prefer **naming and shape first**, `@nlg` second — an `@nlg` is a maintenance
  cost (it can drift from the logic), so reserve it for where it earns its keep.
- One sentence per definition. If you need branching prose, let the structure
  (`CONSIDER`/`IF`) render and annotate the leaves.

---

## Putting it together

Before — terse names, no annotations:

```l4
GIVEN p IS A NUMBER, r IS A NUMBER, n IS A NUMBER
GIVETH A NUMBER
pmt p r n MEANS p TIMES r DIVIDED BY (1 MINUS (1 PLUS r) EXPONENT (0 MINUS n))
```

renders as a bare formula with opaque single letters.

After — descriptive names plus one `@nlg`:

```l4
GIVEN `the principal` IS A NUMBER
      `the monthly rate` IS A NUMBER
      `the term in months` IS A NUMBER
GIVETH A NUMBER
`the monthly repayment on` `the principal`
    `at` `the monthly rate` `over` `the term in months`
    @nlg the level monthly repayment on %the principal% at %the monthly rate% over %the term in months%
  MEANS ...
```

Now both the definition and every call site read as a sentence a lawyer can check.

---

## Refining further with Legalese AI

The renderer gives you a faithful, deterministic baseline — the same input always
produces the same prose, and it never invents facts. That baseline is the right
foundation, but house style, tone, and jurisdiction conventions are editorial
choices that go beyond what deterministic rules should decide.

That is where **Legalese AI** comes in. Once your rendered output is accurate,
you can apply **drafting policies** — reusable style and language rules such as
"use plain English", "prefer active voice", "expand defined terms on first use",
or a firm's house style — and Legalese AI rewrites the rendered prose to match,
while staying anchored to the deterministic output so the meaning is preserved.

In other words: **names, shape, and `@nlg` get the content right; drafting
policies get the _style_ right.** See
[Composing L4 with AI](../llm-integration/composing-l4-with-ai.md) for how
Legalese AI fits into the authoring loop.
