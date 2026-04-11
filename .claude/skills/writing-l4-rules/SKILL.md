---
name: writing-l4-rules
description: Writes, validates, and deploys L4 — a typed functional language for computational law — encoding contracts, regulations, and policy logic as executable rules with type-checked decisions and formally-modeled obligations. Use when the user asks to formalise legal text, draft rules with deadlines and reparations, mark functions for deployment with `@export`/`@desc`, validate `.l4` files, or deploy to `jl4-service` / Legalese Cloud.
---

# Writing L4 Rules

L4 is a statically-typed, pure-functional language for computational law. It looks and reads like legal prose — backtick identifiers hold whole phrases, regulative rules model deadlines and breaches, and every `@export`ed function becomes a REST / MCP tool. It is layout-sensitive like Python and has Haskell-style algebraic data types.

Use this skill when the user wants to **formalise legal text** (legislation, contracts, policies), **encode auditable decision logic**, **model obligations with deadlines** (L4's unique strength), or **deploy rules as a live API or MCP**.

Authoritative docs, always the source of truth for syntax: <https://legalese.com/l4/README.md>. When in doubt about a keyword, link through rather than guess.

---

## Read before you write your first line

**Open [references/traps.md](references/traps.md) first.** It lists twelve high-frequency traps that every LLM (including Claude) hits on first try — flat `ELSE IF` cascades that don't parse, silently-ignored imports, missing record-update syntax, confusing `inferred type HNN` errors for undeclared parties, and more. Each entry has the exact error message, the broken code, and the fix. The file also contains an error-message decoder ring and a pre-flight checklist.

Three other references ship alongside:

- [references/regulative.md](references/regulative.md) — obligations, `HENCE`/`LEST`, `BREACH BECAUSE`, `#TRACE` simulation, recursive payment schedules.
- [references/builtins.md](references/builtins.md) — type coercions (`TONUMBER`, `TODATE`, …), HTTP/JSON built-ins, temporal globals, library index.
- [references/gotchas.md](references/gotchas.md) — syntactic curios worth knowing: ditto `^`, asyndetic `...`/`..`, `§` section markers, computed fields, mixfix notation, the `IS` vs `MEANS` vs `IF` distinction.

Pull these in on demand — don't preload them.

---

## The writing workflow

### Step 1 — read the source with three questions

- **Ontology:** what entities, statuses, categories does the text refer to (even implicitly)? These become `DECLARE`d types.
- **Decisions:** what boolean or numeric outcomes does the rule produce? These become exported functions.
- **Obligations:** who must do what, by when, with what consequence on breach? These become regulative rules.

### Step 2 — model the domain with `DECLARE`

Use sum types for things that are "one of these", records for things that are "a bundle of facts":

```l4
DECLARE RiskCategory IS ONE OF LowRisk, MediumRisk, HighRisk, Uninsurable

DECLARE Driver HAS
    `name`           IS A STRING
    `age`            IS A NUMBER
    `years licensed` IS A NUMBER
    `accident count` IS A NUMBER
    `has tickets`    IS A BOOLEAN
```

When a decision depends on many related facts (applicants, claims, orders, taxpayers), **wrap them in one record and have the top-level decision take a single parameter**. This keeps your exported surface clean even when the domain has 15+ fields, and it matches how legal inputs actually arrive — as a complete fact pattern, not a flat argument list.

Mutually-exclusive options (conviction categories, dwelling types, plan status) should be enums, not booleans — you get exhaustiveness checking and the types document the source text.

Records can declare **computed fields** derived from sibling fields; see [references/gotchas.md](references/gotchas.md).

### Step 3 — write decisions that read like the source

Isomorphic encoding: match the section structure of the source with `§` and `§§` markers, and mirror its conjunctions / disjunctions in the order they appear:

```l4
§ `Part I — Eligibility`
§§ `1.2 Conditions for coverage`

GIVEN damage IS A Damage
GIVETH A BOOLEAN
DECIDE `coverage applies` IF
        NOT `caused by excluded pests` damage
    AND (   `bird damage to contents`    damage
         OR `animal-caused water escape` damage)
```

L4 has three function forms; pick the one that reads closest to the source:

- `DECIDE foo IS <value-expr>` — for numeric and record results.
- `DECIDE foo IF <boolean-expr>` — for predicates.
- `foo MEANS <expr>` — for plain value bindings, no `DECIDE` needed.

Control flow, in the order you should reach for them:

1. **Enum pattern-match** (exhaustive, cleanest lookup tables):

   ```l4
   DECIDE `rating factor` vc IS
       CONSIDER vc
       WHEN `Private Small`      THEN 0.028
       WHEN `Private Medium`     THEN 0.034
       WHEN `Performance Sports` THEN 0.068
       -- ...
   ```

2. **Flat multi-branch grid** — use `BRANCH IF`, which is the clean flat form. Subsequent `IF`s and `OTHERWISE` align with the **first** `IF` after `BRANCH`, not with `BRANCH` itself.

   ```l4
   DECIDE `parking cost` IS
       BRANCH IF is_holiday        THEN 0
              IF is_rotten_weather THEN 2
              IF day_of_week LESS THAN 6 THEN 5
              OTHERWISE                        4
   ```

3. **Two-way:** `IF cond THEN a ELSE b`.
4. **Local helpers:** `WHERE` (trailing) for multiple bindings; `LET x MEANS e IN body` for one inline binding. **Don't chain `LET ... IN`** — use `WHERE`.

**Do not reach for nested `ELSE IF`** — L4's layout rules fight you on flat cascades. `BRANCH` is the right tool. (This is the #1 authoring trap; see [traps.md §1](references/traps.md).)

Backtick any identifier that contains a space or punctuation, including parameter names: `` `days elapsed` IS A NUMBER ``. When you pass a genitive sub-expression to a function, parenthesise it: ``f (r's `employee`)``. These two habits prevent 80% of parse errors ([traps.md §3–§4](references/traps.md)).

### Step 4 — model obligations if the text has deadlines

When the source says "must", "may", "shall not", or "within X days", use regulative rules. The five-keyword skeleton:

```l4
DECLARE `Sale Party` IS ONE OF `The Seller`, `The Buyer`

`delivery obligation` MEANS
    PARTY  `The Seller`
    MUST   `deliver goods`
    WITHIN 30
    HENCE  FULFILLED
    LEST   BREACH BY `The Seller`
           BECAUSE "delivery not made within 30 days of the contracted date"
```

**Declare the parties first.** Every identifier after `PARTY` or in a `BREACH BY` must be a value of a declared type — either a local `DECLARE ... IS ONE OF ...` or an import from `legal-persons`. Missing the declaration produces a cryptic `inferred type HNN` error.

For `MUST` / `MAY` / `SHANT` / `DO` semantics, `RAND` / `ROR` composition, `PROVIDED` / `EXACTLY` matching, recursive obligations, and `#TRACE` simulation, load [references/regulative.md](references/regulative.md).

Regulative rules are traced and simulated, not called over REST. **Do not `@export` them** — the thing a REST client wants is the decision function that says whether the obligation was satisfied.

### Step 5 — test inline with `#ASSERT`, `#EVAL`, `#TRACE`

L4's directive system is its built-in test harness. Run tests against fixture values in the rule file itself, or in a companion `_tests.l4` file. Both styles produce `assertion satisfied` / `assertion failed` diagnostics readable through the validation paths in Step 6.

```l4
`Alice` MEANS Driver WITH
    `name`           IS "Alice"
    `age`            IS 25
    `years licensed` IS 7
    `accident count` IS 0
    `has tickets`    IS FALSE

#EVAL   `assess risk` `Alice`
#ASSERT `assess risk` `Alice` EQUALS LowRisk
```

For a companion test file:

```l4
IMPORT prelude
IMPORT `my-rules`            -- same directory, filename without .l4

§ `Tests`
-- ...
```

**Test-file ground rules — these were all learned the hard way:**

1. `IMPORT` must be the first non-comment content in the file. Imports placed after a `§` or comment are **silently ignored**, and every imported name then reports as "not found".
2. Filenames must use dashes or camelCase, not underscores — `` IMPORT `policy_remote_work` `` fails to resolve `policy_remote_work.l4`.
3. L4 has **no record-update syntax**. You cannot write `` `base case` WITH `field` IS newValue `` — only `TypeName WITH ...` (full construction). For test fixtures that vary one field across many cases, either write each one out fully, or build a tiny helper function.
4. `#ASSERT` / `#EVAL` expressions must fit on one line. Pre-bind long fixtures to named values and reference them in the directive.
5. Name each fixture descriptively (`` `salary at exact floor` ``, `` `with prior immigration breach` ``). Failing assertions echo the identifier, making the failure self-documenting.
6. Write tests _alongside_ the rule, not after. When tests drive development they catch real bugs — the English source is often internally inconsistent, and that tension only surfaces when a concrete fact pattern forces one interpretation over another.

All these points are expanded in [traps.md §7–§12](references/traps.md).

### Step 6 — validate

When Claude runs inside the VS Code extension (the default), `jl4-lsp` recompiles every `.l4` file you touch and the diagnostics come back automatically in the `<ide_diagnostics>` block of the tool hook output. Just edit the file and read the hook output. This covers type errors, parse errors, and every `#ASSERT` / `#EVAL` result in the file you edited. **Iterate by editing, not by running commands.**

Two cases where you need more than passive diagnostics:

- **Validating a file you are not about to edit** — e.g. checking that a test file still passes after you edited the rule it imports. Passive diagnostics only fire on the edited file. Touch the test file or call the stdio driver.
- **Running outside the VS Code extension** — Agent SDK, CI, sandbox. No IDE hook is available.

For those, run the stdio LSP driver shipped with this skill:

```bash
node /Users/tgorissen/.claude/skills/writing-l4-rules/scripts/validate-lsp.mjs path/to/file.l4 [more.l4 ...]
```

It spawns `jl4-lsp` (must be on PATH, which the VS Code extension ensures), opens each file as an LSP document, waits for `publishDiagnostics`, prints every diagnostic with line numbers, and exits non-zero on errors. It is a ~150-line Node script — [scripts/validate-lsp.mjs](scripts/validate-lsp.mjs) — intended for batch use; you do not need it for ordinary authoring.

If you get a confusing error, **check [traps.md](references/traps.md) first** — its decoder ring maps most common symptoms to causes.

### Step 7 — export for deployment

Only functions marked `@export` are visible to `jl4-service`. Place `@export` directly above the function header, not between `GIVETH` and `DECIDE`.

```l4
@export Calculate the annual auto insurance premium under the Comprehensive Motor Policy, including base premium, no-claims discount, young-driver and high-risk-area loadings, anti-theft and multi-vehicle discounts, §6 floor and cap, and 12% Insurance Premium Tax.
GIVEN p IS A `Policy Inputs`
  @desc All rating facts. `sum insured` is the agreed market value in policy currency; `age of youngest driver` in years; `vehicles in account` is the count of policies on the same account (1 means no multi-vehicle discount).
GIVETH A NUMBER
DECIDE `calculate annual premium` p IS ...
```

The `@export` description is the single highest-value sentence in the file — it is what a downstream LLM agent sees when deciding whether to call this rule. Write it for an agent with no other context:

- ✘ `@export do the calculation`
- ✘ `@export Apply the branching logic defined in §3.2`
- ✔ `@export Calculate the annual income tax owed by an individual resident taxpayer`

Put an inline `@desc` on **every** `GIVEN` parameter. These flow into OpenAPI schemas and MCP `inputSchema` — they are what LLMs read to decide how to construct a valid call. Enumerate allowed string values inline (the type system sees `STRING` as opaque), state units and ranges, explain meaning not shape.

**Pick 2–4 exports per file, not one giant entry point.** A good pattern:

- The headline yes/no (`applicant qualifies`, `clause 6 covenants met`).
- The numeric quantity that explains it (`score applicant`, `clause 5 liquidated damages`, `section 15 max uplift`).
- A sub-decision that callers use as a pre-screen before building the full fact record (`employee is eligible for remote work`).
- Individual component calculations useful for UI breakdown (`calculate base premium`, `no claims discount rate`).

The worked example at [assets/example-parking.l4](assets/example-parking.l4) shows a complete `@export` + `@desc` pattern end-to-end.

---

## Writing for legal audiences

L4's users are policy writers and legal authors, not programmers. Rules should read like the underlying text.

```l4
-- ✔ Reads like legal prose
GIVEN person IS A Person
GIVETH A BOOLEAN
DECIDE `the person is eligible for benefits` IF
        `the person is a citizen`
    AND `the person has resided for at least 5 years`
    AND NOT `the person has been disqualified`

-- ✘ Reads like code
isEligible p MEANS p's citizen && p's years >= 5 && !p's disqualified
```

Use **mixfix notation** for binary-ish relations so the call site reads as prose:

```l4
GIVEN employee IS AN Employee
      employer IS A Company
GIVETH A BOOLEAN
`employee` `works for` `employer` MEANS ...

-- Called as: `Alice` `works for` `Acme Corp`
```

Backtick identifiers are the single biggest lever for readability — use them liberally in function names, parameter names, field names, and enum constructors. A rule file where only the keywords are in English is code; one where the vocabulary is in English is law.

---

## Deploying with `jl4-service`

`jl4-service` turns an annotated `.l4` bundle into a multi-endpoint service: REST, batch, query-planning, OpenAPI 3.0, MCP JSON-RPC, WebMCP, and trace visualisation. Self-host via `cabal run jl4-service`, or use the managed [Legalese Cloud](https://legalese.cloud) — both expose the same API surface and accept the same `@export`-annotated source.

### Workflow

1. **Annotate** — `@export` on every publishable function, inline `@desc` on every `GIVEN` parameter.
2. **Validate** locally via Step 6 until all diagnostics are clean.
3. **Bundle** — zip the `.l4` files.
4. **Deploy** via the VS Code extension's Deploy tab, or:

   ```bash
   curl -X POST http://localhost:8080/deployments \
     -F "id=my-rules" \
     -F "sources=@/tmp/bundle.zip"
   ```

5. **Verify** — `GET /deployments/{id}/openapi.json` to inspect the exported surface.
6. **Call**:

   ```bash
   curl -X POST http://localhost:8080/deployments/my-rules/functions/calculate-annual-premium/evaluation \
     -H "Content-Type: application/json" \
     -d '{"arguments": {...}}'
   ```

### Name sanitization

L4 identifiers with spaces are auto-hyphenated for URL / JSON use — `` `calculate annual premium` `` → `calculate-annual-premium`. The REST API accepts either form. Collisions between a spaced name and a dashed name (`` `foo bar` `` and `` `foo-bar` ``) fail compilation explicitly.

### Legalese Cloud

Endpoints on a `.legalese.cloud` host are OAuth-protected. Discovery starts at:

```
https://{org-slug}.legalese.cloud/.well-known/oauth-protected-resource
```

Fetch this first to learn the authorization server and required scopes, then pass `Authorization: Bearer <token>` on REST, MCP, and WebMCP requests. Other useful well-known paths:

- `/.well-known/mcp` — MCP server discovery
- `/.well-known/webmcp` — WebMCP discovery manifest
- `/openapi.json` — org-wide OpenAPI 3.0 spec
- `/deployments?functions=full` — cached metadata for all deployments

---

## Quick syntax reminders

Full glossary: <https://legalese.com/l4/reference/GLOSSARY.md>. Load [references/builtins.md](references/builtins.md) for coercions, HTTP/JSON, and libraries.

**Types:** `NUMBER`, `STRING`, `BOOLEAN`, `DATE` / `TIME` / `DATETIME`, `LIST OF T`, `MAYBE T` (`JUST x` / `NOTHING`), `EITHER A B` (`LEFT x` / `RIGHT y`), `DECLARE T HAS ...`, `DECLARE T IS ONE OF a, b, c`.

**Operators:** `AND` `OR` `NOT` `IMPLIES` `UNLESS` · `EQUALS` `GREATER THAN` `LESS THAN` `AT LEAST` `AT MOST` · `PLUS` `MINUS` `TIMES` `DIVIDED BY` `MODULO` · `CONCAT` `APPEND` · `LIST a, b, c` `EMPTY` `x FOLLOWED BY xs`.

**Records:** `Person WITH \`name\` IS "Alice", \`age\` IS 30`to construct,`person's \`name\``to access. Chain with`application's employee's nationality`. Always parenthesise a genitive sub-expression passed to a function: `f (r's field)`.

**Directives:** `#EVAL`, `#EVALTRACE`, `#TRACE`, `#CHECK`, `#ASSERT`.

**Annotations:** `@desc` (parameter or internal), `@export` (publish for deployment), `@export default` (bundle default), `@nlg` (NLG hint), `@ref` / `@ref-src` / `@ref-map` (legal citations).

**Imports:** must be at the top of the file, before any `§` or content. Filenames must use dashes or camelCase. `IMPORT prelude` is implicit but harmless to be explicit. Library list in [references/builtins.md](references/builtins.md).

---

## Further reading

All authoritative docs live under `https://legalese.com/l4/...`:

- **README and conceptual overview:** <https://legalese.com/l4/README.md>
- **Full glossary:** <https://legalese.com/l4/reference/GLOSSARY.md>
- **Cheat sheet (from other languages):** <https://legalese.com/l4/reference/cheat-sheet.md>
- **Regulative rules reference:** <https://legalese.com/l4/reference/regulative.md>
- **Library catalogue:** <https://legalese.com/l4/reference/libraries.md>
- **Tutorials:** <https://legalese.com/l4/tutorials/getting-started/first-l4-file.md>, <https://legalese.com/l4/tutorials/getting-started/common-patterns.md>, <https://legalese.com/l4/tutorials/deploying-functions/exporting-functions-for-deployment.md>
- **Foundation course — regulative rules module:** <https://legalese.com/l4/courses/foundation/module-5-regulative.md>

Treat the website as ground truth over anything in this skill — it tracks the currently-published L4 release.
