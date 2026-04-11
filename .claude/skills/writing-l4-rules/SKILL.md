---
name: writing-l4-rules
description: Writes, validates, and deploys L4 — a typed functional language for computational law — encoding contracts, regulations, and policy logic as executable rules with type-checked decisions and formally-modeled obligations. Use when the user asks to formalise legal text, draft rules with deadlines and reparations, mark functions for deployment with `@export`/`@desc`, run `jl4-cli`, or deploy to `jl4-service`/Legalese Cloud.
---

# Writing L4 Rules

L4 is a statically-typed, pure-functional language for computational law. It is layout-sensitive like Python, has Haskell-style algebraic data types, and adds legal-drafting affordances: backtick identifiers that read like prose, regulative rules (`PARTY … MUST … WITHIN … HENCE … LEST …`), and `@export`/`@desc` annotations that publish typed decision functions to a REST/MCP API.

**Canonical documentation** — always authoritative for the currently-published L4:

<https://legalese.com/l4/README.md>

This file is a compact operational guide. For anything syntactic you do not remember, link through to the corresponding page on `legalese.com/l4/...` rather than guessing. Three deeper references ship in this skill:

- [references/regulative.md](references/regulative.md) — deep dive on obligations, `#TRACE`, and the `MUST`/`MAY`/`SHANT`/`DO` `HENCE` `LEST` `BREACH` machinery
- [references/builtins.md](references/builtins.md) — coercions, HTTP/JSON, temporal globals, and the library index
- [references/gotchas.md](references/gotchas.md) — traps a general-purpose LLM will not get right (ditto `^`, asyndetic `...`/`..`, `§` sections, computed fields, `IS` vs `MEANS` vs `IF`, mixfix)

---

## When to use L4

Reach for this skill when the user wants to:

1. **Formalise legal text** (legislation, contracts, policies, regulations) as executable rules
2. **Encode decision logic** that must be auditable and type-checked, not hand-waved
3. **Model obligations with deadlines** (pay within 30 days, deliver before X, file by Y) — this is L4's unique strength over general-purpose languages
4. **Deploy rules as an API** via `jl4-service` or [Legalese Cloud](https://legalese.cloud), including as MCP tools for other AI agents
5. **Validate** an existing `.l4` file with `jl4-cli`

L4 is the wrong tool for imperative scripting, UI code, numerical computing, or anything that requires mutation. If the task does not involve legal semantics or auditable decisions, reach for something else.

---

## Core workflow

### 1. Analyse the source

When given a PDF, URL, or natural-language description:

- **Ontology** — what entities, statuses, categories exist? These are often unstated; infer them.
- **Decisions** — what are the boolean or numeric outcomes the rule produces?
- **Obligations** — who must do what, by when, with what consequence on breach?

### 2. Model the domain with `DECLARE`

```l4
-- Enum (sum type)
DECLARE RiskCategory IS ONE OF LowRisk, MediumRisk, HighRisk, Uninsurable

-- Enum with per-constructor fields
DECLARE Shape IS ONE OF
    Circle    HAS radius IS A NUMBER
    Rectangle HAS width  IS A NUMBER
                  height IS A NUMBER

-- Record (product type)
DECLARE Driver HAS
    `name`           IS A STRING
    `age`            IS A NUMBER
    `years licensed` IS A NUMBER
    `accident count` IS A NUMBER
    `has tickets`    IS A BOOLEAN
```

Records can declare **computed fields** (derived attributes) with `MEANS`; see [references/gotchas.md](references/gotchas.md) and <https://legalese.com/l4/reference/types/DECLARE.md>.

### 3. Write decisions

L4 has three function-definition forms. Use whichever reads most like the source text.

```l4
-- General form: DECIDE … IS / … MEANS
GIVEN driver IS A Driver
GIVETH A RiskCategory
DECIDE `assess risk` driver IS
    CONSIDER driver's `accident count`
    WHEN 0 THEN IF driver's `has tickets`
                THEN MediumRisk
                ELSE LowRisk
    WHEN 1 THEN MediumRisk
    WHEN 2 THEN HighRisk
    OTHERWISE  Uninsurable

-- Boolean-returning shortcut: DECIDE … IF
GIVEN driver IS A Driver
GIVETH A BOOLEAN
DECIDE `meets minimum age` IF
    driver's `age` AT LEAST 18

-- Plain MEANS
GIVEN n IS A NUMBER
`square of` n MEANS n TIMES n
```

**Key idioms:**

- `IF … THEN … ELSE` idioms must always be indented in stair-stepping fashion. `BRANCH IF … OTHERWISE` is the flat multi-way-if form. Note that `OTHERWISE` must match `IF` intendation, not `BRANCH`.
- `CONSIDER … WHEN … OTHERWISE …` is the pattern-match form.
- `WHERE` introduces local helpers using `… MEANS`, `DECIDE … IS`, `DECIDE … IF`. `LET x MEANS … IN …` introduces a single local binding.
- `YIELD` makes lambdas: `GIVEN n YIELD n GREATER THAN 0`.
- Backtick identifiers can contain spaces and punctuation (`` `the applicant qualifies` ``); use them to make rules read like legal prose.
- Mixfix lets a function's name intersperse with its arguments: `` `employee` `works for` `employer` ``.
- Field access uses the genitive `'s`: `person's age`, `application's employee's nationality`. Note that function arguments bind stronger than genitive. `f r's foo` parses as `(f r)'s foo`, not `f (r's foo)`.

### 4. Structure like the source

**Isomorphic encoding** — match the logical shape of the source text. If legislation has sections numbered 1.1 / 1.2 / 1.3 with three clauses joined by AND, your L4 should have three conjuncts in the same order. This keeps the rule auditable against the statute.

```l4
§ `Part I — Eligibility`
§§ `1.2 Conditions for coverage`

GIVEN damage IS A Damage
GIVETH A BOOLEAN
DECIDE `coverage applies` IF
        NOT `caused by excluded pests` damage
    AND (   `bird damage to contents`         damage
         OR `animal-caused water escape`      damage)
```

`§`, `§§`, etc. mark sections — they are structural, not comments. See [references/gotchas.md](references/gotchas.md).

### 5. Model obligations and deadlines

When the source text says "must", "may", "shall not", or "within X days", use L4's regulative rules:

```l4
paymentObligation MEANS
    PARTY   `The Borrower`
    MUST    `pay` `outstanding amount` EXACTLY `To Lender`
    WITHIN  30
    HENCE   FULFILLED
    LEST    BREACH BY `The Borrower` BECAUSE "payment deadline exceeded"
```

Full treatment — `MUST`/`MAY`/`SHANT`/`DO`, `HENCE`/`LEST` semantics (note: `SHANT` flips polarity), `BREACH BY … BECAUSE …`, `RAND`/`ROR` composition, `PROVIDED` guards, `EXACTLY` matching, recursive obligations, and `#TRACE` simulation — is in [references/regulative.md](references/regulative.md).

### 6. Validate with `jl4-cli`

```bash
# Preferred
cabal run jl4-cli -- path/to/file.l4

# Or if jl4-cli is on PATH
jl4-cli path/to/file.l4

# Pin "now" for reproducible evaluation
jl4-cli --fixed-now=2025-01-01T00:00:00Z path/to/file.l4
```

A wrapper is provided at [scripts/validate.sh](scripts/validate.sh). Type errors are reported with line numbers — iterate until the check passes.

### 7. Test with `#EVAL`, `#ASSERT`, `#TRACE`

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

Available directives: `#EVAL`, `#EVALTRACE`, `#TRACE`, `#CHECK`, `#ASSERT`.

### 8. Deploy

See the **Deployment** section below. In short: add `@export` above the functions that should become API endpoints, add `@desc` to their parameters. Exported functions should answer the highest utility questions a reader of the rules might want to answer. Often the rule definition is not written as such and a separate file importing the rules needs to decorate those export functions.

---

## Deployment with `jl4-service`

`jl4-service` turns L4 rule bundles into live, multi-tenant REST APIs. The same annotated source is automatically exposed as:

- **REST** — `POST /deployments/{id}/functions/{fn}/evaluation`
- **Batch** — `/functions/{fn}/evaluation/batch` (parallel case evaluation)
- **Query planning** — `/functions/{fn}/query-plan` for interactive questionnaires that only ask the inputs that still matter
- **OpenAPI 3.0** — `/deployments/{id}/openapi.json`
- **MCP JSON-RPC 2.0** — `POST /deployments/{id}/.mcp` for LLM tool-use clients
- **WebMCP** — `<script src="/.webmcp/embed.js">` for browser AI agents
- **Traces** — `?trace=full&graphviz=true` on any evaluation

**Self-hosted or managed.** You can run `jl4-service` yourself (`cabal run jl4-service`), or use the managed [Legalese Cloud](https://legalese.cloud) offering, which gives each org a subdomain (`https://{org-slug}.legalese.cloud`), handled compilation, OAuth protection, and one-click deployment from the VS Code extension's Deploy tab. Both expose the same API shape, so annotations and client code are portable.

### `@export` — publish a function

Only functions marked `@export` are visible to the service. Place it directly above the `GIVEN`/function definition — **not** between `GIVETH` and `DECIDE`:

| Form                            | Effect                                                 |
| ------------------------------- | ------------------------------------------------------ |
| `@export <description>`         | Export this function with a human-readable description |
| `@export default <description>` | Export as the bundle's **default** function            |
| `@desc <description>`           | Internal description only — does **not** export        |

The description is the single highest-value sentence in the whole file: it is what an LLM agent sees in its tool list when deciding whether to call this rule. Write it as if the agent has no other context.

```l4
-- ✘ Vague — agent cannot tell when to call this
@export do the calculation

-- ✘ Implementation detail leaking out
@export Apply the branching logic defined in §3.2

-- ✔ Clear domain intent
@export Calculate the annual income tax owed by an individual resident taxpayer
```

### `@desc` — document parameters

Put an inline `@desc` on **every** `GIVEN` parameter an API caller has to supply. These descriptions flow into the OpenAPI parameter docs and the MCP tool's `inputSchema`, and they are what LLMs read when deciding **how to construct a valid call**.

```l4
@export Calculate the cost of parking for a given day
GIVEN
  day_of_week       IS A NUMBER  @desc Day of the week (1 = Monday, 2 = Tuesday, ..., 7 = Sunday)
  is_public_holiday IS A BOOLEAN @desc Whether the day is a gazetted public holiday
  current_weather   IS A STRING  @desc Current weather conditions. One of: "fair", "rain", "snow"
GIVETH A NUMBER
DECIDE parking_cost IS ...
```

A full working example is at [assets/example-parking.l4](assets/example-parking.l4).

### Writing annotations AI agents can actually use

Because exported metadata is what a downstream LLM sees in its tool-use context:

1. **Enumerate allowed values inline.** If a `STRING` accepts a fixed set, list them in the `@desc`. The type `STRING` is opaque to the schema generator — the LLM only knows what you tell it.
2. **State units and ranges.** `@desc Amount in USD cents` beats `amount`. Same for dates (`ISO 8601, e.g. 2025-03-15`) and durations (`number of days`).
3. **Explain semantics, not syntax.** The JSON type is already in the schema. Use `@desc` for what the number _means_.
4. **`@export` answers "is this the tool I want?"; `@desc` answers "what do I put here?"**
5. **Avoid internal jargon.** The agent has no access to your team glossary.
6. **One sentence per parameter.** Long enough to disambiguate, short enough to fit a crowded tool list.

### Deployment workflow

1. **Annotate** — add `@export` and parameter `@desc`s.
2. **Validate** locally with `jl4-cli`.
3. **Bundle** — zip the `.l4` files.
4. **Deploy** via the VS Code Deploy tab, or:
   ```bash
   curl -X POST http://localhost:8080/deployments \
     -F "id=my-rules" \
     -F "sources=@/tmp/bundle.zip"
   ```
5. **Verify** — `GET /deployments/{id}/openapi.json` to confirm the exported surface.
6. **Call**:
   ```bash
   curl -X POST http://localhost:8080/deployments/my-rules/functions/parking_cost/evaluation \
     -H "Content-Type: application/json" \
     -d '{"arguments": {"day_of_week": 6, "is_public_holiday": false, "current_weather": "fair"}}'
   ```

### Name sanitization

L4 identifiers with spaces (`` `calculate premium` ``) are automatically hyphenated for JSON/URL use (`calculate-premium`). The REST API accepts both the spaced and hyphenated forms. If two L4 names would collide after sanitization (e.g. `` `foo bar` `` and `` `foo-bar` ``), compilation fails with an explicit error.

### Legalese Cloud specifics

When the agent is pointed at a `.legalese.cloud` host, endpoints are OAuth-protected. Discovery starts at:

```
https://{org-slug}.legalese.cloud/.well-known/oauth-protected-resource
```

Fetch this first to learn which authorization server issues tokens and what scopes are required (the same `resource_metadata` link appears in `WWW-Authenticate: Bearer …` challenges). Then pass `Authorization: Bearer <token>` on REST, MCP, and WebMCP requests.

Other useful well-known paths on a Legalese Cloud org:

- `/.well-known/mcp` — MCP server discovery
- `/.well-known/webmcp` — WebMCP discovery manifest
- `/openapi.json` — org-wide OpenAPI 3.0 spec
- `/deployments?functions=full` — cached metadata for all deployments

For the full service reference (CLI flags, resource limits, deontic evaluation shapes), see the `jl4-service` README bundled with the running server.

---

## Syntax anchor

Just enough to write most rules without a round-trip. Anything not here, check <https://legalese.com/l4/reference/GLOSSARY.md>.

### Types

| L4                            | Meaning                                             |
| ----------------------------- | --------------------------------------------------- |
| `NUMBER`                      | Integers and rationals                              |
| `STRING`                      | Text                                                |
| `BOOLEAN`                     | `TRUE` / `FALSE`                                    |
| `DATE` / `TIME` / `DATETIME`  | Calendar date / time-of-day / instant               |
| `LIST OF T`                   | Ordered collection                                  |
| `MAYBE T`                     | Optional (`JUST x` / `NOTHING`)                     |
| `EITHER A B`                  | Choice (`LEFT x` / `RIGHT y`)                       |
| `DECLARE T HAS ...`           | Record                                              |
| `DECLARE T IS ONE OF a, b, c` | Enum (optionally with per-constructor `HAS` fields) |

### Operators

Full table at <https://legalese.com/l4/reference/GLOSSARY.md>. The ones used constantly:

- **Boolean:** `AND`, `OR`, `NOT`, `IMPLIES` (`=>`), `UNLESS` (= `AND NOT`)
- **Comparison:** `EQUALS`, `GREATER THAN` / `ABOVE`, `LESS THAN` / `BELOW`, `AT LEAST` (≥), `AT MOST` (≤)
- **Arithmetic:** `PLUS`, `MINUS`, `TIMES`, `DIVIDED BY`, `MODULO` — or `+`, `-`, `*`, `/`
- **String:** `CONCAT`, `APPEND`
- **List:** `LIST a, b, c`, `EMPTY`, `x FOLLOWED BY xs`

### Control flow

```l4
IF cond THEN a ELSE b

CONSIDER value
WHEN Pat1 THEN r1
WHEN Pat2 WITH field THEN r2
OTHERWISE rDefault

BRANCH IF x EQUALS 1 THEN "one"
       IF ^ EQUALS 2 THEN "two"
       OTHERWISE "other"
```

The caret `^` is the **ditto** operator — "same as the cell above". See [references/gotchas.md](references/gotchas.md).

### Record construction and access

```l4
Person WITH `name` IS "Alice", `age` IS 30
person's `name`
application's employee's nationality   -- chaining
```

### Directives

- `#EVAL expr` — evaluate and print
- `#EVALTRACE expr` — evaluate with execution trace
- `#CHECK expr` — type-check without evaluating
- `#ASSERT bool_expr` — assert must be TRUE
- `#TRACE contract AT time WITH ...` — simulate a regulative rule; see [references/regulative.md](references/regulative.md)

### Annotations

- `@desc` — human-readable description (internal unless paired with `@export`)
- `@export` — mark a function for deployment
- `@nlg` — natural-language generation hint
- `@ref`, `@ref-src`, `@ref-map` — cross-reference to a legal source

### Imports

Imports should be the first lines in a file before anything else.

```l4
IMPORT prelude
IMPORT daydate
```

The prelude is always available. For the full library list (`prelude`, `daydate`, `time`, `datetime`, `timezone`, `math`, `currency`, `legal-persons`, `jurisdiction`, `actus`, `llm`, `excel-date`, `holdings`, `date-compat`), see [references/builtins.md](references/builtins.md) or <https://legalese.com/l4/reference/libraries.md>.

---

## Writing for legal audiences

L4's target users are policy writers and legal authors, not programmers. Write rules that read like prose and make generous use of the tick marked identifiers containing full phrases

```l4
-- ✔ Reads like legal text
GIVEN person IS A Person
GIVETH A BOOLEAN
DECIDE `the person is eligible for benefits` IF
        `the person is a citizen`
    AND `the person has resided for at least 5 years`
    AND NOT `the person has been disqualified`

-- ✘ Reads like programmer code
GIVEN p IS A Person
GIVETH A BOOLEAN
isEligible p MEANS p's citizen && p's years >= 5 && !p's disqualified
```

**Use backtick identifiers liberally.** `` `the applicant` `` not `applicant`. `` `has valid identification` `` not `hasValidID`.

---

## Troubleshooting

- **Parse error: unexpected token** — L4 is layout-sensitive. Check indentation.
- **Pattern match not exhaustive** — add `OTHERWISE` or handle every enum constructor.
- **Not in scope** — define the function before use, or add the needed `IMPORT`.
- **Type mismatch** — use the explicit coercions (`TOSTRING`, `TONUMBER`, `TODATE`, …); L4 does no implicit coercion. See [references/builtins.md](references/builtins.md).
- **`#TRACE` returns a residual obligation instead of `FULFILLED`** — the trace ended in a state with open obligations. Read the residual: it tells you exactly what's still owed and by whom.

For compiler-error recipes, see <https://legalese.com/l4/reference/errors.md>.

---

## Further reading

All documentation for the currently-published L4 release lives under `https://legalese.com/l4/...`:

- **Start here:** <https://legalese.com/l4/README.md>
- **Glossary of every keyword, operator, type:** <https://legalese.com/l4/reference/GLOSSARY.md>
- **Cheat sheet (translation from other languages):** <https://legalese.com/l4/reference/cheat-sheet.md>
- **Regulative rules:** <https://legalese.com/l4/reference/regulative.md>
- **Libraries:** <https://legalese.com/l4/reference/libraries.md>
- **Tutorials — first L4 file:** <https://legalese.com/l4/tutorials/getting-started/first-l4-file.md>
- **Tutorials — common patterns:** <https://legalese.com/l4/tutorials/getting-started/common-patterns.md>
- **Tutorials — deploying functions:** <https://legalese.com/l4/tutorials/deploying-functions/exporting-functions-for-deployment.md>
- **Concepts — regulative rules:** <https://legalese.com/l4/concepts/legal-modeling/regulative-rules.md>
- **Foundation course (Module 5 — regulative rules):** <https://legalese.com/l4/courses/foundation/module-5-regulative.md>

The website tracks the currently-published L4 version; treat it as ground truth over any snippet in this skill.
