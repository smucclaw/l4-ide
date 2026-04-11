# L4 Traps — Things That Will Waste 30+ Minutes If You Don't Know

Read this once before writing your first L4 file. All entries here were
learned empirically by converting nine English rule documents and building a
challenge test suite; each is a trap that a general-purpose LLM (including
Claude) got wrong on first attempt.

If you are iterating and hit a confusing compiler error, scan this file —
most likely your symptom is listed.

---

## Contents

- [The six high-frequency authoring traps](#the-six-high-frequency-authoring-traps)
- [The six test-file-specific traps](#the-six-test-file-specific-traps)
- [Error-message decoder ring](#error-message-decoder-ring)
- [Pre-flight checklist](#pre-flight-checklist)

---

## The six high-frequency authoring traps

### 1. Flat `ELSE IF` cascades do not parse; use `BRANCH` for flat grids

**Broken:**

```l4
DECIDE `young driver loading` age IS
    IF      age EQUALS 17                       THEN 0.75
    ELSE IF age AT LEAST 18 AND age AT MOST 19  THEN 0.60
    ELSE IF age AT LEAST 20 AND age AT MOST 21  THEN 0.40
    ELSE 0
```

Error: `incorrect indentation (got N, should be greater than N)` on the second `ELSE IF`.

**Fix A — staircase** (works, ugly for >3 branches):

```l4
DECIDE `young driver loading` age IS
    IF age EQUALS 17
    THEN 0.75
    ELSE IF age AT LEAST 18 AND age AT MOST 19
         THEN 0.60
         ELSE IF age AT LEAST 20 AND age AT MOST 21
              THEN 0.40
              ELSE 0
```

**Fix B — `BRANCH`** (the real answer for any flat multi-branch decision):

```l4
DECIDE `young driver loading` age IS
    BRANCH IF age EQUALS 17                      THEN 0.75
           IF age AT LEAST 18 AND age AT MOST 19 THEN 0.60
           IF age AT LEAST 20 AND age AT MOST 21 THEN 0.40
           OTHERWISE                                  0
```

Note: each subsequent `IF` and `OTHERWISE` aligns with the _first_ `IF` after
`BRANCH`, not with `BRANCH` itself.

**Rule of thumb:** any decision with three or more peer branches → `BRANCH`.
Two branches → plain `IF ... THEN ... ELSE ...`. Enum pattern match →
`CONSIDER ... WHEN`.

---

### 2. Chained `LET ... IN` with ≥2 bindings does not parse; use `WHERE`

**Broken:**

```l4
DECIDE `max uplift` p IS
    LET `cpi cap pct` MEANS p's `cpi` PLUS 5 IN
    LET `cpi cap`     MEANS p's `current rent` TIMES `cpi cap pct` DIVIDED BY 100 IN
    LET `ten pct cap` MEANS p's `current rent` DIVIDED BY 10 IN
    IF `cpi cap` LESS THAN `ten pct cap` THEN `cpi cap` ELSE `ten pct cap`
```

Error: `incorrect indentation (got 5, should be greater than 5)` on the second `LET`.

**Fix — WHERE block:**

```l4
DECIDE `max uplift` p IS
    IF `cpi cap` LESS THAN `ten pct cap` THEN `cpi cap` ELSE `ten pct cap`
    WHERE
        `cpi cap pct` MEANS p's `cpi` PLUS 5
        `cpi cap`     MEANS p's `current rent` TIMES `cpi cap pct` DIVIDED BY 100
        `ten pct cap` MEANS p's `current rent` DIVIDED BY 10
```

**Rule of thumb:** single local binding → `LET x MEANS e IN body`. Two or
more local bindings → trailing `WHERE` with one binding per line.

---

### 3. Function application binds tighter than the genitive `'s`

**Broken:**

```l4
DECIDE `request is approvable` r IF
        `employee is remote-eligible` r's `employee`
    AND `request is procedurally complete` r
```

Parses as `(\`employee is remote-eligible\` r)'s \`employee\``. Error:
`trying to apply r (of type Remote Request) to 1 argument`.

**Fix — parenthesise the genitive sub-expression:**

```l4
DECIDE `request is approvable` r IF
        `employee is remote-eligible` (r's `employee`)
    AND `request is procedurally complete` r
```

**Rule of thumb:** any time a genitive `x's field` appears as a function
argument, wrap it in parentheses. Belt-and-braces; never wrong.

---

### 4. Multi-word `GIVEN` parameters need backticks

**Broken:**

```l4
GIVEN status        IS An `Order Status`
      days elapsed  IS A NUMBER
```

Error: `unexpected IS / expecting AKA, MEANS, OF, identifier, or space token`.

`days` is being consumed as the parameter name, then `elapsed` is parsed as
its type, which collides with `IS`.

**Fix:**

```l4
GIVEN status          IS An `Order Status`
      `days elapsed`  IS A NUMBER
```

**Rule of thumb:** any parameter name with a space needs backticks. Same rule
as backticked function names — L4 identifier tokenisation treats spaces as a
break, and backticks are the opt-in for multi-word atoms.

---

### 5. `WHERE`-bound helpers must bind with `MEANS`, not `IS`

**Broken:**

```l4
DECIDE `required cooling off days` p IS
    ...
    WHERE
        `director officer cooling off` plan IS
            IF ... THEN 90 ELSE 120
```

Error: `unexpected IS / expecting AKA, MEANS, identifier, or space token`.

**Fix — use `MEANS` in WHERE bindings:**

```l4
DECIDE `required cooling off days` p IS
    ...
    WHERE
        `director officer cooling off` plan MEANS
            IF ... THEN 90 ELSE 120
```

At the top level, `DECIDE foo IS ...` and `foo MEANS ...` are interchangeable
for value bindings. Inside a `WHERE` block, only `MEANS` works.

---

### 6. `PARTY X` in a regulative rule requires `X` to be a declared value

**Broken:**

```l4
`delivery obligation` MEANS
    PARTY  `The Seller`       -- never declared
    MUST   `deliver goods`
    WITHIN 30
```

Error: `I could not find a definition for the identifier \`The Seller\`
which I have inferred to be of type: H107` (H107 is a compiler-internal
type variable name — ignore it).

**Fix — declare the parties first:**

```l4
DECLARE `Sale Party` IS ONE OF `The Seller`, `The Buyer`

`delivery obligation` MEANS
    PARTY  `The Seller`
    MUST   `deliver goods`
    WITHIN 30
```

Or: `IMPORT legal-persons` and use its canonical party types.

**Rule of thumb:** every identifier that appears after `PARTY` or in a
`BREACH BY` must resolve to a declared value of a party type.

---

## The six test-file-specific traps

### 7. `IMPORT` must be the first non-comment content in a file

**Broken:**

```l4
§ `Tests`
-- comment

IMPORT prelude
IMPORT `my-rules`
```

Compiler runs without error, but _silently ignores the imports_. Every name
from the imported file is then "not found" when you try to use it.

**Fix:**

```l4
IMPORT prelude
IMPORT `my-rules`

§ `Tests`
-- comment
```

**Diagnostic:** when imports are working, `jl4-lsp` emits
`[Import Resolution] Resolving import: X from ...` logs. No such log line
means the `IMPORT` is not being seen at all.

---

### 8. Filenames with underscores break `IMPORT`

`IMPORT \`policy_remote_work\``does not resolve the file`policy_remote_work.l4`. Rename files to use dashes: `policy-remote-work.l4`and`IMPORT \`policy-remote-work\`` works.

Every existing L4 file in the jl4-core and jl4/experiments trees uses dashes
or camelCase — never underscores. Follow the convention.

---

### 9. L4 has no record-update syntax

You cannot write:

```l4
`test case` MEANS `base applicant` WITH
    `annual salary` IS 26199
```

`WITH` is construction-only: the LHS must be a type name, not an existing
record value. There is no `existing_record WITH field IS new_value` form
(as of today; see parser-proposals if you want it).

**Workaround A — full construction per case.** Write out every field every
time. Verbose but always works.

**Workaround B — builder function.** Define a helper that takes the fields
you vary and fills in the defaults:

```l4
GIVEN salary IS A NUMBER
      conviction IS A `Conviction History`
GIVETH An Applicant
`make applicant variant` salary conviction MEANS
    Applicant WITH
        `annual salary`      IS salary
        `conviction history` IS conviction
        -- ... all other fields hard-coded to defaults ...
```

Workaround B scales if you vary ≤4 fields. More than that, accept the
verbose full-construction form.

---

### 10. `#ASSERT` / `#EVAL` expressions must fit on a single line

```l4
-- Broken
#ASSERT `applicant qualifies` (`test case` WITH
    `annual salary` IS 26199)

-- Works
`test case` MEANS ... -- full construction on previous lines
#ASSERT `applicant qualifies` `test case`
```

The directive parser terminates at end-of-line. Pre-bind long test fixtures
to named values and use those names in the one-line `#ASSERT`.

---

### 11. Test files benefit from full record fixtures named after their purpose

Rather than:

```l4
`case1` MEANS Applicant WITH ...
#ASSERT p `case1`
#ASSERT NOT q `case1`
```

prefer:

```l4
`just above salary floor` MEANS Applicant WITH ...
#ASSERT p `just above salary floor`
```

The IDE / LSP output echoes the identifier name; human-readable names turn a
failing `assertion failed` into a self-documenting failure report. Spend the
backticks.

---

### 12. Write tests _alongside_ the rule, not after

The 202-assertion suite for the nine validation rules caught a real
rule-interpretation bug (immigration §1(c) clashing with §2(3) tradeable
points — the English source was internally inconsistent). Quiet reading
missed it; the test failing caught it.

Recommendation: sketch the happy-path + two boundary-case fixtures _before_
you write the final version of the rule. Testing at the end tempts you to
skip edge cases.

---

## Error-message decoder ring

| Symptom                                                                                  | Likely cause                                                          | See trap # |
| ---------------------------------------------------------------------------------------- | --------------------------------------------------------------------- | ---------- |
| `incorrect indentation (got N, should be greater than N)` on `ELSE IF`                   | Flat `ELSE IF` cascade                                                | 1          |
| `incorrect indentation (got N, should be greater than N)` on `LET`                       | Chained `LET ... IN`                                                  | 2          |
| `trying to apply X (of type T) to 1 argument here` with `T` a record type                | Unparenthesised genitive in function arg                              | 3          |
| `unexpected IS / expecting AKA, MEANS, OF, identifier, or space token` at a GIVEN line   | Multi-word parameter without backticks                                | 4          |
| `unexpected IS / expecting AKA, MEANS, identifier` inside a WHERE block                  | WHERE helper using `IS` instead of `MEANS`                            | 5          |
| `I could not find a definition for \`X\`, inferred type: HNN` (`HNN` is a small integer) | `X` is an undeclared party or identifier                              | 6          |
| Imported type / function appears "not found" even though the IMPORT line is present      | `IMPORT` placed after `§` / `--` content, or filename has underscores | 7, 8       |
| `trying to apply X of type T (which is not a function) to (named) arguments here`        | `existing_record WITH field IS value` record-update attempt           | 9          |
| `unexpected (` after a directive                                                         | Multi-line `#ASSERT` / `#EVAL` expression                             | 10         |

---

## Pre-flight checklist

Run this through your head before you let the parser see your file for the first time:

- [ ] `IMPORT` statements are on lines 1-N, before any `§`, comment block, or `DECLARE`.
- [ ] Filename uses dashes or camelCase, no underscores.
- [ ] Every multi-word identifier — function name, parameter name, type name, record field, enum constructor — is backticked.
- [ ] Any function that takes a record argument parenthesises genitive sub-expressions: `f (x's field)`, never `f x's field`.
- [ ] Flat multi-branch decisions use `BRANCH` (not chained `ELSE IF`); enum pattern matches use `CONSIDER ... WHEN`.
- [ ] Any function with ≥2 local bindings uses `WHERE`, not chained `LET ... IN`.
- [ ] All `WHERE`-bound helpers use `MEANS` (not `IS`).
- [ ] Every identifier after `PARTY` or `BREACH BY` is declared — either in a local `DECLARE ... IS ONE OF` or imported from `legal-persons`.
- [ ] For test files: directives (`#ASSERT`, `#EVAL`) are one-liners, with fixtures pre-bound to named values.
