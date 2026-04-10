# Built-ins and Libraries Reference

The functions and values Claude is most likely to need but least likely to know from Haskell transfer.

**Canonical references:**

- Built-in coercions: <https://legalese.com/l4/reference/types/coercions.md>
- HTTP / JSON built-ins: <https://legalese.com/l4/reference/builtins/http-json.md>
- Library index: <https://legalese.com/l4/reference/libraries.md>
- Glossary of everything: <https://legalese.com/l4/reference/GLOSSARY.md>

---

## Contents

- [Type coercions (always available)](#type-coercions-always-available)
- [HTTP and JSON built-ins (always available)](#http-and-json-built-ins-always-available)
- [Temporal globals](#temporal-globals)
- [Library index](#library-index)
- [Prelude — most-used functions](#prelude--most-used-functions)

---

## Type coercions (always available)

L4 does **no implicit coercion** between types. Use these explicit conversions. They are in the compiler core — no `IMPORT` required.

| Function          | Signature                                                          | Notes                                                                           |
| ----------------- | ------------------------------------------------------------------ | ------------------------------------------------------------------------------- |
| `TOSTRING`        | `NUMBER \| BOOLEAN \| DATE \| TIME \| DATETIME \| STRING → STRING` | Numbers render canonically; dates `YYYY-MM-DD`; datetimes ISO 8601              |
| `TONUMBER`        | `STRING → MAYBE NUMBER`                                            | Accepts optional sign, decimals, `1.2E3` scientific                             |
| `TODATE`          | `STRING → MAYBE DATE`                                              | Accepts `YYYY-MM-DD`, `YYYY/MM/DD`, `DD-MMM-YYYY`, `DD/MM/YYYY`, `MMM DD, YYYY` |
| `TOTIME`          | `STRING → MAYBE TIME`                                              | Accepts `HH:MM:SS`, `HH:MM`, `h:MM AM/PM`                                       |
| `TODATETIME`      | `STRING → MAYBE DATETIME`                                          | ISO 8601 with timezone or `Z`                                                   |
| `TRUNC`           | `NUMBER NUMBER → NUMBER`                                           | `TRUNC value digits` — truncates toward zero                                    |
| `value AS STRING` | inline                                                             | Equivalent to `TOSTRING value`                                                  |

All parse functions return `MAYBE` and yield `NOTHING` on failure — there are no exceptions. Coercions are deterministic and locale-independent.

```l4
#EVAL TOSTRING 42                 -- "42"
#EVAL TONUMBER "  3.14  "         -- JUST 3.14
#EVAL TODATE "29-Feb-2024"        -- JUST <2024-02-29>
#EVAL TRUNC 12.987 2              -- 12.98
#EVAL 42 AS STRING                -- "42"
```

Reference: <https://legalese.com/l4/reference/types/coercions.md>

---

## HTTP and JSON built-ins (always available)

The bridge from L4 rules to real-world data. No `IMPORT` required.

| Function     | Signature                           | Purpose                                                                                                                     |
| ------------ | ----------------------------------- | --------------------------------------------------------------------------------------------------------------------------- |
| `FETCH`      | `STRING → STRING`                   | HTTP GET; returns response body as STRING. **Lazy + cached** — multiple references to the same call return the same result. |
| `POST`       | `STRING → STRING → STRING → STRING` | HTTP POST: `POST url headers body` where `headers` is a JSON string                                                         |
| `ENV`        | `STRING → STRING`                   | Read environment variable; returns `""` if unset. Use this for API keys.                                                    |
| `JSONENCODE` | `a → STRING`                        | Serialize a value to a JSON string                                                                                          |
| `JSONDECODE` | `STRING → a`                        | Parse a JSON string into a typed value                                                                                      |

```l4
DECIDE uuid     IS FETCH "https://www.uuidtools.com/api/generate/v4"
DECIDE apiKey   IS ENV "ANTHROPIC_API_KEY"
DECIDE response IS POST
    "https://api.example.com/data"
    "{\"Content-Type\": \"application/json\"}"
    "{\"name\": \"test\"}"
```

**Security:** keep secrets in `ENV`, never inline in source.

Reference: <https://legalese.com/l4/reference/builtins/http-json.md>

---

## Temporal globals

| Constant      | Type         | Notes                                   |
| ------------- | ------------ | --------------------------------------- |
| `TRUE`        | `BOOLEAN`    |                                         |
| `FALSE`       | `BOOLEAN`    |                                         |
| `NOTHING`     | `MAYBE a`    |                                         |
| `JUST x`      | `MAYBE a`    |                                         |
| `LEFT x`      | `EITHER a b` |                                         |
| `RIGHT y`     | `EITHER a b` |                                         |
| `EMPTY`       | `LIST a`     |                                         |
| `TODAY`       | `DATE`       | Requires `TIMEZONE IS …` at top of file |
| `NOW`         | `DATETIME`   | Defaults to UTC without `TIMEZONE IS`   |
| `CURRENTTIME` | `TIME`       | Requires `TIMEZONE IS …`                |
| `TIMEZONE`    | `STRING`     | The document's declared timezone        |

**`TIMEZONE IS` is a document-level declaration**, not an expression:

```l4
TIMEZONE IS "Asia/Singapore"

-- Now TODAY, CURRENTTIME, and TIMEZONE all resolve to Singapore values.
```

If you use `TODAY` or `CURRENTTIME` without `TIMEZONE IS`, the compiler errors.

For reproducible evaluation, pin the clock from the CLI:

```bash
jl4-cli --fixed-now=2025-01-01T00:00:00Z my-rules.l4
# or
JL4_FIXED_NOW=2025-01-01T00:00:00Z jl4-cli my-rules.l4
```

---

## Library index

All libraries require `IMPORT` **except prelude**, which is loaded automatically.

| Library         | Purpose                                                 | Reference                                                      |
| --------------- | ------------------------------------------------------- | -------------------------------------------------------------- |
| `prelude`       | Lists, Maybe, Pair, booleans, numbers (always imported) | <https://legalese.com/l4/reference/libraries/prelude.md>       |
| `daydate`       | Date calculations and temporal logic                    | <https://legalese.com/l4/reference/libraries/daydate.md>       |
| `time`          | Wall-clock time-of-day operations                       | <https://legalese.com/l4/reference/libraries/time.md>          |
| `datetime`      | Absolute points in time with timezones                  | <https://legalese.com/l4/reference/libraries/datetime.md>      |
| `timezone`      | IANA timezone constants                                 | <https://legalese.com/l4/reference/libraries/timezone.md>      |
| `excel-date`    | Excel serial-date compatibility                         | <https://legalese.com/l4/reference/libraries/excel-date.md>    |
| `math`          | Mathematical functions                                  | <https://legalese.com/l4/reference/libraries/math.md>          |
| `currency`      | Currency handling (ISO 4217)                            | <https://legalese.com/l4/reference/libraries/currency.md>      |
| `legal-persons` | Legal entity types and capacity                         | <https://legalese.com/l4/reference/libraries/legal-persons.md> |
| `jurisdiction`  | Jurisdiction definitions                                | <https://legalese.com/l4/reference/libraries/jurisdiction.md>  |
| `holdings`      | Ownership and holdings                                  | <https://legalese.com/l4/reference/libraries/holdings.md>      |
| `actus`         | ACTUS financial-contract standards                      | <https://legalese.com/l4/reference/libraries/actus.md>         |
| `llm`           | LLM API integration (from within L4)                    | <https://legalese.com/l4/reference/libraries/llm.md>           |
| `date-compat`   | Legacy DATE syntax compatibility                        | <https://legalese.com/l4/reference/libraries/date-compat.md>   |

Import syntax:

```l4
IMPORT daydate
IMPORT currency
IMPORT "my-custom-lib.l4"   -- custom libraries by file path
```

Reference: <https://legalese.com/l4/reference/libraries.md>

---

## Prelude — most-used functions

The prelude is always imported. These are the functions you will reach for constantly. The full reference is at <https://legalese.com/l4/reference/libraries/prelude.md>.

### Lists

| Function           | Type                            | Purpose                |
| ------------------ | ------------------------------- | ---------------------- |
| `null list`        | `[a] → Bool`                    | Is list empty?         |
| `count list`       | `[a] → Number`                  | Length                 |
| `map f list`       | `(a→b) → [a] → [b]`             | Transform each element |
| `filter f list`    | `(a→Bool) → [a] → [a]`          | Keep matching elements |
| `foldr f z list`   | `(a→r→r) → r → [a] → r`         | Right fold             |
| `foldl f z list`   | `(r→a→r) → r → [a] → r`         | Left fold              |
| `append l1 l2`     | `[a] → [a] → [a]`               | Concatenate            |
| `concat lists`     | `[[a]] → [a]`                   | Flatten                |
| `reverse list`     | `[a] → [a]`                     |                        |
| `at list i`        | `[a] → Number → a`              | 0-based indexing       |
| `take n list`      | `Number → [a] → [a]`            | First n elements       |
| `drop n list`      | `Number → [a] → [a]`            | Drop first n           |
| `elem x list`      | `a → [a] → Bool`                | Membership             |
| `sort list`        | `[Number] → [Number]`           | Ascending              |
| `zip l1 l2`        | `[a] → [b] → [Pair a b]`        | Pair up                |
| `nub list`         | `[a] → [a]`                     | Remove duplicates      |
| `partition f list` | `(a→Bool) → [a] → Pair [a] [a]` | Split by predicate     |

### Quantifiers

```l4
all (GIVEN n YIELD n GREATER THAN 0) numbers    -- all elements positive
any (GIVEN n YIELD n EQUALS 0)      numbers     -- any element zero
```

### Maybe

```l4
CONSIDER maybeValue
WHEN NOTHING THEN defaultResult
WHEN JUST x  THEN process x
```

`fromMaybe`, `isJust`, `isNothing` are also available in the prelude.

### Lambdas with YIELD

```l4
-- Anonymous function: positive?
GIVEN n YIELD n GREATER THAN 0

-- Passed to a higher-order function
filter (GIVEN n YIELD n GREATER THAN 0) numbers
```

`YIELD` is L4's lambda. Without it you cannot pass an anonymous predicate.
