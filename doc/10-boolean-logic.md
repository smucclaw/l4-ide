# Boolean Logic

`TRUE` and `FALSE` values are combined using the operators `AND`, `OR`,
`NOT`, `UNLESS`, and `IMPLIES`.

## Operator Precedence

When multiple boolean operators appear in an expression, L4 follows a standard precedence hierarchy (higher precedence binds tighter):

| Precedence  | Operators           | Description                             |
| ----------- | ------------------- | --------------------------------------- |
| 3 (highest) | `AND`, `...`        | Conjunction (and asyndetic conjunction) |
| 2           | `OR`, `..`          | Disjunction (and asyndetic disjunction) |
| 1 (lowest)  | `IMPLIES`, `UNLESS` | Implication and exception               |

This means `A OR B AND C` is parsed as `A OR (B AND C)`, following standard mathematical convention.

### Layout-Sensitive Precedence

L4's indentation-based syntax provides an alternative to parentheses for grouping. Operators that are more deeply indented bind more tightly:

```l4
-- This evaluates to TRUE because the AND binds tighter (more indented)
DECIDE example1 IF
    FALSE
OR     TRUE
   AND TRUE
```

```l4
-- This evaluates to FALSE because the OR binds tighter (more indented)
DECIDE example2 IF
       FALSE
   AND TRUE
    OR TRUE
```

When operators appear at the **same indentation level**, standard precedence rules apply. The IDE will show a **yellow warning** if `AND` and `OR` appear at the same column on different lines, as this often indicates a precedence mistake:

```l4
-- WARNING: AND and OR at same indentation level
DECIDE ambiguous IF
       TRUE
  OR   FALSE
  AND  FALSE    -- Linter warns here
```

To resolve the warning, use different indentation levels to make your intent explicit:

```l4
-- Clear: AND binds to FALSE only
DECIDE clear_version IF
    TRUE
OR     FALSE
   AND FALSE
```

## UNLESS: Exception Clauses

The `UNLESS` keyword provides a natural way to express exceptions. It is syntactic sugar for `AND NOT`, but with **lower precedence than OR**, ensuring it applies to the entire preceding expression:

```l4
A AND B AND C UNLESS D    -- means: (A AND B AND C) AND NOT D
A OR B OR C UNLESS D      -- means: (A OR B OR C) AND NOT D
```

### Motivation

In natural language, "unless" typically acts as an exception to everything that precedes it:

- "You may enter if you're an employee OR have a badge OR are a contractor UNLESS you've been banned."

With standard AND/NOT, expressing this requires explicit parentheses:

```l4
-- Without UNLESS: parentheses required
DECIDE `may enter` IF
    (    `is employee`
      OR `has badge`
      OR `is contractor`
    )
    AND NOT `has been banned`
```

With UNLESS, the intent is clearer:

```l4
-- With UNLESS: natural exception syntax
DECIDE `may enter` IF
         `is employee`
    OR   `has badge`
    OR   `is contractor`
    UNLESS `has been banned`
```

### UNLESS with AND vs OR

When `UNLESS` appears with `AND` operators at the same indentation level, it behaves as another conjunct and the linter remains silent:

```l4
-- UNLESS with ANDs: no warning
DECIDE `must comply` IF
         `condition one`
  AND    `condition two`
  AND    `condition three`
  UNLESS `is exempt`
```

When `UNLESS` appears with `OR` operators at the same indentation level, the IDE shows a warning because the binding might be surprising:

```l4
-- UNLESS with ORs: warning shown
DECIDE `qualifies` IF
         `path one`
  OR     `path two`
  OR     `path three`
  UNLESS `is disqualified`   -- Warning: AND and OR at same level
```

The warning reminds you that `UNLESS` (which desugars to `AND NOT`) binds to the **entire disjunction**, not just the last alternative. This is usually the intended behavior for exception clauses, but the warning ensures you've considered the semantics.

## Example: XOR Function

```l4
§ xor

GIVEN x IS A BOOLEAN, y IS A BOOLEAN
GIVETH A BOOLEAN
DECIDE xor x y IS
     x AND NOT y
  OR NOT x AND y
```

## Inert Elements: Grammatical Scaffolding

Legal documents often contain phrases that provide context or readability but don't affect the logical outcome. L4 supports **inert elements**—bare string literals in boolean context that serve as grammatical scaffolding.

### How Inert Elements Work

When a string literal (in double quotes) appears as a direct operand of `AND` or `OR`, it becomes an **inert element** with context-aware evaluation:

- **In AND context**: Evaluates to `TRUE` (the identity for AND)
- **In OR context**: Evaluates to `FALSE` (the identity for OR)

This follows the mathematical principle of monoid identities: the value that, when combined with any other value using the operation, returns that other value unchanged.

### Example: Contract Validity

```l4
GIVEN `parties have capacity` IS A BOOLEAN
      `consideration present` IS A BOOLEAN
DECIDE `contract valid` IF
        "notwithstanding any provision to the contrary"
    AND `parties have capacity`
    AND `consideration present`
```

The string `"notwithstanding any provision to the contrary"` evaluates to `TRUE` in this AND chain, so the result depends only on the actual boolean parameters.

### Example: Legal Conditions with OR

```l4
GIVEN `is over 21` IS A BOOLEAN
      `is married` IS A BOOLEAN
      `has parental consent` IS A BOOLEAN
DECIDE `person qualifies` IF
        "whether in sickness or in health"
    ... `is over 21`
    AND     `is married`
        OR  `has parental consent`
        OR  "or any other qualifying circumstance"
```

Here:

- `"whether in sickness or in health"` is in AND context → evaluates to `TRUE`
- `"or any other qualifying circumstance"` is in OR context → evaluates to `FALSE`

The inert strings don't change the logical outcome but make the rule more readable and closer to natural legal language.

### Asyndetic Conjunction (`...`)

L4 supports **asyndetic conjunction**—continuing an AND chain without repeating the conjunction keyword. The **ellipsis (`...`)** is the _syntax_ for this feature; **asyndetic conjunction** is the _semantics_ (what it means).

An _asyndeton_ is a rhetorical device where conjunctions are deliberately omitted, as in "I came, I saw, I conquered" (instead of "I came _and_ I saw _and_ I conquered").

```l4
DECIDE `rule applies` IF
        "subject to the following conditions"
    ... `condition one`
    ... `condition two`
    AND `condition three`
```

This is equivalent to:

```l4
DECIDE `rule applies` IF
        "subject to the following conditions"
    AND `condition one`
    AND `condition two`
    AND `condition three`
```

### Real-World Example: Singapore Penal Code Section 415

The inert elements feature enables direct encoding of complex legal structures like criminal statutes:

```l4
GIVEN `deceives` IS A BOOLEAN
      `fraudulent` IS A BOOLEAN
      `dishonest` IS A BOOLEAN
      `delivers` IS A BOOLEAN
DECIDE `commits cheating` IF
    "by deceiving any person"
    ... `deceives`
    AND "and"
    AND     "fraudulently or dishonestly induces"
            ... `fraudulent`
            OR  `dishonest`
        ... "the person so deceived to"
        AND `delivers`
```

The strings like `"by deceiving any person"` and `"the person so deceived to"` preserve the original statutory language while the boolean parameters (`deceives`, `fraudulent`, etc.) carry the actual logical content.

### When to Use Inert Elements

Use inert elements when you want to:

1. **Preserve original legal language** - Keep statutory or contractual phrases visible in the formalization
2. **Improve readability** - Add context that helps readers understand the rule's purpose
3. **Document intent** - Leave markers indicating where certain conditions belong, even if not yet fully specified
4. **Create isomorphic representations** - Make L4 code that closely mirrors the structure of the source legal document

### Asyndetic Disjunction (`..`)

For symmetry, L4 also supports **asyndetic disjunction**—implicit OR using the **two-dot ellipsis (`..`)**:

```l4
DECIDE `qualifies` IF
        `has license`
    ..  `has permit`
    OR  `has exemption`
```

This is equivalent to:

```l4
DECIDE `qualifies` IF
        `has license`
    OR  `has permit`
    OR  `has exemption`
```

| Syntax | Name               | Semantics                            |
| ------ | ------------------ | ------------------------------------ |
| `...`  | Three-dot ellipsis | Asyndetic conjunction (implicit AND) |
| `..`   | Two-dot ellipsis   | Asyndetic disjunction (implicit OR)  |

### Important Notes

- Inert elements only work in **boolean context** (direct operands of AND/OR)
- A bare string in non-boolean context (e.g., in an EQUALS comparison) remains a string
- Use `...` (three dots) for AND continuation, `..` (two dots) for OR continuation
