# Boolean Logic

`TRUE` and `FALSE` values are combined using the operators `AND`, `OR`,
`NOT`, and `IMPLIES`.

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
