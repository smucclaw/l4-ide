# L4 Design Principles

The core principles that guide L4's language design.

---

## Overview

L4 is designed around five core principles:

1. **Legal Isomorphism** - L4 structure mirrors legal text structure
2. **Human Readability** - Code should be readable by lawyers
3. **Formal Precision** - Unambiguous, machine-checkable semantics
4. **Accessibility** - Usable by non-programmers
5. **Composability** - Small pieces combine into larger systems

These principles sometimes tension with each other—the art of language design is finding the right balance.

---

## Principle 1: Legal Isomorphism

**L4 code should look like the legal text it encodes.**

### Why?

- **Traceability**: Easy to map code to source legislation
- **Review**: Lawyers can verify correctness without learning to code
- **Maintenance**: Changes in law map to changes in code

### Examples

Legal text:

> A person must not sell alcohol if the person is a body corporate.

L4 code:

```l4
DECIDE `a person must not sell alcohol`
IF `the person is a body corporate`
```

The structure matches:

- "A person must not sell alcohol" → `DECIDE \`a person must not sell alcohol\``
- "if" → `IF`
- "the person is a body corporate" → `\`the person is a body corporate\``

### Implications

- Keywords read like legal English (MUST, MAY, SHANT)
- Backtick identifiers allow natural language names
- Conditions use legal phrasing, not programmer conventions

---

## Principle 2: Human Readability

**Code should be understandable by legal professionals.**

### Why?

- **Domain experts** should validate the encoding
- **Stakeholders** need to understand what rules do
- **Auditors** must verify compliance

### Design Choices

**Layout sensitivity (Python-like):**

```l4
-- Structure visible from indentation
DECLARE Person
    HAS name IS A STRING
        age IS A NUMBER
```

**Verbose keywords:**

```l4
-- "IS A" not just ":"
name IS A STRING

-- "MEANS" not just "="
fullName MEANS CONCAT firstName lastName
```

**Natural language operators:**

```l4
-- Can write "AND" instead of "&&"
IF age >= 18 AND hasValidID

-- Can write "EQUALS" instead of "=="
IF status EQUALS Active
```

### Trade-offs

- More typing than terse syntax
- Longer code files
- But: Much more accessible to non-programmers

---

## Principle 3: Formal Precision

**Every L4 program has unambiguous, machine-checkable meaning.**

### Why?

- **Computation**: We can execute rules to get decisions
- **Verification**: We can check rules for consistency
- **Explanation**: We can generate traces of reasoning

### Design Choices

**Strong static typing:**

```l4
-- Types catch errors before runtime
GIVEN person IS A Person    -- Must be a Person, not arbitrary
GIVETH A BOOLEAN            -- Always returns Boolean
```

**Exhaustive pattern matching:**

```l4
-- Must handle all cases
CONSIDER status
WHEN Active THEN ...
WHEN Suspended THEN ...
WHEN Terminated THEN ...
-- Error if case missing
```

**No implicit behavior:**

```l4
-- IF requires ELSE - no "null" fallback
IF condition THEN result1 ELSE result2
```

### Trade-offs

- More explicit code
- Sometimes verbose
- But: No surprises, auditable

---

## Principle 4: Accessibility

**L4 should be usable by domain experts, not just programmers.**

### Why?

- **The knowledge is with lawyers**, not programmers
- **Validation requires understanding** - can't validate what you can't read
- **Maintenance** - laws change, updates shouldn't require programmers

### Design Choices

**No installation required (web editor):**

```
Access L4 via browser, no setup
```

**Familiar constructs:**

```l4
-- IF/THEN/ELSE, not pattern matching
IF age >= 18 THEN "adult" ELSE "minor"

-- Lists look like lists
LIST 1, 2, 3
```

**Error messages reference legal concepts:**

```
Error: Party 'Seller' does not have permission to 'inspect goods'
       Only 'Buyer' has this permission per line 45
```

### Trade-offs

- May feel limiting to experienced programmers
- Some advanced patterns not directly supported
- But: Dramatically lower barrier to entry

---

## Principle 5: Composability

**Small pieces should combine predictably into larger systems.**

### Why?

- **Real legal systems are large** - thousands of rules
- **Reuse** - same patterns appear repeatedly
- **Testing** - test small parts independently

### Design Choices

**Functions are first-class:**

```l4
-- Functions can take other functions
map (GIVEN n YIELD n * 2) numbers
```

**Local definitions with WHERE:**

```l4
-- Complex expressions decompose into named parts
result MEANS finalStep
    WHERE
        step1 MEANS firstOperation input
        step2 MEANS secondOperation step1
        finalStep MEANS thirdOperation step2
```

**Module system:**

```l4
-- Import from other files
IMPORT "types/parties.l4"
IMPORT "rules/eligibility.l4"
```

**Regulative combinators:**

```l4
-- Combine obligations with RAND/ROR
obligation1 RAND obligation2  -- Both required
obligation1 ROR obligation2   -- Either sufficient
```

### Trade-offs

- More moving parts
- Needs good tooling support
- But: Scales to real-world complexity

---

## Principle Tensions

### Readability vs Precision

Natural language is readable but ambiguous:

```l4
-- What does "or" mean here?
-- Inclusive (one or both)?
-- Exclusive (one but not both)?
IF hasLicense OR hasPermit
```

L4 solution: Use natural keywords but with precise semantics.

### Accessibility vs Power

Simple syntax limits expressiveness:

```l4
-- This is accessible
IF age >= 18 AND hasID

-- This is powerful but complex
all (GIVEN p YIELD all (GIVEN q YIELD ...) ...) ...
```

L4 solution: Simple syntax for common cases, full power available when needed.

### Legal Isomorphism vs Consistency

Different jurisdictions write differently:

```l4
-- UK style: "shall"
-- US style: "must"
-- L4 uses both as synonyms
MUST action
SHALL action  -- Same meaning
```

L4 solution: Support multiple phrasings with identical semantics.

---

## Summary

| Principle         | Key Idea               | Trade-off       |
| ----------------- | ---------------------- | --------------- |
| Legal Isomorphism | Mirror legal text      | Longer code     |
| Human Readability | Readable by lawyers    | More verbose    |
| Formal Precision  | Machine-checkable      | More explicit   |
| Accessibility     | Domain expert friendly | Less "powerful" |
| Composability     | Small pieces combine   | More complexity |

---

## Further Reading

- [Syntax Reference](../../reference/syntax/README.md) - Full syntax guide
- [Keywords Reference](../../reference/keywords/README.md) - All L4 keywords
- [Foundation Course](../../courses/foundation/README.md) - Learn L4 from scratch
