# Foundation Course

A complete introduction to L4 for beginners. No programming experience required.

## Overview

This course takes you from zero to writing your first legal rules in L4. You'll learn:

- How to write legal rules that computers can understand and execute
- How to model legal entities and relationships
- How to handle multi-step legal processes
- How to test and simulate legal scenarios

**Prerequisites:** Basic familiarity with legal concepts. No programming experience required.

**Time:** Approximately 4-6 hours total

---

## Modules

### [Module 0: Introduction](module-0-introduction.md)

What is L4 and why use it?

- What L4 is designed for
- How L4 differs from traditional legal drafting
- Setting up your environment

**Time:** 15 minutes

---

### [Module 1: Your First Legal Rule](module-1-first-rule.md)

Write a simple legal obligation in L4.

- The basic structure: GIVEN, PARTY, MUST
- Adding conditions with IF
- Setting deadlines with WITHIN
- Consequences: HENCE and LEST

**Time:** 45 minutes

---

### [Module 2: Legal Entities](module-2-entities.md)

Model structured legal entities and relationships.

- From strings to structured types with DECLARE
- Enumerating legal categories with IS ONE OF
- Connecting multiple entities
- Record field access

**Time:** 60 minutes

---

### [Module 3: Control Flow](module-3-control-flow.md)

Handle conditional logic and pattern matching.

- IF/THEN/ELSE expressions
- Pattern matching with CONSIDER
- Working with lists
- Boolean logic and operators

**Time:** 45 minutes

---

### [Module 4: Functions](module-4-functions.md)

Define reusable legal rules and computations.

- Function signatures with GIVETH
- Simple functions with MEANS
- Decision functions with DECIDE
- Local definitions with WHERE

**Time:** 60 minutes

---

### [Module 5: Regulative Rules](module-5-regulative.md)

Model obligations, permissions, and prohibitions.

- The DEONTIC type
- MUST, MAY, and SHANT
- Chaining obligations with HENCE/LEST
- Testing with #TRACE

**Time:** 60 minutes

---

### [Module 6: Putting It Together](module-6-capstone.md)

Build a complete legal model.

- Combining everything learned
- Best practices
- Common patterns
- Next steps

**Time:** 45 minutes

---

## Learning Path

```
Module 0 ──► Module 1 ──► Module 2 ──► Module 3
                                           │
                                           ▼
              Module 6 ◄── Module 5 ◄── Module 4
```

Each module builds on the previous ones. Complete them in order for the best learning experience.

---

## After This Course

Once you complete the Foundation Course, you can:

1. **[Advanced Course](../advanced/README.md)** - Deep dives into complex L4 patterns
2. **[Tutorials](../../tutorials/README.md)** - Task-focused guides for specific goals
3. **[Reference](../../reference/README.md)** - Look up specific language features

Additional examples in the repository:

- `jl4/examples/legal/` - Real-world legal examples
- `jl4/examples/ok/` - Working code samples

---

## Getting Help

- **Stuck?** Check the [Reference](../../reference/README.md) for syntax details
- **Confused?** Read [Concepts](../../concepts/README.md) for deeper explanations
- **Bug?** Report via [GitHub Issues](https://github.com/smucclaw/l4-ide/issues)
