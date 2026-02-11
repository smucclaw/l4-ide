# Advanced Course

Deep dives into complex L4 patterns for production use.

## Overview

This course covers advanced techniques for modeling complex legal systems:

- Real regulatory schemes with multiple instruments
- Cross-cutting concerns (timing, notices, appeals)
- Multi-temporal reasoning
- Production patterns and debugging

**Prerequisites:** Completion of the [Foundation Course](../foundation/README.md)

**Time:** Approximately 4-6 hours total

---

## Modules

### [Module A1: Real Regulatory Schemes](module-a1-regulatory.md)

Model complete legislative frameworks.

- The three-layer approach (structural, deontic, events)
- Encoding definitions from legislation
- Handling cross-references
- Building from a real example: charity regulation

**Time:** 90 minutes

---

### [Module A2: Cross-Cutting Concerns](module-a2-cross-cutting.md)

Patterns that span multiple rules.

- Timing and deadlines
- Notice requirements
- Appeal procedures
- Grace periods and escalation

**Time:** 60 minutes

---

### [Module A3: Contracts in Depth](module-a3-contracts.md)

Advanced contract modeling.

- Complex payment terms
- Recursive obligations
- Penalty structures
- Real-world example: promissory note

**Time:** 60 minutes

---

### [Module A4: Production Patterns](module-a4-production.md)

Patterns for robust, maintainable code.

- Organizing large codebases
- Testing strategies
- Common debugging patterns
- Integration considerations

**Time:** 45 minutes

---

## Learning Path

Complete modules in order:

```
Foundation Course ──► Module A1 ──► Module A2 ──► Module A3 ──► Module A4
```

---

## Example Code

All examples from this course are based on real files:

- `jl4/experiments/charities/` - Jersey Charities Law encoding
- `jl4/examples/legal/promissory-note.l4` - Complete loan agreement
- `jl4/examples/legal/` - Other real-world examples

---

## After This Course

Continue learning with:

1. **[Tutorials](../../tutorials/README.md)** - Task-focused guides
2. **[Concepts](../../concepts/README.md)** - Theoretical foundations
3. **Real projects** - Apply what you've learned to your domain

