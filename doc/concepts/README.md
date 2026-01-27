# L4 Concepts

Understanding-oriented explanations of the principles, theories, and design decisions behind L4.

## Purpose

Concepts documentation helps you understand:

- **Why** L4 works the way it does
- **How** different features relate to each other
- **When** to use particular patterns or approaches
- **What** theoretical foundations support L4's design

Unlike reference docs (which tell you **what** things are) or tutorials (which show you **how** to do things), concepts explain the **why** and provide deeper understanding.

---

## Topics

### 🎨 [Language Design](language-design/)

The philosophy and principles behind L4's design

- **[Principles](language-design/principles.md)** - Core design principles of L4

**Key Ideas:** Human-readable code, legal text fidelity, accessibility for non-programmers

_More topics planned: Layout Sensitivity, Legal Isomorphism, Scope_

---

### ⚖️ [Legal Modeling](legal-modeling/)

Representing legal concepts in code

- **[Regulative Rules](legal-modeling/regulative-rules.md)** - Obligations, permissions, prohibitions

**Key Ideas:** Deontic modalities, legal rules as code, contract patterns

_More topics planned: Deontic Logic, Contract Composition, Constitutive vs Regulative_

---

### 🏗️ [Type System](type-system/)

How L4's type system works

- **[Algebraic Types](type-system/algebraic-types.md)** - Sum types and product types

**Key Ideas:** Type safety, algebraic data types, functional programming influence

_More topics planned: Maybe and Nothing, Type Inference_

---

## How to Use Concepts

### During Learning

- Read concept docs **after** you've tried the feature in practice
- Concepts build on knowledge from [courses](../courses/README.md) and [tutorials](../tutorials/README.md)

### For Deeper Understanding

- Concepts explain the "why" behind language features
- Use concepts to inform design decisions in your L4 programs

### For Discussion

- Concepts provide vocabulary for discussing L4 design
- Reference concepts when proposing changes or new features

---

## Relationship to Other Documentation

```
┌─────────────┐
│  CONCEPTS   │  Why does it work this way?
│  (explain)  │  What are the principles?
└──────┬──────┘
       │
       ├─────────┐
       │         │
┌──────▼──────┐ ┌▼──────────┐
│  REFERENCE  │ │  COURSES  │
│  (lookup)   │ │  (learn)  │
└──────┬──────┘ └┬──────────┘
       │         │
       └────┬────┘
            │
      ┌─────▼──────┐
      │ TUTORIALS  │  How do I do X?
      │   (tasks)  │
      └────────────┘
```

- **Start with [Courses](../courses/README.md)** to learn L4 systematically
- **Use [Tutorials](../tutorials/README.md)** to accomplish specific tasks
- **Consult [Reference](../reference/README.md)** to look up specific features
- **Read Concepts** to understand the deeper principles

---

## Contributing

Found an error or want to add a concept explanation?

- Report issues via [GitHub Issues](https://github.com/smucclaw/l4-ide/issues)
- Suggest new concept topics in the issue tracker
- Contributions welcome via GitHub pull requests

---

## Further Reading

### Academic Papers

- Coming soon: Links to papers on L4's theoretical foundations

### Design Documents

- See [/specs/proposals/](../../specs/proposals/) for design proposals
- See [/specs/roadmap/](../../specs/roadmap/) for future directions

### Related Languages

- Comparison to other legal programming languages (coming soon)
