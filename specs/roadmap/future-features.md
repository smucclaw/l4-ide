# Future Features

**Ellipsis linting**: LSP diagnostics to warn when ellipsis forms appear adjacent to mismatched operators (e.g., `...` near OR, `..` near AND). See [spec](dev/specs/todo/ASYNDETIC-DISJUNCTION-SPEC.md).

Three carets together will mean "repeat everything above to the end of the line".

Syntax and semantics for regulative rules.

Syntax and semantics for property assertions and bounded deontics. Transpilation to verification reasoner backends: UPPAAL, NuSMV, SPIN, Maude, Isabelle/HOL, Lean.

Transpilation to automatic web app generation.

Set-theoretic syntax for UNION and INTERSECT. Sometimes set-and means logical-or.

WHEN should not be needed at each line in a CONSIDER.

---

## Recently Implemented

The following features have been implemented and moved from this list:

- **Asyndetic conjunction (`...`)**: Implicit AND using three-dot ellipsis syntax. See [Basic Syntax](20-basic-syntax.md#asyndetic-conjunction-).
- **Asyndetic disjunction (`..`)**: Implicit OR using two-dot ellipsis syntax. See [Basic Syntax](20-basic-syntax.md#asyndetic-disjunction-).
- **Inert elements**: String literals in boolean context as grammatical scaffolding. See [Boolean Logic](10-boolean-logic.md#inert-elements-grammatical-scaffolding).
