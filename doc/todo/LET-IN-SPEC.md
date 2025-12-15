# LET/IN Expressions for L4

**Status:** Draft proposal  
**Priority:** Medium  
**Affects:** L4 language syntax, parser, evaluator, trace visualizer

## Motivation

GraphViz traces highlight that repeated expressions (for example `applicant's age`) are recomputed every time they appear. Today authors can only avoid that duplication by promoting subexpressions to top-level `DECIDE` definitions or by threading them through `WHERE` clauses, which is awkward for one-off helpers. A `LET … IN …` form—modeled directly on Haskell—would let authors bind an expression in place and reuse it multiple times inside the body. That makes evaluation traces easier to follow (one binding node feeding many consumers) and reduces redundant work in both human reasoning and future optimizations.

## Proposal

Introduce a local binding syntax:

```
LET <name> IS <expr>
IN <body>
```

or, for multiple bindings,

```
LET
  <name1> IS <expr1>
  <name2> IS <expr2>
IN <body>
```

Semantics follow Haskell’s `let/in`: bindings are lazily evaluated, scoped to the `IN` body, and can reference earlier bindings in the same `LET` block.

## Open Questions (future work)

- Exact grammar placement alongside existing `WHERE` blocks and `GIVEN` clauses.
- How to surface these bindings in `EvalTrace` and GraphViz output (likely as their own labeled nodes).
- Whether we also want pattern bindings (`LET (x, y) IS pair IN …`) or keep it to simple names initially.

This spec intentionally stops short of implementation detail; it just records the motivation and desired feature so we can prioritize the work and flesh out the design later.
