# GraphViz Sharing Visualization for WHERE and LET/IN

**Status:** Proposed
**Author:** Meng Wong, with Claude (Opus 4.5)
**Date:** 2025-12-17
**Related:** LET-IN-SPEC-opus.md

## Motivation

When a WHERE (or future LET/IN) binding is referenced from multiple call sites, the current GraphViz visualizer produces **multiple boxes** with **separate arrows** to each box. This obscures the sharing/memoization semantics.

**Current behavior:**
```
┌─────────┐     ┌─────────┐
│ caller1 │────▶│ shared  │
└─────────┘     └─────────┘

┌─────────┐     ┌─────────┐
│ caller2 │────▶│ shared  │  (duplicate box!)
└─────────┘     └─────────┘
```

**Desired behavior:**
```
┌─────────┐
│ caller1 │───┐
└─────────┘   │   ┌─────────┐
              ├──▶│ shared  │  (single box, multiple arrows)
┌─────────┐   │   └─────────┘
│ caller2 │───┘
└─────────┘
```

This matters because:
- L4's WHERE and LET/IN have **lazy evaluation with sharing** (memoization)
- The visual should reflect that the binding is computed **once** and reused
- Multiple boxes incorrectly suggest multiple evaluations

## Scope

This work is **out of scope** for the LET/IN implementation. It requires:

1. The GraphViz module to receive additional information about binding identity
2. A mechanism to detect when the same binding is referenced multiple times
3. Graph construction that deduplicates nodes by binding identity

## Preliminary Design Sketch

### Option A: Annotate bindings with unique IDs

During typechecking or a dedicated pass, assign each WHERE/LET binding a unique identifier. Pass this to the GraphViz module so it can recognize "same binding, different call site."

### Option B: Use source location as identity

WHERE/LET bindings have source positions. The GraphViz module could use `(file, line, col)` as a deduplication key.

### Option C: Post-process the graph

Generate the graph as today, then merge nodes that represent the same binding based on name + enclosing scope.

## Files Likely Involved

- `jl4-core/src/L4/GraphViz.hs` (or similar)
- Possibly annotation/typechecking passes to propagate binding identity

## Out of Scope for Now

- Full implementation
- Detailed design of identity propagation
- Integration with existing GraphViz infrastructure

This spec exists to capture the motivation and ensure the work isn't forgotten when LET/IN is implemented.
