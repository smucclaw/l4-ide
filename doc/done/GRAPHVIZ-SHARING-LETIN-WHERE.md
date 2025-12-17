# GraphViz Sharing Visualization for WHERE and LET/IN

**Status:** ✅ COMPLETED (2025-12-17)
**Author:** Meng Wong, with Claude (Sonnet 4.5)
**Date:** 2025-12-17
**Related:** LET-IN-SPEC-opus.md
**Implementation:** PR #696 on branch `mengwong/graphviz-sharing`

## Summary

**Completed implementation** of binding deduplication in GraphViz2 module. WHERE and LET/IN bindings that are referenced from multiple call sites now appear as **single boxes with multiple incoming arrows**, correctly reflecting L4's lazy evaluation with sharing semantics.

## Original Motivation

When a WHERE (or future LET/IN) binding is referenced from multiple call sites, the GraphViz visualizer was producing **multiple boxes** with **separate arrows** to each box. This obscured the sharing/memoization semantics.

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

## Implemented Solution

### Approach: Option C + Structural Comparison (Post-process the graph)

The implementation uses a post-processing pass that:

1. Adds `bindingId :: Maybe Resolved` field to `NodeAttrs` to track binding identity
2. Implements `deduplicateBindingsPass` that merges duplicate nodes
3. Uses **structural comparison** to verify nodes represent the same computation

### Key Implementation Details

**File:** `jl4-core/src/L4/EvaluateLazy/GraphViz2.hs`

```haskell
data NodeAttrs = NodeAttrs
  { nodeLabel :: Text
  , fillColor :: Text
  , nodeStyle :: Text
  , bindingId :: Maybe Resolved  -- For deduplication
  } deriving (Eq, Show)

data GraphVizOptions = GraphVizOptions
  { ...
  , deduplicateBindings :: Bool  -- Default: True
  }

deduplicateBindingsPass :: EvalGraph -> EvalGraph
deduplicateBindingsPass graph =
  let allNodes = FGL.labNodes graph
      duplicatePairs = findDuplicatePairs graph allNodes
      mergeMap = buildMergeMap duplicatePairs
      deduplicatedGraph = foldl' (\g (dup, canon) -> mergeNode canon dup g) graph (Map.toList mergeMap)
  in deduplicatedGraph
```

### Structural Comparison

The deduplication doesn't just match on binding ID and result value - it performs a **recursive structural comparison** of the entire evaluation subtree using `sameSubgraph`:

```haskell
sameSubgraph :: EvalGraph -> Node -> Node -> Bool
sameSubgraph gr n1 n2 =
  let children1 = FGL.suc gr n1
      children2 = FGL.suc gr n2
  in length children1 == length children2
     && all (\(c1, c2) -> nodesEqual gr c1 c2) (zip (sort children1) (sort children2))
```

This ensures that two nodes are only merged if they represent **the same computation path**, not just the same surface result.

### CLI Integration

- **`--deduplicate-bindings`** (default): Enable deduplication
- **`--no-deduplicate-bindings`**: Disable if graph would be clearer without merging

### Test Cases

Verified with:
- `sharing-complex.l4` - Binding referenced 3 times (sum + product + difference)
- `sharing-recursive.l4` - WHERE binding in recursive function
- `nested-control-flow.l4` - Complex IF/THEN/ELSE with CONSIDER

All tests confirm that nodes with identical binding IDs but different evaluation paths are **not** incorrectly merged.

## Files Modified

- `jl4-core/src/L4/EvaluateLazy/GraphViz2.hs` - Core deduplication logic
- `jl4/app/Main.hs` - CLI flags
- `jl4-decision-service/src/Backend/Jl4.hs` - Migrated to GraphViz2
- `jl4-repl/app/Main.hs` - Migrated to GraphViz2

## Related Work

- **PR #696**: GraphViz architecture refactoring (includes this feature)
- **GRAPHVIZ-REFACTOR-PLAN.md**: Overall refactor plan
- **TRACE-GRAPHVIZ-ARCHITECTURE.md**: Unified architecture design
