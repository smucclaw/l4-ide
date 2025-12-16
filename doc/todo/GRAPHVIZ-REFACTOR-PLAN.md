# GraphViz Visualization Refactor Plan

**Status**: In progress (commit a910adb1)
**Goal**: Clean recursive architecture using FGL + graphviz library

## Background

### The Problem
The original `GraphViz.hs` accumulates special-case handling ("epicycles"):
- IF expressions with compound branches need special detection
- Nested function calls with control flow don't visualize correctly
- Code duplication between `renderChildren` and `renderChildrenWithReverse`
- Increasingly complex heuristics for each nesting pattern

### Root Cause
Trying to recover tree structure from flattened trace representation by pattern-matching step sequences.

### The Key Insight
**The trace structure is already inductive/recursive**:
```haskell
data EvalTrace = Trace
  (Maybe Resolved)           -- label
  [(Expr, [EvalTrace])]      -- steps with recursive subtraces
  (Either EvalException NF)  -- result
```

**Solution**: Just follow the inductive structure recursively. No special cases.

## Architecture: Three Phases

### Phase 1: Pure Recursive Building (WIP)
Build FGL graph by pure recursion following trace structure:

```haskell
buildGraph :: EvalTrace -> (Graph NodeAttrs EdgeAttrs, NextNodeId)
buildGraph (Trace label steps result) =
  1. Create node for this trace
  2. Recursively build child graphs for each step
  3. Combine into single graph with edges
```

**Key principle**: Each step becomes nodes and edges. No lookahead, no grouping, pure recursion.

### Phase 2: Local Visual Optimization (TODO)
After graph is built, apply local transformations:

```haskell
optimizeVisualLayout :: Graph -> Graph
optimizeVisualLayout graph =
  graph
    |> moveBranchNodesUnderIF     -- Make IF/THEN/ELSE visually siblings
    |> collapseSimplePaths        -- Optional: reduce trivial nodes
    |> adjustEdgeRouting          -- Optional: improve edge layout
```

These are **graph-to-graph transformations**, not rendering concerns.

### Phase 3: Render to DOT (mostly done)
Use graphviz library to generate DOT format. Type-safe, no string manipulation.

## Current Status

### ✅ Completed
- Dependencies added (`fgl`, `graphviz`)
- Module skeleton created (`GraphViz2.hs`)
- Recursive building logic designed
- Edge configuration logic ported
- Stub node generation for unevaluated branches

### ⚠️ Blocked
**FGL API integration issues** (lines 93-97 of GraphViz2.hs):

```haskell
-- BROKEN: Trying to use ufold incorrectly
graph = FGL.mkGraph [thisNode] [] `FGL.ufold` childGraphs
graphWithEdges = FGL.insEdges childEdges graph
```

**Problem**:
- `ufold` doesn't work this way for combining graphs
- Need to collect all nodes/edges first, then build graph once

### 🔧 To Fix

**Step 1**: Change return type to collect nodes and edges
```haskell
-- OLD (broken):
buildGraph :: ... -> (EvalGraph, NextNodeId)

-- NEW (correct):
buildGraph :: ... -> ([LNode NodeAttrs], [LEdge EdgeAttrs], NextNodeId)
```

**Step 2**: Use list concatenation to combine
```haskell
buildGraph (Trace label steps result) =
  let thisNode = [(nodeId, nodeAttrs)]

      -- Recursively build children
      (childNodes, childEdges, nextId) = buildSteps ...

      -- Combine with simple list concatenation
      allNodes = thisNode ++ childNodes
      allEdges = edgesToChildren ++ childEdges

  in (allNodes, allEdges, nextId)
```

**Step 3**: Build final graph at top level
```haskell
traceToGraphViz opts trace =
  let (nodes, edges, _) = buildGraph opts 0 0 trace
      graph = FGL.mkGraph nodes edges
      dotGraph = graphToDot graph
  in Text.Lazy.toStrict $ GV.printDotGraph dotGraph
```

## Implementation Roadmap

### Immediate (Fix FGL Integration)
1. [ ] Change `buildGraph` to return `([LNode], [LEdge], NextId)`
2. [ ] Change `buildSteps` similarly
3. [ ] Change `buildSubtraces` similarly
4. [ ] Use `++` to combine node/edge lists
5. [ ] Build graph once at top level
6. [ ] Verify it compiles

### Testing Phase 1
7. [ ] Wire GraphViz2 into Main.hs with a flag (e.g., `--graphviz2`)
8. [ ] Test on simple cases (no IF): `baseScore MEANS 10`
9. [ ] Test on simple IF: `IF age > 60 THEN 30 ELSE 10`
10. [ ] Test on compound branch: `penalty` function (from trace-showcase.l4)
11. [ ] Test on nested functions: `youngBonus` with internal IF
12. [ ] Compare output structure to original GraphViz.hs

### Phase 2: Visual Optimization
13. [ ] Identify IF patterns in graph (IF node + condition + stubs)
14. [ ] Find branch evaluation nodes (siblings after IF)
15. [ ] Add edges to make branches visual children of IF
16. [ ] Remove original edges that made branches siblings
17. [ ] Test that visual layout matches desired idiom

### Phase 3: Switchover
18. [ ] Verify GraphViz2 handles all test cases correctly
19. [ ] Replace GraphViz.hs with GraphViz2.hs
20. [ ] Update Main.hs to use new module by default
21. [ ] Remove old GraphViz.hs (or keep as GraphVizLegacy.hs)

## Test Cases

### Test 1: Simple IF (Literal Branches)
```l4
baseScore MEANS IF age > 60 THEN 30 ELSE 10
```

**Expected structure** (Phase 1 - pure recursion):
```
baseScore
 ├─ (evaluation steps...)
 └─ IF age > 60
     ├─ age > 60 = FALSE (condition)
     ├─ 30 (THEN stub, dashed)
     └─ 10 (ELSE literal)
```

All siblings in Phase 1. Phase 2 optimization will adjust layout.

### Test 2: Compound Branch (Arithmetic)
```l4
penalty count MEANS IF count <= 0 THEN 0 ELSE 0 - (count * 3)
```

**Expected structure** (Phase 1):
```
penalty
 ├─ count = 2
 ├─ IF count <= 0
 │   └─ count <= 0 = FALSE
 └─ 0 - (count * 3) = -6
     └─ count * 3 = 6
```

**After Phase 2 optimization**:
```
penalty
 └─ IF count <= 0
     ├─ count <= 0 = FALSE (condition)
     ├─ 0 (THEN stub, dashed)
     └─ 0 - (count * 3) = -6 (ELSE)
         └─ count * 3 = 6
```

### Test 3: Nested Function with IF
```l4
baseScore MEANS IF age > 60 THEN 30 ELSE youngBonus applicant

youngBonus person MEANS IF person's age > 30 THEN 10 ELSE 5
```

**Expected structure** (Phase 1):
```
baseScore
 ├─ IF age > 60
 │   └─ age > 60 = FALSE
 └─ youngBonus applicant = 10
     ├─ IF person's age > 30
     │   └─ person's age > 30 = TRUE
     └─ 10 (THEN result)
```

This should work correctly even in Phase 1, because each function evaluation is a separate trace with its own steps.

## Code Locations

- **Main module**: `jl4-core/src/L4/EvaluateLazy/GraphViz2.hs`
- **Test file**: `doc/tutorial-code/trace-showcase.l4`
- **Original**: `jl4-core/src/L4/EvaluateLazy/GraphViz.hs` (still functional)
- **CLI integration**: `jl4/app/Main.hs` (line ~315)

## Key Design Principles

1. **Pure recursion first**: Follow inductive structure, no special cases
2. **Separate concerns**: Structure building vs visual optimization vs rendering
3. **Type safety**: Use FGL and graphviz library, not string manipulation
4. **Testability**: Each phase can be tested independently
5. **Simplicity**: Prefer simple list operations (`++`) over complex graph operations

## Why This Will Work

### Original Problem: youngBonus visualization
The `youngBonus` function's IF was connecting to the wrong parent because we were pattern-matching flat step sequences.

### With Pure Recursion:
```haskell
-- baseScore trace contains:
Trace "baseScore"
  [ (ifExpr, [condTrace])              -- Step 1: IF evaluation
  , (youngBonusCall, [youngBonusTrace]) -- Step 2: function call
  ]
  (Right 10)

-- youngBonusTrace contains:
Trace "youngBonus"
  [ (person's age, [ageTrace])     -- Step 1: arg
  , (ifExpr2, [condTrace2])        -- Step 2: IF evaluation
  , (literalResult, [])            -- Step 3: THEN result
  ]
  (Right 10)
```

When we recursively build:
1. `baseScore` node gets created
2. We recursively build IF node (with condition subtrace)
3. We recursively build youngBonus node
4. Inside youngBonus, we recursively build ITS IF node
5. Everything nests correctly because we're following the trace structure

**No special cases needed!** The trace already encodes the nesting correctly.

## Future Enhancements (Out of Scope)

- [ ] Prune trivial nodes (single-use let bindings)
- [ ] Syntax highlighting in node labels
- [ ] Interactive visualization (clickable nodes)
- [ ] Diff view between two evaluations
- [ ] Performance optimization for large traces

## References

- FGL tutorial: https://hackage.haskell.org/package/fgl/docs/Data-Graph-Inductive.html
- graphviz library: https://hackage.haskell.org/package/graphviz
- Original issue exploration: commit 732ef900
- This refactor start: commit a910adb1

## Notes for Next Session

Start by fixing the FGL integration in `buildGraph`:
1. Open `GraphViz2.hs` line 77
2. Change return type to `([LNode NodeAttrs], [LEdge EdgeAttrs], Node)`
3. Update `buildSteps` and `buildSubtraces` similarly
4. Use list concatenation instead of `ufold`
5. Build final graph at top level in `traceToGraphViz`

The logic is correct, just the FGL API usage needs fixing!
