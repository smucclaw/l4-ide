# Specification: Global Dependency Graph

## Motivation

The computed fields implementation (see `COMPUTED-FIELDS-SPEC.md`, "Cross-Record Cycles") revealed that L4 lacks a global dependency graph. Intra-record cycles between computed fields are caught at compile time, but cross-record cycles through top-level bindings are only caught at runtime by the lazy evaluator's blackhole detection. A global dependency graph would close this gap and enable several other analyses.

## Goals

A single global dependency graph supports three analyses:

1. **Cross-record cycle detection** — find strongly connected components involving computed fields across multiple DECLARE blocks and top-level DECIDE/MEANS definitions
2. **Dead code identification** — flag definitions that are unreachable from any entry point (LSP warning, not a hard error)
3. **Tree-shaking** — prune unreachable definitions from compilation output (JSON schema, decision service, web app generation)

All three are different queries over the same graph.

## Graph Structure

### Nodes

Every named definition is a node:

| Node kind | Source |
|-----------|--------|
| Top-level function | `DECIDE f ...` / `MEANS` at top level |
| Computed field | `DECLARE R HAS f IS A T MEANS expr` (desugared to synthetic DECIDE) |
| Stored field selector | `DECLARE R HAS f IS A T` |
| Type constructor | `DECLARE R HAS ...` / `DECLARE E IS ONE OF ...` |
| ASSUME parameter | `ASSUME x IS A T` |

### Edges

A directed edge `A → B` means "evaluating A may require evaluating B":

| Edge source | Edge target | How to detect |
|-------------|-------------|---------------|
| Function body | Variable reference | `Var`/`App` nodes in the expression |
| Function body | Field projection | `Proj _ expr field` — edge to the selector `field` AND to whatever `expr` resolves to |
| Computed field MEANS | Sibling field | Bare name reference (already detected by `exprFieldRefs`) |
| Computed field MEANS | Top-level definition | `Var`/`App` referencing a global name |
| Computed field MEANS | Nested projection | `expr's field` chains |

### Entry Points

Entry points are the roots for reachability analysis:

- `#EVAL` directives
- `#CHECK` directives
- Exported functions (decision service API)
- Contract directives

## Analysis Passes

### Pass 1: Build the Graph

Walk the full resolved AST (post-type-checking, so names are resolved to unique IDs). For each definition, collect its free-variable references as outgoing edges.

**Timing:** After type checking, before evaluation. The resolved AST has unique IDs on every name, eliminating ambiguity.

**Key insight:** This must run on the **resolved** AST (after type checking), not the parsed AST, because name resolution is needed to distinguish between local bindings and global references. The intra-record cycle detection in `Desugar.hs` works on the parsed AST because it only needs to match sibling field names, but the global graph needs fully resolved names.

### Pass 2: Cycle Detection

Run Tarjan's algorithm (or `Data.Graph.stronglyConnComp`) on the graph. Any SCC with more than one node, or a single node with a self-edge, is a cycle.

Report cycles involving at least one computed field as errors. Cycles involving only top-level functions are already possible in L4 (mutual recursion is valid) — only flag cycles that pass through computed fields, since those indicate non-terminating derived attributes.

**Open question:** Should mutual recursion between top-level functions that happens to pass through a computed field projection always be an error? Or only when the cycle is "tight" (every node in the SCC is a computed field)?

### Pass 3: Reachability / Dead Code

Starting from entry points, compute the reachable set (BFS/DFS). Definitions not in the reachable set are dead code.

**Severity:** Warning, not error. Dead code may be intentional (library definitions, work in progress).

**LSP integration:** Grey out unreachable definitions. Offer "Remove unused definition" quick fix.

### Pass 4: Tree-Shaking (Optional)

For compilation targets that benefit from smaller output (JSON schema, web app generation), include only the reachable set. This is a straightforward filter once reachability is computed.

## Implementation Sketch

### Data Types

```haskell
-- | A node in the global dependency graph.
data DepNode
  = DepFunction Unique        -- top-level DECIDE/MEANS
  | DepComputedField Unique   -- computed field (synthetic DECIDE)
  | DepSelector Unique        -- stored field selector
  | DepConstructor Unique     -- type constructor
  | DepAssume Unique          -- ASSUME parameter
  deriving (Eq, Ord)

-- | The global dependency graph.
type DepGraph = Map DepNode (Set DepNode)
```

### Where It Lives

New module `L4.DependencyGraph` in `jl4-core`. Depends on the resolved AST (`Module Resolved`) and the entity info from type checking.

### Integration Points

| Caller | Purpose |
|--------|---------|
| `TypeCheck.hs` / `doCheckProgramWithDependencies` | Run cycle detection after type checking; emit errors for computed-field cycles |
| `jl4-lsp` / LSP Rules | Dead code warnings, grey-out unreachable definitions |
| `JsonSchema.hs` | Tree-shake definitions not reachable from exported functions |
| `EvaluateLazy` | Potentially skip evaluation of unreachable definitions |

## Relation to Existing Code

- **`Desugar.hs:detectComputedFieldCycles`** — the intra-record cycle detection would remain as a fast, early check (runs before type checking on the parsed AST). The global graph is a more expensive check that runs after type checking and subsumes it for cross-record cases.
- **`Desugar.hs:exprFieldRefs`** — the `Foldable`-based name extraction could be reused or generalized for the global graph builder.
- **`EvaluateLazy/Machine.hs` blackhole detection** — remains as a runtime safety net. Even with compile-time detection, the blackhole mechanism catches cycles that arise from dynamic dispatch or patterns the static analysis is too conservative to prove safe.

## Estimated Scope

| Component | Effort |
|-----------|--------|
| `L4.DependencyGraph` module (graph building) | Medium — walk resolved AST, collect edges |
| Cycle detection (cross-record) | Small — `stronglyConnComp` on the built graph |
| Dead code warnings | Small — BFS from entry points |
| LSP integration (grey-out) | Medium — wire into diagnostics |
| Tree-shaking for JSON schema | Small — filter by reachable set |

## Open Questions

1. **Granularity of cycle errors:** Should the error message show the full cycle path (e.g., `Foo.x → globalBar → Bar.y → globalFoo → Foo.x`), or just list the involved definitions?

2. **Cross-module cycles:** If module A imports module B and both have computed fields that reference each other, should this be detected? The import resolution already detects cyclic imports, but computed field cycles could exist within a valid import DAG if the references are indirect.

3. **Performance:** For large codebases, building the full dependency graph after every edit may be expensive. Consider incremental updates (only rebuild subgraphs for changed modules) or lazy construction (build on demand for specific analyses).
