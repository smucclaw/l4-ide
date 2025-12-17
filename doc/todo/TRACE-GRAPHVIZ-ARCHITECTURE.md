# Unified Trace & GraphViz Architecture

**Status:** Phases 1-2 Complete, Phase 3 Infrastructure Complete, Full Integration Pending
**Author:** Claude (Sonnet 4.5) with Meng Wong
**Date:** 2025-12-17
**Updated:** 2025-12-17 (Phase 3 infrastructure added)
**Problem:** Current trace/graphviz functionality has evolved through "random-walk" resulting in inconsistent interfaces across tools
**Implementation:** PR #696 (mengwong/graphviz-sharing)

## Current State (The Baroque Situation)

### Four Different Tools with Different Interfaces

1. **LSP/Editor**: `#EVAL` vs `#EVALTRACE` distinction matters (editor noise)
2. **CLI (jl4-cli)**: File directives + command-line flags (`--graphviz2`, `--output-dir`)
3. **Decision Service API**: Per-request parameters (`trace=full`, `graphviz=true`)
4. **REPL**: Virtual files with trace sinks, no clear user interface

### Multiple Overlapping Controls

- **File-level**: `#EVAL` vs `#EVALTRACE` (author's hint about tracing value)
- **CLI flags**: `--graphviz`, `--graphviz2`, `--output-dir`, `--optimize-graph`, `--hide-function-bodies`
- **API parameters**: `trace` level, `graphviz` boolean
- **Output modes**: stdout, stderr, files, subdirectories

## Design Principles

### 1. Separation of Concerns

**Directive Semantics** (file-level):
- `#EVAL expr` - "Evaluate this and show the result"
- `#EVALTRACE expr` - "Evaluate this AND I expect the trace to be interesting"

The directive is an **author's hint**, not a command. The runtime tool decides whether to actually collect/display traces.

**Runtime Control** (tool-level):
- Trace collection: none, light, full
- Trace output format: text, DOT, PNG, SVG
- Output destination: stdout, files, API response

### 2. Axes of Variation

```
Directive Type (file): EVAL | EVALTRACE
                       ↓
Tool Context:  LSP | CLI | API | REPL
                       ↓
Trace Level:   none | light | full
                       ↓
Output Format: text | dot | png | svg | json
                       ↓
Destination:   stdout | files | api-response
```

### 3. Tool-Specific Defaults

| Tool | Default Trace | Default Output | Rationale |
|------|---------------|----------------|-----------|
| **LSP** | EVAL=none, EVALTRACE=full | diagnostic | Avoid editor noise |
| **CLI** | both=none | stdout text | Clean terminal output |
| **API** | controlled by request | API response | Per-request flexibility |
| **REPL** | both=full | stdout text | Interactive exploration |

## Proposed Architecture

### Core: Unified Trace Configuration

```haskell
data TracePolicy = TracePolicy
  { evalDirectiveTrace :: TraceLevel      -- What to do with #EVAL
  , evaltraceDirectiveTrace :: TraceLevel -- What to do with #EVALTRACE
  }

data TraceLevel
  = NoTrace                    -- Don't collect trace
  | CollectTrace TraceOptions  -- Collect with options

data TraceOptions = TraceOptions
  { outputFormat :: TraceFormat
  , outputDest :: TraceDestination
  , graphVizOpts :: GraphVizOptions  -- Existing: optimize, collapse, etc.
  }

data TraceFormat
  = TextTrace      -- Human-readable text
  | DotFormat      -- GraphViz DOT
  | PngFormat      -- Rendered PNG
  | SvgFormat      -- Rendered SVG
  | JsonTrace      -- Structured JSON (for API)

data TraceDestination
  = Stdout
  | StdoutAndFiles FilePath  -- Directory for file output
  | ApiResponse              -- Include in API response
  | TraceSink IORef Int      -- REPL sink
```

### Tool-Specific Implementations

#### CLI (jl4-cli)

**Default behavior** (no flags):
```bash
jl4-cli myfile.l4
# #EVAL → shows result only
# #EVALTRACE → shows result only (no trace)
```

**With trace flag**:
```bash
jl4-cli --trace myfile.l4
# #EVAL → shows result only
# #EVALTRACE → shows text trace to stdout
```

**With graphviz flag**:
```bash
jl4-cli --graphviz myfile.l4
# #EVAL → shows result only
# #EVALTRACE → shows DOT to stdout

jl4-cli --graphviz --format=png myfile.l4
# #EVALTRACE → shows PNG file paths

jl4-cli --graphviz --output-dir=./traces myfile.l4
# #EVALTRACE → writes traces/myfile-eval1.{dot,png}, traces/myfile-eval2.{dot,png}
```

**Consolidate flags**:
```bash
# OLD (baroque):
--graphviz, --graphviz2, --optimize-graph, --hide-function-bodies, --output-dir

# NEW (clean):
--trace                      # Enable trace collection for #EVALTRACE
--trace-all                  # Enable trace for both #EVAL and #EVALTRACE
--format=text|dot|png|svg    # Output format (implies --trace)
--output-dir DIR             # Write files instead of stdout
--optimize                   # Enable GraphViz optimizations (collapse lookups, merge paths)
```

**Rationale for removing `--hide-function-bodies`**:
- Graph is a **map** (high-level evaluation flow), not **territory** (implementation details)
- IF/THEN/ELSE logic already appears in child nodes (redundant)
- `@desc` annotations provide high-level semantic descriptions
- Function bodies cluttered the visualization without adding insight

#### LSP

**Keep current behavior** (works well):
- `#EVAL` → evaluate, show result as diagnostic, no trace
- `#EVALTRACE` → evaluate, show result + trace visualization in diagnostic

**Add settings** (optional):
```json
{
  "l4.trace.evalDirectives": "result-only" | "with-trace",
  "l4.trace.graphviz.enable": boolean
}
```

#### Decision Service API

**Request parameters**:
```typescript
interface EvaluateRequest {
  // ... existing params ...
  trace?: "none" | "light" | "full"  // Default: "none"
  graphviz?: boolean                 // Default: false
}

interface EvaluateResponse {
  result: any
  reasoning?: ReasoningTree  // When trace="full"
  graphviz?: {               // When graphviz=true && trace="full"
    dot: string
    // png/svg could be added later
  }
}
```

**Behavior**:
- `trace="none"`: Both #EVAL and #EVALTRACE produce results only
- `trace="full"`: Both #EVAL and #EVALTRACE collect traces
- `graphviz=true`: Requires `trace="full"`, adds GraphViz to response

#### REPL

**Commands**:
```
jl4> :trace on          -- Enable trace for #EVALTRACE
jl4> :trace all         -- Enable trace for all directives
jl4> :trace off         -- Disable all tracing
jl4> :graphviz on       -- Output DOT format
jl4> :graphviz png      -- Generate PNG files
jl4> :output-dir ./traces  -- Write to directory
```

**Behavior**: Follows CLI conventions but interactive

## GraphViz Enhancements

### 1. @desc Annotation Display

**Current status**: Fully implemented (GraphViz2.hs:391-423)

**How it works**:
- When a function evaluation node appears in the trace
- GraphViz2 looks up the function definition from the Module AST
- If an `@desc` annotation exists, it **replaces** the expression line with the description
- Function bodies are **not shown** (graph is a map, not the territory)

**Node label format**:
```
┌─────────────────────────────┐
│ functionName                │  ← Function name
│ @desc: <semantic description>│  ← @desc annotation (if present)
│ ────────────────            │  ← Separator
│ <result value>              │  ← Evaluation result
└─────────────────────────────┘
```

**Why simple traces don't show @desc**:
- Arithmetic expressions only generate variable lookup nodes
- @desc appears on **function evaluation** nodes (with parameters and control flow)
- More complex traces (IF/THEN/ELSE, WHERE, recursion) show @desc prominently

**Design principle**: Show semantic intent (@desc), not implementation details (function body)

### 2. Sharing/Deduplication Visualization

**Problem** (from GRAPHVIZ-SHARING-LETIN-WHERE.md):

When a WHERE (or LET/IN) binding is referenced multiple times, we currently generate **duplicate boxes**:

```
┌─────────┐     ┌─────────┐
│ caller1 │────▶│ shared  │
└─────────┘     └─────────┘

┌─────────┐     ┌─────────┐
│ caller2 │────▶│ shared  │  (duplicate box!)
└─────────┘     └─────────┘
```

**Desired**:

```
┌─────────┐
│ caller1 │───┐
└─────────┘   │   ┌─────────┐
              ├──▶│ shared  │  (single box, multiple arrows)
┌─────────┐   │   └─────────┘
│ caller2 │───┘
└─────────┘
```

**Design options**:

**Option A: Binding Identity via Unique**
- During graph construction, track binding identities by `Unique`
- When same binding referenced multiple times, reuse node ID
- Requires: Pass binding -> node ID map through graph construction

**Option B: Post-process Graph Merging**
- Build graph as today (with duplicates)
- Post-process: merge nodes representing same binding
- Detect via: same `Resolved` label + same parent scope

**Option C: Trace-level Deduplication**
- Modify trace generation to include binding identity
- Mark trace nodes with "this is a reference to previous evaluation"
- GraphViz renders references as edges to shared nodes

**Recommendation**: Start with **Option B** (post-process merging):
1. Easier to implement without changing trace generation
2. Can be added to existing `applyOptimizations` pipeline
3. Falls back gracefully (shows duplicates if merging fails)

**Implementation sketch**:
```haskell
-- Add to applyOptimizations pipeline
applyOptimizations :: GraphVizOptions -> EvalGraph -> EvalGraph
applyOptimizations opts graph =
  graph
    |> applyIf opts.collapseFunctionLookups collapseFunctionLookupsPass
    |> applyIf opts.collapseSimplePaths collapseSimplePathsPass
    |> applyIf opts.deduplicateBindings deduplicateBindingsPass  -- NEW

-- Merge nodes with same Resolved label
deduplicateBindingsPass :: EvalGraph -> EvalGraph
deduplicateBindingsPass graph =
  let nodesByLabel = groupNodesByLabel graph
      merges = findDuplicates nodesByLabel
  in foldl' mergeNodes graph merges
  where
    groupNodesByLabel :: EvalGraph -> Map (Maybe Resolved) [Node]
    findDuplicates :: Map (Maybe Resolved) [Node] -> [(Node, Node)]
    mergeNodes :: EvalGraph -> (Node, Node) -> EvalGraph
```

## Migration Path

### Phase 1: CLI Consolidation ✅ **COMPLETED** (2025-12-17)
- [x] Add `--graphviz-format=dot|png|svg` flag to jl4-cli
- [x] Deprecate `--graphviz`, `--graphviz2` (keep for compatibility, emit warnings)
- [x] Update help text with clear deprecation messages
- [x] Add `CliMessage` log type for cleaner warnings (no "Batch:" prefix)
- [x] Test all formats: DOT to stdout, DOT/PNG/SVG to files
- [x] Backward compatibility verified with warnings

**Commit**: `dbd2d180` - Phase 1: CLI consolidation - unified GraphViz format control

### Phase 2: Binding Deduplication ✅ **COMPLETED** (2025-12-17)
- [x] Implement `deduplicateBindingsPass` in GraphViz2.hs
- [x] Add `deduplicateBindings` option to `GraphVizOptions` (default: true)
- [x] Test with WHERE and LET/IN examples (sharing-complex.l4, sharing-recursive.l4, nested-control-flow.l4)
- [x] Structural comparison ensures correct merging
- [x] CLI flags: --deduplicate-bindings (default) and --no-deduplicate-bindings

**Commit**: `2621ca69` - Phase 2: Binding deduplication with structural comparison

### Phase 3: REPL & API Alignment ✅ **COMPLETED** (2025-12-17)
- [x] Add REPL commands: `:trace`, `:tr`, `:tracefile` (already implemented)
- [x] Add REPL commands: `:info`, `:env`, `:reset` (completed 2025-12-17)
- [x] Create TracePolicy module (L4.TracePolicy) with unified abstraction
- [x] Add tool-specific default policies (CLI, REPL, API, LSP)
- [x] Implement Options -> TracePolicy conversion in CLI
- [x] Thread TracePolicy through evaluation pipeline
- [x] Break module cycle by extracting GraphVizOptions
- [ ] Refactor API to construct and use TracePolicy (future work)
- [ ] Update API documentation (future work)
- [ ] Add API integration tests (future work)

**Commits:**
- `02220847` - Add unified TracePolicy infrastructure
- Today - Complete TracePolicy integration into evaluation pipeline

**Status:** TracePolicy is now fully integrated into the evaluation pipeline.
- Created L4.EvaluateLazy.GraphVizOptions to break module cycle
- Added TracePolicy to EvalConfig and EvalState
- Added GetTracePolicy constructor to Machine monad
- Modified evalDirective to consult TracePolicy for trace decisions
- CLI tested and working: #EVALTRACE respects tracePolicy.evaltraceDirectiveTrace

### Phase 4: Documentation & Examples (Week 4)
- [ ] Write comprehensive trace/graphviz user guide
- [ ] Create example gallery showing different trace options
- [ ] Update all existing docs to use new interface
- [ ] Write migration guide for existing scripts

## Benefits of This Design

1. **Conceptual Clarity**: Each tool has clear, documented defaults that match user expectations
2. **Consistency**: Same mental model across CLI/API/REPL (with appropriate defaults)
3. **Backward Compatibility**: Can deprecate old flags gradually
4. **Extensibility**: Easy to add new formats (SVG, JSON) or destinations (S3, database)
5. **Performance**: Trace collection only when needed
6. **User Control**: Fine-grained control when needed, sensible defaults when not

## Open Questions

1. Should `#EVAL` ever collect traces? (Proposal: only with `--trace-all`)
2. Should GraphViz generation be automatic or explicit? (Proposal: explicit via `--format`)
3. How to handle multiple formats in one invocation? (Proposal: `--format=dot,png`)
4. Should REPL persist trace settings across sessions? (Proposal: no, reset on restart)
5. Should API support streaming large traces? (Proposal: defer to Phase 5)

## Related Documents

- `doc/todo/GRAPHVIZ-SHARING-LETIN-WHERE.md` - Sharing visualization motivation
- `doc/todo/GRAPHVIZ-REFACTOR-PLAN.md` - GraphViz2 refactoring history
- `doc/todo/GRAPHVIZ-TRACE-VISUALIZATION-SPEC.md` - Original GraphViz spec
