# GraphViz Evaluation Trace Visualization

**Status:** Phase 3 Complete (REPL Integration); Phase 4 In Progress (Decision Service graphviz flag + batch output live, docs pending)  
**Priority:** Medium  
**Affects:** jl4-core, jl4-cli, jl4-repl, jl4-decision-service  
**Related Issues:** #691

## Overview

This feature adds GraphViz DOT format visualization for L4 function evaluation traces, enabling developers to visually debug lazy evaluation execution paths and understand how expressions are computed.

When debugging or explaining L4 function behavior, users need to see:

- Which execution path was taken through conditionals
- How values flow through nested function calls
- Which pattern branches matched in CONSIDER expressions
- Why certain branches were not evaluated (lazy evaluation)

Currently, EVALTRACE produces text output with box-drawing characters. GraphViz visualization provides:

- Clearer visual structure
- Better understanding of complex nested evaluations
- Interactive exploration (with GraphViz tools like `xdot`)
- Exportable diagrams for documentation

## Background: L4 Evaluation Traces

### EvalTrace Data Structure

**Location:** `jl4-core/src/L4/EvaluateLazy/Trace.hs`

```haskell
data EvalTrace =
    Trace
      (Maybe Resolved)                     -- optional binder label
      [(Expr Resolved, [EvalTrace])]       -- steps and their sub-traces
      (Either EvalException NF)           -- final result (or exception)
```

Each trace node contains:

- Optional binding metadata (top-level `DECIDE`, `WHERE` function, etc.)
- List of expressions evaluated with their sub-traces
- Final result (either exception or normal form value)

The recorder tags nodes with binder names by watching `AllocPre` events (which fire when we reserve heap slots for named definitions). That metadata now flows all the way to the GraphViz renderer so a node shows `finalScore`, `tierBonus`, etc., instead of only the body expression.

### Current Text Rendering

Example from `lazytrace2.l4`:

```
┌ sumList OF (LIST 1, 2, 3)
│┌ LIST 1, 2, 3
│├ 1 FOLLOWED BY (LIST 2, 3)
││┌ LIST 2, 3
││├ 2 FOLLOWED BY (LIST 3)
│││┌ LIST 3
│││├ 3 FOLLOWED BY (LIST )
││││┌ LIST
││││└ EMPTY
│││└ LIST 3
││└ LIST 2, 3
│└ LIST 1, 2, 3
└ 6
```

### Lazy Evaluation Implications

L4 uses lazy evaluation, so traces show:

- **Only the executed path** - not the full decision tree
- **Short-circuit evaluation** - OR/AND stop when result is determined
- **Pattern matching order** - CONSIDER branches tried sequentially
- **Unevaluated branches** - structurally visible but without computed values

This differs from eager evaluation, which would show all branches.

### Structures That Get Traced

1. **IF-THEN-ELSE**: Shows condition evaluation, highlights taken branch
2. **CONSIDER/WHEN**: Shows pattern matching, highlights matched branch
3. **Function applications**: Shows arguments and recursive calls
4. **Binary operations**: Shows left-to-right evaluation with short-circuiting
5. **List operations**: Shows cons cells and recursive processing

### Evaluation Trace Primer (2025-02-17)

Think of an evaluation trace as a call graph that only captures the _path actually taken_. Each node is the precise expression that ran, annotated with its final value (or exception) and, when available, the name of the binder (`finalScore`, `tierBonus`, etc.). Edges point to the sub-expressions the interpreter forced, so you can replay the computation top-down: “we were evaluating `qualifies` → that forced `walks`, `drinks`, `eats` → the `CONSIDER` picked branch 2…”. Compared to traditional function-call graphs or data-flow diagrams, traces are execution-order snapshots—no speculative branches, no unforced thunks—so short-circuits, lazy skips, and pattern failures pop out visually. That makes them a friendly on-ramp for people who already understand flowcharts: the boxes and arrows here are the literal proof of how a legal rule fired. In that sense, these diagrams are our version of explainable AI: deterministic, reproducible, and easy to narrate to a policy stakeholder or auditor.

## Architecture

### Core Module (Phase 1) ✅ COMPLETE

**Location:** `jl4-core/src/L4/EvaluateLazy/GraphViz.hs`

The core GraphViz module is standalone and reusable across all L4 tools:

```haskell
module L4.EvaluateLazy.GraphViz
  ( traceToGraphViz
  , GraphVizOptions(..)
  , defaultGraphVizOptions
  ) where

-- Convert an EvalTrace to GraphViz DOT format
traceToGraphViz :: GraphVizOptions -> EvalTrace -> Text

-- Configuration options for visualization
data GraphVizOptions = GraphVizOptions
  { includeValues :: Bool      -- Include computed values in nodes
  , simplifyTrivial :: Bool    -- Collapse trivial traces
  , showUnevaluated :: Bool    -- Show unevaluated thunks
  , maxDepth :: Maybe Int      -- Limit tree depth
  }

defaultGraphVizOptions :: GraphVizOptions
```

**Design Principles:**

1. **Modular** - Pure function, no dependencies on CLI/REPL/Decision Service
2. **Configurable** - Options allow different visualization modes
3. **Readable** - DOT output is human-readable and can be tweaked manually
4. **Standalone** - Can be used independently via library import

**Visual Design:**

- Blue nodes (`lightblue`) - Successful value computations
- Pink nodes (`lightpink`) - Error results
- Gray nodes (`lightgray`) - Depth-limited truncated nodes
- Green edges (`darkgreen`) - Evaluation dependencies
- Rounded rectangles for better readability

**Binder-aware labels (2025-02-15)**

- `EvalTrace` now carries the `Resolved` name for every thunk that corresponds to a `DECIDE`/`WHERE` binding. Graph construction reads those labels, so each node headline shows the function name (e.g., `finalScore`, `tierBonus`, `metrics`) before the expression summary.
- The trace builder gathers these names from `AllocPre` actions, so no runtime heuristics are required; it is literally the definition the interpreter forced.
- Result: the showcase diagrams finally read like call graphs instead of anonymous `sumList OF metrics` recursion ladders. See `doc/images/trace-showcase.{png,svg}` for the updated screenshots.
- The decision service piggy-backs on the same metadata—`reasoning.exampleCode` now includes the binder name as the first line, so API consumers see “finalScore” / “tierBonus” in the JSON tree without having to guess from the expression text.
- Root nodes now inherit the label of the first forced binder when we evaluate a top-level directive such as `#EVALTRACE evaluation alice`. That means “evaluation OF alice” immediately calls out `finalScore` in the label, and the graph edges start at the meaningful definition instead of an opaque “(+ 2 more)” placeholder.
- List literals are rendered as true variadic nodes: we hide the internal `FOLLOWED BY` scaffolding that the lazy evaluator produces and attach the trace edges directly to each list element. Inline list expressions authored by users still appear as their own boxes; only the synthetic suffix nodes created by the desugarer disappear. The net effect is that `metrics` now has exactly four children (`baseScore`, `experienceBonus`, `roleBonus`, `dependentPenalty`) and no longer sprouts a distracting staircase of partial `LIST ...` suffixes.

### CLI Integration (Phase 2) ✅ COMPLETE

**Location:** `jl4/app/Main.hs`

**Command-line flag:**

```bash
jl4-cli --graphviz file.l4
jl4-cli -g file.l4
```

**Implementation:**

```haskell
-- Added to Options data type
data Options = Options
  { ...
  , outputGraphViz :: Bool
  }

-- CLI parsing
<*> switch
  ( long "graphviz"
  <> short 'g'
  <> help "Output evaluation trace as GraphViz DOT format"
  )

-- Integration with evaluation
when options.outputGraphViz $
  forM_ mEval $ \evalResults ->
    forM_ evalResults $ \result ->
      let MkEvalDirectiveResult {trace = mtrace} = result
      in case mtrace of
        Just tr -> liftIO $ Text.IO.putStrLn $
          GraphViz.traceToGraphViz GraphViz.defaultGraphVizOptions tr
        Nothing -> pure ()
```

**Usage:**

```bash
# Generate DOT output
jl4-cli --graphviz example.l4 > trace.dot

# Render to SVG
jl4-cli --graphviz example.l4 | dot -Tsvg > trace.svg

# Render to PNG
jl4-cli --graphviz example.l4 | dot -Tpng > trace.png

# Interactive viewing with xdot
jl4-cli --graphviz example.l4 | xdot -
```

### REPL Integration (Phase 3) ✅ COMPLETE

**Status:** `jl4-repl/app/Main.hs` now exposes `:trace` / `:tr`, trace-aware evaluation helpers, DOT formatting utilities, and the new `:tracefile` command that redirects traces into numbered `.dot` files with metadata headers on branch `visualize-logic-graph`.

**Verification so far (2025-02-15):**

- `cabal build jl4-repl`
- `printf ":trace 1\n:quit\n" | cabal run jl4-repl` (ensures the command path executes and gracefully errors when no file is loaded)

Remaining validation: interactive `xdot` run (requires GUI) and broader sample coverage if we add more fixtures.

#### Module Import (✅ DONE)

**Location:** `jl4-repl/app/Main.hs:28`

```haskell
import qualified L4.EvaluateLazy.GraphViz as GraphViz
```

#### Commands Implemented

**1. `:trace` Command Handler**  
`processInput` case expression, `jl4-repl/app/Main.hs:108-123`

```haskell
  | ":trace " `Text.isPrefixOf` stripped = do
      let expr = Text.strip $ Text.drop 7 stripped
      case st.loadedFile of
        Nothing -> pure ("No file loaded. Use :load <file> first.", st, False)
        Just fp -> do
          result <- evalWithTrace st fp expr
          pure (result, st, False)
```

**2. `:tr` Short Form**  
`processInput`, `jl4-repl/app/Main.hs:124-135`

```haskell
  | ":tr " `Text.isPrefixOf` stripped = do
      let expr = Text.strip $ Text.drop 4 stripped
      case st.loadedFile of
        Nothing -> pure ("No file loaded. Use :load <file> first.", st, False)
        Just fp -> do
          result <- evalWithTrace st fp expr
          pure (result, st, False)
```

**3. Helper Function `evalWithTrace`**  
Defined alongside the existing evaluation helpers, `jl4-repl/app/Main.hs:166-236`

```haskell
-- | Evaluate an expression and show its GraphViz trace
evalWithTrace :: ReplState -> FilePath -> Text -> IO Text
evalWithTrace st contextFile exprText = do
  -- Get unique counter
  evalNum <- atomicModifyIORef' st.evalCounter (\n -> (n + 1, n))

  -- Build directive (use #EVALTRACE for explicit tracing)
  let actualExpr = if any (`Text.isPrefixOf` exprText) ["#EVAL", "#EVALTRACE", "#CHECK", "#ASSERT"]
                   then exprText
                   else "#EVALTRACE " <> exprText

  -- Get imports
  importPreamble <- getImportPreamble st

  -- Get filtered source
  let contextUri = normalizedFilePathToUri (toNormalizedFilePath contextFile)
  [mTc] <- shakeRunDatabase st.ideState.shakeDb [Shake.use Rules.SuccessfulTypeCheck contextUri]
  originalContent <- case mTc of
    Just tc -> pure $ Print.prettyLayout (filterIdeDirectives tc.module')
    Nothing -> do
      mContent <- Shake.getVirtualFileText st.ideState contextUri
      case mContent of
        Just content -> pure content
        Nothing -> Text.IO.readFile contextFile

  let replContent = importPreamble <> originalContent <> "\n\n-- REPL trace " <> Text.pack (show evalNum) <> "\n" <> actualExpr <> "\n"

  -- Create virtual file
  let replPath = st.curDir <> "/.repl_trace_" <> show evalNum <> ".l4"
      replNfp = toNormalizedFilePath replPath
      replUri = normalizedFilePathToUri replNfp

  _ <- shakeRunDatabase st.ideState.shakeDb [Shake.addVirtualFile replNfp replContent]

  -- Type check and evaluate
  [mtc] <- shakeRunDatabase st.ideState.shakeDb [Shake.use Rules.SuccessfulTypeCheck replUri]
  case mtc of
    Nothing -> pure "Failed to type check expression"
    Just tc | not tc.success -> do
      let errors = tc.infos
      pure $ "Type error:\n" <> Text.unlines (map (Text.pack . show) $ take 3 errors)
    Just _tc -> do
      -- Get evaluation results WITH trace
      [meval] <- shakeRunDatabase st.ideState.shakeDb [Shake.use Rules.EvaluateLazy replUri]
      case meval of
        Nothing -> pure "Evaluation failed"
        Just [] -> pure "(no result)"
        Just results -> pure $ formatTraceResults results

-- | Format evaluation results showing GraphViz DOT trace
formatTraceResults :: [EvalDirectiveResult] -> Text
formatTraceResults results = Text.unlines $ map formatTraceResult results

formatTraceResult :: EvalDirectiveResult -> Text
formatTraceResult (MkEvalDirectiveResult _range _res mtrace) = case mtrace of
  Nothing -> "(no trace available)"
  Just tr -> GraphViz.traceToGraphViz GraphViz.defaultGraphVizOptions tr
```

**4. Help Text Update**  
`helpText`, `jl4-repl/app/Main.hs:323-351`

```haskell
  , "  :trace <expr>   Show evaluation trace as GraphViz DOT"
  , "  :tr <expr>      Short for :trace"
  , ""
  , "Trace visualization:"
  , "  :trace 5 cubed           Show DOT graph"
  , "  :trace 5 cubed | xdot -  View interactively"
  , "  Requires GraphViz tools (dot, xdot) installed separately"
```

#### Usage Examples

**Basic Trace Display (using `jl4/experiments/cubed-postfix.l4`):**

```
jl4> :load jl4/experiments/cubed-postfix.l4
Loaded: .../cubed-postfix.l4

jl4> :trace 5 cubed
digraph evaluation_trace {
  rankdir=TB;
  node [shape=box, style=rounded];
  node0 [label="<multiple steps>\n────────────────\n125", fillcolor="#d0e8f2", style=filled];
  node0 -> node1 [color="#2ca02c", penwidth=2];
  node0 -> node2 [color="#2ca02c", penwidth=2];
}
```

**Pipe to GraphViz (non-interactive shell session):**

```bash
# Strip the `jl4>` prompt and feed DOT straight into `dot`
printf ":trace 5 cubed\n:quit\n" \
  | jl4-repl jl4/experiments/cubed-postfix.l4 \
  | sed -n '/jl4> digraph evaluation_trace {/,/^}$/p' \
  | sed 's/^jl4> //' \
  | dot -Tsvg > /tmp/trace.svg

# Same idea for PNG
… | dot -Tpng > /tmp/trace.png

# xdot (requires GUI) can consume the filtered stream too:
… | xdot -
```

**Short Form:**

```
jl4> :tr 5 cubed
```

#### Data Flow

```
User: :trace 5 cubed
  ↓
processInput parses command
  ↓
evalWithTrace creates virtual file
  ↓
#EVALTRACE 5 cubed  (wrapped)
  ↓
Type check + Evaluate
  ↓
Extract EvalDirectiveResult.trace
  ↓
GraphViz.traceToGraphViz
  ↓
DOT text output to terminal
```

#### Architecture Notes

**REPL Infrastructure** (already exists):

- Virtual file evaluation: Creates temporary `.l4` files for REPL expressions
- Import accumulation: Session-persistent `IMPORT` statements
- Type queries: `:type` command already uses evaluation + diagnostic extraction
- Pattern matching: NoFieldSelectors extension requires explicit field extraction

**Integration Pattern** (follows existing `:type` command):

- `:type` evaluates → extracts diagnostics
- `:trace` evaluates → extracts traces
- Both use virtual files: `/.repl_eval_N.l4` and `/.repl_trace_N.l4`
- Both wrap expressions: `#EVAL` and `#EVALTRACE`

**NoFieldSelectors Pattern:**

```haskell
-- ❌ Doesn't work
result.trace

-- ✅ Required pattern
let MkEvalDirectiveResult {trace = mtrace} = result
in case mtrace of
  Just tr -> GraphViz.traceToGraphViz GraphViz.defaultGraphVizOptions tr
  Nothing -> "(no trace available)"
```

#### Testing Checklist

**Manual Testing (latest run: 2025-02-15):**

- [x] `:trace 5 cubed` in `jl4/experiments/cubed-postfix.l4` → DOT graph emitted
- [x] `:tr 5 cubed` → same DOT output as long form
- [x] `:trace foo` (undefined symbol) → `Failed to type check expression`
- [x] No file loaded → error message (`printf ":trace 1\n:quit\n" | cabal run jl4-repl`, 2025-02-15)
- [x] Pipe to `dot` → `sed`-filtered output converted to `/tmp/repl-trace.png` via `dot -Tpng`
- [x] `:trace sumList (LIST 1,2,3)` in `jl4/examples/ok/lazytrace2.l4` (multi-branch trace)
- [x] `:tracefile /tmp/repl-traces/session` → writes `/tmp/repl-traces/session-01.dot` with header comments (expression, directive, timestamp, loaded file, imports, result)
- [ ] Pipe to `xdot` → interactive viewer opens (not run in headless CLI environment)
- [x] Trace File Sink Mode (`:tracefile <prefix>`) → writes numbered `.dot` files with metadata

**Verification log (2025-02-15 snippets):**

```bash
# Long form trace
printf ":trace 5 cubed\n:quit\n" | cabal run jl4-repl jl4/experiments/cubed-postfix.l4

# Short alias
printf ":tr 5 cubed\n:quit\n" | cabal run jl4-repl jl4/experiments/cubed-postfix.l4

# Undefined symbol → type-check failure
printf ":trace foo\n:quit\n" | cabal run jl4-repl jl4/experiments/cubed-postfix.l4

# Deeper recursion sample from lazytrace2.l4
printf ":load jl4/examples/ok/lazytrace2.l4\n:trace sumList (LIST 1,2,3)\n:quit\n" \
  | cabal run jl4-repl

# Trace file sink mode (writes /tmp/repl-traces/session-01.dot)
printf ":tracefile /tmp/repl-traces/session\n:trace sumList (LIST 1,2,3)\n:tracefile off\n:quit\n" \
  | cabal run jl4-repl jl4/examples/ok/lazytrace2.l4

# Pipe through dot (strip REPL prompt so DOT starts at column 0)
printf ":trace 5 cubed\n:quit\n" \
  | cabal run jl4-repl jl4/experiments/cubed-postfix.l4 \
  | sed -n '/jl4> digraph evaluation_trace {/,/^}$/p' \
  | sed 's/^jl4> //' \
  | dot -Tpng > /tmp/repl-trace.png
```

#### Trace File Sink Mode (Implemented)

Console output remains the default, but `:tracefile <prefix>` now toggles a persistent sink that stores every subsequent trace as `<prefix>-NN.dot` (zero-padded, starting at 01). Highlights:

- Directories are created automatically (via `createDirectoryIfMissing` on `takeDirectory prefix`)
- Each saved trace prints `Saved trace to …` so users know which file to inspect
- `:tracefile off` reverts to stdout streaming without touching existing files
- `.dot` files begin with metadata comments so tooling can correlate context:

```dot
// Expression: sumList (LIST 1,2,3)
// Directive: #EVALTRACE sumList (LIST 1,2,3)
// Timestamp: 2025-12-14T23:45:07Z
// Loaded file: /Users/.../jl4/examples/ok/lazytrace2.l4
// Imports: (none)
// Result: 6

digraph evaluation_trace {
  ...
}
```

The comment block includes the raw REPL expression, the fully wrapped directive (showing when `#EVALTRACE` was auto-inserted), ISO-8601 UTC timestamp, active `.l4` file, accumulated `IMPORT`s (or `(none)`), and a one-line summary of the evaluation result or error. That context makes it easy to feed the DOT body into `dot`, `xdot`, or viz.js later without digging through terminal history. Future enhancements (append/resume numbering, JSON manifests) can layer on top of the current UX if needed.

#### Fixture Library: `jl4/examples/ok/lazytrace*.l4`

The `jl4/examples/ok` tree already ships several `lazytrace*.l4` samples that cover CONSIDER branches, list recursion, exceptions, and arithmetic mixes. They provide richer validation than the tiny `cubed` demo:

- `lazytrace.l4` — exercises nested list recursion (`sum (everyOther example)`)
- `lazytrace2.l4` — mixes booleans, algebraic data, and multiple `#EVALTRACE` directives
- `lazytrace-exception.l4` — shows how failing branches appear
- Golden expectations live under `jl4/examples/ok/tests/lazytrace*.golden`

Suggested manual/CLI recipes:

```bash
# REPL: capture a deeper trace (works today)
printf ":load jl4/examples/ok/lazytrace2.l4\n:trace sumList (LIST 1,2,3)\n:quit\n" \
  | cabal run jl4-repl

# CLI flag paired with sed filter for pretty DOT diffing
cabal run jl4-cli -- --graphviz jl4/examples/ok/lazytrace2.l4 \
  | dot -Tsvg > /tmp/lazytrace2.svg

# Compare against existing golden text (should remain semantically consistent even if DOT styling evolves)
sed -n '/digraph/,/^}/p' jl4/examples/ok/tests/lazytrace2.golden
```

**Integration Tests:**

```bash
echo ":trace 5 cubed" \
  | jl4-repl jl4/experiments/cubed-postfix.l4 \
  | sed -n '/jl4> digraph evaluation_trace {/,/^}$/p' \
  | sed 's/^jl4> //' \
  | dot -Tpng > test.png
# Verify PNG created and valid
```

### Decision Service Integration (Phase 4) 📋 OPTIONAL

**Potential Use Cases:**

1. **Debug endpoint** - API endpoint to get trace visualization:

   ```
   GET /api/trace/{function}?args={json}
   Response: SVG or DOT content
   ```

2. **Web UI enhancement** - Embed trace visualization in web interface
3. **Audit trail** - Include trace diagrams in execution logs

**Design Considerations:**

- Should traces be generated on-demand or cached?
- What are the performance implications for production?
- How to handle large/complex traces in web context?
- Security implications of exposing evaluation internals?

## Implementation Status

### ✅ Phase 1: Core GraphViz Module (COMPLETE)

- [x] Create `L4.EvaluateLazy.GraphViz` module
- [x] Implement `traceToGraphViz` function
- [x] Add `GraphVizOptions` configuration
- [x] Define visual styling (colors, shapes, layout)
- [x] Add to `jl4-core.cabal` exposed modules
- [x] All tests passing

**Files Modified:**

- `jl4-core/src/L4/EvaluateLazy/GraphViz.hs` (new, 139 lines)
- `jl4-core/jl4-core.cabal` (added to exposed-modules)

### ✅ Phase 2: CLI Integration (COMPLETE)

- [x] Add `--graphviz` / `-g` command-line flag
- [x] Add `outputGraphViz` field to `Options` data type
- [x] Import GraphViz module in Main.hs
- [x] Extract trace from `EvalDirectiveResult`
- [x] Handle NoFieldSelectors pattern matching
- [x] Output DOT format to stdout
- [x] Verify build succeeds
- [x] Test CLI flag appears in help

**Files Modified:**

- `jl4/app/Main.hs` (imports, Options type, parser, integration logic)

**Usage Example:**

```bash
cabal run jl4-cli -- --graphviz test.l4 | dot -Tsvg > trace.svg
```

#### Trace Output Gating (2025-12-15)

- Added `--trace MODE` flag to `jl4-cli` where `MODE ∈ {full, none}` (default `full`). The flag controls whether textual `#EVALTRACE` output is embedded in the new evaluation summaries.
- `--graphviz/-g` still streams DOT snapshots. Users can now combine the flags (e.g. `--trace=none --graphviz`) to keep machine-friendly DOT output without duplicating the ASCII trace in terminal logs.
- Evaluation summaries are rendered explicitly (result header + optional trace block) instead of relying on Shake logger side-effects, so future consumers (batch tooling, scripting) can pick the format they need.
- Known limitation: the Shake diagnostics that power IDE/LSP scenarios still emit their own `Source: eval` entries. Those remain visible for now even when `--trace=none`, and we have a follow-up task to hide them once the logger grows per-source filtering.

### ✅ Phase 3: REPL Integration (COMPLETE)

**Implementation PR/branch:** `visualize-logic-graph`

- [x] Add `:trace` command handler to `processInput`
- [x] Add `:tr` short form
- [x] Implement `evalWithTrace` helper function
- [x] Implement `formatTraceResults` and `formatTraceResult`
- [x] Update `helpText` with trace commands
- [x] Build: `cabal build jl4-repl` (2025-02-15)
- [x] Test: basic `:trace` command against real `.l4` file (`jl4/experiments/cubed-postfix.l4`, 2025-02-15)
- [x] Test: short `:tr` form with real `.l4` file (same session as above)
- [x] Test: error cases (no file loaded guard exercised via piped REPL session)
- [ ] Test: pipe to `xdot` / `dot -Tsvg` (pending)
- [x] Update this spec with completion notes

**Dependencies:**

- ✅ REPL infrastructure exists (`jl4-repl/`)
- ✅ GraphViz module available in `jl4-core`
- ✅ Module import added (line 28)
- ⚠️ Requires GraphViz tools installed by user (`dot`, `xdot`)

### 📋 Phase 4: Decision Service Integration (IN PROGRESS)

**Status (2025-02-17):**  
`POST /functions/<name>/evaluation` accepts `?trace=full&graphviz=true` and now returns a `graphviz :: Maybe GraphVizResponse` payload. Each response includes the DOT source plus relative URLs for the PNG/SVG endpoints (`/functions/<name>/evaluation/trace.{png,svg}`), keeping the main evaluation endpoint pure while still pointing callers at ready-made images. Batch requests mirror the same contract: when paired with `trace=full`, every case gains an `@graphviz` object with `{dot,png,svg}` so downstream systems can archive traces or fetch screenshots lazily. The dedicated PNG/SVG endpoints (`/evaluation/trace.png` and `/evaluation/trace.svg`) still rerun the evaluation on demand via `Backend.GraphVizRender`. Remaining Phase‑4 work is documentation polish plus future ideas like caching and richer styling presets.

**Example:**

```bash
curl -s -X POST 'http://localhost:8081/functions/compute_qualifies/evaluation?trace=full&graphviz=true' \
  -H 'Content-Type: application/json' \
  -d '{"fnEvalBackend":"JL4","fnArguments":{"walks": true}}' | jq '.graphviz'
```

**Next up:** finalize documentation/UX polish (README + OpenAPI, CLI examples) and consider batch image endpoints or caching strategies now that DOT output is consistent across single and batched evaluations.

**Estimated Time (remaining):** ~1h for docs/tests clean-up plus any follow-on stories for image rendering in batch mode.

Add flexible GraphViz trace visualization to the REST API with multiple output format options.

#### Design Overview

**Format Options:**

- **DOT text:** Included in JSON response when requested (lightweight, no external dependencies)
- **PNG/SVG images:** Separate endpoints with proper Content-Type for direct browser display (requires GraphViz `dot` command)
- **Multiple formats:** Client can request any combination simultaneously

#### API Design

**1. Enhanced Evaluation Endpoints**

Add optional `graphviz` query parameter to existing endpoints:

```
POST /functions/<name>/evaluation?trace=full&graphviz=true
POST /functions/<name>/batch?trace=full&graphviz=true
```

_Status (2025-02-16):_

- ✅ `/functions/<name>/evaluation` honors `graphviz=true` (only when `trace=full`) and includes DOT in `ResponseWithReason.graphviz`. Implementation lives in `Backend/Api.hs`, `Backend/Jl4.hs`, and `Server.hs`.
- ✅ `/functions/<name>/batch` now emits DOT when `trace=full&graphviz=true`, surfacing it via an `@graphviz` pseudo-attribute on each case object.

**Response Extension:**

```haskell
-- Backend/Api.hs
data GraphVizResponse = GraphVizResponse
  { dot :: Text           -- original DOT program
  , png :: Maybe Text     -- relative URL to /functions/<name>/evaluation/trace.png
  , svg :: Maybe Text     -- relative URL to /functions/<name>/evaluation/trace.svg
  }
  deriving (Show, Read, Ord, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data ResponseWithReason = ResponseWithReason
  { values :: [(Text, FnLiteral)]
  , reasoning :: Reasoning
  , graphviz :: Maybe GraphVizResponse
  }
  deriving (Show, Read, Ord, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
```

**Example Request/Response:**

```bash
curl -X POST 'http://localhost:8081/functions/myFunc/evaluation?trace=full&graphviz=true' \
  -H 'Content-Type: application/json' \
  -d '{"x": 5, "y": 10}'

# Response:
{
  "values": [["result", "15"]],
  "reasoning": { ... },
  "graphviz": {
    "dot": "digraph evaluation_trace {\n  rankdir=TB;\n  node [shape=box, style=rounded];\n  ...\n}",
    "png": "/functions/myFunc/evaluation/trace.png",
    "svg": "/functions/myFunc/evaluation/trace.svg"
  }
}
```

The `png`/`svg` URLs are intentionally _relative_ and stateless: every fetch simply reruns the same evaluation to produce a fresh image, keeping the primary `/evaluation` call pure (no mutable caches or shared blobs for clients to invalidate).

**Implementation Notes (2025-02-15):**

- `RunFunction.runFunction` now receives `Bool includeGraphViz` (`Backend/Api.hs:89-98`)
- JL4 backend passes the flag through both fast/slow paths and only emits DOT when `traceLevel == TraceFull` (`Backend/Jl4.hs:96-211`)
- `Server.hs` exposes the `graphviz` query param on `/functions/<name>/evaluation` and gates it behind `trace=full`
- `SchemaSpec` and arbitrary generators updated so tests understand the optional field

**2. New Image Rendering Endpoints**

For direct browser display:

```
POST /functions/<name>/evaluation/trace.png?trace=full
POST /functions/<name>/evaluation/trace.svg?trace=full
```

_Status (2025-02-15):_ Both endpoints are live. PNG responses stream as `application/octet-stream` using the new `PngImage` wrapper, and SVG responses stream as `text/plain`. Requests must specify `trace=full`; otherwise the server returns HTTP 400. When GraphViz's `dot` binary is missing, the handlers return HTTP 503.

**Behavior:**

- Accept same JSON body as regular evaluation endpoint
- Return image with proper Content-Type (`image/png` or `image/svg+xml`)
- Return 503 if GraphViz not installed
- Return 400 for batch requests (not supported for images)

**Example:**

```bash
# Get PNG directly
curl -X POST 'http://localhost:8081/functions/myFunc/evaluation/trace.png?trace=full' \
  -H 'Content-Type: application/json' \
  -d '{"x": 5}' > trace.png

# Use in HTML
<img src="/functions/myFunc/evaluation/trace.svg?trace=full"
     onclick="POST with form data" />
```

**3. Batch Evaluation**

For `POST /functions/<name>/batch`, `?graphviz=true` now mirrors the single-evaluation contract: when paired with `trace=full`, each `cases[i]` object contains an `@graphviz` pseudo-attribute alongside the regular outcome attributes. This keeps the JSON shape Oracle-compatible (everything is still a flat attribute bag) while carving out a reserved metadata key for visualization data.

**Example:**

```bash
POST /functions/myFunc/batch?trace=full&graphviz=true
{
  "outcomes": ["result"],
  "cases": [
    {"@id": 1, "x": 5},
    {"@id": 2, "x": 10}
  ]
}

# Response:
{
  "cases": [
    {
      "@id": 1,
      "result": "25",
      "@graphviz": {
        "dot": "digraph evaluation_trace { ... }",
        "png": "/functions/myFunc/evaluation/trace.png",
        "svg": "/functions/myFunc/evaluation/trace.svg"
      }
    },
    {
      "@id": 2,
      "result": "100",
      "@graphviz": {
        "dot": "digraph evaluation_trace { ... }",
        "png": "/functions/myFunc/evaluation/trace.png",
        "svg": "/functions/myFunc/evaluation/trace.svg"
      }
    }
  ],
  "summary": {
    "casesRead": 2,
    "casesProcessed": 2,
    "casesIgnored": 0,
    "processorDurationSec": 0.12,
    "processorCasesPerSec": 16.4,
    "processorQueuedSec": 0.0
  }
}
```

**Image endpoints:** still return 400 Bad Request for batch POSTs ("Image rendering not supported for batch evaluation").

#### Implementation Requirements

**Dependencies:**

- **DOT generation:** No new dependencies (pure text generation using `L4.EvaluateLazy.GraphViz`)
- **Image rendering:** Requires `graphviz` system package with `dot` command
- **Process execution:** Use `System.Process` for shelling out to `dot`

**Files to Modify:**

1. **`jl4-decision-service/src/Backend/Api.hs`**

   - Add `graphviz :: Maybe GraphVizResponse` field to `ResponseWithReason`
   - Update JSON instances (automatic via deriving)

2. **`jl4-decision-service/src/Backend/Jl4.hs`**

   - Import `L4.EvaluateLazy.GraphViz`
   - Modify `evaluateWithCompiled` to accept boolean flag for GraphViz generation
   - When `trace=full AND graphviz=true`:
     - Extract `EvalTrace` from evaluation result
     - Convert to DOT: `GraphViz.traceToGraphViz GraphViz.defaultGraphVizOptions trace`
     - Include in `ResponseWithReason`

3. **`jl4-decision-service/src/Backend/GraphVizRender.hs` (NEW)**

   ```haskell
   module Backend.GraphVizRender where

   import qualified Data.ByteString as BS
   import qualified Data.Text as Text
   import System.Process (readProcessWithExitCode)
   import System.Exit (ExitCode(..))

   -- | Render DOT to PNG bytes
   renderPNG :: Text -> IO (Either Text BS.ByteString)
   renderPNG dotSource = do
     (exitCode, stdout, stderr) <- readProcessWithExitCode "dot" ["-Tpng"] (Text.unpack dotSource)
     case exitCode of
       ExitSuccess -> Right <$> BS.pack stdout
       ExitFailure _ -> pure $ Left $ "GraphViz error: " <> Text.pack stderr

   -- | Render DOT to SVG text
   renderSVG :: Text -> IO (Either Text Text)
   renderSVG dotSource = do
     (exitCode, stdout, stderr) <- readProcessWithExitCode "dot" ["-Tsvg"] (Text.unpack dotSource)
     case exitCode of
       ExitSuccess -> pure $ Right $ Text.pack stdout
       ExitFailure _ -> pure $ Left $ "GraphViz error: " <> Text.pack stderr

   -- | Check if GraphViz is available
   isGraphVizAvailable :: IO Bool
   isGraphVizAvailable = do
     (exitCode, _, _) <- readProcessWithExitCode "which" ["dot"] ""
     pure $ exitCode == ExitSuccess
   ```

4. **`jl4-decision-service/src/Server.hs`**

   - Add routes for `POST /functions/:name/evaluation/trace.png`
   - Add routes for `POST /functions/:name/evaluation/trace.svg`
   - Parse `graphviz` query parameter in evaluation handlers
   - Set proper Content-Type headers for image endpoints
   - Return 503 if GraphViz unavailable for image endpoints

5. **`jl4-decision-service/src/Application.hs`**
   - Add startup check: log warning if GraphViz not available
   - Optional: Add `/health` endpoint showing GraphViz status

#### Error Handling

**GraphViz Not Installed:**

- **DOT text endpoints:** Always work (pure text generation)
- **PNG/SVG endpoints:** Return `503 Service Unavailable` with body:
  ```json
  {
    "error": "GraphViz not available",
    "message": "Image rendering requires GraphViz 'dot' command. Install with: apt-get install graphviz",
    "graphviz_available": false
  }
  ```

**Invalid DOT:**

- Return `500 Internal Server Error` with:
  ```json
  {
    "error": "GraphViz rendering failed",
    "message": "<stderr from dot command>",
    "dot_source": "<DOT text for debugging>"
  }
  ```

**Batch Image Request:**

- Return `400 Bad Request`:
  ```json
  {
    "error": "Batch image rendering not supported",
    "message": "Use ?graphviz=true for DOT text in batch responses, or call individual evaluations for images"
  }
  ```

#### Testing Strategy

**Unit Tests:**

1. DOT generation from EvalTrace (pure function)
2. `isGraphVizAvailable` detection
3. Response JSON serialization with graphviz field

**Integration Tests:**

```haskell
-- Test DOT in JSON response
testGraphVizParameter :: Spec
testGraphVizParameter = do
  it "includes DOT when graphviz=true" $ do
    response <- POST "/functions/myFunc/evaluation?trace=full&graphviz=true" {"x": 5}
    response.graphviz `shouldSatisfy` isJust
    response.graphviz `shouldContain` "digraph evaluation_trace"

  it "omits DOT when graphviz=false" $ do
    response <- POST "/functions/myFunc/evaluation?trace=full&graphviz=false" {"x": 5}
    response.graphviz `shouldBe` Nothing

-- Test image endpoints (if GraphViz available)
testImageEndpoints :: Spec
testImageEndpoints = do
  it "returns PNG with correct Content-Type" $ do
    response <- POST "/functions/myFunc/evaluation/trace.png?trace=full" {"x": 5}
    response.contentType `shouldBe` "image/png"
    response.body `shouldStartWith` "\x89PNG"  -- PNG magic bytes

  it "returns SVG with correct Content-Type" $ do
    response <- POST "/functions/myFunc/evaluation/trace.svg?trace=full" {"x": 5}
    response.contentType `shouldBe` "image/svg+xml"
    response.body `shouldContain` "<svg"

  it "returns 503 when GraphViz unavailable" $ withoutGraphViz $ do
    response <- POST "/functions/myFunc/evaluation/trace.png?trace=full" {"x": 5}
    response.status `shouldBe` 503
```

**Manual Testing:**

```bash
# Test DOT in JSON
curl -X POST 'http://localhost:8081/functions/myFunc/evaluation?trace=full&graphviz=true' \
  -H 'Content-Type: application/json' \
  -d '{"x": 5}' | jq '.graphviz'

# Test PNG generation
curl -X POST 'http://localhost:8081/functions/myFunc/evaluation/trace.png?trace=full' \
  -H 'Content-Type: application/json' \
  -d '{"x": 5}' > test.png && file test.png

# Test SVG in browser
open 'http://localhost:8081/functions/myFunc/evaluation/trace.svg?trace=full' \
  -d '{"x": 5}'
```

#### Usage Examples

**Web UI Integration:**

```javascript
// Fetch trace data
const response = await fetch(
  "/functions/myFunc/evaluation?trace=full&graphviz=true",
  {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ x: 5, y: 10 }),
  },
);
const { values, reasoning, graphviz } = await response.json();

// Display result
console.log("Result:", values);

// Render trace with local GraphViz
if (graphviz) {
  renderDOT(graphviz); // Using viz.js or similar
}

// Or display image directly
<img
  src={`/functions/myFunc/evaluation/trace.svg?trace=full`}
  onClick={() => postData({ x: 5 })}
/>;
```

**Debugging Workflow:**

```bash
# Get DOT for local analysis
curl -X POST 'http://localhost:8081/functions/myFunc/evaluation?trace=full&graphviz=true' \
  -H 'Content-Type: application/json' \
  -d '{"x": 5}' | jq -r '.graphviz' > trace.dot

# Render locally with custom options
dot -Tpdf -Grankdir=LR trace.dot > trace.pdf

# Or get PNG directly from API
curl -X POST 'http://localhost:8081/functions/myFunc/evaluation/trace.png?trace=full' \
  -H 'Content-Type: application/json' \
  -d '{"x": 5}' > trace.png && open trace.png
```

#### Documentation Updates

1. **`jl4-decision-service/README.md`**

   - ~~Add section: "Trace Visualization"~~ ✅ (2025-02-16) — includes `graphviz` flag, PNG/SVG endpoints, batch `@graphviz`, install hint, and a “What am I looking at?” primer.

2. **Swagger/OpenAPI Spec**

   - ~~Add `graphviz` query parameter to evaluation endpoints~~ ✅ (documented via parameter descriptions + default=false note)
   - ~~Add `/functions/{name}/evaluation/trace.png` endpoint~~ ✅ (Servant schema already exposed; OpenAPI now advertises `image/png` response)
   - ~~Add `/functions/{name}/evaluation/trace.svg` endpoint~~ ✅
   - ~~Document response schemas with graphviz field~~ ✅ (`OutputCase` now includes optional `@graphviz`)
   - Add example responses (still TODO)

3. **Example Visualizations**
   - Include sample PNG/SVG in docs
   - Link to GraphViz documentation

#### Implementation Checklist

**Core (DOT Text):**

- [x] Update `ResponseWithReason` in `Backend/Api.hs`
- [x] Modify `evaluateWithCompiled` in `Backend/Jl4.hs`
- [x] Import `L4.EvaluateLazy.GraphViz`
- [x] Extract trace and convert to DOT when requested
- [x] Parse `graphviz` query parameter in Server.hs (both evaluation and batch endpoints)
- [x] Extend `/functions/<name>/batch` to honor `graphviz=true` (when `trace=full`) and surface DOT via `@graphviz` pseudo-attribute per case
- [x] Update tests (Schema arbitrary + build)
- [x] Build and verify (`cabal build jl4-decision-service`, 2025-02-15)
- [x] Manual `curl` test against running decision service (see commands below)

**Image Rendering (PNG/SVG):**

- [x] Create `Backend/GraphVizRender.hs`
- [x] Implement `renderPNG`, `renderSVG`, `isGraphVizAvailable`
- [x] Add image routes to `Server.hs`
- [x] Set proper Content-Type headers (`OctetStream` for PNG, `PlainText` for SVG)
- [x] Handle GraphViz unavailable (503 response from helper)
- [ ] Add startup check in `Application.hs` (still TODO)
- [x] Test with GraphViz installed (`curl` commands below)
- [ ] Test graceful degradation without GraphViz
- [x] Update Swagger docs (Servant schema now includes binary response via `PngImage` newtype)

**Documentation:**

- [x] Update `README.md` with trace visualization section (jl4-decision-service/README.md)
- [x] Update Swagger/OpenAPI spec (parameter docs + `@graphviz` schema entry, 2025-02-16)
- [ ] Add example images to docs
- [x] Document installation requirements (README now mentions installing GraphViz/`dot`)
- [x] Surface tips in training material (`doc/foundation-course-ai/module-6` + `doc/advanced-course-ai/module-a4`) so new users know how to request traces graphically
- [x] Ensure Dev setup + Nix shells install GraphViz (Dev.md + `nix/shell.nix` + server `environment.systemPackages` + Docker runtime for decision service)

#### Manual Testing (Decision Service)

- [x] `curl -X POST /functions/compute_qualifies/evaluation?trace=full&graphviz=true` → `graphviz` field populated (local server on :8081 via `cabal run jl4-decision-service-exe -- --port 8081`, 2025-02-15)
- [x] `curl ...?trace=none&graphviz=true` → response contains `"graphviz": null` (flag ignored when trace disabled)
- [x] `curl .../evaluation/trace.png?trace=full` → receives PNG bytes (written to `/tmp/trace.png`)
- [x] `curl .../evaluation/trace.svg?trace=full` → receives SVG text (written to `/tmp/trace.svg`)
- [x] `curl .../evaluation/trace.png?trace=none` → HTTP 400 (trace must be full)
  - [x] `curl .../batch?graphviz=true` → returns `@graphviz` strings per case when `trace=full`

```bash
# Start server (separate terminal)
cabal run jl4-decision-service-exe -- --port 8081

# GraphViz enabled (trace=full required)
curl -s -X POST 'http://localhost:8081/functions/compute_qualifies/evaluation?trace=full&graphviz=true' \
  -H 'Content-Type: application/json' \
  -d '{"fnEvalBackend":"JL4","fnArguments":{"walks":true,"eats":true,"drinks":true}}' \
  | jq '.contents.graphviz.dot | length'

# GraphViz suppressed when trace=none
curl -s -X POST 'http://localhost:8081/functions/compute_qualifies/evaluation?trace=none&graphviz=true' \
  -H 'Content-Type: application/json' \
  -d '{"fnEvalBackend":"JL4","fnArguments":{"walks":true,"eats":true,"drinks":true}}' \
  | jq '.contents.graphviz'

# PNG endpoint
curl -s -o /tmp/trace.png \
  -X POST 'http://localhost:8081/functions/compute_qualifies/evaluation/trace.png?trace=full' \
  -H 'Content-Type: application/json' \
  -d '{"fnEvalBackend":"JL4","fnArguments":{"walks":true,"eats":true,"drinks":true}}'

# SVG endpoint
curl -s -o /tmp/trace.svg \
  -X POST 'http://localhost:8081/functions/compute_qualifies/evaluation/trace.svg?trace=full' \
  -H 'Content-Type: application/json' \
  -d '{"fnEvalBackend":"JL4","fnArguments":{"walks":true,"eats":true,"drinks":true}}'

# Batch GraphViz (per-case @graphviz attribute)
curl -s -X POST 'http://localhost:8081/functions/compute_qualifies/batch?trace=full&graphviz=true' \
  -H 'Content-Type: application/json' \
  -d '{"outcomes":["result"],"cases":[{"@id":1,"walks":true,"eats":true,"drinks":true}]}' \
  | jq '.cases[0]["@graphviz"].dot | length'
```

#### Backward Compatibility

**JSON Response:**

- `graphviz` field is optional (`Maybe GraphVizResponse`)
- Existing clients that only care about DOT can read `.graphviz.dot` (or the legacy string) and ignore new keys
- Only present when `?graphviz=true` parameter used
- Default behavior unchanged (no graphviz field)

**New Endpoints:**

- Image endpoints are completely new
- No existing functionality affected

#### Performance Considerations

**DOT Generation:**

- Pure text manipulation (fast)
- Negligible performance impact
- No caching needed

**Image Rendering:**

- External process execution (slower, ~100-500ms)
- Consider caching rendered images by hash of (function + inputs)
- Rate limiting recommended for production
- May want to run in background thread for large traces

**Future Optimizations:**

- Cache GraphViz availability check
- Reuse `dot` process for multiple renders
- Pre-render common traces
- Add `?format=dot|png|svg` parameter for more control

### GraphViz Library Refactor (Phase 5) 📋 FUTURE ENHANCEMENT

**Status:** Planned refactoring to replace manual DOT generation with official library

**Motivation:**

The current implementation (Phase 1) generates DOT format text manually using string concatenation. While this works and has no external dependencies, using the official `graphviz` Hackage library would provide:

1. **Type Safety** - Structured API prevents malformed DOT syntax
2. **Feature Completeness** - Access to full GraphViz feature set (subgraphs, clusters, custom attributes)
3. **Maintainability** - Library handles edge cases and escaping automatically
4. **Extensibility** - Easier to add advanced features (custom shapes, HTML labels, etc.)
5. **Industry Standard** - Well-tested, widely-used library with active maintenance

#### Current Implementation Analysis

**Location:** `jl4-core/src/L4/EvaluateLazy/GraphViz.hs` (139 lines)

**Current Approach:**

```haskell
-- Manual text generation
traceToGraphViz :: GraphVizOptions -> EvalTrace -> Text
traceToGraphViz opts trace = Text.unlines
  [ "digraph evaluation_trace {"
  , "  rankdir=TB;"
  , "  node [shape=box, style=rounded];"
  , renderNodes trace
  , renderEdges trace
  , "}"
  ]
```

**Limitations:**

- Manual escaping of special characters in node labels
- Limited control over layout algorithms
- No support for subgraphs or clusters
- Custom attributes require manual string formatting
- No validation of generated DOT syntax

#### Proposed Library-Based Approach

**Hackage Package:** `graphviz` (latest version: 2999.20.2.0)

**Dependencies to Add:**

```cabal
-- jl4-core/jl4-core.cabal
library
  build-depends:
    , graphviz >= 2999.20 && < 3000
    , fgl >= 5.7 && < 6  -- Required by graphviz
```

**New Implementation Pattern:**

```haskell
module L4.EvaluateLazy.GraphViz
  ( traceToGraphViz
  , traceToGraph      -- NEW: produce FGL graph
  , graphToDot        -- NEW: convert to DOT
  , graphToSVG        -- NEW: render to SVG
  , GraphVizOptions(..)
  , defaultGraphVizOptions
  ) where

import Data.GraphViz
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Types.Generalised as G
import Data.Graph.Inductive.Graph (Gr)

-- Convert EvalTrace to FGL graph structure
traceToGraph :: GraphVizOptions -> EvalTrace -> Gr NodeLabel EdgeLabel
traceToGraph opts trace = buildGraph opts trace

-- Convert graph to DOT using graphviz library
graphToDot :: GraphVizOptions -> Gr NodeLabel EdgeLabel -> DotGraph Int
graphToDot opts graph = graphToDot' params graph
  where
    params = nonClusteredParams
      { fmtNode = \(_, label) -> nodeAttributes opts label
      , fmtEdge = \(_, _, label) -> edgeAttributes opts label
      }

-- Main entry point (backward compatible)
traceToGraphViz :: GraphVizOptions -> EvalTrace -> Text
traceToGraphViz opts trace =
  Text.pack $ printDotGraph $ graphToDot opts $ traceToGraph opts trace

-- NEW: Direct SVG rendering (requires graphviz command installed)
graphToSVG :: GraphVizOptions -> Gr NodeLabel EdgeLabel -> IO (Either Text Text)
graphToSVG opts graph = do
  let dot = graphToDot opts graph
  result <- runGraphvizCommand Dot dot Svg
  pure $ Right $ Text.pack result
```

#### Refactoring Strategy

**Step 1: Add Dependencies (Low Risk)**

- Add `graphviz` and `fgl` to `jl4-core.cabal`
- Verify build succeeds across all platforms
- No code changes yet

**Step 2: Parallel Implementation (Zero Risk)**

- Create new module `L4.EvaluateLazy.GraphViz.Library`
- Implement library-based version alongside existing manual version
- Add feature flag to switch between implementations:

  ```haskell
  data GraphVizBackend = ManualText | GraphVizLib

  data GraphVizOptions = GraphVizOptions
    { backend :: GraphVizBackend  -- NEW
    , includeValues :: Bool
    , simplifyTrivial :: Bool
    , showUnevaluated :: Bool
    , maxDepth :: Maybe Int
    }
  ```

**Step 3: Testing & Validation (Medium Risk)**

- Add tests comparing outputs of both implementations
- Verify DOT syntax equivalence (semantic, not textual)
- Test all GraphVizOptions combinations
- Benchmark performance (library may be slower for small traces)
- Test on all platforms (Linux, macOS, Windows)

**Step 4: Migration (Medium Risk)**

- Default to library implementation
- Keep manual implementation as fallback
- Update documentation
- Monitor for issues in production

**Step 5: Cleanup (Low Risk)**

- After stable period (1-2 releases), remove manual implementation
- Simplify GraphVizOptions (remove backend field)
- Update tests to remove comparison tests

#### Benefits of Phased Approach

1. **Backward Compatibility** - Old implementation remains available during transition
2. **Risk Mitigation** - Can rollback to manual generation if issues found
3. **Gradual Migration** - Users can opt-in to new implementation
4. **Performance Testing** - Can compare performance in real-world scenarios
5. **Platform Testing** - Can verify library works on all target platforms

#### Advanced Features Enabled

Once library-based implementation is stable, new features become feasible:

**Subgraphs for Function Contexts:**

```haskell
-- Group traces by function call
clusterByFunction :: EvalTrace -> Gr NodeLabel EdgeLabel
```

**HTML Labels with Rich Formatting:**

```haskell
-- Show type information, hover tooltips
nodeAttributes :: NodeLabel -> [Attribute]
nodeAttributes label =
  [ Label $ HtmlLabel $ Table [HtmlCell (HtmlText $ renderExpr label.expr)]
  , Tooltip $ renderType label.typ
  ]
```

**Custom Layout Algorithms:**

```haskell
data LayoutAlgorithm = Dot | Neato | Fdp | Sfdp | Circo | Twopi
```

**Interactive Features:**

```haskell
-- Generate clickable nodes that link to source
nodeURL :: NodeLabel -> Text
nodeURL label = "file://" <> label.sourceFile <> "#L" <> show label.line
```

**Export to Multiple Formats:**

```haskell
renderGraph :: GraphVizOptions -> GraphOutputFormat -> Gr NodeLabel EdgeLabel -> IO (Either Text ByteString)
data GraphOutputFormat = DOT | PNG | SVG | PDF | PS
```

#### Dependencies Analysis

**Package: `graphviz`**

- Mature package (15+ years of development)
- Latest stable: 2999.20.2.0
- GHC 9.8.4 compatible
- Dependencies: `fgl`, `containers`, `text`, `bytestring`, `process`
- All transitive dependencies already in L4 project

**Package: `fgl` (Functional Graph Library)**

- Core Haskell graph library
- Version: 5.8.x
- Used by many Haskell projects
- Well-tested, stable API

**Binary Dependency: `graphviz` system package**

- Required only for direct rendering (PNG/SVG/PDF)
- Not required for DOT generation
- Already documented requirement for Phase 4 image endpoints

#### Implementation Checklist

**Preparation:**

- [ ] Research `graphviz` library API and examples
- [ ] Create spike/prototype in separate branch
- [ ] Benchmark current manual implementation
- [ ] Document current DOT output format

**Step 1: Dependencies**

- [ ] Add `graphviz` to `jl4-core.cabal` build-depends
- [ ] Add `fgl` to `jl4-core.cabal` build-depends
- [ ] `cabal build jl4-core` - verify successful build
- [ ] `cabal test jl4-core` - verify tests still pass
- [ ] Test on CI/CD pipeline

**Step 2: Implementation**

- [ ] Create `L4.EvaluateLazy.GraphViz.Library` module
- [ ] Implement `traceToGraph :: EvalTrace -> Gr NodeLabel EdgeLabel`
- [ ] Implement `graphToDot :: Gr NodeLabel EdgeLabel -> DotGraph Int`
- [ ] Define node/edge attribute functions
- [ ] Add backend selection to `GraphVizOptions`
- [ ] Update `traceToGraphViz` to dispatch based on backend

**Step 3: Testing**

- [ ] Add unit tests for library implementation
- [ ] Add property tests comparing both implementations
- [ ] Benchmark performance differences
- [ ] Test all GraphVizOptions combinations
- [ ] Visual diff of generated graphs
- [ ] Test on Linux, macOS, Windows

**Step 4: Integration**

- [ ] Update CLI to support `--graphviz-backend=lib` flag
- [ ] Update REPL to support backend selection
- [ ] Update Decision Service configuration
- [ ] Add documentation for new backend option
- [ ] Update examples and tutorials

**Step 5: Migration**

- [ ] Change default backend to library
- [ ] Monitor for issues (1-2 releases)
- [ ] Deprecate manual backend
- [ ] Remove manual implementation code
- [ ] Simplify GraphVizOptions
- [ ] Update all documentation

#### Risks and Mitigations

**Risk: Performance Regression**

- _Mitigation:_ Benchmark before and after; keep manual as fallback
- _Likelihood:_ Medium (library adds abstraction overhead)
- _Impact:_ Low (traces are typically small)

**Risk: Binary Size Increase**

- _Mitigation:_ Accept tradeoff for maintainability
- _Likelihood:_ High (graphviz library is substantial)
- _Impact:_ Low (binary size not critical for L4 tools)

**Risk: Platform Compatibility Issues**

- _Mitigation:_ Test on all platforms early; keep manual as fallback
- _Likelihood:_ Low (graphviz library is mature)
- _Impact:_ Medium (could block deployment)

**Risk: Breaking Changes in Library API**

- _Mitigation:_ Pin to stable version range; monitor deprecations
- _Likelihood:_ Low (library API is stable)
- _Impact:_ Medium (would require refactoring)

**Risk: User Confusion with Two Implementations**

- _Mitigation:_ Clear documentation; sensible defaults
- _Likelihood:_ Medium (during transition period)
- _Impact:_ Low (temporary during migration)

#### Success Criteria

**Phase Complete When:**

1. Library-based implementation generates equivalent DOT output
2. All existing tests pass with library backend
3. Performance within 2x of manual implementation
4. Documentation updated
5. At least one release with both backends available
6. No reported issues with library backend for one release cycle

**Migration Complete When:**

1. Library backend is default for 2+ releases
2. No issues reported with library backend
3. Manual implementation code removed
4. Tests simplified (no longer compare implementations)
5. Documentation updated to remove backend references

#### Alternative Approaches Considered

**1. Stay with Manual Implementation**

- _Pros:_ No dependencies; full control; fast
- _Cons:_ Limited features; manual escaping; maintenance burden
- _Decision:_ Not recommended long-term; technical debt accumulates

**2. Use `diagrams` Package Instead**

- _Pros:_ More general; better rendering control
- _Cons:_ Heavier dependency; not DOT-compatible; different paradigm
- _Decision:_ Not suitable; DOT format is standard for graph visualization

**3. Generate JSON for vis.js/cytoscape.js**

- _Pros:_ Interactive web visualization; no GraphViz dependency
- _Cons:_ Doesn't work for CLI/REPL; different use case
- _Decision:_ Complementary approach; could add in Phase 6

**4. Use Mermaid Diagram Syntax**

- _Pros:_ Simpler syntax; Markdown-compatible
- _Cons:_ Less expressive; not standard for evaluation traces
- _Decision:_ Interesting for documentation; not suitable for traces

## Testing Strategy

### Unit Tests

**Core Module:**

```haskell
-- Test basic trace conversion
testSimpleTrace :: Spec
testSimpleTrace =
  it "converts simple trace to DOT" $ do
    let trace = ... -- construct simple trace
    let dot = traceToGraphViz defaultGraphVizOptions trace
    dot `shouldContain` "digraph"
    dot `shouldContain` "node_0"

-- Test options
testMaxDepth :: Spec
testMaxDepth =
  it "respects maxDepth option" $ do
    let opts = defaultGraphVizOptions { maxDepth = Just 2 }
    let trace = ... -- construct deep trace
    let dot = traceToGraphViz opts trace
    -- Should contain depth limit indicator
```

### Integration Tests

**CLI:**

```bash
# Test flag exists
jl4-cli --help | grep graphviz

# Test output format
jl4-cli --graphviz test.l4 | grep "digraph"

# Test pipeline to dot
jl4-cli --graphviz test.l4 | dot -Tsvg > /dev/null
```

**REPL:**

```bash
# Test command exists
echo ":help" | jl4-repl | grep ":trace"

# Test basic execution
echo ":load jl4/experiments/cubed-postfix.l4\n:trace 5 cubed" | jl4-repl | grep "digraph"

# Test short form
echo ":load jl4/experiments/cubed-postfix.l4\n:tr 5 cubed" | jl4-repl | grep "digraph"
```

### Golden Tests

Create golden files for trace visualization:

```
jl4-core/test/graphviz-golden/
├── simple-eval.l4
├── simple-eval.dot.golden
├── complex-trace.l4
└── complex-trace.dot.golden
```

## Documentation

### User Documentation

**CLI Usage:**

```markdown
## Visualizing Evaluation Traces

Use the `--graphviz` flag to generate GraphViz DOT format visualization:

    jl4-cli --graphviz yourfile.l4 > trace.dot
    dot -Tsvg trace.dot > trace.svg

Open `trace.svg` in your browser to see the evaluation tree.
```

**REPL Usage:**

```markdown
## REPL Trace Commands

- `:trace <expr>` - Show trace in DOT format
- `:tr <expr>` - Short form of `:trace`

Examples:
jl4> :trace 5 cubed
jl4> :trace 5 cubed | xdot -
jl4> :tr 5 cubed
```

### Developer Documentation

**Using the GraphViz Module:**

```haskell
import qualified L4.EvaluateLazy.GraphViz as GraphViz

-- Get trace from evaluation
let (result, trace) = evaluateWithTrace expr

-- Convert to DOT format
let dot = GraphViz.traceToGraphViz GraphViz.defaultGraphVizOptions trace

-- Customize options
let opts = GraphViz.defaultGraphVizOptions
      { GraphViz.maxDepth = Just 5
      , GraphViz.simplifyTrivial = True
      }
let dot = GraphViz.traceToGraphViz opts trace
```

## Future Enhancements

### Interactive Visualization

Instead of static DOT files, could generate:

- Interactive web-based trace explorer
- Collapsible/expandable tree nodes
- Hover tooltips with full values
- Click to jump to source code

### Trace Analysis

Additional tools built on trace data:

- Performance profiling (identify slow evaluations)
- Memory usage analysis (identify space leaks)
- Evaluation statistics (cache hit rates, etc.)

### Export Formats

Support additional output formats:

- JSON (for programmatic analysis)
- HTML (self-contained visualization)
- ASCII art (for terminal viewing)
- Mermaid diagram syntax

### REPL Enhancements (Deferred)

Optional: Direct image generation in REPL:

```haskell
:trace-svg 5 cubed  -- Saves to /tmp, prints path
:trace-png 5 cubed  -- Same, PNG format
```

**Decision:** Deferred - piping to `dot` is more flexible. Users can control format, resolution, and output location.

## Architecture Review

**Modularity Score: ✅ Excellent**

The design achieves strong modularity:

1. **Core module is pure** - No IO, no dependencies on CLI/REPL
2. **Single responsibility** - Only handles DOT generation
3. **Reusable** - Can be imported and used anywhere
4. **Testable** - Pure functions are easy to test
5. **Extensible** - Options pattern allows future additions

**REPL Integration Design:**

The REPL integration (Phase 3) maintains modularity by:

1. **Separation of concerns:**

   - GraphViz module: trace → DOT conversion
   - REPL module: command parsing, evaluation, output handling
   - External tools: DOT → image rendering

2. **No circular dependencies:**

   ```
   jl4-core (GraphViz) ← jl4-repl
   (GraphViz doesn't know about REPL)
   ```

3. **Pluggable output:**

   - REPL can add new output formats without touching GraphViz
   - GraphViz can be improved without touching REPL

4. **State management:**
   - REPL owns its state (including trace options)
   - GraphViz remains stateless

This architecture ensures that:

- CLI, REPL, and Decision Service can all use the same core
- Each tool adds its own UI/interaction layer
- Core functionality is tool-agnostic
- Testing can focus on pure logic in core module

## Notes

- The `EvalTrace` data structure already exists in `jl4-core/src/L4/EvaluateLazy/Trace.hs`
- Trace capture is enabled by default in lazy evaluator
- REPL already supports expression evaluation via virtual files
- GraphViz tools (`dot`, `neato`, `xdot`, etc.) must be installed separately by users
- NoFieldSelectors extension used throughout codebase requires explicit field extraction patterns

### GraphViz Rendering Helper

`jl4-decision-service/src/Backend/GraphVizRender.hs` centralizes PNG/SVG rendering. It shells out to `dot` via `CreateProcess`, captures stdout/stderr as ByteStrings, and exposes:

- `renderPNG :: Text -> IO (Either Text ByteString)`
- `renderSVG :: Text -> IO (Either Text Text)`
- `isGraphVizAvailable :: IO Bool`

`Server.hs` calls `ensureGraphVizAvailable` before invoking these helpers so the image endpoints can fail fast with HTTP 503 when GraphViz isn't installed.
