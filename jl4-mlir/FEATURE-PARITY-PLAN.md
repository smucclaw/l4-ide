# jl4-mlir → jl4-service feature parity plan

Goal: bring the L4→MLIR→WASM compiler to the point where it can serve as the
evaluation engine behind the deployment API in place of the Haskell
`jl4-service`, with results we can trust for legal/financial computation.

## What "parity" means here

`jl4-service` is the reference. For a given deployment and request, the WASM
path must return JSON **byte-identical** to `jl4-service` for:

- `POST .../evaluation` and `.../evaluation/batch` — including the
  `ResponseWithReason` envelope when `trace=full`, not just `SimpleResponse`.
- `POST .../query-plan`
- `GET .../state-graphs` and `.../state-graphs/{name}`
- DEONTIC functions (with `startTime` + `events`).
- `metadata-cache.json` shape (functions[] with `returnSchema`,
  `x-sanitized-name`, `isDeontic`; files[] with exports) — this is the contract
  the auth-proxy readmodel already consumes.

Anything we cannot make byte-identical, we either (a) fix, or (b) **detect at
compile time and refuse**, so the system never silently returns a wrong legal
answer.

## Guiding principles

1. **Parity harness before features.** We cannot claim "up to speed" without a
   differential tester that runs a corpus through both engines and diffs every
   byte. It is Milestone 0 and gates every later claim.
2. **Fail loud, never silently degrade.** The current `0.0` fallbacks for
   regulative/IO constructs are the single biggest risk. Close that first.
3. **Keep the fast path fast.** The whole reason MLIR exists is the ~1000×
   speedup on simple decision rules. Trace/exact/deontic support must be
   _opt-in variants_, never a tax on the common case.
4. **Reuse the interpreter's logic for parity, don't re-derive it.** Where the
   semantics are subtle (deontic event replay, rational rounding), port the
   exact algorithm from jl4-core rather than reinventing it in codegen.

## The honest shape of the work

The gaps are not uniform. Three are bounded plumbing; three are
interpreter-shaped and partly fight the reason we compiled in the first place.

| Gap                          | Nature                                   | Effort                   | Keeps the speedup?                  |
| ---------------------------- | ---------------------------------------- | ------------------------ | ----------------------------------- |
| Strings                      | Implement 3 runtime fns                  | ~1–2 days                | yes                                 |
| Compile coverage / fail-loud | Detect + reject unsupported              | ~1 week                  | yes                                 |
| Query-plan + state-graphs    | Precompute into metadata at compile time | ~1 week total            | yes (not runtime at all)            |
| Exact numbers                | Boxed rationals + runtime arithmetic     | ~4–6 weeks               | **erodes the arithmetic part only** |
| Reasoning traces             | Instrumented codegen variant             | ~3–4 weeks               | yes (opt-in path)                   |
| Deontic                      | Step-function codegen + TS replay driver | ~4–6 weeks, highest risk | partial                             |

Total to true parity: **~4–5 months** of focused work. Tiers 3–4 (exact
numbers, deontic) partly negate the original speed rationale — that tension is a
decision, not a detail (see "Decisions required").

---

## Milestone 0 — Differential parity harness (gate for everything)

Generalize `scripts/batch-compile.sh` (which today only reports compile
success) into a **differential evaluation tester**.

- Corpus: all `.l4` in the monorepo + the auth-proxy `validation/test.l4`
  fixture (12 exports, the existing 11/12 byte-identical baseline).
- For each exported function: generate/seed sample inputs (start from the
  fixtures already used in auth-proxy validation), POST to both
  `jl4-service` and the WASM HTTP wrapper (`scripts/wasm-server.mjs`), diff the
  JSON responses.
- Report a matrix: `byte-identical | differs | jl4-mlir-error | service-error |
refused-unsupported`.
- Wire into CI so parity regressions are visible per-PR.

Deliverable: a single command that prints the parity matrix. Every milestone
below moves cells from "differs/error" to "byte-identical" (or, legitimately,
to "refused-unsupported").

Files: `scripts/` (new differential runner), reuse `scripts/wasm-server.mjs`,
auth-proxy `validation/`.

**DONE.** `scripts/parity-harness.mjs` compiles each `.l4` to WASM, deploys the
same source to a live `jl4-service`, and evaluates identical (schema-generated
or curated `<file>.cases.json`) arguments against both, classifying each result:
`byte-identical | value-equal | ulp-differs | differs | wasm-error |
service-error | both-error | refused-unsupported`. The gate fails only on
`differs`/`wasm-error`; `ulp-differs` is the tracked f64-vs-rational gap (M4),
not a regression.

First run (auth-proxy `test.l4`, 12 functions, + the deontic `ceo` file):
**11 byte-identical, 1 ulp-differs, 1 refused-unsupported → gate PASS.** The
`ulp-differs` is `total-interest-paid`: `0.016583333333333332` (service, exact
rational) vs `…335` (WASM, f64) — exactly the M4 signature, now caught
automatically instead of by eyeball. The deontic `ceo` export is correctly
`refused-unsupported` (M1a flag honoured). Toolchain: Homebrew `llvm` + `lld`
(LLVM 22) — the existing pass pipeline works unmodified.

This harness is the gate for M4–M6: as exact numbers (M4) and deontic (M6) land,
their cells move from `ulp-differs`/`refused-unsupported` to `byte-identical`.

---

## Milestone 1 — Fail loud + strings (Tier 1, the trust foundation)

### 1a. Reject unsupported constructs at compile time

Today `Regulative`/`Event`/`Fetch`/`Post`/`Env`/`Breach` lower to `arithConstantFloat 0.0`
([Lower.hs:1336-1342](src/L4/MLIR/Lower.hs)). Replace each with a hard compile
error that names the construct and the function, so a deployment containing
deontic/IO rules **fails to compile to WASM** rather than returning silent
`FALSE`. The compiler emits, per function, a `supported: true|false` +
`unsupportedReason` flag into the schema so the proxy can route unsupported
functions to the fallback engine instead of trusting WASM.

Files: `src/L4/MLIR/Lower.hs`, `src/L4/MLIR/Schema.hs`.

### 1b. Real string operations

`__l4_str_concat` / `__l4_str_len` / `__l4_to_string` are identity/zero stubs
([runtime/jl4-runtime.mjs:306-309](runtime/jl4-runtime.mjs)). The allocator and
memory I/O primitives already exist and strings are immutable + per-call (heap
resets each call), so no GC is needed.

- Implement concat (alloc len1+len2+1, memcpy, NUL-terminate), length (walk to
  NUL), and number→string (match jl4-core's formatting exactly — integer vs
  Double rendering).
- Confirm against jl4-core semantics for the higher-level string intrinsics
  (`TOUPPER`, `TRIM`, `CONTAINS`, `SUBSTRING`, `REPLACE`, …) already lowered as
  runtime calls; implement any that are still stubbed.

Files: `runtime/jl4-runtime.mjs`, `src/L4/MLIR/Runtime/Builtins.hs` (signatures
only if needed).

Exit criterion: every string-using fixture is byte-identical or legitimately
refused; no function silently returns `0.0` for a regulative body.

---

## Milestone 2 — Compile coverage triage

The batch script reports 52/431 failures, all at jl4-core typecheck (not the
MLIR backend) — so they're a _language coverage_ question, not a codegen one.

- Categorize the 52: which L4 features (higher-order, advanced types, IO) they
  use. Decide per-category: support, or refuse-with-clear-error.
- Audit Lower.hs for any remaining silent placeholders (e.g.
  [Lower.hs:1844](src/L4/MLIR/Lower.hs)) and convert to fail-loud.

Deliverable: a coverage report mapping each unsupported feature to
"will-support / will-refuse", feeding the proxy's routing decision.

**DONE.** `scripts/coverage-sweep.py` swept 438 files; results in
`coverage-report/` (`FINDINGS.md` + `coverage.{txt,json}`). Headline: of 37 real
exportable files, 35 lower cleanly and 2 are correctly flagged
`supported: false` (the deontic `ceo-performance-award` rule). No silent
degradation and no real lowering crashes remain. The "52 typecheck failures"
are 27 experiments + 23 deliberately-broken fixtures + 2 unrelated parse-level
edge cases — not backend coverage gaps. The only actionable gap is the deontic
family (tracked as M6).

---

## Milestone 3 — Query-plan + state-graphs as compile-time metadata

Neither is runtime evaluation — both are **pure AST analyses**
([QueryPlan.hs](../jl4-query-plan/src/L4/Decision/QueryPlan.hs),
[StateGraph.hs](../jl4-core/src/L4/StateGraph.hs)). So they don't belong in the
WASM at all: compute them once at compile time and bake them into
`metadata-cache.json`, then have the proxy serve `GET .../query-plan` and
`.../state-graphs` straight from the cache (no WASM call).

- In the compile step, call `L4.StateGraph.extractStateGraphs` on the same
  resolved module the lowering already has, and emit their JSON into the schema.
- Verify byte-identity against jl4-service's runtime responses for these routes.

Files: `src/L4/MLIR/Schema.hs`, `src/L4/MLIR/Pipeline.hs`. (This also benefits
the proxy regardless of eval engine — state-graphs become free reads.)

**State-graphs: DONE.** `bundleExports` now calls `extractStateGraphs` and
renders each graph's Graphviz DOT with `stateGraphToDot defaultStateGraphOptions`
— the _same pure function_ jl4-service calls — into a `stateGraphs: [{name, dot}]`
array in the `.schema.json`. Byte-identical by construction. State graphs are
module-level (jl4-service ignores the function-name path segment and keys by
graph name), and only appear for functions whose body is _top-level_ regulative
(`findRegulativeExpr` doesn't descend into `IF/THEN/ELSE`), matching jl4-service
exactly. No new cabal deps (`L4.StateGraph` is in jl4-core; graphviz comes with
it). Covered by two unit tests.

**Query-plan: CANNOT be precomputed — stays a compute endpoint.** Investigation
([QueryPlan.hs:234], [DecisionQueryPlan.hs:165]) shows the `QueryPlanResponse` is
_request-dependent_: `determined`, `stillNeeded`, `ranked`, `inputs`, and `asks`
all change with the request's answered bindings (`flattenedLabelBindings`). Only
the underlying `CachedDecisionQuery` (BDD + atom labels/deps) is static — and
building even that requires `LadderViz.doVisualize` from the LSP visualization
stack, not a lightweight pure call. jl4-service itself builds this cache lazily
on first request and then runs the pure `queryPlan cached bindings` per request.

Conclusion: query-plan is not a "bake into the schema" item. The replacement
needs the pure `queryPlan` function available at request time over a per-function
cache. Two viable routes (decide later, with the sole-vs-hybrid decision):
(a) include the query-plan/BDD evaluator in the eval-core WASM and call it per
request; or (b) keep query-plan on the fallback evaluator. Either way, M3 does
not block it — flagging it here so it isn't mistaken for an open compile-time
task.

---

## Milestone 4 — Exact numbers (the speed-vs-correctness decision)

jl4-core's `NUMBER` is `Rational` (exact); MLIR uses `f64`. Crucially,
jl4-service already **rounds to `Double` at the JSON boundary** for non-integer
results ([ValueLazy.hs:271-273](../jl4-core/src/L4/Evaluate/ValueLazy.hs)). So
parity = "compute exactly in rational, round to Double at the end exactly as
jl4-service does," not "emit rationals on the wire."

**DECISION (locked): MLIR is the sole engine.** So full exact numbers are
mandatory — no f64 fast path to fall back to, no hybrid routing.

### Architecture: BigInt-backed rationals in the runtime (not linear memory)

Matching jl4-core's `Rational` requires _arbitrary precision_ (numerator and
denominator grow unbounded under repeated `+`/`*`). Bignums don't live naturally
in wasm linear memory, but the proxy runs Node — which has `BigInt`. So:

- `NUMBER` becomes an opaque **handle**: an integer index into a per-call
  JS-side table of `{ num: BigInt, den: BigInt }` (normalised, `den > 0`,
  `gcd == 1`). The handle is boxed across the ABI exactly like a pointer
  (integer → f64 bit-pattern). Booleans/enums/strings/records keep their
  current boxing.
- All arithmetic is runtime imports — `__l4_rat_add/_sub/_mul/_div/_neg/_cmp`,
  plus the numeric builtins (`min/max/abs/floor/ceil/round/pow/...`) — computed
  exactly in `BigInt`. The handle table resets each call alongside the heap.
- **Consequence (locks an M7 choice):** putting arithmetic in JS `BigInt` ties
  the eval runtime to Node, so M7 isolation = Node `worker_threads` with
  terminate-on-timeout, **not** out-of-process wasmtime. Accepted.

### Three parity sub-problems (all must be exact)

1. **Exact arithmetic** — `BigInt` rational ops. Straightforward; the only
   subtlety is normalising (gcd + sign) after every op.
2. **Output: rational → Double** — jl4-core renders a non-integer result as
   `fromRational r :: Double` (integers go through the `Integer` path and are
   emitted exactly, _not_ as Double). `fromRational` is **round-half-to-even**
   (IEEE default), same as CPython's `long_true_divide` / `float(Fraction)`.
   The correct BigInt formulation: scale so the quotient mantissa has 53 bits,
   keep the **exact remainder**, and round with `2·rem vs den` (`>` up; `==`
   ties-to-even). The exact remainder subsumes guard+sticky, so this is
   provably correctly-rounded. **Validation oracle:** for operands `< 2^53`,
   JS `Number(n)/Number(d)` is itself correctly-rounded RNE — brute-check the
   BigInt path against thousands of random rationals.
3. **Output: Double → string** — _already matches._ Aeson encodes `Double` via
   bytestring `doubleDec` = **Ryu (shortest round-trip)**; JS `Number.toString`
   is spec-mandated shortest round-trip too, so the digits agree (this is why
   M0 is already byte-identical on fractional outputs). Residual risk: the
   scientific-notation threshold — validate empirically, normalise if needed.
4. **Input: JSON number → rational** — jl4-core parses a JSON number to
   `Scientific` then `toRational`, so `0.1` becomes exactly `1/10`. JS parses
   `0.1` to the _Double_ `0.1` (≈`0.1000000000000000055`); `toRational` of that
   diverges. Fix: parse request-body number literals from their **decimal text**
   (not the parsed Double) into an exact rational. Affects fractional inputs
   only — integer inputs are exact either way.

### Slices

- **Slice 1 — DONE (pure JS, no ABI change):** `runtime/rational.mjs` — BigInt
  rational core (`makeRat`, add/sub/mul/div/neg/cmp, normalise) + correctly-
  rounded `ratToDouble` + `ratToJSONValue` (integer vs Double path) + handle
  pool. 23 tests in `runtime/rational.test.mjs` pass, including: exact
  arithmetic (`1/10+2/10 == 3/10`, not f64 `0.30000000000000004`); ties-to-even;
  the M0 divergence case `199/12000 → 0.016583333333333332` (matching
  jl4-service, fixing the f64 `…335`); and **20,000 random rationals** verified
  against the `Number(n)/Number(d)` correctly-rounded oracle. Sub-problems 1+2
  de-risked.
- **Slice 2a — DONE (runtime ABI, dormant):** the rational core is inlined into
  `runtime/jl4-runtime.mjs` (single embedded source, so the `jl4-mlir run` CLI —
  which bakes the runtime in via Template Haskell and runs it through `node -e` —
  stays self-contained with no extra imports). Added a per-call rational pool +
  `__l4_rat_parse/_from_int/_add/_sub/_mul/_div/_neg/_cmp/_to_f64/_f64_to_rat`
  imports operating on f64-boxed handles. Dormant until the lowering emits them,
  so existing eval is untouched: 37 rational/ABI + 14 string tests pass,
  `jl4-mlir run` verified (`calculate-bonus → 25000`), and the parity harness is
  unchanged (gate PASS). `runtime/rational.test.mjs` now also exercises the
  imports end-to-end (`0.1+0.2 → 0.3` through `__l4_rat_*`).
- **Slice 2b — next:** the lowering itself. `NUMBER` box kind becomes a handle;
  route every numeric literal (→ `__l4_rat_parse` of the interned decimal),
  arithmetic op (`arith.addf`→`__l4_rat_add`, etc.), comparison
  (`arith.cmpf`→`__l4_rat_cmp`), and numeric builtin through the runtime; final
  marshal renders via `ratToJSONValue`. This is the invasive multi-week core.
- **Slice 3:** input decimal→rational parsing in the request marshaler.
- Gate each slice on the M0 harness: watch `ulp-differs` cells flip to
  `byte-identical`.

Files: `runtime/rational.mjs` (new), `runtime/jl4-runtime.mjs` (wire in),
`src/L4/MLIR/ABI.hs` (numeric box kind = handle), `src/L4/MLIR/Lower.hs` (all
arithmetic/literal/builtin lowering → runtime calls).

---

## Milestone 5 — Reasoning traces (instrumented codegen variant)

`jl4-service` returns `ResponseWithReason` with a nested reasoning tree when
`trace=full`; the tree is the interpreter's `EvalTrace`, built _during_
evaluation and converted by `traceToReasoning`
([Jl4.hs:1268-1286](../jl4-service/src/Backend/Jl4.hs)). Straight-line compiled
code has no such tree.

Approach — **two entry points per function**:

- `fn` — the existing fast, untraced path (unchanged; used when `trace=none`).
- `fn$trace` — instrumented: each lowered node brackets itself with runtime
  calls `__l4_trace_enter(labelPtr)` / `__l4_trace_exit(resultVal)`; the runtime
  assembles the tree and serializes it to the exact `Reasoning` JSON shape. The
  lowering already walks nodes with labels available, so instrumentation is
  bracketing each `lowerExpr` case — bounded but it touches every case.
- The proxy calls `fn$trace` only when the request asks for `trace=full`, so the
  fast path pays nothing.

Match `traceToReasoning`'s granularity (labeled decision nodes), not every
primitive op, to keep the tree identical.

Files: `src/L4/MLIR/Lower.hs` (instrumented variant), `runtime/jl4-runtime.mjs`
(trace sink + serializer), `src/L4/MLIR/Schema.hs` (advertise `fn$trace`).

### Slices

- **Slice 1 — DONE (wire-shape, no codegen yet):** the wasm-server and parity
  harness understand `?trace=full`; the runtime emits an Aeson-shaped envelope
  with a `Reasoning` value via `wrapEvaluationEnvelope` / `synthesizeReasoning`.
  Without instrumented codegen the runtime returns a degenerate one-node tree
  (function name + `Result: <pretty value>`), so the harness's new trace
  sub-matrix reads `12 trace-differs`. Aeson key-sorting is on in
  `aesonStringify` (required for nested Reasoning byte-id), and the M4 main
  parity matrix is unchanged at 12 byte-identical. The number of
  `trace-differs` cells is now the visible backlog for later slices — same
  pattern M4's `ulp-differs` cell played for exact arithmetic.
- **Slice 2A — DONE (compile-time strings + runtime label):** `FunctionExport`
  gains a `traceMeta` block (`fnName`, `body`, `params`) populated at
  `bundleExports` time by running jl4-core's `L4.Print.prettyLayout` on the
  function head, the `Decide` body, and each parameter — the exact strings
  jl4-service's interpreter feeds into `Reasoning.exampleCode`, so the
  runtime never reimplements the pretty-printer in JS. The runtime's
  `synthesizeReasoning` now uses `traceMeta.fnName` (the backticked source
  spelling, e.g. `` `is eligible` ``) instead of the sanitized API form. The
  parity harness's trace sub-matrix is still `12 trace-differs` — the shape
  is still a one-node stub — but the bytes are closer (correct backticked
  function name; ground truth for body / param strings is now available to
  later slices). Covered by `test "traceMeta pretty-print baked"` in the
  Haskell suite plus a slice-2A JS test in `rational.test.mjs`.
- **Slice 2B — DONE (ABI + body-root instrumentation):** the trace machinery
  is wired end-to-end. Lowering now emits a `<fn>$trace` clone for every
  `Decide` that brackets the body with `__l4_trace_enter(0)` /
  `__l4_trace_exit(box, kindByte)`; `Runtime.Builtins` declares the two
  externs; the JS runtime maintains a per-call `tracePool` (stack-based
  tree builder), and `invokeFunctionWithReasoning` routes to `<fn>$trace`
  whenever the symbol exists. The resulting Reasoning is the 3-node template
  (top synthesised application + fn-value child + body subtree from the
  pool). Result rendering covers NUMBER (rational handle → Aeson Double),
  BOOLEAN, STRING; records / lists / MAYBE inner values stay "other"
  (slice 3). All 12 main-parity cells stay `byte-identical` (no regression);
  the trace sub-matrix is still `12 trace-differs`, but the bytes match
  much further now (e.g. `is-eligible` 38 → 136, `calculate-bonus` ~40 → 140) — the body root and result rendering line up, the gap is now the
  body's _children_, which slices 2C/4 close.
- **Slice 2C — DONE (per-subexpression instrumentation):**
  `Schema.collectTraceNodes` walks each function body in pre-order,
  allocates a `TraceNode` for every traceable subexpression (skipping
  `Lit` and zero-arg `App` — the `Var` pattern — to mirror
  `simplifyEvalTrace` pruning), and renders each via `Print.prettyLayout`.
  `InfoMap` flows into `bundleExports` so node `resultKind` is set from
  the typechecker. The schema's `traceMeta.nodes` array carries
  `{id, exampleCode, resultKind}` per node. Lowering's `lowerExpr` is
  split into a wrapper (does the trace bracketing) and `lowerExprCases`
  (the existing per-shape cases); when `<fn>$trace` is being emitted,
  the wrapper looks up each expression's `(nodeId, kind)` by source range
  via `traceNodeMap` (built deterministically by the same
  `collectTraceNodes` walk) and emits `__l4_trace_enter(id)` /
  `__l4_trace_exit(box, kind)` around every traceable subexpression.
  Result: main parity still 12/12 byte-identical, trace sub-matrix still
  `12 trace-differs`, but `is-eligible`'s `firstDiffByte` jumps from
  **136 → 245** (the body's first two children — both `Geq` nodes — now
  match jl4-service exactly). The remaining gap on AND/OR-using
  functions is the IF-THEN-ELSE subtree the interpreter inserts when
  expanding `__AND__` / `__OR__` (slice 4). Functions with records-as-args
  (e.g. `denial-reason`) still diverge early because the top-level
  "fn OF args" header needs schema-driven value pretty-printing
  (slice 3). Covered by `test "traceMeta nodes populated"`.
- **Slice 3 — DONE (schema-driven value pretty-printer + arg-eval trees):**
  Runtime now ships `prettyL4Value(value, schema)` that walks a JS value
  using the parameter schema's `x-l4-type` (the L4 user-declared type
  name baked in by jl4-core's `typeToParameter`) to produce strings
  matching `Print.prettyLayout NF` byte-for-byte: records →
  `<TypeName> OF <field1>, <field2>, …` in `propertyOrder`; MAYBE →
  `JUST OF <inner>` / `NOTHING`, derived from the parent's `required`
  list; enum tags as bare names; primitives (NUMBER via Aeson Double
  formatter, BOOLEAN → TRUE/FALSE, STRING JSON-quoted, DATE/TIME
  pass-through). `synthesizeArgEvalTree(value, schema)` builds the
  Reasoning subtree jl4-service emits before the body trace for each
  _compound_ arg (records, JUST-of-record); MAYBE-of-primitive renders
  in the parent's `exampleCode` but doesn't push its own child node
  (jl4-service only pushes one when the body forces the field — a lazy
  semantic we can't detect statically; slice 4). `buildReasoningFromTrace`
  threads these as the top-level children before fn-value + body, so the
  child order is `[arg-evals…, fn-value, body]`, matching
  `traceToReasoning`.

  Result: main parity still 12/12 byte-identical, trace sub-matrix still
  `12 trace-differs`, but `firstDiffByte` advances on every record-arg
  function:

  | Function                                                  | slice 2C | slice 3 |
  | --------------------------------------------------------- | -------- | ------- |
  | `denial-reason`                                           | 51       | **159** |
  | `loan-is-approved`                                        | 51       | **159** |
  | `monthly-loan-payment`                                    | 51       | **159** |
  | `total-interest-paid`                                     | 51       | **159** |
  | `calculate-tax`                                           | 69       | **155** |
  | `effective-tax-rate`                                      | 69       | **155** |
  | `is-eligible`                                             | 245      | 245     |
  | `calculate-bonus`                                         | 153      | 153     |
  | `order-total` / `tickets-available` / `waitlist-position` | 51       | 51      |
  | `requires-foreign-income-disclosure`                      | 51       | 51      |

  The record-arg functions all match the top-level `<fn> OF (<record>
OF …)` header plus the first compound arg-eval child. Beyond byte 155,
  the diff is jl4-core's _lazy WHNF_ treatment: the inner Result line
  uses `(...)` placeholders for fields that were forced during body
  evaluation (slice 4's territory). List-of-records args
  (`order-total` etc.) stay at byte 51 because jl4-core desugars
  `[a, b, c]` into a deeply-nested `Cons` trace which we currently
  flatten into a single array node (also slice 4). Covered by 12 new
  unit tests in `rational.test.mjs` (records, MAYBE wrapping inside
  records, JSON-quoted strings, enum tags, dates,
  `synthesizeArgEvalTree`'s "MAYBE-of-primitive doesn't push child"
  rule, nested record children).

- **Slice 4A — DONE (AND/OR/NOT desugar + nested-call fn-value):**
  Schema marks @App "**AND**"@ / @"**OR**"@ / @"**NOT**"@ + @And@ /
  @Or@ / @Not@ AST nodes with a @tnSpecial@ flag. The runtime appends
  a synthetic @IF a THEN b ELSE FALSE@ sub-tree to each AND frame (and
  the symmetric forms for OR/NOT), applies short-circuit filtering on
  the rhs sibling, and renders the _taken_ branch leaf (rhs value for
  AND-TRUE, FALSE literal for AND-FALSE, etc.). Each `<fn>$trace` now
  brackets its body with `__l4_trace_enter_fn(symPtr)` /
  `__l4_trace_exit_fn()` and emits a synthetic fn-value frame at start
  — so nested calls produce a proper [arg-evals, fn-value, body] tree
  under the calling App. `callL4` routes user-function calls to the
  callee's `$trace` variant when tracing. IF-branch `Lit`/`Var`
  children are force-instrumented (the `ELSE 0` in `calculate-bonus`
  becomes a real trace leaf). The runtime's trace pool now accumulates
  multiple top-level roots (since each $trace call pushes both the
  fn-value AND the body frames), and node lookups are PER-FRAME via
  the fn-stack so a nested call's IDs resolve against the called
  function's `traceMeta.nodes`.

  Result: **2 cells flipped to `trace-byte-identical`** — `is-eligible`
  and `calculate-bonus` (which calls `is-eligible` recursively).
  Covered by `test "AND/OR/NOT marked special"` and
  `test "fnValue node + enter_fn/exit_fn"`.

- **Slice 4B — DONE (lazy-NF `(...)` placeholders in Result lines):**
  `prettyL4Value` gained an `inResult` / `depth` mode so the `Result: …`
  side of each trace node renders records with jl4-core's lazy-NF
  placeholder rule. Empirical heuristic (reverse-engineered from
  jl4-service's traces): a record's NUMBER fields render as `(...)`
  when EITHER (a) the record is nested inside another record (depth
  ≥ 1), OR (b) the record has no nested-record fields itself (no
  sub-record forcing happened, so its own primitives stay
  addressed). Records that contain nested records keep their direct
  NUMBER fields as values — only the inner record's fields get the
  placeholder.

  Result: `denial-reason`, `loan-is-approved`, `monthly-loan-payment`,
  `total-interest-paid` jump from `firstDiffByte=159` to **376/383**.
  The remaining diff is jl4-core's per-field lazy-evaluation order
  (some fields show as values because the body forced them, others
  stay `(...)`) — that's compile-time-undecidable without static body
  access analysis, so slice 4D or later.

- **Slice 4C — DONE (Cons-list desugaring):** `prettyL4Value` for
  arrays now emits jl4-core's `LIST <e1>, <e2>, ...` form (no
  brackets, compound elements wrapped in parens) and the lazy-NF
  variant `EMPTY` for empty lists. `synthesizeArgEvalTree` for an
  array routes through a new `buildConsListChildren` helper that
  produces the right nested `LIST → CONS head tail → ...` Reasoning
  tree jl4-service emits — each CONS node has exampleCode
  `(head) FOLLOWED BY (tail-list)` and children `[head-tree,
tail-list-tree]`; the empty tail is a `LIST ` leaf with Result
  `EMPTY`. The lazy-NF (...) rule from slice 4B applies to records
  inside list elements too.

  Result: `order-total`, `tickets-available`, `waitlist-position`
  advance from 51 → 173/108/108. The remaining diff is the same
  body-access lazy-NF issue (slice 4D — `age` is forced as NUMBER in
  Attendee while `attendee name` / `is member` aren't; we currently
  apply the inverse heuristic).

- **Slice 4D — DONE (per-field lazy-eval via runtime mark_forced):**
  Three pieces working together:

  1. **mark_forced runtime ABI.** New `__l4_mark_forced(pathPtr, fieldPtr)`
     declared in Runtime.Builtins; runtime maintains a per-call
     `forcedFields: Set<string>`. `prettyL4Value` gained `inResult` +
     `path` + `forced` opts so a record-field renders as VALUE when
     `forcedFields.has("path.field")` and as `(...)` otherwise.
     `synthesizeArgEvalTree` threads `path` through nested records and
     only pushes a sub-child when the field is forced.

  2. **Lowering emits mark_forced at each Proj.** `staticPathOf`
     recursively computes the path string for `Proj` expressions whose
     `record` is a param-rooted chain (`Var` ⟶ name; `Proj` ⟶ inner
     path + field). `lowerProjection`, in trace mode, emits
     `__l4_mark_forced(path, field)` before the slot load.

  3. **AND/OR short-circuit at the wasm level.** `lowerBoolop`
     replaced with `lowerAndShortCircuit` / `lowerOrShortCircuit` for
     `__AND__`/`__OR__`/`And`/`Or` — uses `scf.if` so the rhs (and
     any markers in it) only fires when the lhs demands it. This is
     what makes the marker set match jl4-core's lazy AND/OR.

  4. **Call-site arg-path translation.** New
     `__l4_trace_push_arg_path` / `__l4_trace_pop_arg_paths` builtins
     plus a runtime `argPathStack`. At each user-function call in
     trace mode the lowering pairs each arg with the helper's
     declared param name (from a new `funcParams` map in
     `LowerState`) and pushes the binding before the call (one Map
     per arg, popped after). Markers fired inside the helper get
     their path's first segment rewritten through the stack (e.g.
     `profile.bankruptcy history` ⟶ `req.applicant.bankruptcy history`).

  Result: substantial progress on every record-arg cell — 4 functions
  past 500 bytes matching, the Taxpayer cluster more than doubled,
  `requires-foreign-income-disclosure` jumped 51 ⟶ 418.

- **Slice 4E — DONE (value-pointer-identity mark_forced):** the path-
  based marker mechanism is replaced by a cleaner value-pointer
  approach:

  1. **Marshaler registers compounds by their wasm address.**
     `marshalStruct` adds each JS object to a per-call `valueByPtr:
Map<wasmPtr, jsObject>` map, indexed by the linear-memory
     address the struct was allocated at.

  2. **Marker carries the record's wasm value, not a path string.**
     `lowerProjection` emits `__l4_mark_forced(recordVal, fieldPtr)`
     — `recordVal` is the f64-boxed pointer of the record being
     projected. The runtime looks up the JS object via `valueByPtr`
     and attaches `value.__forced.add(field)` directly. Helper calls
     share pointers with their callers, so no path translation is
     needed (the helper's local `profile` is the SAME wasm pointer
     as the caller's `req's applicant`).

  3. **Renderer reads `__forced` off the JS object.** `prettyL4Value`
     in `inResult` mode applies the slice-4B `hasNestedRecord`
     heuristic at depth 0 (records with a nested-record field
     render all top-level fields as values; primitives-only records
     consult `__forced`); at depth ≥ 1 always consults `__forced`.
     `synthesizeArgEvalTree` likewise pushes sub-children only for
     forced compound fields by reading `value.__forced`.

  Slice 4D's path-based machinery is no longer needed:
  `staticPathOf`, `push_arg_path` / `pop_arg_paths` emission, the
  `funcParams` map and `argPathStack` runtime stack are all retained
  but unused — keeping the ABI surface intact for older bundles and
  documented in one place.

  Result: ALL list-arg cells jump significantly. `order-total` flips
  from 173 to **940** (the biggest single-function gain in M5
  so far) because list elements share pointers between marshaling
  and wasm-side iteration — `mark_forced` from inside the recursive
  list-walk lands on the same JS Attendee object the renderer
  visits. The remaining cells trade some bytes off vs slice 4D on
  the LoanRequest cluster (the per-trace-point lazy snapshot jl4-core
  takes mid-body isn't replicable from wasm — slice 4D's path
  translation got slightly closer for those particular cells by
  accident of the rendering order).

Final trace sub-matrix state at end of M5 slice 4E:

| Function                             | slice 4D | slice 4E |
| ------------------------------------ | -------- | -------- |
| `is-eligible`                        | ✓        | ✓        |
| `calculate-bonus`                    | ✓        | ✓        |
| `order-total`                        | 173      | **940**  |
| `requires-foreign-income-disclosure` | 418      | 418      |
| `monthly-loan-payment`               | 551      | 383      |
| `total-interest-paid`                | 541      | 383      |
| `denial-reason`                      | 525      | 376      |
| `loan-is-approved`                   | 541      | 376      |
| `calculate-tax`                      | 356      | 356      |
| `effective-tax-rate`                 | 319      | 319      |
| `tickets-available`                  | 108      | 108      |
| `waitlist-position`                  | 108      | 108      |

**2 byte-identical, 10 differs.** Sum of bytes matching across the
10 differing cells: slice 4D `3,640` → slice 4E `3,767` (≈+3.5%
overall).

- **Slice 4F — DONE (drop the `hasNestedRecord` heuristic):** the
  slice-4B depth-0 exception ("if the record has nested-record
  fields, render all its direct fields as values") was based on a
  misread of jl4-service's trace — it actually applies the lazy-NF
  `__forced` rule at _every_ depth, including depth 0. Once
  slice-4E's value-pointer markers populate `__forced` accurately,
  the same rule generalises. Two trace-result fixes landed alongside:
  `prettyResultText` accepts an optional `returnType` so the
  top-level `Result: …` line quotes STRINGs (`JUST OF "x"` vs
  `JUST OF x`), and `renderTraceResult` for kind `3` (OTHER) now
  looks up the result's wasm pointer in `valueByPtr` (extended to
  carry `{value, schema}`) and walks via `prettyL4Value` instead of
  leaking the raw f64 bit-pattern.

  Result: the LoanRequest cluster recovers everything 4D had while
  keeping 4E's `order-total` win:

  | Function                             | slice 4E | slice 4F |
  | ------------------------------------ | -------- | -------- |
  | `is-eligible`                        | ✓        | ✓        |
  | `calculate-bonus`                    | ✓        | ✓        |
  | `order-total`                        | 940      | **940**  |
  | `monthly-loan-payment`               | 383      | **551**  |
  | `loan-is-approved`                   | 376      | **541**  |
  | `total-interest-paid`                | 383      | **541**  |
  | `denial-reason`                      | 376      | **525**  |
  | `requires-foreign-income-disclosure` | 418      | **418**  |
  | `calculate-tax`                      | 356      | **356**  |
  | `effective-tax-rate`                 | 319      | **319**  |
  | `tickets-available`                  | 108      | 108      |
  | `waitlist-position`                  | 108      | 108      |

  **Sum across the 10 differing cells: 4,407 bytes** (vs 3,767 in 4E,
  3,640 in 4D — **+17%** vs 4E, **+21%** vs 4D). Frame-local
  `__forced` snapshots turn out NOT to be necessary — the live
  `__forced` set works fine once we stop overriding it with the
  depth-0 heuristic.

Two remaining systemic gaps blocking the last 10 cells:

1. **Intermediate-wasm-allocated compounds render as raw pointers.**
   `valueByPtr` only registers values the marshaler created — but
   when a body expression CONSTRUCTS a fresh compound (e.g. `IF cond
THEN JUST OF "..."`), that pointer isn't in the map. Trace nodes
   whose result is such a compound (kind = 3) fall through to
   `String(raw)` and emit garbage like `5.494e-321`. Fix would need
   either per-trace-node return-type metadata + a wasm-side
   unmarshaller, or compile-time-emitted compound markers that
   register intermediates in `valueByPtr` as they're built.

2. **Property-selector CONSIDER-chain desugaring.** `req's applicant`
   in jl4-core desugars to a deeper trace: an extra `attendees OF
order` (etc.) child wrapping a `CONSIDER LoanRequest WHEN ... THEN
applicant` node. We currently emit a flat Proj trace. Fix would
   need the compile-time AST walk to recognise `Proj` and inject the
   synthetic CONSIDER sub-tree (similar to slice 4A's AND/OR → IF
   injection).

#### Slice 4G (property-selector desugar) — landed

`Schema.collectTraceNodes` now flags `Proj` expressions with
`special = "PROJ"` and a `proj` payload (`appForm`, `fieldName`,
`considerEx`). The runtime renderer expands each into a 4-level
sub-tree:

```
record's field                    — original Proj
  └── field OF record             — App-form (children moved here)
        ├── (record arg-eval)
        ├── field                 — fn-value (Result: <function>)
        └── CONSIDER <Ty> ... THEN field
              └── field           — bound-var lookup
```

CONSIDER body indent matches jl4-core's prettyprinter
(`prefix.length + 1` columns, one past the first field's start).

#### Slice 4H (helper traceMeta exposure) — landed

`bundleHelpers` (a `Map wasmSymbol TraceMeta`) is built for every
non-@export'd `Decide` in the module tree — top-level helpers and
WHERE/LET locals alike — and serialized as `helperTraceMeta` in the
schema JSON. The runtime's `setBundleHelperTraceMeta` merges it
into `nodesByFn`, so when an instrumented body recurses into a
helper, the helper's trace_enter calls resolve their node IDs
against the helper's own table rather than corrupting them with
the caller's metadata.

Without this, `gross_tax$trace` (called from `calculate_tax$trace`)
was emitting frames whose `nodeId=2` was looked up in
`calculate-tax`'s 5-node table, picking up _its_ node 2 ("max OF
0, ((gross tax OF tp) MINUS (tp's tax credits))") instead of
gross-tax's "tax across brackets OF income, brackets". The visible
fallout was wild duplicated subtrees with raw-pointer results like
`7.273e-321` deep inside calculate-tax's trace.

#### Slice 4I (intermediate-compound rendering) — landed

`TraceNode.tnReturnSchema` carries a minimal layout-aware schema
(`RSScalar | RSEnum | RSMaybe | RSList | RSRecord`) for every
`kind=3` (compound) trace node. The runtime's `walkWasmValue`
decodes the wasm-allocated value at the result pointer using that
schema and feeds the reconstructed JS object to `prettyL4Value`,
matching jl4-service's render byte-for-byte. Layout assumptions
mirror `marshalStruct` / `marshalList` / `marshalMaybe`:

- `RSMaybe` — 16-byte block: tag (0 = NOTHING) + 8-byte payload.
- `RSList` — cons-cell head; each cell `{ item(8B), tail(8B) }`,
  NULL tail = end.
- `RSRecord` — `length fieldOrder * 8` bytes, one 8-byte slot per
  field in declaration order.
- `RSScalar` — the raw f64 is the value (decoded per `type`).

Combined slice 4G/4H/4I result on the 12-fn test corpus:

| Function                             | slice 4F | + 4G/4H/4I    |
| ------------------------------------ | -------- | ------------- |
| `is-eligible`                        | ✓        | ✓             |
| `calculate-bonus`                    | ✓        | ✓             |
| `denial-reason`                      | 525      | **IDENTICAL** |
| `requires-foreign-income-disclosure` | 418      | **IDENTICAL** |
| `monthly-loan-payment`               | 551      | **4,665**     |
| `total-interest-paid`                | 541      | **3,110**     |
| `order-total`                        | 940      | **1,874**     |
| `loan-is-approved`                   | 541      | **580**       |
| `effective-tax-rate`                 | 319      | **505**       |
| `calculate-tax`                      | 356      | **423**       |
| `tickets-available`                  | 108      | 108           |
| `waitlist-position`                  | 108      | 108           |

**4 byte-identical (vs 2 at slice 4F).** Sum across the 8 remaining
differing cells: **11,373 bytes** (vs 4,407 at slice 4F — **+158%**).

#### Slice 4J (cons-list head pruning + LIST … rule) — landed

Two related lazy-NF rendering fixes for list args:

- `marshalStruct` now initialises `__forced` as an empty non-enumerable
  `Set` on every JS arg. That lets the renderer distinguish "wasm ran
  but never forced this compound" (`__forced.size === 0`) from
  "synthetic context, no wasm" (no `__forced` property at all — the
  default-to-forced fallback). Non-enumerable keeps `__forced` out of
  the JSON when the harness re-sends the same args object to
  jl4-service.
- `buildConsNodeReasoning` skips the head sub-tree entirely when the
  head is a record with `__forced.size === 0` — matching jl4-core's
  lazy NF, which never traces a constructor whose fields stayed
  Omitted.
- `prettyL4Value`'s array NF branch collapses to the literal
  `LIST ...` (three ASCII dots) when every element is an unforced
  record — mirroring `prettyNFWithConstructorFields`'s
  `goList Omitted Omitted = "..."` clause in `L4.Print`.

#### Slice 4K (prelude helper trace routing) — landed

Two prelude-helper plumbing fixes so the trace tree reaches the
helpers' bodies:

- `bundleExports` now collects `Decide`s from dependency modules
  too (the prelude), not just the main module. The resulting
  `helperTraceMeta` entries cover prelude `count`/`go`/`max`/`min`
  etc., so frames pushed inside those bodies resolve their node IDs
  against the right table.
- `Lower.hs` intercepts for `min` / `max` / `count` were
  short-circuiting to runtime calls (`__l4_min` / `__l4_max` /
  `__l4_list_count`) even in trace mode, skipping the lowered L4
  body. They now check `tracingNow` and fall through to `callL4`
  (which routes to `<name>$trace`) when tracing — keeping the
  fast-path runtime call for the non-traced pass.

Net trace-parity after slice 4G–4K (12-fn corpus):

| Function                             | slice 4F | slice 4G–4K   |
| ------------------------------------ | -------- | ------------- |
| `is-eligible`                        | ✓        | ✓             |
| `calculate-bonus`                    | ✓        | ✓             |
| `denial-reason`                      | 525      | **IDENTICAL** |
| `requires-foreign-income-disclosure` | 418      | **IDENTICAL** |
| `monthly-loan-payment`               | 551      | **4,665**     |
| `total-interest-paid`                | 541      | **3,110**     |
| `order-total`                        | 940      | **1,874**     |
| `waitlist-position`                  | 108      | **1,661**     |
| `tickets-available`                  | 108      | **1,544**     |
| `loan-is-approved`                   | 541      | **580**       |
| `effective-tax-rate`                 | 319      | **505**       |
| `calculate-tax`                      | 356      | **423**       |

**4 byte-identical (vs 2 at slice 4F).** Sum across the 8 remaining
differing cells: **14,362 bytes** (vs 4,407 at slice 4F — **+226%**).

#### Slice 4L (Proj `resultKind` from declared field type) — landed

`Schema.resultKindOf` for a `Proj` expression now reads the field's
declared type out of `declares` directly (via a new `projFieldKind`
helper), bypassing the InfoMap. The InfoMap's range index doesn't
always have an entry for an inner Proj — particularly when the Proj
sits inside a comparison or other wrapping expression — so the
previous code fell through to the syntactic default and tagged
`tp's annual income` with `kind=3` (compound). `__l4_trace_exit`
then handed the renderer the raw f64 of the field's rational handle
under the compound code path, which `walkWasmValue` couldn't decode
and fell through to `String(raw)` — printing the (almost always
near-zero) handle's bit pattern as a decimal.

Result on `effective-tax-rate`: 505 → 1,344 (sum across the 8
differing cells 14,362 → 15,201 bytes).

#### Slice 4M (WHERE-binding wrappers) — landed

Each zero-arg `WHERE` / `LET IN` binding now gets a wrapper trace
node so the rendered Reasoning matches jl4-core's `traceToReasoning`
(`labelExample (Just resolved)`-style 2-string `exampleCode`):

- **Schema** — `TraceNode` gains `tnBindingLabel`. `collectTraceNodes`'
  `Where`/`LetIn` case walks the locals list through a new
  `chainLocals` helper that allocates one wrapper per zero-arg
  `LocalDecide` (`bindingLabel = name`, `exampleCode = rhs-pretty`,
  `resultKind = resultKindOf rhs`). Multi-arg locals are skipped
  (they're lambda-lifted to standalone `<fn>$trace` functions).
  Crucially we do NOT recurse into the rhs here — the wrapper's
  children come from the helper `$trace`'s own events firing inside
  the wrapper's enter/exit pair; an extra walk would emit a
  redundant "rhs outer" node and nest the wrapper inside it.
- **Schema** — final `collectTraceNodes` step now `sortOn tnId`
  instead of just `reverse`, because the prepended-then-sorted
  allocator no longer round-trips to id-sorted order under a simple
  reverse. (Runtime indexes `traceMeta.nodes` by `id`, so any
  out-of-order entry would make a frame's `lookupNode` pick up the
  wrong text.)
- **Lower.hs** — `lowerLocalDecl` for zero-arg locals brackets the
  rhs evaluation with `__l4_trace_enter(wrapperId)` /
  `__l4_trace_exit(value, kind)` in trace mode, looking up the
  wrapper ID via the `Decide`'s source range in `traceNodeMap`.
- **Runtime** — `renderFrame` prepends `node.bindingLabel` to the
  trace node's `exampleCode` array when set, producing
  `["income", "taxable income OF tp"]` on the wire.

Result on `calculate-tax`: 436 → 2,151; `effective-tax-rate`:
1,357 → 3,072 (sum across the 8 differing cells 15,227 → 18,657
bytes, **+22%**).

#### Slice 4N (operator-shape kind classifier + NUMBER var dispatch) — landed

Two correctness fixes for comparisons inside prelude helpers:

- **Schema** — `resultKindOf` for an `App` expression now matches on
  the operator name (`__GEQ__`, `__LEQ__`, `__PLUS__`, …) and assigns
  the right kind syntactically. The previous order tried the
  typechecker's `InfoMap` first, which is keyed off main-module
  source ranges and never has entries for prelude bodies — so
  `x AT LEAST y` was getting kind=3 (compound) and the runtime
  rendered the boxed boolean's raw f64 (`0` or `1`) instead of
  `TRUE`/`FALSE`. Plus reordered the per-shape cases (`Plus → 0`,
  `Geq → 1`, …) to try BEFORE the InfoMap so polymorphic types
  that come back as unresolved variables don't poison the result.
- **Lower.hs** — `LowerState` gains `bindingL4Types`; `lowerDecide`
  populates it (alongside the existing MLIR-typed `bindingTypes`)
  with each param's source-level `Type'` from the `TypeSig`.
  `isNumberExpr` for a `Var` reference (App with no args) consults
  this map when `typeOfExpr` returns Nothing — so a NUMBER-typed
  param's comparison dispatches through `__l4_rat_cmp` instead of
  comparing rational-pool indices as raw f64. Without this, `max`'s
  `x AT LEAST y` body did `arith.cmpf oge handle6 handle7` and almost
  always returned FALSE (the rational handles themselves obey no
  ordering).

Result: `calculate-tax` 2,151 → 2,190; `effective-tax-rate`
3,072 → 3,111 (sum 18,657 → 18,735). The remaining diff is a
trace-shape gap — jl4-core propagates a binding's label down
through nested call frames (so `income|max OF …` becomes
`income|IF …` becomes `income|x` for the taken branch), and the
force-traced `x`/`y` leaves in an IF need their kind set from the
enclosing function's param table (the schema currently has no
access to that during the walk).

#### Slice 4O (param-types + label inheritance + enum rendering) — landed

Four related fixes that unlocked the next layer of `traceToReasoning`
parity:

- **Schema** — `collectTraceNodes` gains a `Map Text (Type' Resolved)`
  param-type argument. `buildTraceMetaFromDecide` builds it from
  each `Decide`'s `TypeSig.GivenSig`. `resultKindOf` for `App _ n []`
  (Var) consults the map first, so a force-traced Var inside an IF
  branch gets the right kind even when the typechecker's `InfoMap`
  has no exact-range entry. Same plumbing for the lowering's
  `collectTraceNodes` call so the wasm-side rangeMap kinds agree.
- **Schema** — `collectTraceNodes` also takes a function-name →
  GIVETH-type map. `resultKindOf` for `App f args` (with args)
  consults it BEFORE the InfoMap — same coverage gap as Var
  references but for `taxable income tp`, `max 0 x`, etc.
  Recursive shape rules added for `IfThenElse` / `MultiWayIf` /
  `Consider` / `Where` / `LetIn` (their kind = the branches' kind),
  so the outer `IF … THEN … ELSE …` in a prelude body picks up
  NUMBER instead of falling to kind=3.
- **Schema** — for colliding helper names (e.g. the two `max`
  definitions in prelude — one NUMBER, one MAYBE NUMBER),
  `helperMetas` now uses `Map.fromListWith (\_ keep -> keep)` so
  the FIRST source-order entry wins, matching wasm-side
  `shouldSkip`'s dedup. Previously the schema described whichever
  definition came last in source order, which wasn't necessarily
  the wasm function that actually runs.
- **Schema** — `chainLocals` no longer skips the rhs's children:
  it recurses via `foldExprChildren` (not `goE`) on each
  zero-arg WHERE/LET binding's rhs so nested `Proj`/`IF`/`Cmp`
  expressions get their own trace nodes (slice 4G's Proj synth
  needs them for `brackets for status (tp's filing status)` etc.).
  We deliberately don't allocate a node for the rhs's OUTER App —
  the wrapper's enter/exit already brackets the call site.
- **Lower.hs** — `LowerState` gains `bindingL4Types :: Map Text
(Type' Resolved)`; `lowerDecide` populates it (alongside
  `bindingTypes`) from each param's source-level `Type'` from the
  `TypeSig`. `isNumberExpr` for a Var consults this map when
  `typeOfExpr` returns Nothing — so a NUMBER-typed param's
  comparison dispatches through `__l4_rat_cmp` instead of comparing
  rational-pool indices as raw f64. Without this, `max`'s body did
  `arith.cmpf oge handle6 handle7` and almost always returned FALSE.
- **Runtime** — `renderFrame` now propagates a `bindingLabel`
  down through the LAST child of each frame, matching jl4-core's
  recursive `(Trace lbl ((expr,kids):rest) val)` rewrite. The
  effect on the wire: `income|max OF …` becomes `income|IF …`
  becomes `income|x` (for the taken branch), nested through
  several levels of call frames.
- **Runtime** — `walkWasmValue`'s `enum` case now reads the
  integer tag off the raw f64 and looks up the constructor name
  in `schema.values`, instead of (mis)dispatching through
  `walkScalar(raw, "string")` which read a bogus C-string at the
  tag's bit pattern. Renders `Single`/`Married`/… in trace
  results instead of `0`/`1`/…

Result: `calculate-tax` 2,190 → 3,380; `effective-tax-rate`
3,111 → 4,301; `order-total` 1,874 → 3,303; `monthly-loan-payment`
4,665 → 5,069; `total-interest-paid` 3,110 → 3,514. Sum across the
8 differing cells: **18,735 → 23,352 bytes (+25%)**.

#### Slice 4P (force-trace literal CONSIDER branches) — landed

`foldBranches` in `collectTraceNodes` now passes @forced=True@ for
`MkBranch _ _ Lit{}` branch bodies — matching jl4-core's
@simplifyEvalTrace@, which keeps the literal taken-branch leaf
("0.199 :: Result: 0.199" for the @WHEN Uninsurable THEN 0.199@
clause). Non-literal branch bodies (Var references like @WHEN Single
THEN \`single brackets\`@) stay unforced: those routes go through
the helper's @<fn>$trace@, and force-tracing them here would emit
both the force-traced enter/exit AND the helper's body events while
jl4-core prunes the helper's body when it returns a constructor
value.

Result: `order-total` 3,303 → 6,336 (+3,033); `monthly-loan-payment`
5,069 → 5,794; `total-interest-paid` 3,514 → 4,239. Sum across the
8 differing cells: **23,352 → 27,835 bytes (+19%)**.

#### Slice 4Q (split Double formatters: Aeson vs Fixed) — landed

Trace text and value-level JSON disagree on how to render a
non-integer Double. jl4-core's @L4.Utils.Ratio.prettyRatio@ uses
@Data.Scientific.formatScientific Fixed Nothing@ — always decimal,
never scientific. The value-level JSON encoder still uses Aeson's
default (Ryu-shortest, scientific when @k=-1@). So the SAME rational
@1.6583333333333332e-2@ renders as:

- value-level (Aeson): `1.6583333333333332e-2`
- trace text (Fixed): `0.016583333333333332`

Until now both wasm-side paths went through a single
`formatAesonDouble`. Forcing it to one mode regressed the other.

Fix: added `formatFixedDouble` (same exponent decode but no
scientific cutoff — always emits decimal) and routed every trace-text
caller through it: `renderTraceResult` (kind=0), `prettyResultText`
(the trace's outer `Result: …` line for the function), and
`prettyL4Value` (the schema-driven NF prettyprinter for record /
list / MAYBE results). `aesonStringify` keeps using
`formatAesonDouble` so the wire bytes for the JSON value stay
Aeson-shaped.

Result: `monthly-loan-payment` flipped to byte-identical (was 5,794).
`total-interest-paid` flipped to byte-identical (was 4,239).
**6 byte-identical (vs 4 at slice 4O).** Sum across the 6 remaining
differing cells: **27,835 → 17,766 bytes** — the two flips removed
~10K bytes of remaining diff.

Net trace-parity through slice 4Q (12-fn corpus):

| Function                             | slice 4O | slice 4P  | slice 4Q      |
| ------------------------------------ | -------- | --------- | ------------- |
| `is-eligible`                        | ✓        | ✓         | ✓             |
| `calculate-bonus`                    | ✓        | ✓         | ✓             |
| `denial-reason`                      | ✓        | ✓         | ✓             |
| `requires-foreign-income-disclosure` | ✓        | ✓         | ✓             |
| `monthly-loan-payment`               | 5,069    | 5,794     | **IDENTICAL** |
| `total-interest-paid`                | 3,514    | 4,239     | **IDENTICAL** |
| `order-total`                        | 3,303    | **6,336** | 6,336         |
| `calculate-tax`                      | 3,380    | 3,380     | 3,380         |
| `effective-tax-rate`                 | 4,301    | 4,301     | 4,301         |
| `tickets-available`                  | 1,544    | 1,544     | 1,544         |
| `waitlist-position`                  | 1,661    | 1,661     | 1,661         |
| `loan-is-approved`                   | 580      | 580       | 580           |

**6 byte-identical, 6 trace-differs.** Sum across the 6 remaining
differing cells: **17,802 bytes**.

#### Slice 4R (helper-name arity disambiguation) — landed

Three changes working together to fix the prelude `go` collision:

- **Schema** — new `collectAllDecidesWithArity` walks the module
  tree maintaining a "scope" set of variables visible at each
  Decide's definition point. Computes each Decide's post-closure
  arity = source params + captured free vars (free vars in the
  body that resolve to the enclosing scope, after excluding
  top-level names and the Decide's own params/self-name). Also
  inlines `freeVarsOfExpr` from Lower.hs since the two can't share
  a module without inducing an import cycle (Lower imports Schema
  for the bundle types).
- **Schema** — mirrors Lower's `sigHasFunctionParam` filter:
  Decides with a function-typed parameter are SKIPPED entirely
  (their bodies become externs in the wasm; their WHERE locals
  never get a `<fn>$trace`). Without this, foldr's `go` and
  foldl's `go` would shadow count's `go` at the schema's
  helperTraceMeta key even though their wasm functions don't
  exist.
- **Schema** — `helperTraceMeta` keys are now
  `<name>__<postClosureArity>` instead of bare `<name>`. The
  combiner is `fromListWith (\_new old -> old)` (keep-first),
  matching Lower's `dedupByName` which scans source-chronological
  order and keeps the first occurrence per mangled name.
- **Lower.hs** — `lowerDecide` interns `<funcName>__<length params>`
  (where `params` already includes closure-extended args) as the
  `__l4_trace_enter_fn` symbol. This is the same key the schema
  uses, so the runtime's `nodesByFn[sym]` lookup matches per-helper
  rather than collapsing all `go`s onto one entry.
- **Runtime** — `setBundleFunctions` builds the export key as
  `<wasmSymbol>__<paramOrder length>` (minus 2 deontic extras),
  matching Lower's interning convention for exports too.

Net effect on the 12-fn corpus:

| Function                             | slice 4Q | slice 4R  |
| ------------------------------------ | -------- | --------- |
| `is-eligible`                        | ✓        | ✓         |
| `calculate-bonus`                    | ✓        | ✓         |
| `denial-reason`                      | ✓        | ✓         |
| `requires-foreign-income-disclosure` | ✓        | ✓         |
| `monthly-loan-payment`               | ✓        | ✓         |
| `total-interest-paid`                | ✓        | ✓         |
| `order-total`                        | 6,336    | **6,465** |
| `calculate-tax`                      | 3,380    | 3,380     |
| `effective-tax-rate`                 | 4,301    | 4,301     |
| `tickets-available`                  | 1,544    | **1,673** |
| `waitlist-position`                  | 1,661    | **1,790** |
| `loan-is-approved`                   | 580      | 580       |

**6 byte-identical, 6 trace-differs.** Sum across the 6 remaining
differing cells: **17,802 → 18,189 bytes**. tickets-available /
waitlist-position / order-total each advanced 129 bytes — the
post-disambiguation lookups now find count's `go` (not foldr's),
so the trace renders `CONSIDER l WHEN EMPTY THEN acc, …` text
instead of foldr's `cons OF x, (go OF xs)` text. The new wall
is force-traced @Var@ bodies in CONSIDER branches (`WHEN EMPTY
THEN acc` — svc renders @acc :: 1@; our slice 4P only
force-traces Lit bodies).

### Slice 4S — constructor-helper expansion + Var leaves + lazy-NF heuristics

Closes the constructor-helper expansion gap on calculate-tax /
effective-tax-rate / order-total, plus the CONSIDER Var-body leaves
on tickets-available / waitlist-position — all five flip to
trace-byte-identical.

Three coordinated fixes:

1. **`App _ _ []` (Var) as a single leaf in trace mode.** Schema's
   `foldBranches` now force-traces CONSIDER branch bodies that are
   Var references (App with no args), not just Lits. Lower adds
   `callL4Direct` and routes 0-arg App calls through it whenever
   `tracing` is true — bypassing the `$trace` dispatch so the
   helper's body events don't pollute the parent's trace pool.
   jl4-core treats a bare Var lookup as a single leaf with the
   helper's already-evaluated value (lazy NF); the parent's
   force-trace wrapper now provides that leaf, with the correct
   kind from the schema's resultKindOf chain.

2. **Schema kind inference for un-annotated helpers.** Most prelude
   helpers (`count`'s `go acc l MEANS …`, `at`'s recursive helper, …)
   have no explicit GIVEN annotations or GIVETH. Schema previously
   defaulted these to kind 3 (compound), so a Var leaf on `acc`
   rendered as a raw pointer (e.g. `2.5e-323`). Two new sets thread
   through `collectTraceNodes`: `unannotatedParams` (per-Decide,
   defaults Var leaves to NUMBER kind 0) and `unannotatedFnNames`
   (bundle-wide, defaults un-annotated helper return types to
   NUMBER). Lower mirrors the param-set default via a Lower-side
   kind override in the `lowerExpr` wrapper, consulting
   `bindingTypes` for the actual MLIR type — the runtime then uses
   `frame.result.kind` as an override when the schema's kind is
   the default 3.

3. **`returnSchema` fallback + lazy-NF record heuristic.**
   `goE`'s and `chainLocals`' retSchema computation now falls back
   to `fnReturnTypes[head]` when the InfoMap has no entry for the
   App's range — closing the case where `brackets for status OF (…)`
   carries kind 3 but no walkable schema. The runtime renderer
   also unwraps `{__l4_aeson_double: x}` / `{__l4_raw_json: s}`
   tagged values in `prettyL4Value`'s number branch (was leaking
   `[object Object]` for record fields decoded from wasm memory),
   and uses Aeson Ryu form when `inResult` is false (exampleCode
   mode) vs Fixed form when true (trace's "Result: …" line) —
   matching jl4-service's split between `prettyLayout` and
   `prettyRatio`. For body-allocated records walked via
   `walkWasmValue`, the renderer attaches a `__forced` set
   containing only the first field — approximating jl4-core's
   lazy-NF rendering of a LIST element where only the head field
   has been demanded at the trace observation point (e.g.
   `TaxBracket OF 0, (...), (...)`).

Net effect on the 12-fn corpus:

| Function                             | slice 4R | slice 4S |
| ------------------------------------ | -------- | -------- |
| `is-eligible`                        | ✓        | ✓        |
| `calculate-bonus`                    | ✓        | ✓        |
| `denial-reason`                      | ✓        | ✓        |
| `requires-foreign-income-disclosure` | ✓        | ✓        |
| `monthly-loan-payment`               | ✓        | ✓        |
| `total-interest-paid`                | ✓        | ✓        |
| `order-total`                        | 6,465    | ✓        |
| `calculate-tax`                      | 3,380    | ✓        |
| `effective-tax-rate`                 | 4,301    | ✓        |
| `tickets-available`                  | 1,673    | ✓        |
| `waitlist-position`                  | 1,790    | ✓        |
| `loan-is-approved`                   | 580      | 580      |

**11 trace-byte-identical, 1 trace-differs.** The only remaining
gap is `loan-is-approved`'s NOT-range collision (slice 4N) — a
parser-level limitation where @NOT P@ and the inner @P@ share a
`SrcRange`, so both `lowerExpr` wrappers look up the same node and
emit duplicate trace events.

### Slice 4T — final cell: NOT-range disambiguation + NOT-desugar taken branch

Closes the last trace-differs cell (`loan-is-approved`) to land at
**12/12 trace-byte-identical**. Two coordinated fixes:

1. **Trace 'rangeMap' is now keyed by `(SrcRange, Maybe Text)`.**
   When the parser collapses a unary operator and its inner
   expression into a shared 'SrcRange' (NOT and its operand here),
   the old `Map SrcRange (Int, Int)` clobbered one of the two
   entries — Lower's wrapper then either lost the inner PROJ
   subtree entirely or duplicated NOT's subtree depending on which
   side won. A new module-level helper 'exprDisambiguator' pairs
   each 'SrcRange' with a per-constructor tag (Just "Not", Just
   "Proj", …); Schema's @goE@ inserts using that key and Lower's
   wrapper looks up using the same key on the AST expr it's
   currently lowering, so NOT and the inner PROJ get distinct
   entries and both fire as expected. WHERE-binding wrappers in
   'chainLocals' use the sentinel '(rng, Nothing)' — Decide ranges
   encompass the whole binding and never collide with an Expr
   range. A defensive 'openTraceNode' field on 'LowerState' guards
   against any residual same-shape same-range case by skipping a
   nested wrapper that would re-enter the currently-open node.

2. **'synthesizeNotDesugar' now emits the taken-branch leaf.**
   The runtime's NOT desugar previously rendered only the @a@
   input leaf inside @IF a THEN FALSE ELSE TRUE@. svc also emits
   the _taken_ branch as a leaf — @FALSE@ when @a@ is TRUE, @TRUE@
   when @a@ is FALSE — mirroring AND/OR's pattern in
   'synthesizeBoolDesugar'. Adding that second child closes the
   final byte gap.

Final corpus state:

| Function                             | slice 4S | slice 4T |
| ------------------------------------ | -------- | -------- |
| `is-eligible`                        | ✓        | ✓        |
| `calculate-bonus`                    | ✓        | ✓        |
| `denial-reason`                      | ✓        | ✓        |
| `requires-foreign-income-disclosure` | ✓        | ✓        |
| `monthly-loan-payment`               | ✓        | ✓        |
| `total-interest-paid`                | ✓        | ✓        |
| `order-total`                        | ✓        | ✓        |
| `calculate-tax`                      | ✓        | ✓        |
| `effective-tax-rate`                 | ✓        | ✓        |
| `tickets-available`                  | ✓        | ✓        |
| `waitlist-position`                  | ✓        | ✓        |
| `loan-is-approved`                   | 580      | ✓        |

**12 trace-byte-identical, 0 trace-differs.** M0 differential
parity stays at 12/12 byte-identical.

Known still-broken: `freeVarsOfExpr` doesn't catch the
function-name head of `App _ headRes args` (e.g. @cons@ in
@cons OF x, go xs@) as a free variable, so foldr's go is
incorrectly classified as arity 2 instead of arity 3 by BOTH
Schema and Lower. We rely on the fact that foldr / foldl are
filtered out (function-typed param) so this miscount doesn't
surface — but if a non-filtered helper depended on this bug
the schema and Lower would still agree.

All three previously-documented gaps were closed by slices 4S and 4T:

- **Constructor-helper expansion** — closed by slice 4S's
  `callL4Direct` + force-traced Var-body branches + lazy-NF first-field
  heuristic in `walkWasmValue`.
- **Prelude helper-name collision** — closed earlier by slice 4Q's
  post-closure arity mangling (`go__N`); `unannotatedFnNames` /
  `unannotatedParams` in slice 4S kept the inferred kinds aligned.
- **NOT-range collision** — closed by slice 4T's `(SrcRange, Maybe Text)`
  rangeMap keying via `exprDisambiguator`.

### M5 outcome

**12/12 byte-identical (M0)** and **12/12 trace-byte-identical (M5)**
on the 12-fn fixture. Regression tests in `test/Main.hs`:

- `traceMeta nodes populated`, `AND/OR/NOT marked special`,
  `fnValue node + enter_fn/exit_fn`, `NOT-range disambiguation`.

The trace-mode sub-matrix has burned down from "no instrumentation
at all" (slice 1) to byte-for-byte equal with jl4-service's
reasoning trees.

---

## Milestone 6 — Deontic (highest risk, do last)

Deontic eval replays a timed `events` stream from `startTime` against the
regulative structure, producing fulfilment/breach trace; jl4-service implements
it by generating an EVALTRACE wrapper and running it through the interpreter
over first-class `ValObligation` values
([Jl4.hs:539-574](../jl4-service/src/Backend/Jl4.hs),
[CodeGen.hs:512-579](../jl4-service/src/Backend/CodeGen.hs),
[ValueLazy.hs:57](../jl4-core/src/L4/Evaluate/ValueLazy.hs)).

Approach — **step-function codegen + TS replay driver** (keeps the hard temporal
logic where it's easy to get right and port for parity):

- Compile each regulative rule to a WASM transition:
  `step(statePtr, eventPtr) -> (newStatePtr, breachInfo)` — wasm handles the
  per-event predicate work (deadline comparisons, party/action matching,
  guards), which it's good at.
- Keep the event-replay loop, obligation bookkeeping, and trace assembly in the
  TS/Node runtime, **ported directly from the interpreter's deontic loop** so
  the breach/fulfilment ordering and `ReasonForBreach` output match exactly.
- This depends on M5 (trace machinery) and M1 (regulative no longer lowers to
  0.0).

Risk: this is where compiled code fights the interpreter's home turf. Budget
generous parity-debugging time; the differential harness (M0) carries the most
weight here.

Files: `src/L4/MLIR/Lower.hs` (regulative → step fns), `src/L4/MLIR/ABI.hs`
(obligation/event layout), `runtime/jl4-runtime.mjs` (replay driver + trace),
`src/L4/MLIR/Schema.hs` (`isDeontic`, event/party/action types).

### Architecture choice — JS-side interpreter, not step-function codegen

The plan originally proposed a WASM step function per regulative rule. We
chose the **JS-only** variant after the operator clarified that future
event flows will come from a log-stream sink with checkpointed intermediate
state: the interpreter loop needs to live somewhere a checkpoint can be
snapshotted cheaply (≠ WASM linear memory). The wasm body for a deontic
function is now an inert `arith.constant 0.0` placeholder; the schema
bakes the contract tree as JSON metadata and the runtime's
`runDeontic` walks it against the request's `startTime` + `events`.

### Slice 1 — JS interpreter foundation, simple cascade

Lands the schema/runtime/HTTP plumbing on the canonical
'the sale contract' fixture (jl4-service's `deonticExportJL4`).

- **Schema** — new `DeonticContract` ADT (`OBLIGATION` / `FULFILLED` /
  `BREACH`) baked under `FunctionExport.deonticContract`; party / action
  / deadline pre-rendered via `L4.Print.prettyLayout` so the runtime
  never re-runs the pretty-printer. `extractDeonticContract` walks
  `Regulative` / nested HENCE-LEST / bare FULFILLED / `Breach`.
- **Lower** — `Regulative{}` / `Breach{}` stop `markUnsupported`-ing
  (they emit an `arith.constant 0.0` placeholder). The wasm body is
  never invoked for deontic; the runtime detects `meta.isDeontic`
  and dispatches to `runDeontic`.
- **Runtime** — new `runDeontic` walker + `DeonticInputError`
  sentinel. `invokeFunction` / `invokeFunctionWithReasoning` route
  deontic calls through the JS interpreter. Event matching strips
  backticks from contract-side strings so '`the seller`' matches
  the bare '"the seller"' that arrives on the wire.
- **Worker/server** — `startTime` / `events` forwarded through the
  worker `eval` message; merged into args when `meta.isDeontic`,
  refused on non-deontic (400). `DeonticInputError` → 400, non-fatal.

5/5 scenarios byte-identical against jl4-service:
FULFILLED · initial OBLIGATION · residual OBLIGATION · 400 on missing
startTime · 400 on missing events.

### Slice 2 — IF guards + record-typed parties

Closes the 'Seatbelt Requirement' fixture
(jl4-service's `deonticRecordPartyJL4`).

- **Schema** — `DeonticContract` gains `DCIfThenElse { cond, then, else }`
  for branching on GIVEN parameters; party / action move from a flat
  `Text` to `DeonticExpr` (`DELiteral` or `DEParam`). New
  `DeonticGuard` ADT (lit / var / proj / eq / and / or / not / lt /
  gt / leq / geq) covers IF conditions like
  `car's number of wheels EQUALS 4`. `exprToGuard` handles both the
  direct-AST forms (`Equals` / `Plus` / …) and the `App "__OP__"`
  desugared variants the typechecker leaves depending on context.
- **Runtime** — `runDeontic` walks `IF` branches via
  `evalDeonticGuard`; `MAY` modal terminates as FULFILLED when no
  matching event arrives (permission, not requirement). New
  `obligationStart` tracks per-obligation start time; deadline
  surfaces as a JSON _number_ ("remaining") when `now` has
  advanced past the obligation start, else the original WITHIN
  literal as a _string_. Record-typed parties get tagged with their
  L4 type name from the schema's `x-l4-type` stamp
  (`{Driver: {name: "Alice"}}`).

3/3 scenarios byte-identical:
4-wheel-FULFILLED · 4-wheel-drive-only OBLIGATION · 3-wheel-FULFILLED.

### Slice 3 — parity-harness wiring

Locks the deontic fixtures into the default harness run so they
gate every change. Two `.cases.json` sidecars (`deontic-sale.cases.json`
and `deontic-seatbelt.cases.json`) supply concrete `{arguments,
startTime, events}` request bodies — the schema-driven `genArgs`
can't synthesize meaningful event streams, so deontic functions
without curated cases are surfaced as `skip-no-cases` in the
report.

Harness changes:

- `serviceEval` accepts an optional `deonticExtras` and merges it
  into the request body shape `{arguments, startTime, events}`.
- Each case is now either `{arguments, startTime, events}` (deontic)
  or a bare args bag (non-deontic). The runtime call merges
  `startTime` / `events` into the args bag before dispatch.

Net effect — full corpus parity tally:

|                            | M0 byte-identical | M5 trace-byte-identical |
| -------------------------- | ----------------- | ----------------------- |
| 12-fn fixture (test.l4)    | ✓ 12              | ✓ 12                    |
| deontic-sale (3 cases)     | ✓ 3               | ✓ 3 (after slice 4)     |
| deontic-seatbelt (3 cases) | ✓ 3               | trace-differs (no gate) |
| **Total**                  | **18 / 18**       | **15 / 18**             |

### Slice 4 — synthesize the deontic `TraceResponse` (parameter-less)

Reconstructs the synthetic 'EVALTRACE OF <fn>, <startTime>,
<events>' reasoning tree jl4-service emits for `?trace=full`, so
the trace-mode column lights up byte-identical for the
parameter-less deontic fixture (`deontic-sale`'s 3 scenarios).

- `runDeonticInternal` now returns
  `{value, residual, residualDeadline, ctx}` so the synthesizer
  can pretty-print the residual contract (the "a OF b, c" Result
  line shows the remaining `PARTY … MUST … WITHIN <remaining>`
  text after the cascade so far).
- New `synthesizeDeonticReasoning` builds the four-part
  `EVALTRACE` wrapper:
  events arg-eval Cons chain + `EVALTRACE` fn-value leaf +
  `a OF b, c` call frame + inner `a` body leaf. Mirrors svc's
  lazy-NF rendering of the events list — when the outcome is
  residual OBLIGATION the simulator's full traversal materializes
  the deepest Cons's empty tail and drops the `", ..."` suffix;
  for FULFILLED / BREACH the list stays a lazy chain with
  head-only children + `", ..."` continuation markers.
- New `renderContractAsSource` pretty-prints a `DeonticContract`
  subtree back to L4 source (`PARTY …\nMUST …\nWITHIN …\nHENCE …`
  one clause per line) — used for both Result lines on residual
  OBLIGATION responses.
- New `deontic-trace-baseline.json` (captured from jl4-service)
  drives the e2e test in `/tmp/test-deontic-trace-e2e.mjs` which
  diffs all 6 deontic trace scenarios byte-for-byte.

Parameterised deontic functions (seatbelt) still surface as
`trace-differs` — svc generates a much richer wrapper involving
`decodeArgs` / `JSONDECODE` / `CONSIDER InputArgs WHEN …` that
mirrors its generated EVALTRACE shell. Synthesising that takes
substantially more code for parity gain on three test cells;
deferred until a real consumer needs deontic-trace mode on a
parameterised function.

Still open: BREACH/by/because rendering (no fixture exercises it
yet); error envelopes for events provided to non-deontic functions
(we 400 with a different message than svc's); the parameterised
'TraceResponse' wrapper for the remaining 3 deontic trace cells.

---

## Milestone 7 — Runtime hardening (lands with the proxy work)

- Wire `--wasmtime`/`--wasmer` to the schema-driven marshaler (today Node-only),
  or commit to Node `worker_threads` with terminate-on-timeout + `Memory`
  `maximum` cap. Either way: per-eval timeout, memory ceiling, trap isolation so
  a faulty module logs an error instead of taking down the host.
- Single warm instantiation per module per worker (cold GHC-wasm RTS / module
  instantiation cost amortized across requests).

This milestone is the seam to the auth-proxy integration and can proceed in
parallel once M1 lands.

### Slice 1 — memory cap, trap recovery, peak telemetry

Lands the runtime side of the four-item list above (everything except
the worker-threads timeout):

- **Memory ceiling.** `createRuntime({ maxHeapBytes })` defaults to
  64 MiB; `allocBytes` throws a `MemoryLimitError` once a single eval
  would push past it. Caps the JS heap pressure from a runaway wasm
  body (infinite cons chain, unbounded grow) without needing
  `WebAssembly.Memory(maximum:)` — the bump-pointer is the
  authoritative allocator at this layer.
- **Warm instantiation.** `wasm-server.mjs` now compiles the wasm
  bytes once into a `WebAssembly.Module` and re-instantiates the
  module after every failed eval — keeps the cold-start cost a
  one-time tax per process while still recovering from a trap
  (which leaves the instance unusable per WebAssembly spec) without
  taking the host down.
- **Trap isolation.** The request handler maps a `MemoryLimitError`
  to 413, a `WebAssembly.RuntimeError` (and other exceptions) to 500,
  and triggers a re-instantiation in either case so the next request
  starts from a clean memory + a clean runtime state.
- **Peak-allocation telemetry.** Every response carries
  `x-jl4-peak-heap-bytes` / `x-jl4-max-heap-bytes` headers, so the
  proxy can monitor per-eval memory pressure (and raise the cap when
  a real rule needs it) without instrumenting the wasm itself.

`JL4_MAX_HEAP_BYTES` env override lets ops tune the cap per
deployment. Regression tests in `runtime/jl4-runtime.test.mjs`
(memory-cap throws + resetHeap clears state) and a new e2e harness
at `scripts/wasm-server.test.mjs` (happy path + header surface +
404 doesn't trigger re-instantiation) lock the contract.

### Slice 2 — worker_threads + per-eval timeout

Moves the wasm runtime + instance into a Node `worker_thread`. The
parent owns request routing, the wall-clock budget, and the
terminate-and-respawn lifecycle; the worker owns the warm runtime.

- **Process layout.** `wasm-server.mjs` keeps one warm worker (single
  in-flight eval at a time — matches the pre-slice-1 throughput
  shape; a pool comes in slice 4). The worker stays cold-cached
  across requests; the parent re-spawns it after every fatal failure.
- **Per-eval timeout.** `JL4_EVAL_TIMEOUT_MS` (default 5000 ms)
  arms a `setTimeout` for each posted message. If the budget
  elapses with no reply, the parent `terminate()`s the worker,
  returns a 504, and starts spawning a replacement. The next
  request blocks briefly on `spawnWorker()` rather than getting
  served by a hung worker.
- **Fatal-failure respawn.** The worker marks `MemoryLimitError`
  and `WebAssembly.RuntimeError` responses with `fatal: true` so
  the parent kills + respawns it even on a graceful HTTP-level
  failure (a trap leaves the wasm instance unusable per spec; an
  OOM leaves the bump-pointer state inconsistent).
- **Headers.** `x-jl4-eval-timeout-ms` joins the existing
  `x-jl4-peak-heap-bytes` / `x-jl4-max-heap-bytes` so the proxy
  can correlate slow responses with budget.

`scripts/wasm-server.test.mjs` covers the happy path through the
worker, the 404 short-circuit (no respawn), and the timeout-header
contract. A true wall-clock timeout assertion needs a fixture
slow enough to outrun `setTimeout` (>2 ticks) — the M0 corpus is
sub-ms, so the timeout-path itself is exercised manually for now.

### Slice 3 — backend abstraction (wasmtime / wasmer entry point)

The `runtime/wasm-backend.mjs` module factors `WebAssembly.compile` +
`WebAssembly.instantiate` behind a tiny interface so alternative
runtimes can plug in without restructuring the marshaling code:

```js
{ name, compile(bytes) -> Module, instantiate(Module, imports) -> { instance } }
```

`createBackend(name)` resolves built-in `'v8'` (the default) or
treats any other string as an ES-module specifier whose default
export is a backend record. A missing or invalid module
**soft-fails** to V8 with a stderr warning — a misconfigured
`JL4_BACKEND` env doesn't take a deploy down.

`wasm-worker.mjs` consumes the abstraction; `wasm-server.mjs` plumbs
the env knob through `workerData` and stamps `x-jl4-backend` on
every response so the proxy can see which runtime served the call.
V8 stays the only shipped backend; wasmtime/wasmer can land later
as their own modules without changes here.

Unit tests in `runtime/wasm-backend.test.mjs` cover the V8 default,
the round-trip on a minimal module, and the missing-module fallback.

### Slice 4 — worker pool for concurrency

Replaces the single in-flight worker with a `createWorkerPool(N)`
abstraction. Each slot owns one warm worker + runtime + instance;
requests dispatch to the first idle slot, queue when all are busy.

- `JL4_WORKER_POOL_SIZE` (default 4) sets the slot count.
- Per-slot lifecycle: a fatal result (`MemoryLimitError` /
  `WebAssembly.RuntimeError`) or timeout kills the worker; the
  slot's `exit` handler auto-respawns into the same slot and
  drains any queued work behind it.
- `x-jl4-pool-size` / `x-jl4-pool-queued` headers expose queue
  depth so the proxy can detect saturation.
- Graceful shutdown: SIGTERM / SIGINT closes the listener, then
  terminates every worker.

Tests: `scripts/wasm-server.test.mjs` now fires 12 concurrent
requests against a pool of 3, asserts every response returns the
correct cubed value, and confirms the pool-size header reflects
the env knob.

**M7 status:** the four bullets on the original M7 list (per-eval
timeout, memory ceiling, trap isolation, warm instantiation) are
done; the proxy seam now has stable runtime hardening + a backend
extension point for the longer-term wasmtime/wasmer story.

---

## Decisions required

1. **Exact numbers (M4): boxed rationals, hybrid exact-mode, or accept f64 +
   route exact functions to fallback?** This is the crux — boxed rationals buy
   full correctness but spend most of the speedup that justified MLIR.
2. **Is MLIR the _sole_ engine, or the fast tier of a hybrid?** If a fallback
   evaluator stays for deontic/exact/trace, M4 and M6 can be deferred or scoped
   down, and "up to speed" means Tiers 1–3 + fail-loud routing. If MLIR must
   stand alone, all six milestones are mandatory and the timeline is ~4–5 months.
3. **Trace fidelity bar:** byte-identical reasoning tree, or
   "semantically-equivalent" tree? Byte-identity (M5) is materially more work.
4. **Isolation runtime (M7):** Node workers (simpler, reuses jl4-runtime.mjs) vs
   out-of-process wasmtime (stronger CPU/memory guarantees).

## Risk register

- **Silent degradation (regulative → 0.0)** — _highest_; mitigated by M1a,
  must land first.
- **Deontic parity divergence** — high; mitigated by porting the interpreter
  loop verbatim + M0 harness.
- **Numeric divergence on financial rules** — high; only fully mitigated by M4
  boxed rationals.
- **Speed regression once M4/M6 land** — medium; mitigated by opt-in variants
  and keeping the f64 fast path for eligible functions.
- **Coverage cliffs (the 52 + future language features)** — medium; mitigated
  by fail-loud + proxy routing to fallback.

## Suggested sequencing

```
M0 parity harness ─┐ (gate)
M1 fail-loud + strings ──► trustworthy fast tier
M2 coverage triage ──────► known support surface
M3 query-plan/state-graphs (parallel; helps proxy regardless)
        │
        ▼ (decision point: sole engine vs hybrid)
M4 exact numbers ──┐
M5 traces ─────────┼─► full parity
M6 deontic ────────┘
M7 runtime hardening (parallel from M1 onward, lands with proxy)
```

Stop after M1–M3 if the answer to Decision #2 is "hybrid" — that's the
high-value, speed-preserving subset and it's ~3–4 weeks. M4–M6 are the long
poles that only pay off if MLIR must be the sole engine.
