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
