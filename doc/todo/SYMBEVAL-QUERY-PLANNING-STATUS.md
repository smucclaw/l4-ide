# Symbolic Evaluation / Query Planning / Boolean Minimization — Status

Planning window: 2025-11 → present  
Branch (recent work): `mengwong/symbeval`

This doc is the project tracker for our current line of work around:

- **Symbolic evaluation** (partial evaluation under unknowns)
- **Query planning** (what to ask next, and why)
- **Boolean minimization / relevance** (don’t-care detection, impact analysis)

It complements the deeper design specs:

- `doc/todo/BOOLEAN-MINIMIZATION-SPEC.md`
- `doc/todo/PARTIAL-EVAL-VISUALIZER-SPEC.md`

## Current Status (High Level)

| Area                                          |       Status | Notes                                                                                                                   |
| --------------------------------------------- | -----------: | ----------------------------------------------------------------------------------------------------------------------- |
| TS partial-eval analyzer (local, interactive) |           ✅ | Uses ROBDD when possible; falls back when `App` present.                                                                |
| Decision-service `/query-plan` endpoint       |           ✅ | Produces `stillNeeded`, `impact`, `inputs`, `asks`; accepts bindings by label/unique/atomId; includes `asks[*].schema`. |
| Stable atom IDs                               |           ✅ | `atomId` is UUIDv5 derived from function + atom label + input refs.                                                     |
| Cache invalidation for query-plan             |           ✅ | `fnDecisionQueryCache` self-invalidates on PUT/DELETE.                                                                  |
| Shared query-plan core library                |           ✅ | Extracted to `jl4-query-plan` for reuse by LSP/decision-service/others.                                                 |
| Nested schema exposure in function metadata   | ✅ (partial) | Record-typed params expose `properties`; arrays expose `items` (still no `$ref`/`oneOf`).                               |
| “TYPICALLY” priors integration                |           ⏳ | Intentionally deferred; will affect optimizer/prioritization semantics.                                                 |

## Work Items

Legend: ✅ done · 🔄 in progress · ⏳ todo · ⚠️ blocked/deferred

|  ID | Work item                                      | Spec                                                                                    | Impl | Tests | Notes / Pointers                                                                                                                                      |
| --: | ---------------------------------------------- | --------------------------------------------------------------------------------------- | ---: | ----: | ----------------------------------------------------------------------------------------------------------------------------------------------------- |
|   1 | Symbolic evaluation (core)                     | `doc/todo/BOOLEAN-MINIMIZATION-SPEC.md`                                                 |   ✅ |    ✅ | Implemented on `mengwong/symbeval` (see commits below).                                                                                               |
|   2 | TS partial-eval analyzer for ladder UI         | `doc/todo/PARTIAL-EVAL-VISUALIZER-SPEC.md`                                              |   ✅ |    ✅ | `ts-shared/l4-ladder-visualizer/src/lib/eval/partial-eval.ts` + tests.                                                                                |
|   3 | Decision-service `/query-plan` endpoint        | `doc/todo/BOOLEAN-MINIMIZATION-SPEC.md`                                                 |   ✅ |    ✅ | Includes per-ask schema via `QueryAsk.schema` for elicitation (`jl4-decision-service/src/Backend/DecisionQueryPlan.hs`).                              |
|   4 | Stable atom IDs (`atomId`)                     | `doc/todo/BOOLEAN-MINIMIZATION-SPEC.md`                                                 |   ✅ |    ✅ | Enables stable frontend caching and cross-process references.                                                                                         |
|   5 | Cache invalidation for query-plan              | —                                                                                       |   ✅ |    ✅ | Prevents stale query-plan after function updates.                                                                                                     |
|   6 | Refactor query-plan core into reusable library | —                                                                                       |   ✅ |    ✅ | New package: `jl4-query-plan/`.                                                                                                                       |
|   7 | LSP reusable query-plan builder                | —                                                                                       |   ✅ |    ✅ | `jl4-lsp/src/LSP/L4/Viz/QueryPlan.hs`.                                                                                                                |
|   8 | Provenance beyond top-level params             | `doc/todo/BOOLEAN-MINIMIZATION-SPEC.md`                                                 |   ✅ |    ✅ | `/query-plan` now includes `asks[*].path :: [Text]` (segments) alongside `key`; schema lookup tolerates dotted segments.                              |
|   9 | JSON schema parity for arrays (`items`)        | —                                                                                       |   ✅ |    ✅ | Decision-service `Parameter.items` added; list-of-records can now expose element `properties`.                                                        |
|  10 | “TYPICALLY” priors in optimizer                | `doc/todo/TYPICALLY-DEFAULTS-SPEC.md` / `doc/todo/TYPICALLY-STATUS-AND-NEXT-STEPS.html` |   ⚠️ |    ⚠️ | Deferred; would change relevance/prioritization semantics.                                                                                            |
|  11 | Schema-aware ask ordering                      | —                                                                                       |   ✅ |    ✅ | Decision-service orders `asks` using schema shape (`items`) + declaration field order; array indices sort numerically.                                |
|  12 | TS types/helpers for `/query-plan` consumers   | —                                                                                       |   ✅ |    ✅ | New package `@repo/decision-service-types` exposes `QueryPlanResponse` types and schema/path helpers.                                                 |
|  13 | Ladder UI: highlight “next questions”          | —                                                                                       |   ✅ |    ✅ | Ladder nodes now emphasize top-ranked unassigned inputs; irrelevant/short-circuited subgraphs fade+shrink with animation.                             |
|  14 | REPL: text-mode query-plan                     | —                                                                                       |   ✅ |    ✅ | `jl4-repl` supports `:decides` and `:queryplan`/`:qp` for DECIDE query-plans under boolean bindings.                                                  |
|  15 | jl4-web: wire `/query-plan` into ladder UI     | —                                                                                       |   ✅ |     — | `ts-apps/jl4-web` now upserts current buffer to decision-service and uses `/query-plan` to drive in-diagram highlighting.                             |
|  16 | VSCode webview: wire `/query-plan` into ladder | —                                                                                       |   ✅ |    ✅ | `ts-apps/webview` requests query-plan via extension RPC; extension upserts doc text to decision-service (config: `jl4.decisionServiceUrl`).           |
|  17 | Prefer `asks[*]` for “next” emphasis           | —                                                                                       |   ✅ |    ✅ | Ladder UI supports multiple “next” nodes (top ask group) via `PartialEvalAnalysis.next`; jl4-web + VSCode webview derive `next` from `asks[0].atoms`. |
|  18 | Annotate “next” nodes with ask schema          | —                                                                                       |   ✅ |    ✅ | Ladder nodes now show `asks[0].label` and a light schema summary (e.g. `Number`, `enum(n)`) on the highlighted “next” atoms, without adding a new pane. |

## Recent Commits (Milestones)

- `7df863e2` — REPL: add `:decides` and `:queryplan`/`:qp` commands
- `8f74e1f1` — Ladder: animate irrelevant nodes; highlight “next questions” via ranked still-needed vars
- `585674fb` — TS: add `@repo/decision-service-types` (query-plan response/types, schema/path helpers, tests)
- `dcc9ba9e` — Decision-service: preserve record field order in schema; schema-aware sorting for `asks` (numeric indices + dotted keys)
- `2cc54bc9` — Query-plan: add `asks[*].path` segments (provenance) and schema lookup for dotted keys
- `586836e2` — TS tests for `PartialEvalAnalyzer`
- `a26f523b` — Decision-service: nested `parameterProperties` for record types
- `db88a4eb` — Decision-service: add `Parameter.items` for arrays
- `a85c3af0` — Decision-service: add `asks[*].schema` to `/query-plan`
- `818e74f9` — Extract query-plan into shared `jl4-query-plan` package
- `6ee39aca` — Decision-service: self-invalidating `fnDecisionQueryCache`
- `a3363062` — Decision-service: stable `atomId` for query-plan
- `61b437ec` / `56038bdc` / `d42240fe` / `1ec4208f` — Query-plan endpoint + ranking + nested asks + tests
- `917697be` — Partial eval visualizer spec formatting/update

## What’s Next (Recommended)

1. Switch atom matching from `label` to stable `atomId` end-to-end once the ladder side can compute or receive it (eliminates label collisions and cross-compiler unique drift).
2. Improve schema summaries (e.g. `Record{...}`, `List<T>`, enum value previews) and consider tooltips for multiple asks per atom.
