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

|  ID | Work item                                      | Spec                                                                                    | Impl | Tests | Notes / Pointers                                                                                                         |
| --: | ---------------------------------------------- | --------------------------------------------------------------------------------------- | ---: | ----: | ------------------------------------------------------------------------------------------------------------------------ |
|   1 | Symbolic evaluation (core)                     | `doc/todo/BOOLEAN-MINIMIZATION-SPEC.md`                                                 |   ✅ |    ✅ | Implemented on `mengwong/symbeval` (see commits below).                                                                  |
|   2 | TS partial-eval analyzer for ladder UI         | `doc/todo/PARTIAL-EVAL-VISUALIZER-SPEC.md`                                              |   ✅ |    ✅ | `ts-shared/l4-ladder-visualizer/src/lib/eval/partial-eval.ts` + tests.                                                   |
|   3 | Decision-service `/query-plan` endpoint        | `doc/todo/BOOLEAN-MINIMIZATION-SPEC.md`                                                 |   ✅ |    ✅ | Includes per-ask schema via `QueryAsk.schema` for elicitation (`jl4-decision-service/src/Backend/DecisionQueryPlan.hs`). |
|   4 | Stable atom IDs (`atomId`)                     | `doc/todo/BOOLEAN-MINIMIZATION-SPEC.md`                                                 |   ✅ |    ✅ | Enables stable frontend caching and cross-process references.                                                            |
|   5 | Cache invalidation for query-plan              | —                                                                                       |   ✅ |    ✅ | Prevents stale query-plan after function updates.                                                                        |
|   6 | Refactor query-plan core into reusable library | —                                                                                       |   ✅ |    ✅ | New package: `jl4-query-plan/`.                                                                                          |
|   7 | LSP reusable query-plan builder                | —                                                                                       |   ✅ |    ✅ | `jl4-lsp/src/LSP/L4/Viz/QueryPlan.hs`.                                                                                   |
|   8 | Provenance beyond top-level params             | `doc/todo/BOOLEAN-MINIMIZATION-SPEC.md`                                                 |   🔄 |    ⏳ | “Ask keys” are derived from `InputRef` paths; next step is richer schema alignment (arrays/items).                       |
|   9 | JSON schema parity for arrays (`items`)        | —                                                                                       |   ✅ |    ✅ | Decision-service `Parameter.items` added; list-of-records can now expose element `properties`.                           |
|  10 | “TYPICALLY” priors in optimizer                | `doc/todo/TYPICALLY-DEFAULTS-SPEC.md` / `doc/todo/TYPICALLY-STATUS-AND-NEXT-STEPS.html` |   ⚠️ |    ⚠️ | Deferred; would change relevance/prioritization semantics.                                                               |

## Recent Commits (Milestones)

- `586836e2` — TS tests for `PartialEvalAnalyzer`
- `a26f523b` — Decision-service: nested `parameterProperties` for record types
- `db88a4eb` — Decision-service: add `Parameter.items` for arrays
- `818e74f9` — Extract query-plan into shared `jl4-query-plan` package
- `6ee39aca` — Decision-service: self-invalidating `fnDecisionQueryCache`
- `a3363062` — Decision-service: stable `atomId` for query-plan
- `61b437ec` / `56038bdc` / `d42240fe` / `1ec4208f` — Query-plan endpoint + ranking + nested asks + tests
- `917697be` — Partial eval visualizer spec formatting/update

## What’s Next (Recommended)

1. Use `items` + `properties` to improve query-plan elicitation ordering (especially for record-of-lists / list-of-records).
2. Revisit “provenance beyond top-level params” with a consistent cross-layer notion of _input path_ (schema path ↔ ladder `InputRef.path` ↔ client keys).
