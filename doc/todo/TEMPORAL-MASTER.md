# Temporal Work Master Tracker

This note summarizes the current state of temporal support across l4-ide and opm2l4 and records decisions from the temporals-2 branch review. It aligns the historical experiments with the latest proposal `~/src/legalese/opm2l4/docs/PROPOSAL_L4_IDE_TEMPORAL.md` and `doc/todo/TEMPORAL_EVAL_SPEC.md`.

## Current Findings

- Runtime: no evaluator support exists for multi-axis temporals. There is no `TemporalContext` plumbing for rule-version/rule-encoding/system time, no `withEvalContext`, and no uppercase builtins (`EVER/ALWAYS BETWEEN`, `AS OF RULES …`, `EVAL`) wired into Haskell. Parser coverage for these constructs is also absent.
- Prelude: `jl4-core/libraries/temporal-prelude.l4` ships a pure-L4 shim that simulates windows via day-stamp recursion and treats ``retroactive to`` as `expr retroDate`. It ignores rule-version/system time axes, so it is demo-only and not suitable for opm2l4 codegen targets.
- Tests: none of the acceptance fixtures in the proposal (`tests/temporal/*`) exist; no file in the tree `IMPORT`s `temporal-prelude`, so the new syntax is unexercised.
- Documentation: conceptual background lives in `doc/multitemporals.md` and Prolog `doc/tutorial-code/temporals-bna.pl`; `doc/advanced-course-ai/module-a3-temporal-logic.md` marks EVAL as WIP and points at `TEMPORAL_EVAL_SPEC.md`.

## Dead Ends / Deprecated Approaches

- Continuing to extend the pure-L4 shim as the semantics would diverge from the desired multi-temporal monad. Treat it as scaffolding only.
- Relying on Prolog experiments or doc-only examples as implementation guidance; they do not connect to the L4 evaluator.

## Promising Direction (to pursue)

- Implement the runtime as specified in `TEMPORAL_EVAL_SPEC.md`: add `EvalClause`, stack-based `withEvalContext`, and `TemporalContext` fields for valid/system/rule-version/rule-encoding axes; expose uppercase builtins for `EVAL`, `AS OF RULES …`, `EVER/ALWAYS BETWEEN`, `VALUE AT`, `WHEN LAST/NEXT`.
- Rework `temporal-prelude.l4` so mixfix forms delegate to these builtins (no manual day iteration, no fake retroactivity), keeping helper date arithmetic where needed.
- Add parser and evaluator golden tests from the proposal (`interval_always`, `within_window`, `retroactive_to`, parser round-trip) to lock syntax and semantics.

## Next Steps (actionable)

1. **Evaluator**: implement multi-temporal runtime hooks and `withEvalContext` per `TEMPORAL_EVAL_SPEC.md`; ensure audit/context restoration and git snapshot handling.
2. **Prelude**: refactor `jl4-core/libraries/temporal-prelude.l4` to call the new builtins; keep mixfix signatures stable for opm2l4.
3. **Tests**: add `tests/temporal/*` fixtures and Haskell assertions described in `PROPOSAL_L4_IDE_TEMPORAL.md` (§Acceptance Tests); run twice to generate/verify goldens.
4. **Codegen**: in opm2l4, emit macros for OPM temporal functions and auto-inject `IMPORT temporal-prelude` when used.
5. **Docs**: update `doc/advanced-course-ai/module-a3-temporal-logic.md` status once runtime lands; note migration away from the shim in `temporal-prelude.l4`.

### WIP log
- Added `L4.TemporalContext` scaffolding (context record + EvalClause, pure `applyEvalClauses`, default seeding) and threaded an IORef through `EvalState` to prepare evaluator wiring.
- Extended the evaluation machine with `GetTemporalContext`/`PutTemporalContext` instructions and exported helpers (`getTemporalContext`, `setTemporalContext`, `withEvalClauses`) so future temporal builtins can snapshot/restore context during evaluation.

Owners: temporals-2 branch (l4-ide runtime & prelude), opm2l4 team (codegen/import), docs (advanced course).
