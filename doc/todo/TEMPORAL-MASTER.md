# Temporal Work Master Tracker

This note summarizes the current state of temporal support across l4-ide and opm2l4 and records decisions from the temporals-2 branch review. It aligns the historical experiments with the latest proposal `~/src/legalese/opm2l4/docs/PROPOSAL_L4_IDE_TEMPORAL.md` and `doc/todo/TEMPORAL_EVAL_SPEC.md`.

## Current Findings

- Runtime: evaluator now carries a `TemporalContext` and supports clause-scoped overrides for system time, valid time, rules effective/encoded, commit, and retroactive shorthand. TODAY/NOW read from the context. Rule/commit axes are _not_ hooked to git checkout or rule resolution yet; `EvalClause` stacks restore properly but do not fetch historical rule sets.
- Prelude: `jl4-core/libraries/temporal-prelude.l4` still simulates windows via day-stamp recursion but now delegates retroactivity to the runtime builtin (`EVAL RETROACTIVE TO`) so both system-time and rule axes are set consistently. It remains demo-only until runtime date builtins land.
- Tests: golden files for `temporal-prelude` (parse/exactprint/NLG) are current and `cabal test all` passes. Proposal acceptance fixtures (`tests/temporal/*`) are still absent, and nothing yet imports `temporal-prelude` in the test corpus.
- Documentation: conceptual background lives in `doc/multitemporals.md` and Prolog `doc/tutorial-code/temporals-bna.pl`; `doc/advanced-course-ai/module-a3-temporal-logic.md` marks EVAL as WIP and points at `TEMPORAL_EVAL_SPEC.md`.

## Dead Ends / Deprecated Approaches

- Continuing to extend the pure-L4 shim as the semantics would diverge from the desired multi-temporal monad. Treat it as scaffolding only.
- Relying on Prolog experiments or doc-only examples as implementation guidance; they do not connect to the L4 evaluator.

## Promising Direction (to pursue)

- Implement the runtime as specified in `TEMPORAL_EVAL_SPEC.md`: keep `EvalClause`/`withEvalContext` and the multi-axis `TemporalContext`; expose uppercase builtins for `EVAL`, `AS OF RULES …`, `EVER/ALWAYS BETWEEN`, `VALUE AT`, `WHEN LAST/NEXT`.
- Rework `temporal-prelude.l4` so mixfix forms delegate to these builtins (no manual day iteration, no fake retroactivity), keeping helper date arithmetic where needed.
- Add parser and evaluator golden tests from the proposal (`interval_always`, `within_window`, `retroactive_to`, parser round-trip) to lock syntax and semantics.

## Next Steps (actionable)

1. **Evaluator**: implement multi-temporal runtime hooks and `withEvalContext` per `TEMPORAL_EVAL_SPEC.md`; ensure audit/context restoration and git snapshot handling.
2. **Prelude**: refactor `jl4-core/libraries/temporal-prelude.l4` to call the new builtins; keep mixfix signatures stable for opm2l4.
3. **Tests**: add `tests/temporal/*` fixtures and Haskell assertions described in `PROPOSAL_L4_IDE_TEMPORAL.md` (§Acceptance Tests); run twice to generate/verify goldens.
4. **Codegen**: in opm2l4, emit macros for OPM temporal functions and auto-inject `IMPORT temporal-prelude` when used.
5. **Docs**: update `doc/advanced-course-ai/module-a3-temporal-logic.md` status once runtime lands; note migration away from the shim in `temporal-prelude.l4`.

### WIP log

- Added `EVAL RETROACTIVE TO` builtin that applies the combined `RetroactiveTo` clause (system + rules-effective axes) around a thunk. Updated `temporal-prelude` retroactivity macro to call it.
- Regenerated `temporal-prelude` exactprint/NLG goldens; `cabal test all` now passes.
- Added `L4.TemporalContext` scaffolding (context record + EvalClause, pure `applyEvalClauses`, default seeding) and threaded an IORef through `EvalState` to prepare evaluator wiring.
- Extended the evaluation machine with `GetTemporalContext`/`PutTemporalContext` instructions and exported helpers (`getTemporalContext`, `setTemporalContext`, `withEvalClauses`) so future temporal builtins can snapshot/restore context during evaluation.
- Pointed `TODAY/NOW` builtins at the new temporal context so future `EVAL AS OF SYSTEM TIME …` overrides will affect date/time builtins automatically.
- Added a first runtime clause handler: `EVAL AS OF SYSTEM TIME <serial> <thunk>` is recognized at the evaluator level, overrides `tcSystemTime` for the thunk (with automatic restoration), and threads the override through the temporal context.
- Updated `temporal-prelude.l4` retroactivity macro to delegate to the new builtin (`EVAL AS OF SYSTEM TIME`) instead of simulating retroactivity in pure L4.
- Parser/builtin naming note: `#EVAL` (directive with `#`) and `EVAL AS OF SYSTEM TIME` (expression-level builtin) currently coexist; keep an eye on lexer/parser handling to ensure `EVAL` without `#` is treated as a term, not a directive. Rename option: `EVALAT` internal name while preserving mixfix surface.
- Added more clause hooks: `EVAL UNDER VALID TIME`, `EVAL UNDER RULES EFFECTIVE AT`, `EVAL UNDER RULES ENCODED AT`, and `EVAL UNDER COMMIT` builtins now set the corresponding `TemporalContext` fields around thunk evaluation (no git checkout yet; commit is stored in context for future wiring).
- Fixed `temporal-prelude.l4` to use `WHERE` bindings (no `LET/IN` in L4), unblocking parser/typecheck of the library.

### Current EVAL runtime status

- Builtins in `TypeCheck.Environment`: `EVAL AS OF SYSTEM TIME`, `EVAL UNDER VALID TIME`, `EVAL UNDER RULES EFFECTIVE AT`, `EVAL UNDER RULES ENCODED AT`, `EVAL UNDER COMMIT`, `EVAL RETROACTIVE TO` (all `a -> a` with an extra clause argument). They are preallocated in the evaluator and adjust `TemporalContext` during thunk evaluation, restoring afterwards. Retroactive combines rules-effective and system time overrides on the same day.
- Context fields tracked: `tcSystemTime`, `tcValidTime`, `tcRuleVersionTime`, `tcRuleValidTime`, `tcRuleEncodingTime`, `tcRuleCommit`, `tcDecisionTime`.
- System/valid/rule-effective/encoded overrides are applied; commit is stored (no git checkout yet).
- `TODAY`/`NOW` read `tcSystemTime`, so `EVAL AS OF SYSTEM TIME` affects date/time builtins automatically.
- `temporal-prelude.l4` retroactivity macro delegates to `EVAL RETROACTIVE TO` via `stamp of`.

### EVAL TODOs (hand-off)

- Wire commit/rule-axis overrides to real git/rule resolution: resolve commit hashes to encoded rule sets, adjust `tcRuleEncodingTime`/`tcRuleVersionTime` and auditor state; avoid touching the working tree (use cached builds/detached worktrees).
- Add parser/evaluator fixtures that exercise nested clauses (`retroactive to`, `under commit`, `under rules effective at`, etc.) once DATE builtin lands.
- Provide a user-facing combined EVAL form (list of clauses) once the clause builtins are stable, or keep the mixfix surface and ensure parser treats uppercase EVAL (no `#`) as a term.

### Forthcoming: DATE as builtin (plan)

- Problem: interval/when/value-at runtime builtins need DATE arguments, but DATE currently only exists in `daydate.l4`, not as a core builtin. Without a DATE unique, Haskell builtins won’t typecheck.
- Proposed approach:
  1. Promote DATE into `TypeCheck.Environment` (name/unique refs) and `Print` so the parser/typechecker recognizes it as a builtin type alongside NUMBER/STRING. Runtime carrier stays `Data.Time.Day` (`ValDate Day`).
  2. Expose principled conversions:
     - `DATEFROMTEXT`/`DATEVALUE` for ISO-8601-style parsing,
     - `DATE_SERIAL : DATE -> NUMBER` and `DATE_FROM_SERIAL : NUMBER -> DATE` (bridge to serials),
     - optional `DATE_FROM_DMY : NUMBER -> NUMBER -> NUMBER -> EITHER STRING DATE` for compatibility with `daydate.l4`.
  3. Add runtime builtins for `EVER/ALWAYS BETWEEN`, `WHEN LAST/NEXT`, `VALUE AT` taking DATE args; implement with a helper to apply closures to dates via evaluator frames.
  4. Keep `daydate.l4` as a convenience layer that uses the serial converters; `temporal-prelude.l4` macros delegate to the new builtins (no pure-L4 date loops), keeping mixfix signatures stable.
  5. Add acceptance fixtures (interval_always, within_window, retroactive_to, parser round-trip) once DATE builtin is in place.
- Commit/rule-axis wiring (git lookups) can proceed independently; commit strings are already stored in `TemporalContext`.

Owners: temporals-2 branch (l4-ide runtime & prelude), opm2l4 team (codegen/import), docs (advanced course).
