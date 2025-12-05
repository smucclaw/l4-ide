# PEVAL with TYPICALLY Defaults – Current Status

## ✅ What Works Today

1. **Directive rewriting to wrappers** – `#PEVAL`, `#PEVALTRACE`, and `#PASSERT` are rewritten during type checking to call the generated `'presumptive …'` helpers. Missing arguments are padded with `NOTHING`, user-provided arguments become `JUST …`, and CLI output now surfaces the explicit `Maybe` result (`JUST TRUE`, `NOTHING`, etc.). For assertions, a `NOTHING` result now fails the check.
2. **Runtime auto-defaults as safety net** – The evaluator still falls back to `maybeApplyDefaults` if a presumptive wrapper is missing, so old scripts continue to run while we finish migrating downstream callers.
3. **Wrapper generation for downstream callers** – The type checker synthesizes prefix-only `'presumptive …'` helpers with `MAYBE`-wrapped parameters so the Decision Service, SDKs, and CLI can all share the same default-aware entry points.
4. **Mixfix guard rails** – Mixfix interpretation remains IDE-only. Resolver logic skips identifiers that already start with `'presumptive `, guaranteeing that wrappers stay in canonical prefix form for REST payloads, presumptive directives, and audit logs.

## ⚠️ Temporarily Disabled

- **Transitive wrapper propagation** – Generated wrappers still defer to the original DECIDE body, so calls that happen _inside_ user code only see defaults when the caller itself supplied the argument. Propagating presumptive calls through nested DECIDEs is tracked in `doc/todo/TYPICALLY-DEFAULTS-SPEC.md`.
- **Four-state inputs** – The presumptive helpers still speak in terms of `JUST`/`NOTHING`. The `InputState` model from `RUNTIME-INPUT-STATE-SPEC.md` (explicit/unknown/not-provided/not-applicable) remains on the backlog.

## Next Steps

1. **Propagate presumptions across nested decisions** – Decide whether we rewrite DECIDE bodies to call the `'presumptive …'` helpers or thread an alternate evaluation mode so inner calls inherit defaults automatically.
2. **Move to the four-state runtime** – Replace the current `Maybe` plumbing with the `InputState` representation so Decision Service clients can distinguish “not asked” from “explicit unknown,” while keeping CLI defaults intuitive.

### Recent Work Log

- Revisited the TYPICALLY, runtime-input, and mixfix specs plus the newly merged foundation/advanced “course” docs so documentation updates stay consistent with the latest design voice.
- Added a type-checking rewrite pass that wraps PEVAL/PEVALTRACE/PASSERT arguments (`JUST` vs `NOTHING` padding), unwraps PASSERT results, and blocks mixfix resolution on `'presumptive …'` names. This proved out the approach but exposed regressions (e.g. ok/foo.l4 ambiguity errors) so the code currently needs further hardening before it can stay enabled.
- Tried deduplicating `AmbiguousTermError` cases by comparing canonical origin info (range + raw name). That fixed some duplicate warnings but still left the CLI with “internal ambiguity” diagnostics when wrapper generation emits multiple siblings; expect to revisit once the wrapper map is threaded everywhere.
- Documentation was partially updated (HELP.md, specs, mixfix doc, course modules) to reflect the intended wrapper behavior and the “mixfix stays in IDE” rule. Need follow-up to polish wording once the code path stabilizes.
