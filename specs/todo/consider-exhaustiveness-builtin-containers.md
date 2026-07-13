# Exhaustiveness checking for LIST / CONTRACT / EVENT

## Status

- **LIST: DONE** (commit `feat(typecheck): LIST exhaustiveness checking, via
oracle injection` on `mengwong/consider-exhaustiveness`).
- **CONTRACT: permanently excluded, by design** (documented at the exclusion
  site).
- **EVENT: sound to enable, blocked by an unrelated name-shadowing bug**
  (issue to file; see below).

## How LIST was enabled (NOT the re-tagging plan this doc originally sketched)

The original plan here — re-tag `cons` from `Computable` to `Constructor` so
the `entityInfo` scan sees `[EMPTY, cons]` — was superseded by something
strictly smaller, found during the second adversarial design pass:
`desugarBranches` already desugars every `FOLLOWED BY` pattern into a guard
keyed by `consRef`/`consUnique`, and every consumer of the oracle compares
entries by `getUnique` (rendering by `getName`) only. So the oracle
(`constructorsInScopeFromEntityInfo`) simply **injects** the complete pair
`listUnique -> [emptyRef, consRef]` instead of deleting `listUnique`. `cons`
stays `Computable` — it is applied in expression position in prelude, so
re-tagging it would have broken that.

Rendering: missing cons cases are synthesized as `PatCons` and rendered in
surface syntax — ``WHEN `_` FOLLOWED BY `_` THEN`` — by the dedicated
missing-branch renderer (never the internal cons constructor name).

Partial-match policy that made this shippable (adjudicated across five
red-team reviews):

- **P5 — make remediable sites total**: prelude `minimum`/`maximum` over
  `LIST OF MAYBE NUMBER` gained `WHEN EMPTY THEN NOTHING`; `loop.l4`'s `tail`
  gained `WHEN EMPTY THEN EMPTY`.
- **P1 — `@nonexhaustive` decorator** for the genuinely head-partial trio (`at`,
  `maximum`/`minimum` over `NUMBER`): author-declared partiality suppresses
  the missing-branch warning for that definition only (redundancy stays on);
  runtime crash-on-EMPTY semantics unchanged.
- **P2 (ASSUME/TBD as bottom) was REJECTED**: a lazy assumed value that
  reaches an output boundary unforced reports SUCCESS (`<assumed:TBD>`),
  silently converting today's hard crash into a wrong-but-confident answer —
  unacceptable for a legal DSL. If in-language `unreachable` is ever wanted,
  that is a strict `panic` builtin, a separate language-design decision.
- **P3 (demote LIST warnings to SInfo) and P4 (defer) were rejected** as
  dominated: the blast radius was census-bounded to 6 sites in 2 CI files,
  all remediated above at uniform SWarn severity.

## CONTRACT: why the exclusion is permanent

The regulative/deontic values form an **open sum**: only `FULFILLED` has a
pattern form; obligations/operators (`ValObligation`/`ValROp`/`ValBreached`)
have none, and the regulative core matches contract values partially on
purpose. Any oracle set would be unsound in both directions — it would flag
load-bearing partial matches as incomplete AND certify unmatchable cases as
covered.

## EVENT: sound but blocked — issue to file

The scanned set for `EVENT` (just the event constructor; `WAIT UNTIL` is
`Computable` and delegates to it) is **complete**, so checking would be
sound. It stays excluded only because `initialEnvironment` claims
`NormalName "EVENT"` twice (`eventUnique` type at Environment.hs ~916,
`eventCUnique` term constructor at ~917; `Map.fromList` keeps the later
entry), so the `EVENT` type is unnameable in user annotations. **File as a
standalone issue**; the commit that fixes the shadowing should also drop
`eventUnique` from `builtinNonExhaustiveTypeUniques` and add fixtures.
