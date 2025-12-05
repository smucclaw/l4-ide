# Temporal Prelude Macros (Draft)

This note implements the "next step" from the temporal monad spec: give L4
authors concrete combinators that sit between the runtime builtins and domain
libraries. These macros live in a dedicated `temporal-prelude.l4` module that
is meant to be imported after the stock `prelude` + `daydate` bundles.

The intent is that the macros **desugar** to the Haskell monad operators
described in `docs/L4_TEMPORAL_MONAD_SPEC.md`. Until `jl4-core` exposes that
runtime, we stage the definitions here so translators (like opm2l4) can emit the
canonical surface syntax.

## Module Skeleton (L4)

```l4
IMPORT prelude
IMPORT daydate

DECLARE `Temporal Scope`
  HAS `valid start`   IS A DATE
      `valid end`     IS A DATE
      `rule snapshot` IS A DATE

GIVEN duration IS A NUMBER
      anchor   IS A DATE
      verdict  IS A FUNCTION FROM DATE TO BOOLEAN
GIVETH A BOOLEAN
`within` duration `days after` anchor `ever` verdict MEANS
  LET endDate   IS addDays anchor duration
  IN EVER BETWEEN anchor AND endDate
       UNDER RULES EFFECTIVE AT EACH DATE
       EVALUATE verdict OF effectiveDate

GIVEN anchor IS A DATE
      test   IS A FUNCTION FROM DATE TO BOOLEAN
GIVETH A BOOLEAN
`strictly after` anchor `ever` test MEANS
  LET nextDay IS addDays anchor 1
  IN EVER BETWEEN nextDay AND infinityDate
       UNDER RULES EFFECTIVE AT EACH DATE
       EVALUATE test OF effectiveDate

GIVEN retroDate IS A DATE
      expr      IS A FUNCTION FROM UNIT TO a
GIVETH a
`retroactive to` retroDate `evaluate` expr MEANS
  AS OF RULES EFFECTIVE AT retroDate
    AS OF RULES ENCODED AT retroDate
      asOfRules retroDate expr
```

Notes:

1. `addDays` and `infinityDate` come from `daydate`. For prose clarity the
   snippet omits explicit `GIVEN/WHERE` clauses.
2. Each macro is just sugar; `EVER BETWEEN …` already lowers to `everBetween` in
   the runtime.

## Haskell Bindings

At code generation time, the macros compile to the helper functions below. They
are thin wrappers over the primitives already defined in
`docs/L4_TEMPORAL_MONAD_SPEC.md`.

```haskell
withinDaysAfter
  :: MonadTemporal m
  => NominalDiffTime  -- duration in days
  -> Day              -- anchor
  -> (Day -> m Bool)  -- predicate
  -> m Bool
withinDaysAfter duration anchor predicate =
  everBetween anchor (addDays duration anchor)
    (\effectiveDate -> predicate effectiveDate)

strictlyAfter
  :: MonadTemporal m
  => Day
  -> (Day -> m Bool)
  -> m Bool
strictlyAfter anchor predicate =
  let next = addDays 1 anchor
  in everBetween next maxBound predicate

retroactiveTo
  :: MonadTemporal m
  => Day
  -> m a
  -> m a
retroactiveTo snapshot action =
  asOfRulesEffectiveAt snapshot $ do
    encodingCommit <- lookupEncoding snapshot
    underEncoding encodingCommit action
```

`lookupEncoding` resolves `tcRuleEncodingTime` (first-class now) to the git
commit that contains the relevant L4 encoding. `underEncoding` is a helper that
switches both `tcRuleVersionTime` and `tcRuleEncodingTime` so audit trails stay
coherent.

## Example Usage

```l4
GIVEN applicant IS AN Applicant
      notice    IS A DATE
DECIDE `responded within 14 days`
  IF within 14 days after notice ever
       (GIVEN decisionDate YIELD `response logged` applicant decisionDate)

DECIDE `eligible only after appointed day`
  IF strictly after `T_BCIA_Commencement` ever
       (GIVEN effectiveDate YIELD `is eligible` applicant effectiveDate)

DECIDE `historic injustice made good`
  IF retroactive to January 13 2010 evaluate
       (`is eligible` applicant)
```

These formulas are what opm2l4 can now emit when it sees the corresponding OPM
temporal patterns (e.g., `interval-sometimes`, `interval-always`, rolling
windows). The runtime takes care of desugaring into the multi-temporal monad.

## Next Work Items

1. Port the draft L4 snippets into `jl4-core/libraries/prelude.l4` once the
   upstream repository accepts contributions.
2. Replace the placeholder `lookupEncoding` with a concrete lookup into the new
   `docs/domain-kits/uk-nationality-commencements.yaml` file (or a compiled
   SQLite table) so rule-encoding provenance is automatic.
3. Expand the macro set with `survives`, `annually`, and `as_if` once the first
   client statutes demand them.
