# Temporal Logic Work - Status & Overview

**Last Updated:** 2025-12-05
**Branch:** `mengwong/temporals-2`
**Status:** Work-in-progress, not yet merged to main

---

## Quick Summary

The temporal work introduces **runtime multi-temporal reasoning** to L4, allowing code to evaluate expressions under different temporal contexts (valid time, rule versions, Git commits, system knowledge time).

**Key Innovation:** `EVAL ... DO ...` construct - like time travel for legal rules.

---

## Branch Structure

### Active Branches

| Branch | Purpose | Status | Commits Ahead of Main |
|--------|---------|--------|----------------------|
| `mengwong/temporals-2` | Main temporal work | Active | 1 commit (dfd1746a) |
| `mengwong/eval-for-temporals` | Sub-branch | Active | - |
| `mengwong/temporals` | Older approach | Stale? | - |

### Files Added (416 total lines)

1. **`doc/todo/TEMPORAL_EVAL_SPEC.md`** (142 lines)
   - Specification for runtime `EVAL ... DO ...` construct
   - Multi-temporal reasoning semantics
   - Git commit integration

2. **`doc/todo/TEMPORAL_PRELUDE_MACROS.md`** (131 lines)
   - High-level combinators for temporal operations
   - Prelude macros that desugar to runtime primitives
   - Examples: `within`, `strictly after`, `retroactive to`

3. **`jl4-core/libraries/temporal-prelude.l4`** (143 lines)
   - Implementation of temporal prelude library
   - Helpers: `ever between`, `always between`, `find last stamp`
   - Date arithmetic utilities

---

## The EVAL Construct

### What It Does

`EVAL ... DO ...` is a **runtime meta-evaluation** construct that:
1. Captures current temporal context
2. Temporarily rewires time/rule context (including Git commits)
3. Evaluates an expression in that modified context
4. Restores original context
5. Returns the result

### Syntax

**Canonical form:**
```l4
EVAL
  UNDER VALID TIME January 1 2010
  UNDER RULES EFFECTIVE AT January 1 2010
  UNDER COMMIT "abc123"
  AS OF SYSTEM TIME July 1 2015
  DO `may purchase alcohol` applicant
```

**Mixfix sugar (preferred):**
```l4
EVAL `retroactive to` January 1 2010
     `under commit` "abc123"
     `as of system time` July 1 2015
     `evaluate` (`may purchase alcohol` applicant)
```

### Temporal Axes Supported

L4 now distinguishes **six temporal dimensions**:

| Axis | Meaning | Example Use |
|------|---------|-------------|
| `tcValidTime` | When facts are true | "Alice's age on Jan 1, 2010" |
| `tcSystemTime` | When system knew about facts | "Registry snapshot from July 2015" |
| `tcRuleVersionTime` | When rules came into force | "Tax law effective Jan 1, 2010" |
| `tcRuleValidTime` | Validity period of rules | "Rule valid from 2010-2020" |
| `tcRuleEncodingTime` | When L4 code was written | "Git commit abc123 from 2015" |
| `tcDecisionTime` | When decision was made | "Application processed today" |

### Examples

#### 1. Historic Eligibility Check
```l4
DECIDE `historic injustice made good` applicant MEANS
  EVAL `retroactive to` January 13 2010
       `evaluate` (`is eligible` applicant)
```

#### 2. Compare Current vs Previous Regime
```l4
DECIDE `decision changed since commit` commitHash applicant MEANS
  LET oldResult IS EVAL `under commit` commitHash `evaluate`
                      (`is eligible` applicant)
      currentResult IS `is eligible` applicant
  IN oldResult NOT EQUALS currentResult
```

#### 3. Sliding Window Query
```l4
DECIDE `ever eligible in past year` applicant today MEANS
  EVAL `within` 365 `days before` today
       `ever` (`is eligible` applicant)
```

---

## Temporal Prelude Macros

### Core Combinators

#### Sliding Windows
```l4
GIVEN duration IS A NUMBER
      anchor   IS A DATE
      verdict  IS A FUNCTION FROM DATE TO BOOLEAN
GIVETH A BOOLEAN
`within` duration `days after` anchor `ever` verdict MEANS
  LET endDate IS addDays anchor duration
  IN EVER BETWEEN anchor AND endDate
       UNDER RULES EFFECTIVE AT EACH DATE
       EVALUATE verdict OF effectiveDate
```

#### Strict Temporal Bounds
```l4
GIVEN anchor IS A DATE
      test   IS A FUNCTION FROM DATE TO BOOLEAN
GIVETH A BOOLEAN
`strictly after` anchor `ever` test MEANS
  LET nextDay IS addDays anchor 1
  IN EVER BETWEEN nextDay AND infinityDate
       UNDER RULES EFFECTIVE AT EACH DATE
       EVALUATE test OF effectiveDate
```

#### Retroactive Evaluation
```l4
GIVEN retroDate IS A DATE
      expr      IS A FUNCTION FROM UNIT TO a
GIVETH a
`retroactive to` retroDate `evaluate` expr MEANS
  AS OF RULES EFFECTIVE AT retroDate
    AS OF RULES ENCODED AT retroDate
      asOfRules retroDate expr
```

### Date Utilities
```l4
-- Add days to a date
`add days` baseDate numberOfDays

-- Get next day
`next day` currentDate

-- Check if predicate ever true in date range
`ever between stamps` startStamp endStamp verdict

-- Check if predicate always true in date range
`always between stamps` startStamp endStamp verdict

-- Find last date where predicate is true
`find last stamp` startStamp predicate

-- Find next date where predicate is true
`find next stamp` startStamp endStamp predicate
```

---

## Implementation Strategy

### Haskell Runtime

The specs describe a `MonadTemporal` type class:

```haskell
class Monad m => MonadTemporal m where
  getTemporalContext :: m TemporalContext
  setTemporalContext :: TemporalContext -> m ()

  -- Time travel
  asOfRulesEffectiveAt :: Day -> m a -> m a
  underEncoding :: CommitHash -> m a -> m a

  -- Temporal queries
  everBetween :: Day -> Day -> (Day -> m Bool) -> m Bool
  alwaysBetween :: Day -> Day -> (Day -> m Bool) -> m Bool
```

### Git Integration

Key insight: **Git commits are first-class temporal snapshots**

- `UNDER COMMIT "abc123"` loads rules from that commit
- `UNDER RULES EFFECTIVE AT date` resolves commit tagged for that date
- `withEvalContext` manages Git checkout/restore
- Use libgit2 or detached worktrees to avoid trashing working directory

### Caching Strategy

- `getRulesAtCommit` caches compiled rule sets
- Nested `EVAL` calls use a context stack
- Audit events gain `AuditEval` variant for temporal queries

---

## Use Cases

### 1. Regulatory Compliance - Retroactive Assessment
```l4
-- "Would this transaction have been compliant under the old rules?"
EVAL `under rules effective at` June 1 2020
     `evaluate` (`transaction is compliant` tx)
```

### 2. Legal Research - Rule Evolution
```l4
-- "How did eligibility criteria change over time?"
DECIDE `eligibility history` applicant MEANS
  MAP (\year -> (year, EVAL `retroactive to` year
                           `evaluate` (`is eligible` applicant)))
      [2010, 2011, 2012, 2013, 2014, 2015]
```

### 3. Policy Simulation - Counterfactuals
```l4
-- "What if we had applied today's rules to historical cases?"
DECIDE `would have qualified under new rules` oldApplicant MEANS
  EVAL `as of valid time` oldApplicant's applicationDate
       `under rules effective at` today
       `evaluate` (`is eligible` oldApplicant)
```

### 4. Audit & Compliance - Decision Verification
```l4
-- "Was this historical decision correct given the rules at that time?"
DECIDE `decision was correct` historicCase MEANS
  LET computedResult IS
        EVAL `retroactive to` historicCase's decisionDate
             `evaluate` (`compute outcome` historicCase)
      actualResult IS historicCase's recordedOutcome
  IN computedResult EQUALS actualResult
```

---

## Relationship to Other L4 Features

### TYPICALLY Defaults
**Status:** Independent but complementary

TYPICALLY provides spatial defaults (values when not provided), while EVAL provides temporal context switching. They could interact:

```l4
GIVEN age IS A NUMBER TYPICALLY 18
canVote age MEANS age >= 18

-- What was the default voting age in 2010?
EVAL `retroactive to` January 1 2010
     `evaluate` (`canVote` NOTHING)  -- Uses TYPICALLY 18
```

### Decision Service
**Status:** Could integrate with defaultMode

The Decision Service could support temporal queries:

```json
POST /functions/eligibility/evaluation
{
  "fnArguments": { "applicant": {...} },
  "temporalContext": {
    "validTime": "2010-01-01",
    "rulesEffectiveAt": "2010-01-01",
    "commit": "abc123def"
  }
}
```

### Mixfix Notation
**Status:** Already used in temporal specs

The EVAL construct heavily uses mixfix for readability:
- `` `retroactive to` date ``
- `` `under commit` hash ``
- `` `within` n `days after` date ``

---

## Testing Strategy

### Unit Tests Needed

1. **Context switching:**
   - Save/restore temporal context
   - Nested EVAL calls
   - Error handling (invalid commit, bad date)

2. **Temporal queries:**
   - `ever between` with various ranges
   - `always between` with edge cases
   - Date boundary conditions

3. **Git integration:**
   - Load rules from historical commits
   - Cache compiled rule sets
   - Detached worktree management

### Integration Tests Needed

1. **Retroactive eligibility:**
   - Test historical rule application
   - Compare current vs past results

2. **Rule evolution:**
   - Track changes across commits
   - Verify decision differences

3. **Audit trail:**
   - Log temporal context switches
   - Trace nested evaluations

### Golden Tests

Create test files in `jl4/examples/ok/`:
- `temporal-eval-basic.l4` - Simple EVAL examples
- `temporal-retroactive.l4` - Retroactive assessments
- `temporal-sliding-window.l4` - Time range queries
- `temporal-git-commits.l4` - Commit-based evaluation

---

## Merge Strategy

### Prerequisites

1. **Clean up branches:**
   - Decide: Keep `temporals-2`, deprecate `temporals`?
   - Merge `eval-for-temporals` into `temporals-2`?

2. **Rebase on latest main:**
   - Currently only 1 commit ahead
   - Should be straightforward

3. **Add comprehensive tests:**
   - Unit tests for core primitives
   - Integration tests for EVAL construct
   - Golden tests for prelude macros

### Migration Path

**Phase 1: Merge specs (Low Risk)**
- Bring spec files into main
- Document temporal features
- No runtime changes yet

**Phase 2: Add prelude library (Medium Risk)**
- Add `temporal-prelude.l4` to libraries
- Make it importable
- Test date utilities

**Phase 3: Implement EVAL runtime (High Risk)**
- Add `MonadTemporal` type class
- Implement context switching
- Add Git integration
- Enable EVAL syntax

**Phase 4: Decision Service Integration (Future)**
- Add temporal query parameters
- Expose historical evaluation
- Audit temporal context switches

---

## Current Blockers

### None Identified

The temporal work appears to be self-contained and ready for review. The main question is:

**Q:** Should this be merged before or after TYPICALLY is unblocked?

**Recommendation:** Independent - can proceed in parallel.

### Questions for Review

1. **Git integration:** How to handle commits in production? Read-only snapshots?
2. **Performance:** Caching strategy for compiled rule sets per commit?
3. **API design:** Should temporal context be explicit parameters or ambient context?
4. **Testing:** How to test Git-dependent features in CI?

---

## Next Steps

### Immediate (This Week)

1. **Review specs:**
   - Read both TEMPORAL spec files fully
   - Identify any gaps or ambiguities
   - Check consistency with existing L4 semantics

2. **Test library:**
   - Checkout `temporals-2` branch
   - Run `temporal-prelude.l4` through CLI
   - Verify date utilities work

3. **Document merge plan:**
   - Create PR checklist
   - Identify review stakeholders
   - Plan testing strategy

### Short Term (This Month)

1. **Rebase on latest main:**
   - Resolve any conflicts
   - Ensure compatibility with recent changes

2. **Add comprehensive tests:**
   - Write unit tests
   - Create integration tests
   - Add golden test files

3. **Create PR:**
   - Merge specs into main first
   - Follow with library implementation
   - Save runtime changes for separate PR

### Long Term (Next Quarter)

1. **Implement runtime:**
   - Add `MonadTemporal` to evaluator
   - Implement Git integration
   - Enable EVAL syntax

2. **Decision Service:**
   - Design temporal API
   - Add temporal query support
   - Document for users

3. **Documentation:**
   - Add to foundation course
   - Create temporal tutorial
   - Document best practices

---

## Resources

### Code Locations

| File | Location | Status |
|------|----------|--------|
| EVAL Spec | `doc/todo/TEMPORAL_EVAL_SPEC.md` | On `temporals-2` |
| Prelude Macros Spec | `doc/todo/TEMPORAL_PRELUDE_MACROS.md` | On `temporals-2` |
| Prelude Library | `jl4-core/libraries/temporal-prelude.l4` | On `temporals-2` |

### Related Documentation

- Temporal monad spec (referenced but not in repo)
- Multi-temporal database theory (Snodgrass, et al.)
- VAX/VMS file versioning (inspiration)

### Branches

```bash
# Check out temporal work
git checkout mengwong/temporals-2

# View specs
cat doc/todo/TEMPORAL_EVAL_SPEC.md
cat doc/todo/TEMPORAL_PRELUDE_MACROS.md

# View library
cat jl4-core/libraries/temporal-prelude.l4

# Compare with main
git diff main...mengwong/temporals-2 --stat
```

---

## Summary for PROJECT-MASTER.md

Add to Technical Debt / Future Work:

### Medium Priority

**Temporal Logic Integration**
- **Status:** Specs complete, on `temporals-2` branch
- **Specs:** `TEMPORAL_EVAL_SPEC.md`, `TEMPORAL_PRELUDE_MACROS.md`
- **Features:** Runtime `EVAL ... DO ...` construct for multi-temporal reasoning
- **Ready for:** Review and merge (specs first, then implementation)
- **Effort:** 2-3 weeks for full integration
- **Dependencies:** None (independent of TYPICALLY work)

---

**Document Owner:** Next developer working on temporal features
**Last Reviewed:** 2025-12-05
**Status:** Ready for review and merge planning
