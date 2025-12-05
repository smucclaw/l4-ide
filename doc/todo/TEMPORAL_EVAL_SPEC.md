# Temporal EVAL Specification

## Overview

`EVAL … DO …` is a runtime meta-evaluation construct for L4. Unlike the compile-
time `#EVAL` directive, runtime `EVAL` captures an L4 thunk, temporarily rewires
the temporal/rule context (including Git commits), evaluates the thunk, and then
restores the original context. This lets authors answer questions like "would
`may purchase alcohol` have been true under rules effective on 1 Jan 2010 using
the encoding from commit abc123?" without diving into Haskell.

## Motivation

Multi-temporal reasoning needs more than valid-time parameters. We want to:

1. Evaluate predicates at arbitrary valid times (`tcValidTime`).
2. Re-run logic under historical rule versions / encodings (`tcRuleVersionTime`,
   `tcRuleEncodingTime`) without rebuilding the evaluator for each axis.
3. Slice by system knowledge ("as at registry snapshot dated T").

Git already tracks rule versions and encoding history, so `EVAL` treats commits
as first-class snapshots, akin to VAX/VMS-style file versioning. L4 code can now
ask "what did my program conclude eight commits ago?" entirely from userland.

## Syntax

Canonical block form:

```l4
EVAL
  UNDER VALID TIME January 1 2010
  UNDER RULES EFFECTIVE AT January 1 2010
  UNDER COMMIT "abc123"
  AS OF SYSTEM TIME July 1 2015
  DO `may purchase alcohol` applicant
```

Mixfix sugar (preferred for readability):

```l4
EVAL `retroactive to` January 1 2010
     `under commit` "abc123"
     `as of system time` July 1 2015
     `evaluate` (`may purchase alcohol` applicant)
```

Each clause is optional; absent clauses inherit the caller's context.

## Semantics

Given current context Γ = (`tcValidTime`, `tcSystemTime`, `tcRuleVersionTime`,
`tcRuleValidTime`, `tcRuleEncodingTime`, `tcDecisionTime`):

1. Save Γ plus the current Git checkout / compiled rule set.
2. Apply clauses in order:
   - `UNDER VALID TIME t` → `tcValidTime := t`.
   - `AS OF SYSTEM TIME t` → `tcSystemTime := t`.
   - `UNDER RULES EFFECTIVE AT d` → resolve commit tagged for `d`, update
     `tcRuleVersionTime`, `tcRuleValidTime`, `tcRuleEncodingTime`.
   - `UNDER COMMIT c` → load rules from commit `c`, update
     `tcRuleVersionTime`, `tcRuleEncodingTime`.
   - `RETROACTIVE TO d` → shorthand for "under rules effective at d" and
     "as of system time d".
3. Evaluate the thunk (`DO expr`) under the modified context.
4. Restore Γ and the previous checkout/cache state.
5. Return the result of `expr`.

Errors (missing commit, invalid date) abort evaluation and restore Γ.

## Runtime Implementation

Add a helper:

```haskell
withEvalContext :: [EvalClause] -> L4Temporal a -> L4Temporal a
```

`EvalClause` is an ADT covering the clauses above. `withEvalContext` pushes the
current `TemporalContext`/git state on a stack, applies clauses, runs the
computation, then pops the stack. Nested `EVAL`s just grow this stack.

Key reuse points:

- `getRulesAtCommit` already caches compiled rule sets.
- `AuditEvent` should gain an `AuditEval` variant recording requested clauses.
- Use libgit2 APIs (or a detached worktree) to avoid trashing the caller's
  working tree when checking out historical commits.

## Interaction with Builtins and Prelude

- Builtins: provide uppercase forms (`EVAL`, `UNDER RULES EFFECTIVE AT`,
  `UNDER COMMIT`). The runtime enforces context switching and caching.
- Prelude: add mixfix wrappers so authors can write
  ``EVAL `retroactive to` d `evaluate` expr``. These wrappers just build the
  appropriate `EvalClause` list and call into the builtin.
- Libraries: optional—domain kits can expose constants (e.g.,
  `` `T_BCIA_Commencement` ``) so `EVAL` clauses reference symbolic dates.

## Examples

### 1. Historic eligibility check

```l4
DECIDE `historic injustice made good` applicant MEANS
  EVAL `retroactive to` January 13 2010
       `evaluate` (`is eligible` applicant)
```

### 2. Compare current vs previous regime

```l4
DECIDE `decision changed since commit` commitHash applicant MEANS
  LET oldResult IS EVAL `under commit` commitHash `evaluate`
                      (`is eligible` applicant)
      currentResult IS `is eligible` applicant
  IN oldResult NOT EQUALS currentResult
```

### 3. Nested eval (retroactive counterfactual)

```l4
EVAL `retroactive to` January 1 2005 `evaluate`
  (EVAL `under commit` "windrush_fix"
        `evaluate` (`grant compensation` applicant))
```

## Testing Strategy

1. **Unit tests** in Haskell to verify `withEvalContext` restores contexts even
   on exceptions, and that caching prevents redundant compilation.
2. **Integration tests** in L4 verifying nested `EVAL`s, commit selection, and
   audit logging. Use a fixture repo with two commits whose rules differ.
3. **Performance**: stress-test repeated `EVAL` calls across many commits to
   validate cache hit rates and absence of stale working-tree state.

## Future Work

- Expose read-only views of git blame/history to L4 (`EVAL HISTORY OF rule`?).
- Allow `EVAL` to target tagged states ("latest in-force version") rather than
  explicit commits.
- Combine `EVAL` with streaming evidence updates: `AS OF SYSTEM TIME t` +
  `WITH FACTS AS OF t`.
