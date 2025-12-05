# L4 Project Master Tracker

**Last Updated:** 2025-12-05
**Current Branch:** `mengwong/635`
**Purpose:** Track ongoing work, completed PRs, and active priorities

---

## Executive Summary

This document provides a high-level view of all active work streams in the L4 project, linking to detailed specifications and tracking implementation status.

### Recent Velocity (Last 10 PRs)

- **10 PRs merged** between Nov 29 - Dec 4, 2025
- **Major achievements:**
  - 325x performance speedup (PR #653)
  - @export annotation support (PRs #654, #657)
  - Dictionary data type added (PR #656)
  - Pattern matching spec added (PR #659)
  - Foundation course AI materials (PR #660)

---

## Active Work Streams - Summary

| # | Work Stream | Status | Priority | Key Docs | Branch |
|---|-------------|--------|----------|----------|--------|
| 1 | **Issue #635: Decision Service** | 🔄 8/11 Done | High | [Status](./ISSUE-635-PLANNING-STATUS.md) | `mengwong/635` |
| 2 | **TYPICALLY Defaults** | ❌ Blocked | **CRITICAL** | [Status](./TYPICALLY-STATUS-AND-NEXT-STEPS.md), [Spec](./TYPICALLY-DEFAULTS-SPEC.md) | `mengwong/635` |
| 3 | **Language Features** | ✅ Done | - | Mixfix, Dictionary, Primitives | `main` |
| 4 | **Foundation Course** | ✅ Done | - | [Course Docs](../foundation-course-ai/) | `main` |
| 5 | **Developer Tooling** | ✅ Done | - | Agent support, Security fixes | `main` |
| 6 | **Temporal Logic** | 📋 Specs Ready | Medium | [Status](./TEMPORAL-WORK-STATUS.md), [EVAL Spec](./TEMPORAL_EVAL_SPEC.md) | `mengwong/temporals-2` |

### Quick Status Legend
- ✅ **Done** - Merged and complete
- 🔄 **In Progress** - Active development
- ❌ **Blocked** - Has blockers (see status doc)
- 📋 **Specs Ready** - Designed, ready for implementation

---

## Active Work Streams - Detailed

### 1. Issue #635: Critical Decision Service Improvements

**Branch:** `mengwong/635`
**Tracking Doc:** [`doc/todo/ISSUE-635-PLANNING-STATUS.md`](./ISSUE-635-PLANNING-STATUS.md)

| Item | Feature                       | Spec                                                                                          | Status         | PRs        | Notes                                                               |
| ---- | ----------------------------- | --------------------------------------------------------------------------------------------- | -------------- | ---------- | ------------------------------------------------------------------- |
| 1    | Conditional Trace Returns     | [`doc/done/CONDITIONAL-TRACE-SPEC.md`](../done/CONDITIONAL-TRACE-SPEC.md)                     | ✅ Done        | -          | X-L4-Trace header, ?trace= param (commit 131dd4a0)                  |
| 2    | IDE Directive Filtering       | [`doc/done/DECISION-SERVICE-JSONDECODE-SPEC.md`](../done/DECISION-SERVICE-JSONDECODE-SPEC.md) | ✅ Done        | #651, #652 | JSONDECODE approach, 18 tests pass (commit fc320987)                |
| 3    | Enhanced YAML/JSON Support    | [`doc/done/DECISION-SERVICE-JSONDECODE-SPEC.md`](../done/DECISION-SERVICE-JSONDECODE-SPEC.md) | ✅ Done        | #651, #652 | Nested objects via JSONDECODE                                       |
| 4    | Boolean Minimization          | [`BOOLEAN-MINIMIZATION-SPEC.md`](./BOOLEAN-MINIMIZATION-SPEC.md)                              | ⏳ Todo        | -          | See Issue #638 (larger feature)                                     |
| 5    | Dynamic File Management       | -                                                                                             | 🔄 Partial     | #649       | Push support done; file watching may be needed                      |
| 6    | EXPORT API Syntax             | [`EXPORT-SYNTAX-SPEC.md`](./EXPORT-SYNTAX-SPEC.md)                                            | ✅ Done        | #654, #657 | Lexer, parser, Export.hs complete                                   |
| 7    | Import/Export Coordination    | -                                                                                             | ✅ Done        | -          | IMPORT support (commit 4e1c7cd1)                                    |
| 8    | Performance Optimization      | [`PERFORMANCE-OPTIMIZATION-SPEC.md`](./PERFORMANCE-OPTIMIZATION-SPEC.md)                      | ✅ Done        | #653       | **325x speedup!** Precompilation + parallel batch (commit 43c1ee11) |
| 9    | ASSUME Parameter Requirements | [`ASSUME-DEPRECATION-SPEC.md`](./ASSUME-DEPRECATION-SPEC.md)                                  | ⏳ Todo        | -          | Can be done independently                                           |
| 10   | TYPICALLY Defaults            | [`TYPICALLY-DEFAULTS-SPEC.md`](./TYPICALLY-DEFAULTS-SPEC.md)                                  | 🔄 In Progress | -          | Core syntax done; runtime integration pending                       |
| 11   | Runtime Input State           | [`RUNTIME-INPUT-STATE-SPEC.md`](./RUNTIME-INPUT-STATE-SPEC.md)                                | ⏳ Todo        | -          | Four-state model for interactive apps                               |

**Key Achievements:**

- **Performance:** 325x speedup for batch evaluation (5,000 cases/second)
- **Response Size:** 20x smaller with trace disabled (152 vs 3,109 bytes)
- **Architecture:** Module precompilation eliminates redundant parsing/typechecking

---

### 2. TYPICALLY Defaults & Rebuttable Presumptions

**Main Spec:** [`TYPICALLY-DEFAULTS-SPEC.md`](./TYPICALLY-DEFAULTS-SPEC.md)
**Status Doc:** [`TYPICALLY-STATUS-AND-NEXT-STEPS.md`](./TYPICALLY-STATUS-AND-NEXT-STEPS.md) ⭐ **Start here!**
**Related:** [`RUNTIME-INPUT-STATE-SPEC.md`](./RUNTIME-INPUT-STATE-SPEC.md)

**Status:** Core implementation complete, runtime integration blocked by ambiguity errors

#### Completed

- ✅ Lexer: TYPICALLY keyword
- ✅ AST: TypedName, OptionallyTypedName extensions
- ✅ Parser: TYPICALLY clause parsing
- ✅ Type Checking: Validate TYPICALLY values
- ✅ IDE: Syntax highlighting + autocomplete
- ✅ Strict directives: #EVALSTRICT, #ASSERTSTRICT, #EVALTRACESTRICT
- ✅ Presumptive directives: #PEVAL, #PEVALTRACE, #PASSERT (syntax)
- ✅ TYPICALLY on ASSUME error (disallowed)
- ✅ Wrapper generation: `'presumptive <fn>'` functions created at type-check time

#### Blocked (See [TYPICALLY-STATUS-AND-NEXT-STEPS.md](./TYPICALLY-STATUS-AND-NEXT-STEPS.md))

- ❌ **Directive rewriting** (causes ambiguity errors - main blocker)
- 🔄 Wrapper body transitive rewriting (depends on above)
- 🔄 Decision Service API defaultMode parameter (depends on above)

#### Todo

- ⏳ Four-state InputState model (NotProvided vs ExplicitUnknown)
- ⏳ Decision Service wrapper-based default handling
- ⏳ Hover showing defaults (requires architectural changes)

**Test Files:**

- `jl4/examples/ok/typically-basic.l4`
- `jl4/examples/ok/evalstrict.l4`
- `jl4/examples/ok/peval-test.l4`
- `jl4/examples/not-ok/tc/typically-type-mismatch.l4`
- `jl4/examples/not-ok/tc/typically-on-assume.l4`

---

### 3. Language Features

#### Pattern Matching (Spec Added)

- **PR:** #659
- **Spec:** Proposes Haskell-style pattern matching in DECIDE/MEANS
- **Status:** Specification only; not yet implemented
- **Features:**
  - Multiple function clauses with patterns
  - Wildcards with `_name` syntax
  - Parameter-GIVEN correspondence rules
  - Decision table equivalence

#### Dictionary Data Type

- **PR:** #656
- **Status:** ✅ Implemented
- **Description:** Data.Map-like dictionary support in prelude

#### String Primitives

- **PR:** #650
- **Status:** ✅ Implemented
- **Features:** SPLIT and CHARAT string operations

#### Mixfix & Postfix Notation

- **PR:** #647
- **Status:** ✅ Implemented
- **Description:** Support for mixfix and postfix operators

#### Primitives Expansion

- **PR:** #644
- **Status:** ✅ Implemented
- **Features:** JSONENCODE, JSONDECODE, FETCH, POST, ENV, CONCAT, AS STRING

---

### 4. Foundation Course & Documentation

#### Foundation Course AI Materials

- **PR:** #660
- **Branch:** `mengwong/foundation-course-ai`
- **Status:** ✅ Merged to main
- **Files:**
  - `doc/foundation-course-ai/module-2-functions.md` (updated)
  - `doc/advanced-course-ai/module-a4-decision-service.md` (updated)
  - `doc/advanced-course-ai/module-a5-rebuttable-presumptions.md` (updated)

#### Documentation Updates

- `doc/25-functions.md` - Enhanced function documentation
- `doc/default-values.md` - TYPICALLY defaults conceptual background
- `doc/mixfix-operators.md` - Mixfix notation guide

---

### 5. Developer Tooling & Agent Support

#### Coding Agents Support

- **PR:** #655
- **Tool:** Crush support enhanced
- **Files Added:**
  - `AGENTS.html` - Agent documentation
  - `CLAUDE.html` - Claude interaction guidelines
  - `Dev.html` - Developer guide
  - `HELP.html` - Help documentation
  - `MIXFIX-STATUS.html` - Mixfix status tracker
  - `Quickstart.html` - Quick start guide
  - `README.html` - Main README

#### Security Fixes

- **PR:** #658
- **Status:** ✅ Done
- **Impact:** Reduced vulnerabilities from 16 to 5 (1 moderate, 4 low)

---

### 6. Temporal Logic - Multi-Temporal Reasoning

**Status Doc:** [`TEMPORAL-WORK-STATUS.md`](./TEMPORAL-WORK-STATUS.md)
**Specs:** [`TEMPORAL_EVAL_SPEC.md`](./TEMPORAL_EVAL_SPEC.md), [`TEMPORAL_PRELUDE_MACROS.md`](./TEMPORAL_PRELUDE_MACROS.md)
**Branch:** `mengwong/temporals-2` (1 commit ahead of main)

**Status:** Specs complete, ready for implementation

#### Overview

Runtime `EVAL...DO` construct for multi-temporal reasoning:
- Evaluate expressions under different temporal contexts
- Git commits as first-class temporal snapshots
- Six temporal axes: valid time, system time, rule version, rule validity, encoding time, decision time

#### Key Features

**EVAL Construct:**
```l4
EVAL `retroactive to` January 1 2010
     `under commit` "abc123"
     `evaluate` (`may purchase alcohol` applicant)
```

**Temporal Combinators:**
- `within N days after date ever predicate` - Sliding windows
- `strictly after date ever test` - Temporal bounds
- `retroactive to date evaluate expr` - Historic evaluation

#### Use Cases

1. **Regulatory Compliance:** "Was this transaction compliant under old rules?"
2. **Legal Research:** "How did eligibility criteria change over time?"
3. **Policy Simulation:** "What if we applied today's rules to historical cases?"
4. **Audit Verification:** "Was this historical decision correct?"

#### Files Added

- `doc/todo/TEMPORAL_EVAL_SPEC.md` - 142 lines, runtime EVAL specification
- `doc/todo/TEMPORAL_PRELUDE_MACROS.md` - 131 lines, temporal combinators
- `jl4-core/libraries/temporal-prelude.l4` - 143 lines, L4 implementation (on temporals-2)

#### Dependencies

- ✅ None - independent of other work streams
- ✅ Compatible with TYPICALLY defaults
- ✅ Works with Decision Service
- ✅ Integrates with mixfix notation

#### Next Steps

1. **Review specs** - Validate design and examples
2. **Merge specs to main** - Bring documentation into main branch
3. **Plan implementation** - Design MonadTemporal runtime
4. **Test strategy** - Git integration, context switching
5. **Implement runtime** - Add EVAL construct to evaluator

**Effort Estimate:** 2-3 weeks for full implementation

---

## Technical Debt & Future Work

### Critical Priority

1. **TYPICALLY Ambiguity Bug Fix**
   - **Status Doc:** [`TYPICALLY-STATUS-AND-NEXT-STEPS.md`](./TYPICALLY-STATUS-AND-NEXT-STEPS.md) ⭐
   - **Main Spec:** [`TYPICALLY-DEFAULTS-SPEC.md`](./TYPICALLY-DEFAULTS-SPEC.md)
   - **Blocker:** Feature flag at `TypeCheck.hs:3405` disabled due to test failures
   - **Impact:** Blocks Decision Service integration, transitive propagation
   - **Effort:** Estimated 1-2 weeks to debug and fix

### High Priority

2. **Boolean Minimization (Issue #638)**

   - Spec: [`BOOLEAN-MINIMIZATION-SPEC.md`](./BOOLEAN-MINIMIZATION-SPEC.md)
   - Dependencies: Integrates with TYPICALLY defaults
   - Complexity: Large feature, requires separate epic

3. **Four-State Runtime Model**

   - Spec: [`RUNTIME-INPUT-STATE-SPEC.md`](./RUNTIME-INPUT-STATE-SPEC.md)
   - Purpose: Distinguish NotProvided vs ExplicitUnknown vs Explicit vs NotApplicable
   - Enables: Smarter chatbot question flows
   - Dependencies: Requires TYPICALLY unblocked first

4. **ASSUME Deprecation**
   - Spec: [`ASSUME-DEPRECATION-SPEC.md`](./ASSUME-DEPRECATION-SPEC.md)
   - Reason: Confusing to non-programmers ("assume x is a boolean" sounds like x=TRUE)
   - Migration: Use GIVEN for parameters, DECLARE for types

### Medium Priority

5. **Dynamic File Management**

   - Status: Partial (push done in #649)
   - Remaining: File watching for auto-reload

6. **Pattern Matching Implementation**

   - Spec: Complete (PR #659)
   - Implementation: Not started
   - Estimate: Medium complexity (4 implementation approaches outlined)

7. **REF Annotation Support**
   - Spec: [`REF-ANNOTATION-SPEC.md`](./REF-ANNOTATION-SPEC.md)
   - Purpose: Cross-references to legal sources

### Low Priority

8. **WITHIN Absolute Date Support**

   - Spec: [`WITHIN-ABSOLUTE-DATE-SPEC.md`](./WITHIN-ABSOLUTE-DATE-SPEC.md)
   - Purpose: Temporal logic enhancements

9. **FETCH/POST JSON**
   - Spec: [`FETCH-POST-JSON-SPEC.md`](./FETCH-POST-JSON-SPEC.md)
   - Status: Basic implementation done (PR #644)
   - Future: Enhanced error handling, streaming

---

## Branch Strategy

### Main Branches

| Branch                          | Purpose             | Status      | Merge Target           |
| ------------------------------- | ------------------- | ----------- | ---------------------- |
| `main`                          | Stable release      | Active      | -                      |
| `mengwong/635`                  | Issue #635 work     | **Current** | `main`                 |
| `mengwong/foundation-course-ai` | Course materials    | ✅ Merged   | -                      |
| `mengwong/temporals-2`          | Temporal logic      | Active      | `main`                 |
| `mengwong/eval-for-temporals`   | Temporal evaluation | Active      | `mengwong/temporals-2` |
| `mengwong/temporals`            | Temporal (older)    | Stale?      | -                      |
| `mengwong/decision-imports`     | Import support      | Active      | `main`                 |

### Merge Strategy

- Work on feature branches
- Merge to `main` when stable
- `mengwong/635` is the main integration branch for decision service improvements

---

## Specification Inventory

### Completed (in `doc/done/`)

- ✅ `BIDIRECTIONAL-TYPE-CHECKING-SPEC.md`
- ✅ `CONDITIONAL-TRACE-SPEC.md`
- ✅ `DECISION-SERVICE-JSONDECODE-SPEC.md`
- ✅ `STRING-CONCAT-SPEC.md`

### In Progress (in `doc/todo/`)

- 🔄 `TYPICALLY-DEFAULTS-SPEC.md` - Core done, runtime pending
- 🔄 `RUNTIME-INPUT-STATE-SPEC.md` - Spec complete, implementation pending
- 🔄 `EXPORT-SYNTAX-SPEC.md` - Implementation complete (May 2025 note)

### Planned (in `doc/todo/`)

- ⏳ `BOOLEAN-MINIMIZATION-SPEC.md`
- ⏳ `ASSUME-DEPRECATION-SPEC.md`
- ⏳ `REF-ANNOTATION-SPEC.md`
- ⏳ `WITHIN-ABSOLUTE-DATE-SPEC.md`
- ⏳ `FETCH-POST-JSON-SPEC.md`
- ⏳ `PERFORMANCE-OPTIMIZATION-SPEC.md` (spec exists, implementation done)
- 📋 `TEMPORAL_EVAL_SPEC.md` - Runtime EVAL construct (from temporals-2 branch)
- 📋 `TEMPORAL_PRELUDE_MACROS.md` - Temporal combinators (from temporals-2 branch)

---

## Key Metrics & Performance

### Decision Service Performance (as of PR #653)

- **Sequential throughput:** 10x improvement (~100ms → ~10ms per request)
- **Parallel batch:** 325x improvement (~50s → ~0.02s for 100 cases)
- **Sustained rate:** 5,000 cases/second
- **Response size:** 20x reduction with trace disabled (152 vs 3,109 bytes)

### Test Coverage

- **Total examples:** 402 test files
- **Status:** All passing after recent changes
- **Integration tests:** 18 decision service tests passing
- **Golden tests:** Extensive golden file coverage

---

## Risk Areas & Blockers

### Current Risks

1. **TYPICALLY Blocked by Ambiguity Errors** ⚠️ **HIGH PRIORITY**

   - Risk: Feature 70% complete but gated behind flag due to test failures
   - Impact: Blocks Decision Service integration, transitive defaults
   - Next Step: Debug minimal reproduction case (see [TYPICALLY-STATUS-AND-NEXT-STEPS.md](./TYPICALLY-STATUS-AND-NEXT-STEPS.md))
   - Status: **Needs immediate attention**

2. **Four-State Model Migration**

   - Risk: Breaking change for Decision Service API consumers
   - Mitigation: Maintain backward compatibility via wrapper layer
   - Status: Not started

3. **ASSUME Deprecation**
   - Risk: Large codebase refactor, existing examples use ASSUME
   - Mitigation: Gradual migration with deprecation warnings
   - Status: Warnings implemented, migration not started

### Technical Debt

- Multiple HTML files in root (AGENTS.html, CLAUDE.html, etc.) - consider consolidation
- Some branches may be stale (mengwong/temporals)
- Pattern matching spec exists but no implementation

---

## Communication & Collaboration

### For New Contributors

1. **Start here:** Read [PROJECT-MASTER.md](./PROJECT-MASTER.md) (this file) for overview
2. **Active work:** Check [TYPICALLY-STATUS-AND-NEXT-STEPS.md](./TYPICALLY-STATUS-AND-NEXT-STEPS.md) for current priority
3. **Testing:** See [TESTING-GUIDE.md](../../TESTING-GUIDE.md) for hands-on examples
4. **Decision service:** Check [`ISSUE-635-PLANNING-STATUS.md`](./ISSUE-635-PLANNING-STATUS.md)
5. **Specs:** Browse `doc/todo/` for planned features, `doc/done/` for completed ones

### For Code Review

- PRs should reference relevant spec files
- Update this master doc when completing major milestones
- Mark specs as "done" and move to `doc/done/` when implemented

### For Project Planning

- Use this doc to identify available work streams
- Prioritize based on Risk/Priority sections
- Consider dependencies (e.g., Boolean Minimization needs TYPICALLY)

---

## Appendix: Quick Links

### 🎯 Start Here for Active Work

- **[TYPICALLY Status & Next Steps](./TYPICALLY-STATUS-AND-NEXT-STEPS.md)** - Current blocker and how to fix
- **[Temporal Logic Status](./TEMPORAL-WORK-STATUS.md)** - Multi-temporal reasoning (ready for implementation)
- **[Testing Guide: Web Sessions & Mixfix](../TESTING-GUIDE.md)** - Hands-on testing instructions
- **[Project Master](./PROJECT-MASTER.md)** - This document (overall status)

### Specifications

- [Issue #635 Status](./ISSUE-635-PLANNING-STATUS.md)
- [TYPICALLY Defaults](./TYPICALLY-DEFAULTS-SPEC.md)
- [TYPICALLY Status Doc](./TYPICALLY-STATUS-AND-NEXT-STEPS.md) ⭐
- [Runtime Input State](./RUNTIME-INPUT-STATE-SPEC.md)
- [Boolean Minimization](./BOOLEAN-MINIMIZATION-SPEC.md)
- [EXPORT Syntax](./EXPORT-SYNTAX-SPEC.md)
- [Performance Optimization](./PERFORMANCE-OPTIMIZATION-SPEC.md)
- [Temporal Logic Status](./TEMPORAL-WORK-STATUS.md) 📋
- [Temporal EVAL Spec](./TEMPORAL_EVAL_SPEC.md)
- [Temporal Prelude Macros](./TEMPORAL_PRELUDE_MACROS.md)

### Recent PRs (Last 10)

- [#660](https://github.com/smucclaw/l4-ide/pull/660) - Foundation course AI
- [#659](https://github.com/smucclaw/l4-ide/pull/659) - Pattern matching spec
- [#658](https://github.com/smucclaw/l4-ide/pull/658) - Security fixes
- [#657](https://github.com/smucclaw/l4-ide/pull/657) - Export annotation done
- [#656](https://github.com/smucclaw/l4-ide/pull/656) - Dictionary data type
- [#655](https://github.com/smucclaw/l4-ide/pull/655) - Agent support (Crush)
- [#654](https://github.com/smucclaw/l4-ide/pull/654) - @export annotation
- [#653](https://github.com/smucclaw/l4-ide/pull/653) - **Performance: 325x speedup!**
- [#652](https://github.com/smucclaw/l4-ide/pull/652) - JSONDECODE Either String
- [#651](https://github.com/smucclaw/l4-ide/pull/651) - #635 priorities

### Key Files

- Decision Service: `jl4-decision-service/src/`
- Core Language: `jl4-core/src/L4/`
- LSP: `jl4-lsp/src/`
- Examples: `jl4/examples/`

---

**Document Maintenance:**

- Update this file when merging major PRs
- Move completed specs from `doc/todo/` to `doc/done/`
- Keep branch status current
- Add new work streams as they emerge
