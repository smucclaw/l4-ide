# Branch Separation Summary

The mixed work from `mengwong/auto-update-decision-service-from-websessions` has been separated into the following feature branches:

## Created Branches

### 1. **mengwong/typically-first-draft**
- Based at commit: `a465cddb` (2 commits from base)
- Contains: Initial TYPICALLY keyword implementation
- Key commits:
  - `6fea6b8e` Add TYPICALLY keyword for default values and ASSUME deprecation warnings
  - `a465cddb` Add TYPICALLY type checking validation

### 2. **mengwong/typically-with-fixes**
- Based at commit: `aa7ed332` (includes all TYPICALLY work + fixes)
- Contains: Complete TYPICALLY implementation with bug fixes
- Additional commits beyond first-draft:
  - `d33fdf34` Update TYPICALLY spec with IDE support status
  - `b5c5f8a2` Add required default handling mode to runtime spec
  - `ace965de` Update TYPICALLY spec and add runtime input state spec
  - `7a50e22e` Fix test compilation errors after DefaultMode addition
  - `aa7ed332` Fix DefaultMode OpenAPI schema to match ToJSON encoding

### 3. **mengwong/peval-first-draft**
- Based at commit: `afba20a0` (includes TYPICALLY as dependency + 15 PEVAL commits)
- Contains: Presumptive evaluation (PEVAL) implementation
- Key commits (14 PEVAL-specific):
  - `a249d8b4` Add PEVAL/PASSERT directive syntax
  - `ff299551` Clarify PEVAL implementation approach
  - `1c06b550` WIP: Begin presumptive wrapper generation
  - `25f1bd8d` Shift to compile-time wrapper generation
  - `08280fd6` Document runtime Unknown propagation
  - `352924a3` → `afba20a0` Iterative PEVAL implementation and refinement

### 4. **mengwong/websessions-docs** ⚠️ MOSTLY REDUNDANT
- Based at commit: `24aae93a` (includes PEVAL + 4 websessions commits)
- Contains: **~90% already merged to main**
- **Already in main:**
  - Foundation course docs (PR #660)
  - Websessions auto-push integration (commit ac5a8d20, PR #690)
  - DEPLOYMENT.md, dev-start.sh, dev-config.md (PR #690)
- **Unique content (3 commits, minor doc updates):**
  - `914e03b3` Run npm format on markdown files
  - `9ec2f49d` Rename 'CRUD server' to 'websessions'
  - `24aae93a` Clarify bidirectional communication
- **Recommendation:** Cherry-pick the 3 doc commits or close branch as redundant

### 5. **mengwong/temporal-specs**
- Based from: `origin/main` (clean branch)
- Contains: Temporal logic specification documents only
- Files added:
  - `doc/todo/TEMPORAL-WORK-STATUS.md`
  - `doc/todo/TEMPORAL_EVAL_SPEC.md`
  - `doc/todo/TEMPORAL_PRELUDE_MACROS.md`
  - `doc/todo/WITHIN-ABSOLUTE-DATE-SPEC.md`

## Commits NOT Captured

The following commits from the original branch were NOT included in any feature branch (debug/cleanup):
- `2fb292fe` Remove noisy debug trace for operator overload disambiguation
- `fc639ac0` Remove debug trace statements from TypeCheck
- `8a6c76f9` Remove verbose function key listing from startup output
- `a718cd7c` Apply formatting with prettier
- `9cfcd06d` Remove unused Data.Map import
- `3c15d00f` Comment out ASSUME deprecation warnings (could be added to typically-with-fixes)
- `cf8d943a` Revert debug trace removals
- `fceb23ec` Document heisenbug and remove safe debug traces

## Dependencies

- **PEVAL depends on TYPICALLY**: The PEVAL branch includes all TYPICALLY commits as a base
- **Websessions depends on PEVAL**: The websessions branch includes both TYPICALLY and PEVAL

## Already Merged to Main

- Foundation course documentation (PR #660, commit 48f95ce1)
- Websessions auto-push feature (commits ac5a8d20, 89171708)
- Deployment/dev tooling (PR #690: DEPLOYMENT.md, dev-start.sh, dev-config.md, PROVISIONING.md)
- Advanced course documentation (with minor differences in websessions-docs branch)

## Recommendations

1. **typically-with-fixes** should be reviewed and merged first (foundational)
2. **peval-first-draft** can be reviewed after TYPICALLY merges (will need rebase)
3. **websessions-docs** is ~90% redundant - consider cherry-picking only the 3 unique doc commits or closing
4. **temporal-specs** is independent and can be merged anytime
5. The original `mengwong/auto-update-decision-service-from-websessions` branch can be deleted after verification

## Merge Priority

**High priority:**
- typically-with-fixes (foundational feature)
- temporal-specs (pure documentation)

**Medium priority:**
- peval-first-draft (depends on TYPICALLY, complex implementation)

**Low priority / Consider closing:**
- websessions-docs (mostly already merged, only 3 minor doc commits unique)
- typically-first-draft (superseded by typically-with-fixes)

## Branch URLs

- https://github.com/smucclaw/l4-ide/tree/mengwong/typically-first-draft
- https://github.com/smucclaw/l4-ide/tree/mengwong/typically-with-fixes
- https://github.com/smucclaw/l4-ide/tree/mengwong/peval-first-draft
- https://github.com/smucclaw/l4-ide/tree/mengwong/websessions-docs
- https://github.com/smucclaw/l4-ide/tree/mengwong/temporal-specs
