# L4 Versioning

A lightweight versioning scheme for the L4 language and compiler. Designed to add semantic meaning to the existing monotonic build numbers without disrupting the release process or introducing constraint-solver machinery.

**Status:** proposed. **Dependencies:** none. **Unblocks:** the L4-compiler-version pinning work in [STATEFUL-CONTRACT-DEPLOYMENT.md](STATEFUL-CONTRACT-DEPLOYMENT.md#L4_VERSIONING).

---

## 1. Goal

Replace the current bare-build-number identity (build 53) with a scheme that:

- Tells consumers whether two L4 releases are drop-in compatible
- Preserves the existing build number as the authoritative artifact identity
- Adds no constraint solver, no edition cadence commitment, no per-release ceremony beyond a one-line CHANGELOG entry
- Gives legal-grade reproducibility: any compilation can be reproduced years later from public inputs

## 2. The scheme: `MAJOR.MINOR.BUILD`

Three parts. One is a counter; one is mechanically derived; one is a deliberate human act.

| Part      | Source                               | When it changes                                                                                                                |
| --------- | ------------------------------------ | ------------------------------------------------------------------------------------------------------------------------------ |
| **MAJOR** | Maintainer decision, manual only     | Conceptual shift, paradigm change, or intentional repositioning of the language. Never automatic.                              |
| **MINOR** | Automated release check              | The release introduces an interface change that prevents existing L4 source from deploying unchanged against the new compiler. |
| **BUILD** | The existing monotonic build counter | Every build, automatically. Never resets, never goes backward.                                                                 |

Examples:

- Current `build 53` → `0.7.53`
- Next build, no interface changes detected: `0.7.54`
- Next build, automated check flags a breaking interface change: `0.8.54`
- Maintainer manually declares the next release a paradigm shift: `1.0.54`

**The build counter is global**: it doesn't reset when MAJOR or MINOR bump. This is the only departure from textbook SemVer, and it's the point — the build number is the artifact's identity. Bumping MINOR shouldn't disturb that.

Within a `MAJOR.MINOR` line, two builds are guaranteed compatible: `0.7.53` and `0.7.99` will accept the same source and produce the same semantics. Across MINOR, existing source files may need the new compiler to deploy. Across MAJOR, conceptual change is asserted — compatibility is not implied either way.

## 3. Automated classification at release

The MINOR/BUILD decision is mechanical. On every release-candidate build, a `l4 release-check` step runs and emits a classification verdict by comparing the new compiler against the last released build's artifact:

```
$ l4 release-check --baseline 0.7.52
COMPATIBLE — bump BUILD only → 0.7.53
```

or:

```
$ l4 release-check --baseline 0.7.53
BREAKING — bump MINOR → 0.8.54
  - jl4/examples/safe-governance.l4: type-check fails (was OK in 0.7.53)
  - prelude function `applyRebate`: signature changed
  - DEONTIC JSON serialization: action variant `pay` now requires `currency` field
```

### What the check looks at

L4's interface is mechanically extractable; the check compares the new build's interface against the last release's recorded one. Three layers:

1. **Test corpus.** Every `.l4` file under `jl4/examples/`, `jl4/tests/`, and `validation/` is parsed, type-checked, and (where applicable) evaluated through any `#EVAL` / `#TRACE` directives. Any file that previously compiled and now fails, or any directive that previously produced output X and now produces output Y, is a breaking signal.

2. **Interface manifest.** The compiler emits a structured manifest of its public surface:

   - The grammar's keyword/operator set
   - Prelude / standard library: every exported type, function, and signature
   - Builtin functions and their signatures
   - DEONTIC value JSON serialization shape
   - MCP tool schemas generated from `@export` declarations

   Two manifests are compared categorically:

   - **Removed** symbols or **changed** signatures → breaking
   - **Added** symbols → compatible (existing source doesn't reference them)
   - **Keyword additions** are checked against the corpus: if any corpus file used the new keyword name as an identifier, that's breaking; otherwise compatible

3. **Runtime semantics.** Each `#EVAL` / `#TRACE` in the corpus has a recorded expected output. A mismatch is a breaking signal even if the source still type-checks.

### The classification rules

| Signals observed                                                                     | Verdict                               |
| ------------------------------------------------------------------------------------ | ------------------------------------- |
| No corpus failures, no manifest removals/changes, no semantics drift                 | **COMPATIBLE** — bump BUILD only      |
| Any corpus failure, any manifest removal or signature change, or any semantics drift | **BREAKING** — bump MINOR (and BUILD) |
| Maintainer explicitly invokes `--major`                                              | **MAJOR** bump, manual only           |

### Override mechanism

The verdict can be overridden by the maintainer in two directions, each requiring an explicit CHANGELOG note:

- `--force-compatible "<rationale>"` — accept a breaking signal as patch-level (rare: e.g., the corpus failure is in a deprecated test that's being retired this release).
- `--force-breaking "<rationale>"` — bump MINOR even though the check is clean (rare: e.g., a behaviour change the corpus doesn't exercise).

Both overrides are recorded in the CHANGELOG so the audit trail makes the human judgment explicit. Without an override, the check is the source of truth.

### Caveats

The check is empirical: it catches what the corpus exercises. Two practical implications:

1. **The corpus must be kept comprehensive.** Every new language feature should ship with corpus entries that exercise it. Treat corpus expansion as part of the feature.
2. **Behavioural changes invisible to the corpus will be misclassified as compatible.** This is unavoidable for any empirical check. The mitigation is maintainer review of the change list (not the classification) on every release — `--force-breaking` exists for exactly this case.

### CHANGELOG output

The release check writes the CHANGELOG entry automatically:

```markdown
## 0.7.54 — 2026-05-23

**COMPATIBLE** (auto)

- Fix: dictionary lookup with missing key now returns NOTHING instead of erroring
- Internal: faster type-checker for deeply nested ANYOF types
```

```markdown
## 0.8.54 — 2026-05-23

**BREAKING** (auto)

- Removed: `applyRebate` prelude function (renamed to `apply_rebate` for consistency)
- Changed: DEONTIC JSON serialization now includes a `version` field on the root
- Corpus impact: jl4/examples/safe-governance.l4 needs update to deploy on 0.8

Migration: <one-liner pointer to release notes>
```

Two manual additions per release: the release date (auto-fillable) and the one-line description of each change (the developer writes it as they merge the PR). The classification, the corpus impact list, and the bumps are all automatic.

## 4. Content hash per release

Every tagged release publishes a SHA-256 over the compiler+runtime+stdlib artifact tarball.

```
Release: 0.7.53
  artifact: l4-0.7.53.tar.gz
  sha256:   3f2a9b7c4e1d0f8b6a5c2d9e4f7a1b3c5d8e0f2a4b6c8d0e2f4a6b8c0d2e4f6a
```

The hash is recorded:

- in the GitHub release notes
- in `CHANGELOG.md` next to the version
- in a machine-readable manifest the deployment service can fetch (`releases.json` or similar)

The label is for people; the hash is for the machine. Anyone reproducing a build years later can verify they have the exact compiler that was used.

## 5. Pinning options

Downstream tools — including jl4-service — accept any of three pin shapes for the L4 compiler version a bundle requires:

| Shape            | Meaning                                                                                | Use                                                                        |
| ---------------- | -------------------------------------------------------------------------------------- | -------------------------------------------------------------------------- |
| `"0.7"`          | Loose: any build with MAJOR.MINOR `0.7`. Automatically picks up patches and bug fixes. | The default recommendation for authors.                                    |
| `"0.7.53"`       | Strict: this exact build.                                                              | Reproducible builds; when an author has tested against one specific build. |
| `"sha256:3f2a…"` | Hash: this exact artifact, byte-identical.                                             | Legal-grade reproducibility audits.                                        |

`0.7.53` and the corresponding hash resolve to the same artifact. They differ only in tamper-evidence: the hash is unforgeable.

Future across-major ranges (`">=0.7,<2"`) are out of scope until a real use case demands them. The three pin shapes above cover everything practical.

## 6. The transition from build numbers

Mechanical steps to move from "build 53" to `0.7.53`:

1. **Pick the starting MAJOR.MINOR.** Recommended: `0.7`. The `0.x` indicates pre-1.0 (the language is still evolving); `.7` reflects roughly how mature the design currently is. Adjust if the maintainer prefers a different starting point.
2. **Tag the current build** as `0.7.53` (or whatever the current build number is).
3. **Record the interface manifest** for `0.7.53` — this becomes the baseline the next release's `l4 release-check` compares against.
4. **Publish the content hash** for the `0.7.53` artifact.
5. **Wire `l4 release-check` into the release pipeline** so the next tag is classified automatically.
6. **Add CHANGELOG.md** with a single entry for the current state (`0.7.53 — initial versioned release`). All prior history can be summarised in one note.

That's the entire transition. Existing tooling that knows about build numbers keeps working (build 53 is still 53; it just also has a label). From the next release onwards, classification is automatic.

## 7. Editions are deferred

Rust-style editions — declared, multi-year breaking-change boundaries with source-level migration tooling — are a great idea but require commitment the language isn't ready to make:

- A promise that source compiles unchanged within an edition, forever.
- A `cargo fix --edition` style migration tool.
- A multi-year edition cadence.

Defer until L4 reaches 1.0. By then, the CHANGELOG will have years of real data about what kinds of changes the project actually makes — a much better basis for choosing edition cadence than guessing now. Until then, MAJOR bumps in `0.x` serve the same role at much lower commitment.

## 8. Anti-patterns

A few patterns to avoid, drawn from watching other projects struggle.

- **Resetting BUILD at MAJOR/MINOR boundaries.** Defeats the "build number = identity" property. Two artifacts with the same build number cause grief that takes years to untangle.
- **Tying the L4 compiler version to the jl4-service version.** They must evolve independently. Conflating them means a service patch breaks every deployment, or a language patch is held hostage to a service release.
- **Skipping `l4 release-check`.** Manually deciding "this feels compatible" silently re-introduces the discipline problem the automation was meant to solve. The check is the source of truth; the override exists, is logged, and is the only legitimate escape hatch.
- **Letting the corpus rot.** The automated check is only as good as the test files it runs. Every new language feature should land with corpus coverage; the check should fail a release whose feature isn't exercised anywhere.
- **Four-part versions (`0.7.53.1`).** PVP-style numbering solves a problem (mature ecosystems with strong constraint solvers) L4 doesn't have. SemVer-plus-build covers every practical need.
- **CalVer alone (`2025.04`).** Tells you how old something is, not whether it's safe to upgrade.

## 9. Open decision

**Recommended starting version.** The body of this doc uses `0.7.53` as the example, but the actual value should be a maintainer call. Reasonable choices:

- `0.7.53` — implies "we are roughly 70% of the way to a stable release"
- `0.1.53` — implies "we are still early"
- `1.0.53` — implies "we are committing to the current API surface"

Pick the one that best signals to downstream consumers where the language actually is. This decision is best made once, when the first tag goes out; thereafter the scheme drives itself.

## 10. References

- [STATEFUL-CONTRACT-DEPLOYMENT.md](STATEFUL-CONTRACT-DEPLOYMENT.md) — uses this versioning scheme for the `@l4_version` annotation on bundles
- [SemVer 2.0.0](https://semver.org/) — the standard this scheme adapts
- [Rust editions](https://doc.rust-lang.org/edition-guide/) — the model deferred until L4 1.0
- [PVP](https://pvp.haskell.org/) — the model explicitly rejected as over-engineered
