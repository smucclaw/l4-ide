# GHC 9.10 Upgrade Specification

## Status: TODO

## Overview

Upgrade the project from GHC 9.6.6 to GHC 9.10.x to unify the native and WASM toolchains and eliminate CPP version conditionals.

## Motivation

1. **Unified toolchain**: WASM builds require GHC 9.10+; native builds currently use 9.6.6
2. **Eliminate CPP hacks**: Version-specific code paths (e.g., `MIN_VERSION_text(2,1,2)` in `Base/Text.hs`) add complexity
3. **Access to newer features**: GHC 9.10 includes performance improvements and new language features
4. **Simplified CI**: One GHC version to test against

## Current State

### GHC Versions in Use

| Component         | Current GHC Version | Source                                                               |
| ----------------- | ------------------- | -------------------------------------------------------------------- |
| Native builds     | 9.6.6               | `tested-with` fields in cabal files                                  |
| CI main workflow  | 9.8.4               | `.github/workflows/build.yml`                                        |
| CI platform tests | 9.6.6               | `linux-test.yml`, `macos-test.yml`, `windows-test.yml`               |
| WASM builds       | 9.10.x              | `.github/workflows/wasm-build.yml` (env: `GHC_WASM_VERSION: "9.10"`) |
| Developer docs    | 9.8.4               | `doc/dev/setup.md`                                                   |

### Files with `tested-with` Declarations

| File                                              | Current Value |
| ------------------------------------------------- | ------------- |
| `jl4-core/jl4-core.cabal`                         | `GHC==9.6.6`  |
| `jl4-lsp/jl4-lsp.cabal`                           | `GHC==9.6.6`  |
| `jl4/jl4.cabal`                                   | `GHC==9.6.6`  |
| `jl4-websessions/jl4-websessions.cabal`           | `GHC==9.6.6`  |
| `jl4-decision-service/jl4-decision-service.cabal` | `ghc ==9.6.6` |
| `jl4-query-plan/jl4-query-plan.cabal`             | `ghc ==9.6.6` |
| `jl4-repl/jl4-repl.cabal`                         | `GHC==9.6.6`  |

### Known Compatibility Issues

- **`Data.Text.show`**: Added in text >= 2.1.2 (bundled with GHC 9.10). Already handled with `textShow` wrapper in `Base/Text.hs`.

## CPP Conditionals Audit

The following files use CPP that may need review:

### Version-Specific Conditionals (MUST REMOVE)

| File                        | Conditional               | Purpose                             | Action                                 |
| --------------------------- | ------------------------- | ----------------------------------- | -------------------------------------- |
| `jl4-core/src/Base/Text.hs` | `MIN_VERSION_text(2,1,2)` | Hide `Data.Text.show` on newer text | Remove CPP, always use `hiding (show)` |

### Platform-Specific Conditionals (KEEP)

| File                                      | Conditional        | Purpose                                 |
| ----------------------------------------- | ------------------ | --------------------------------------- |
| `jl4-lsp/src/LSP/Core/FileUtils.hs`       | `mingw32_HOST_OS`  | Windows vs POSIX file modification time |
| `jl4-core/src/L4/Wasm.hs`                 | `wasm32_HOST_ARCH` | WASM-specific JS FFI exports            |
| `jl4-core/src/L4/EvaluateLazy/Machine.hs` | `HTTP_ENABLED`     | Feature flag for HTTP support           |

### Other CPP Usage (REVIEW)

| File                                        | Notes                                          |
| ------------------------------------------- | ---------------------------------------------- |
| `jl4-lsp/src/LSP/Core/Shake.hs`             | CPP pragma present but no version conditionals |
| `jl4-lsp/src/LSP/Core/Types/Diagnostics.hs` | CPP pragma present but no version conditionals |

## Tasks

### 1. Update Toolchain

- [ ] Update GHCup to install GHC 9.10.x as default
- [ ] Update `cabal.project` to specify GHC 9.10 (if needed)
- [ ] Update all `tested-with` fields in cabal files:
  - [ ] `jl4-core/jl4-core.cabal`: `GHC==9.6.6` → `GHC==9.10.1`
  - [ ] `jl4-lsp/jl4-lsp.cabal`: `GHC==9.6.6` → `GHC==9.10.1`
  - [ ] `jl4/jl4.cabal`: `GHC==9.6.6` → `GHC==9.10.1`
  - [ ] `jl4-websessions/jl4-websessions.cabal`: `GHC==9.6.6` → `GHC==9.10.1`
  - [ ] `jl4-decision-service/jl4-decision-service.cabal`: `ghc ==9.6.6` → `GHC==9.10.1`
  - [ ] `jl4-query-plan/jl4-query-plan.cabal`: `ghc ==9.6.6` → `GHC==9.10.1`
  - [ ] `jl4-repl/jl4-repl.cabal`: `GHC==9.6.6` → `GHC==9.10.1`

### 2. Remove Version Conditionals

- [ ] Simplify `jl4-core/src/Base/Text.hs`:

  **Before:**

  ```haskell
  {-# LANGUAGE CPP #-}
  module Base.Text (module X, textShow) where

  #if MIN_VERSION_text(2,1,2)
  import Data.Text as X hiding (show)
  #else
  import Data.Text as X
  #endif
  import Data.Text.IO as X
  ```

  **After:**

  ```haskell
  module Base.Text (module X, textShow) where

  import Data.Text as X hiding (show)
  import Data.Text.IO as X
  ```

- [ ] Verify no other `MIN_VERSION` conditionals exist (audit complete - none found)

### 3. Fix Breaking Changes

Known issues to address:

- [x] `Data.Text.show` now exported (already handled with `textShow` wrapper)
- [ ] Review [GHC 9.10.1 Release Notes](https://downloads.haskell.org/ghc/9.10.1/docs/users_guide/9.10.1-notes.html) for breaking changes
- [ ] Review [GHC 9.8 Release Notes](https://downloads.haskell.org/ghc/9.8.1/docs/users_guide/9.8.1-notes.html) (intermediate version)
- [ ] Update any deprecated APIs
- [ ] Check for changes in `base` library
- [ ] Check for changes in `template-haskell` if TH is used

### 4. Update CI

The following GitHub Actions workflows need updating:

| Workflow     | File                                 | Current GHC | Action           |
| ------------ | ------------------------------------ | ----------- | ---------------- |
| CI (main)    | `.github/workflows/build.yml`        | 9.8.4       | Update to 9.10.x |
| Linux Test   | `.github/workflows/linux-test.yml`   | 9.6.6       | Update to 9.10.x |
| macOS Test   | `.github/workflows/macos-test.yml`   | 9.6.6       | Update to 9.10.x |
| Windows Test | `.github/workflows/windows-test.yml` | 9.6.6       | Update to 9.10.x |
| WASM Build   | `.github/workflows/wasm-build.yml`   | 9.10        | Already correct  |

- [ ] Update `build.yml`: Change `ghc-version: ["9.8.4"]` to `ghc-version: ["9.10.1"]`
- [ ] Update `linux-test.yml`: Change `ghc-version: "9.6.6"` to `ghc-version: "9.10.1"`
- [ ] Update `macos-test.yml`: Change `ghc-version: ["9.6.6"]` to `ghc-version: ["9.10.1"]`
- [ ] Update `windows-test.yml`: Change `ghc-version: ["9.6.6"]` to `ghc-version: ["9.10.1"]`
- [ ] Verify WASM build still works (already uses 9.10)
- [ ] Update any Docker images used for builds (check Nix configs)
- [ ] Run all CI jobs and verify they pass

### 5. Update Documentation

- [ ] Update `doc/dev/setup.md`: Change `ghc 9.8.4` to `ghc 9.10.1` in requirements
- [ ] Update README if it mentions GHC version requirements
- [ ] Update any other setup/installation docs
- [ ] Update CONTRIBUTING.md if it mentions GHC version

### 6. Dependency Compatibility Check

Verify all dependencies work with GHC 9.10:

- [ ] Check `cabal.project` `index-state` is recent enough (currently `2025-03-31`)
- [ ] Build and test to find any incompatible dependencies
- [ ] Update `allow-newer` in cabal.project if needed temporarily
- [ ] Report/fix any upstream issues discovered

## Testing Plan

### Pre-Upgrade Testing

1. Record current test results as baseline: `cabal test all 2>&1 | tee /tmp/pre-upgrade.txt`

### Post-Upgrade Testing

1. **Native build**: `cabal build all`
2. **Full test suite**: `cabal test all`
3. **WASM build**: `wasm32-wasi-cabal --project-file=cabal-wasm.project build jl4-wasm`
4. **LSP functionality**:
   - Install LSP: `cabal install exe:jl4-lsp --overwrite-policy=always`
   - Open VS Code, load an L4 file
   - Verify hover, diagnostics, completions work
5. **Decision service**:
   - Run service: `cabal run jl4-decision-service-exe -- --port 8081`
   - Test API endpoints
6. **REPL**: `cabal run jl4-repl -- jl4/examples/factorial.l4`
7. **Web IDE**: Start local services and verify web editor works

### CI Verification

- [ ] All platform tests pass (Linux x86_64, Linux aarch64, macOS, Windows)
- [ ] WASM build continues to work
- [ ] TypeScript monorepo build unaffected

## Rollback Plan

If issues are discovered post-upgrade:

1. Revert all `tested-with` changes to `GHC==9.6.6`
2. Restore CPP conditionals in `Base/Text.hs`
3. Revert CI workflow GHC version changes
4. Pin GHC 9.6.6 in GHCup: `ghcup set ghc 9.6.6`

## Implementation Order

1. **First**: Update local development environment to GHC 9.10
2. **Second**: Fix `Base/Text.hs` (remove CPP)
3. **Third**: Build and test locally (`cabal build all && cabal test all`)
4. **Fourth**: Update cabal files (`tested-with`)
5. **Fifth**: Update CI workflows
6. **Sixth**: Update documentation
7. **Finally**: Create PR and verify all CI passes

## Dependencies

- ghc-wasm toolchain compatibility (currently uses 9.10.3.20251220) ✓
- Hackage package compatibility with GHC 9.10 (to be verified)
- haskell-actions/setup@v2 support for GHC 9.10 ✓

## Estimated Effort

| Task                    | Estimate                            |
| ----------------------- | ----------------------------------- |
| Toolchain setup         | 30 min                              |
| Remove CPP conditionals | 15 min                              |
| Fix breaking changes    | 1-4 hours (depends on issues found) |
| Update CI               | 30 min                              |
| Testing                 | 1-2 hours                           |
| Documentation           | 30 min                              |
| **Total**               | **3-8 hours**                       |

## Success Criteria

- [ ] All tests pass on GHC 9.10
- [ ] All CI workflows pass
- [ ] No CPP version conditionals remain (only platform conditionals)
- [ ] WASM and native builds use the same GHC major version
- [ ] Documentation reflects GHC 9.10 as the required version

## References

- [GHC 9.10.1 Release Notes](https://downloads.haskell.org/ghc/9.10.1/docs/users_guide/9.10.1-notes.html)
- [GHC 9.8.1 Release Notes](https://downloads.haskell.org/ghc/9.8.1/docs/users_guide/9.8.1-notes.html)
- [Migration Guide 9.10](https://gitlab.haskell.org/ghc/ghc/-/wikis/migration/9.10)
- [Migration Guide 9.8](https://gitlab.haskell.org/ghc/ghc/-/wikis/migration/9.8)
- [ghc-wasm-meta](https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta)
