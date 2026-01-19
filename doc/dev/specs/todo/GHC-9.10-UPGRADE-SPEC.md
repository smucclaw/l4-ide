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

- **Native builds**: GHC 9.6.6 (specified in `tested-with` field)
- **WASM builds**: GHC 9.10.3.20251220 (via ghc-wasm)
- **Known compatibility issue**: `Data.Text.show` added in text >= 2.1.2 (bundled with GHC 9.10)

## Tasks

### 1. Update Toolchain

- [ ] Update GHCup to install GHC 9.10.x as default
- [ ] Update `cabal.project` to use GHC 9.10
- [ ] Update all `tested-with` fields in cabal files:
  - `jl4-core/jl4-core.cabal`
  - `jl4-lsp/jl4-lsp.cabal`
  - `jl4-cli/jl4-cli.cabal`
  - `jl4-decision-service/jl4-decision-service.cabal`
  - `jl4-websessions/jl4-websessions.cabal`
  - `jl4/jl4.cabal`

### 2. Remove Version Conditionals

- [ ] Simplify `jl4-core/src/Base/Text.hs`:
  ```haskell
  -- Remove CPP, just use:
  import Data.Text as X hiding (show)
  ```
- [ ] Audit for other `MIN_VERSION` or `#if` conditionals

### 3. Fix Breaking Changes

Known issues to address:

- [ ] `Data.Text.show` now exported (already handled with `textShow` wrapper)
- [ ] Review GHC 9.10 release notes for other breaking changes
- [ ] Update any deprecated APIs

### 4. Update CI

- [ ] Update GitHub Actions workflow to use GHC 9.10
- [ ] Update any Docker images used for builds
- [ ] Verify all CI jobs pass

### 5. Update Documentation

- [ ] Update README with new GHC version requirement
- [ ] Update any setup/installation docs
- [ ] Update CONTRIBUTING.md if it mentions GHC version

## Testing

1. Full test suite passes: `cabal test all`
2. Native build works: `cabal build all`
3. WASM build works: `wasm32-wasi-cabal --project-file=cabal-wasm.project build jl4-core`
4. LSP works in VS Code
5. Decision service works

## Rollback Plan

If issues are discovered post-upgrade:
1. Revert `tested-with` changes
2. Restore CPP conditionals
3. Pin GHC 9.6.6 in CI

## Dependencies

- ghc-wasm toolchain compatibility (currently uses 9.10.3.20251220)
- Hackage package compatibility with GHC 9.10

## References

- [GHC 9.10.1 Release Notes](https://downloads.haskell.org/ghc/9.10.1/docs/users_guide/9.10.1-notes.html)
- [Migration Guide](https://gitlab.haskell.org/ghc/ghc/-/wikis/migration/9.10)
