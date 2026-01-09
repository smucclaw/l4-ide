# Multi-Platform Build System

This document describes the multi-platform build and release system for L4 IDE.

## Overview

The L4 IDE project now supports automated builds and releases for multiple platforms and architectures via GitHub Actions:

| Platform    | Architecture                | Runner             | Status        |
| ----------- | --------------------------- | ------------------ | ------------- |
| **Windows** | x86_64                      | `windows-latest`   | ✅ Configured |
| **Linux**   | x86_64                      | `ubuntu-latest`    | ✅ Configured |
| **Linux**   | aarch64 (ARM64)             | `ubuntu-24.04-arm` | ✅ Configured |
| **macOS**   | Apple Silicon (M1/M2/M3/M4) | `macos-latest`     | ✅ Configured |

## Workflows

### Test Workflows (Run on PRs)

These workflows run automatically on pull requests to verify builds work:

- **`.github/workflows/windows-test.yml`** - Windows build verification
- **`.github/workflows/linux-test.yml`** - Linux multi-architecture build verification
- **`.github/workflows/macos-test.yml`** - macOS Apple Silicon build verification

**Triggers:**

- Pull requests to `main` branch
- Changes to `jl4*/**`, `cabal.project`, or workflow files
- Manual dispatch via GitHub Actions UI

**What they do:**

1. Set up Haskell toolchain (GHC 9.6.6 + Cabal 3.16.1.0)
2. Configure build with tests enabled
3. Restore/save dependency cache for faster subsequent runs
4. Build all packages
5. Run test suite
6. Verify expected executables were built

### Release Workflows (Run on Tags)

These workflows create GitHub releases with binary distributions:

- **`.github/workflows/windows-release.yml`** - Windows binary releases
- **`.github/workflows/linux-release.yml`** - Linux multi-architecture releases
- **`.github/workflows/macos-release.yml`** - macOS Apple Silicon releases

**Triggers:**

- Git tags matching `v*` (e.g., `v1.0.0`, `v0.1.0-beta`)
- Manual dispatch with optional release creation

**What they do:**
Everything from test workflows, plus:

1. Package executables into archive files
2. Strip debug symbols to reduce size
3. Generate VERSION.txt and README.md
4. Create platform-specific archives (`.tar.gz` or `.zip`)
5. Upload artifacts to workflow
6. Create GitHub release (on tag push)
7. Generate detailed build summaries

## Workflow Features

### Caching

All workflows use GitHub Actions caching to speed up builds:

- **Cache key:** Based on OS, architecture, GHC version, Cabal version, and `plan.json` hash
- **Cached data:** Cabal package store (`~/.cabal/store` or `C:\sr`)
- **Benefit:** Reduces build time from 45-60 minutes to 5-15 minutes after first build

### Concurrency Control

Test workflows use concurrency groups to prevent multiple runs per PR:

```yaml
concurrency:
  group: <platform>-build-${{ github.ref }}
  cancel-in-progress: true
```

This saves CI resources and provides faster feedback.

### Build Summaries

All workflows generate detailed summaries in the GitHub Actions UI showing:

- Platform and architecture
- GHC version
- List of built executables with sizes
- Archive file size
- Any warnings or issues

## Creating a Release

### Automatic Release (Recommended)

1. Ensure all changes are merged to `main`
2. Create and push a version tag:
   ```bash
   git checkout main
   git pull
   git tag -a v1.0.0 -m "Release version 1.0.0"
   git push origin v1.0.0
   ```
3. GitHub Actions automatically:
   - Builds for all platforms
   - Runs tests
   - Creates release with binaries
   - Generates release notes

### Manual Release

1. Go to Actions tab in GitHub
2. Select the appropriate release workflow
3. Click "Run workflow"
4. Choose branch/tag
5. Optionally enable "Create a GitHub release"
6. Click "Run workflow"

## Release Artifacts

### Windows (`l4-ide-windows-<version>.zip`)

**Contents:**

- `bin/` - All `.exe` files
- `VERSION.txt` - Build metadata
- `README.md` - Installation instructions

**Executables:**

- `jl4-cli.exe`
- `jl4-schema.exe`
- `jl4-lsp.exe`
- `jl4-repl.exe`
- `jl4-decision-service-exe.exe`
- `jl4-websessions.exe`

**Requirements:** Windows 10 or later (64-bit)

### Linux (`l4-ide-linux-<arch>-<version>.tar.gz`)

**Architectures:** `x86_64`, `aarch64`

**Contents:**

- `bin/` - All executable files (stripped)
- `VERSION.txt` - Build metadata including architecture
- `README.md` - Installation instructions

**Executables:** Same as Windows (without `.exe` extension)

**Requirements:** Linux kernel 3.2+ with glibc 2.17+

### macOS (`l4-ide-macos-arm64-<version>.tar.gz`)

**Platform:** Apple Silicon (M1/M2/M3/M4)

**Contents:**

- `bin/` - All executable files (stripped and ad-hoc signed)
- `VERSION.txt` - Build metadata
- `README.md` - Installation and Gatekeeper bypass instructions

**Executables:** Same as Linux

**Requirements:** macOS 11.0 (Big Sur) or later, Apple Silicon Mac

**Note:** Binaries are ad-hoc signed but not notarized. Users must bypass Gatekeeper on first run:

```bash
xattr -dr com.apple.quarantine bin/*
```

## Platform-Specific Notes

### Windows

**Build Configuration:**

- Uses GHC 9.6.6 (officially supported on Windows)
- Builds take ~45-60 minutes on first run
- Cache reduces to ~20-30 minutes
- Uses Git Bash for shell commands

**Known Issues:**

- None currently

**Dependencies:**

- Removed unused `Win32` package dependency from `jl4-lsp`
- Uses `directory` package for file operations on Windows

### Linux

**Build Configuration:**

- Separate matrix jobs for x86_64 and aarch64
- Uses native Ubuntu runners
- Binaries are stripped to reduce size

**Known Issues:**

- ARM64 builds may have dependency resolution issues (under investigation)

**Runner Details:**

- x86_64: `ubuntu-latest` (currently Ubuntu 22.04)
- aarch64: `ubuntu-24.04-arm` (GitHub-hosted ARM runners)

### macOS

**Build Configuration:**

- Uses `macos-latest` runner (currently macOS 14 on M1)
- Binaries are stripped and ad-hoc code signed
- Uses `-perm +111` for finding executables

**Known Issues:**

- None currently

**Runner Details:**

- Free tier includes Apple Silicon runners as of macos-14
- Previous `macos-latest-xlarge` required paid billing

## Troubleshooting

### Build Fails with "unused packages" Error

**Symptom:** Build fails with `-Werror=unused-packages` error

**Solution:** Remove the unused dependency from the cabal file. Example:

```diff
- if os(windows)
-   build-depends: Win32
- else
-   build-depends: unix
+ if !os(windows)
+   build-depends: unix
```

### Cache Not Working

**Symptom:** Every build takes full time even after first build

**Possible causes:**

1. `plan.json` changed (dependencies updated)
2. GHC or Cabal version changed
3. Cache expired (GitHub keeps caches for 7 days)

**Solution:** This is usually expected behavior. The cache will rebuild automatically.

### Executables Not Found

**Symptom:** "No executables found" in build summary

**Check:**

1. Did the build complete successfully?
2. Are executables named correctly in cabal files?
3. Are you filtering test executables correctly?

**Find command used:**

```bash
# Windows
find dist-newstyle -type f -name "*.exe"

# Linux/macOS
find dist-newstyle -type f -executable -name 'jl4-*' ! -name '*-test'
```

### ARM64 Dependency Resolution Fails

**Symptom:** Linux ARM64 build fails with "Could not resolve dependencies"

**Status:** Under investigation

**Workaround:** May need to pin specific dependency versions or exclude ARM64 temporarily

## Development Workflow

### Testing Workflows Locally

You can't run GitHub Actions locally, but you can simulate the build:

```bash
# Install GHC and Cabal
ghcup install ghc 9.6.6
ghcup install cabal latest
ghcup set ghc 9.6.6

# Build and test
cabal update
cabal configure --enable-tests --disable-documentation
cabal build all
cabal test all
```

### Adding a New Platform

1. Create `<platform>-test.yml` workflow
2. Create `<platform>-release.yml` workflow
3. Update this documentation
4. Add platform to the overview table
5. Test with a draft PR
6. Update `doc/dev/windows-build.md` or create platform-specific guide

### Modifying Workflows

When modifying workflows:

1. **Test with draft PRs** to avoid spamming notifications
2. **Use workflow_dispatch** triggers for manual testing
3. **Check logs carefully** for warnings and errors
4. **Update documentation** to reflect changes
5. **Consider cache invalidation** if changing dependencies

## CI Resources and Costs

### Free Tier Limits

GitHub Actions free tier (public repositories):

- **Linux/Windows:** Unlimited minutes
- **macOS:** 2000 minutes/month (10x multiplier = 200 actual minutes)

### Optimization Strategies

1. **Use caching** - Reduces build times by 70-80%
2. **Use concurrency groups** - Prevents redundant builds
3. **fail-fast: false** - Continue other matrix jobs even if one fails
4. **Conditional workflows** - Only run on relevant file changes

## Future Improvements

Potential enhancements:

1. **Cross-compilation:** Build Windows binaries on Linux (faster, cheaper)
2. **Docker images:** Pre-built images for faster CI startup
3. **Nix integration:** Use Nix for deterministic builds
4. **Binary signing:** Code sign macOS/Windows binaries properly
5. **Installers:** MSI (Windows), DMG (macOS), deb/rpm (Linux)
6. **Package managers:** Homebrew, Chocolatey, Scoop, apt
7. **x86_64 macOS:** Support Intel Macs (requires separate workflow)

## Related Documentation

- [Developer Setup Guide](./setup.md) - Local development setup
- [Windows Build Guide](./windows-build.md) - Windows-specific details
- [Deployment Guide](./deployment/) - NixOS deployment
- [Testing Guide](../../AGENTS.md) - AI agent testing

## References

- [GitHub Actions Documentation](https://docs.github.com/en/actions)
- [haskell-actions/setup](https://github.com/haskell-actions/setup)
- [GHCup](https://www.haskell.org/ghcup/)
- [Cabal User Guide](https://cabal.readthedocs.io/)
