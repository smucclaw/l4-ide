# Windows Build Guide

This document describes how to build L4 IDE on Windows and how the Windows release process works.

## GitHub Actions Workflows

### Windows Release Workflow

**File:** `.github/workflows/windows-release.yml`

**Triggers:**

- Automatically on git tags matching `v*` (e.g., `v1.0.0`)
- Manually via workflow_dispatch

**What it does:**

1. Sets up a Windows runner with GHC 9.8.4
2. Builds all Haskell packages using Cabal
3. Runs the test suite
4. Collects all `.exe` files from the build output
5. Packages them into a zip file with documentation
6. Creates a GitHub release with the binaries (if triggered by a tag)

**Artifacts produced:**

- `l4-ide-windows-<version>.zip` containing:
  - `bin/` - All executable files (.exe)
  - `VERSION.txt` - Build metadata
  - `README.md` - Quick start guide

### Windows Test Workflow

**File:** `.github/workflows/windows-test.yml`

**Triggers:**

- Pull requests that modify Haskell code or workflows
- Manually via workflow_dispatch

**What it does:**

- Builds and tests on Windows to catch platform-specific issues early
- Does NOT create releases or artifacts
- Provides faster feedback for PR authors

## Building Locally on Windows

### Prerequisites

1. **Install GHCup** (Haskell toolchain manager)

   ```powershell
   # In PowerShell
   Set-ExecutionPolicy Bypass -Scope Process -Force;[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; try { Invoke-Command -ScriptBlock ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -ArgumentList $true } catch { Write-Error $_ }
   ```

2. **Install GHC and Cabal via GHCup**

   ```powershell
   ghcup install ghc 9.8.4
   ghcup install cabal latest
   ghcup set ghc 9.8.4
   ```

3. **Install Git** (if not already installed)

   - Download from: https://git-scm.com/download/win

4. **Clone the repository**
   ```bash
   git clone https://github.com/smucclaw/l4-ide.git
   cd l4-ide
   git submodule update --init
   ```

### Build Steps

```bash
# Update package index
cabal update

# Build all packages
cabal build all

# Run tests
cabal test all

# Install executables to ~/.cabal/bin/
cabal install all --overwrite-policy=always
```

### Finding Built Executables

After building, executables are located in:

```
dist-newstyle/build/x86_64-windows/ghc-9.8.4/<package>/x/<exe-name>/build/<exe-name>/
```

For example:

```
dist-newstyle/build/x86_64-windows/ghc-9.8.4/jl4-0.1/x/jl4-cli/build/jl4-cli/jl4-cli.exe
```

Or use `cabal install` to copy them to `~/.cabal/bin/`:

```bash
cabal install jl4:jl4-cli --overwrite-policy=always
```

## Creating a Windows Release

### Method 1: Via Git Tag (Recommended)

```bash
# Create and push a version tag
git tag -a v1.0.0 -m "Release version 1.0.0"
git push origin v1.0.0
```

This automatically triggers the Windows release workflow.

### Method 2: Manual Trigger

1. Go to: https://github.com/smucclaw/l4-ide/actions/workflows/windows-release.yml
2. Click "Run workflow"
3. Select branch
4. Optionally check "Create a GitHub release"
5. Click "Run workflow"

### Method 3: Local Build and Manual Upload

```bash
# Build everything
cabal build all

# Create release directory
mkdir -p release/bin

# Copy executables
# Note: Adjust paths based on your GHC version
find dist-newstyle -type f -name "*.exe" -exec cp {} release/bin/ \;

# Create zip archive
cd release
tar -czf ../l4-ide-windows-manual.zip *
cd ..

# Upload to GitHub releases manually
```

## Troubleshooting

### Build Fails on Windows

**Common issues:**

1. **Long path issues**

   - Windows has a 260 character path limit by default
   - Enable long paths in Windows 10/11:
     ```powershell
     # Run as Administrator
     New-ItemProperty -Path "HKLM:\SYSTEM\CurrentControlSet\Control\FileSystem" -Name "LongPathsEnabled" -Value 1 -PropertyType DWORD -Force
     ```
   - Or use shorter directory names for the repository

2. **Antivirus interference**

   - Some antivirus software may quarantine Haskell executables
   - Add `dist-newstyle/` and `~/.cabal/` to antivirus exclusions

3. **Missing system dependencies**

   - GHCup installer should handle most dependencies
   - If you get linker errors, you may need Visual Studio Build Tools
   - Download from: https://visualstudio.microsoft.com/downloads/ (look for "Build Tools for Visual Studio")

4. **Out of memory**
   - Haskell compilation can be memory-intensive
   - Close other applications
   - Or build packages one at a time:
     ```bash
     cabal build jl4-core
     cabal build jl4-lsp
     # etc.
     ```

### Tests Fail on Windows

Some tests may fail due to path separator differences or line ending issues:

- File path tests: Windows uses `\` instead of `/`
- Line endings: Windows uses `\r\n` instead of `\n`
- Case sensitivity: Windows filesystem is case-insensitive

These are expected and don't necessarily indicate problems with the core functionality.

### Executables Not Found

If `cabal install` doesn't put executables in your PATH:

1. Find your cabal bin directory:

   ```bash
   cabal path --installdir
   ```

2. Add it to your PATH:

   ```powershell
   # In PowerShell
   $env:PATH += ";$HOME\.cabal\bin"

   # To make permanent, add to your PowerShell profile
   Add-Content $PROFILE "`n`$env:PATH += `";`$HOME\.cabal\bin`""
   ```

## Performance Notes

### Build Times

Windows builds are typically slower than Linux builds:

- **First build:** 30-60 minutes (downloads and compiles all dependencies)
- **Incremental builds:** 5-15 minutes (only recompiles changed modules)
- **CI builds:** ~45 minutes with warm cache

### Caching

The GitHub Actions workflows use caching to speed up builds:

- **Cache key:** Based on GHC version, Cabal version, and `plan.json` hash
- **Cache contents:** `~/.cabal/store/` (pre-compiled dependencies)
- **Cache invalidation:** Automatic when dependencies change

To maximize cache hits:

- Don't change `cabal.project` unnecessarily
- Keep dependency versions stable
- Run `cabal freeze` to lock versions

## Comparison with Linux Build

| Aspect            | Linux        | Windows            |
| ----------------- | ------------ | ------------------ |
| Build time        | ~20-30 min   | ~45-60 min         |
| Executable size   | ~50-100 MB   | ~50-100 MB         |
| Test pass rate    | ~100%        | ~95% (path issues) |
| Container support | Yes (Ubuntu) | No                 |
| Native runner     | Yes          | Yes                |

## Future Improvements

Potential enhancements to the Windows build:

1. **Cross-compilation from Linux**

   - Use GHC cross-compiler to build Windows binaries on Linux
   - Faster and more consistent with main CI

2. **MSI Installer**

   - Create a proper Windows installer using WiX Toolset
   - Register file associations for `.l4` files
   - Add to Windows PATH automatically

3. **Code signing**

   - Sign executables with a code signing certificate
   - Reduces Windows SmartScreen warnings

4. **Scoop/Chocolatey packages**

   - Publish to Windows package managers
   - Easier installation and updates

5. **Portable version**
   - Bundle with minimal runtime dependencies
   - Run from USB drive without installation

## References

- [GHCup documentation](https://www.haskell.org/ghcup/)
- [Cabal User Guide](https://cabal.readthedocs.io/)
- [GitHub Actions - Windows runners](https://docs.github.com/en/actions/using-github-hosted-runners/about-github-hosted-runners#supported-runners-and-hardware-resources)
- [haskell-actions/setup](https://github.com/haskell-actions/setup)
