# VFS Import Resolution Noise

**Type:** Bug / UX  
**Date:** 2025-12-11  
**Priority:** Low (cosmetic, not blocking)

## Summary

`jl4-cli` emits alarming "File does not exist" error diagnostics during import resolution, even when imports resolve successfully from the filesystem/cabal store.

## Reproduction

```bash
$ jl4-cli /path/to/file-with-imports.l4 2>&1 | head -20
```

Output includes:

```
Severity: DiagnosticSeverity_Error
Message:  File does not exist: /path/to/prelude.l4
...
[Import Resolution] Found on filesystem: ~/.cabal/store/.../prelude.l4
```

The file **is found** — but not before emitting error-severity diagnostics for every VFS cache miss.

## Problem

1. **Misleading severity** — VFS cache misses are logged as `DiagnosticSeverity_Error`
2. **Noisy output** — Multiple "File does not exist" messages per import
3. **Confusing for tooling** — Automated tools (like opm2l4's test harness) must filter these false positives

## Expected Behavior

- VFS cache misses should be `Debug` level, not `Error`
- Only emit `Error` if import **ultimately fails** (not found anywhere)
- Clean output when imports resolve successfully

## Location

`/Users/mengwong/src/smucclaw/l4-ide/jl4-core/src/` — likely in import resolution / VFS handling code.

## Workaround

Filter stderr for actual errors:

```bash
jl4-cli file.l4 2>&1 | grep -v "File does not exist"
```

## Acceptance Criteria

- [x] VFS cache misses logged at Debug level
- [x] No Error diagnostics for successfully resolved imports
- [x] Error only when import genuinely cannot be found

## Resolution

**Date:** 2025-12-12  
**Status:** Fixed

### Root Cause

In `jl4-lsp/src/LSP/Core/FileStore.hs:165`, the `getFileContentsImpl` function was calling `use_ GetModificationTime file`, which uses the default pattern with `missingFileDiagnostics=True`. This caused error diagnostics to be emitted whenever a file didn't exist in the VFS, even during normal import resolution when the file would subsequently be found on the filesystem.

### Solution

Changed line 165-166 in `LSP/Core/FileStore.hs` from:

```haskell
time <- use_ GetModificationTime file
```

to:

```haskell
-- Don't emit error diagnostics for missing files - they might not be in VFS yet
time <- use_ (GetModificationTime_ False) file
```

This suppresses the error diagnostics for VFS cache misses during file content retrieval, which is appropriate since:

1. Files may not be in VFS yet during import resolution
2. The import resolution code properly checks the filesystem afterwards
3. Only genuine import failures (file not found anywhere) should emit errors

### Verification

Tested with files that import `prelude`:

- **Before:** Multiple "File does not exist" error messages
- **After:** Clean output with only `[Import Resolution] VFS MISS` debug messages and proper error reporting for genuinely missing modules

### Files Changed

- `jl4-lsp/src/LSP/Core/FileStore.hs` - Modified `getFileContentsImpl` to suppress VFS cache miss diagnostics

### Test Results

- Main test suite: **441 examples, 0 failures** ✓
- No "File does not exist" errors in CLI output ✓
- Import resolution works correctly ✓
