# Testing Windows Workflows

This document describes how to test the Windows build workflows before they're merged.

## Pre-merge Testing Strategy

Since GitHub Actions workflows only become available after they're merged to the default branch, we have several testing approaches:

### Option 1: Create a Draft PR (Recommended)

The `windows-test.yml` workflow is triggered by pull requests, so:

1. Create a draft PR from the `mengwong/windows` branch
2. The workflow will automatically run on the PR
3. Review the workflow results in the Actions tab
4. Fix any issues and push updates - the workflow will re-run
5. Once tests pass, convert to a regular PR for review

**Command:**

```bash
gh pr create --draft --title "Add Windows build support" \
  --body "Testing Windows build workflows. See [Windows Build Guide](./doc/dev/windows-build.md) for details."
```

### Option 2: Manual Validation

Before creating the PR, validate locally:

1. **YAML Syntax Check** - Ensure workflows are valid YAML
2. **Path Matching Test** - Verify trigger paths are correct
3. **Command Simulation** - Test key commands on Windows (if available)

### Option 3: Force Run on Branch

After merging to main, you can test the release workflow:

```bash
# Trigger the test workflow (once merged)
gh workflow run "Windows Build Test" --ref main

# Check status
gh run list --workflow=windows-test.yml --limit 5

# Watch logs in real-time
gh run watch
```

## What Gets Tested

### windows-test.yml (On PRs)

- ✓ GHC 9.8.4 installation on Windows
- ✓ Cabal configuration and dependency resolution
- ✓ Full build of all Haskell packages
- ✓ Test suite execution
- ✓ Executable discovery and validation
- ✓ Caching behavior

### windows-release.yml (On Tags)

- Everything from windows-test.yml, plus:
- ✓ Executable packaging
- ✓ Documentation generation
- ✓ Archive creation
- ✓ GitHub Release creation
- ✓ Artifact upload

## Common Issues to Watch For

### During First Run

1. **Long build times** (~45-60 minutes for first build)

   - Expected: dependencies need to compile
   - Cache will speed up subsequent runs

2. **Path length issues**

   - Windows has 260 character path limit by default
   - GitHub Actions runners have long paths enabled
   - Should not be an issue, but watch for it

3. **Line ending problems**

   - We set `autocrlf: false` to prevent issues
   - Shell scripts should use LF not CRLF

4. **Test failures**

   - Some path-based tests may fail on Windows
   - Document any expected failures

5. **Missing executables**
   - The workflow checks for expected .exe files
   - Some packages may not build executables on Windows

## Interpreting Results

### Success Indicators

- ✅ All packages build without errors
- ✅ Tests pass (or expected failures documented)
- ✅ All expected executables are found
- ✅ Archives are created with reasonable sizes

### Warning Signs

- ⚠️ Build takes >90 minutes (possible hanging)
- ⚠️ Missing expected executables
- ⚠️ Archive is much smaller than expected
- ⚠️ Excessive warnings in build output

### Failure Indicators

- ❌ Compilation errors
- ❌ Linker errors (missing Windows libraries)
- ❌ Test suite crashes
- ❌ Workflow syntax errors

## Next Steps After Testing

1. **If tests pass**: Convert draft PR to ready for review
2. **If tests fail**: Fix issues, push updates, re-test
3. **After merge**: Test release workflow with a test tag
4. **Document findings**: Update windows-build.md with any discoveries

## Monitoring Live Runs

```bash
# List recent workflow runs
gh run list --workflow=windows-test.yml --limit 10

# View specific run
gh run view <run-id>

# Watch logs in real-time
gh run watch <run-id>

# Download logs for offline analysis
gh run download <run-id>
```

## Manual Cleanup

If testing creates unwanted artifacts:

```bash
# List releases
gh release list

# Delete a test release
gh release delete <tag-name>

# Delete a test tag
git push --delete origin <tag-name>
```
