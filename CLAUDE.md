# Claude Development Notes for L4 IDE

## Testing

Before committing changes, always run the full test suite to ensure nothing is broken:

```bash
cabal test all
```

The test suite should pass before creating a git commit.

### Golden Files

The test suite uses golden files for snapshot testing. Golden files capture the expected output of tests.

**Important behaviors:**

- **First run**: If a golden file doesn't exist, it will be created automatically on the first test run. The test will fail with "Failed because failFirstTime is set to True".
- **Second run**: Running the test again will compare against the newly created golden file and should pass.
- **When adding new test files**: You typically need to run `cabal test all` twice - once to create the golden files, and once to verify they match.

**Updating golden files:**

If you've made intentional changes that affect test output and you're confident the new behavior is correct, you can:

1. Delete the relevant golden files (usually in `.golden/` directories)
2. Run `cabal test all` twice to regenerate and verify the golden files

This is preferable to manually editing golden files, as it ensures the test output exactly matches what the system produces.

## Building

To build the entire project:

```bash
cabal build all
```

To run the CLI tool:

```bash
cabal run jl4-cli -- <file.l4>
```
