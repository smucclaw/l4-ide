# L4 Skill Scripts

Support scripts for the L4 skill.

## validate.sh

A wrapper around the `l4` CLI that type-checks and evaluates an L4 file.
It prefers an `l4` binary on `PATH`, falling back to `cabal run l4` when
run from inside a local checkout of the `l4-ide` repository.

### Usage

```bash
./validate.sh path/to/file.l4

# Pin evaluation time for reproducible results
./validate.sh path/to/file.l4 --fixed-now=2025-01-01T00:00:00Z
```

Exit codes:

- `0` — validation successful
- non-zero — validation failed (see stderr for the actual error)

### Requirements

Either:

- The `l4` CLI available on `PATH` (install from the L4 VS Code
  extension's sidebar menu: **Install L4 CLI**), or
- A checkout of [`legalese/l4-ide`](https://github.com/legalese/l4-ide)
  with `cabal` available — the script will invoke `cabal run l4 -- run ...`.

### Common errors

- **Type mismatches** — passing wrong types to a function
- **Undefined functions or types** — missing `IMPORT` or definition order
- **Pattern match not exhaustive** — missing `OTHERWISE` or enum case
- **Layout errors** — L4 is layout-sensitive like Python/Haskell
