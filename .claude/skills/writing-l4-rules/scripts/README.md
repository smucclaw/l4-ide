# L4 Skill Scripts

Support scripts for the L4 skill.

## validate.sh

A wrapper around `jl4-cli` that type-checks an L4 file. It prefers a `jl4-cli`
binary on `PATH`, falling back to `cabal run jl4-cli` when run from inside a
local checkout of the `l4-ide` repository.

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

- `jl4-cli` available on `PATH`, or
- A checkout of [`legalese/l4-ide`](https://github.com/legalese/l4-ide) with `cabal` available (the script will invoke `cabal run jl4-cli -- ...`)

### Common errors

- **Type mismatches** — passing wrong types to a function
- **Undefined functions or types** — missing `IMPORT` or definition order
- **Pattern match not exhaustive** — missing `OTHERWISE` or enum case
- **Layout errors** — L4 is layout-sensitive like Python/Haskell
