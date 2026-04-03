# Specification: Path-Aware and Version-Aware Module Imports

## Executive Summary

L4's current module import system (`IMPORT \`module-name\``) resolves imports by searching a flat list of filesystem paths. This works for simple cases but breaks down when:

- Deployment bundles (jl4-service) contain files in subdirectories
- Projects need to import from external libraries not on the standard search path
- Multiple versions of the same module exist (e.g., `ifema-definitions` v1 vs v2)

This spec proposes extending the import system with path-aware resolution (relative and absolute paths, package identifiers) and version-aware resolution (git commits, tags, releases), while maintaining backward compatibility with the current bare-name style.

## Motivation: IFEMA Cross-Default API

The immediate motivation comes from the `actus2026` project, which formalizes IFEMA 1997 in L4 and deploys `@export` functions as REST endpoints via jl4-service.

### Problem 1: Deployment bundle path resolution

The project has this structure:

```
actus2026/
  ifema/
    ifema-definitions.l4
    ifema-section5-closeout.l4
    find-cross-defaults.l4      -- IMPORT `ifema-definitions`
    close-out-netting.l4        -- IMPORT `ifema-definitions`
                                -- IMPORT `ifema-section5-closeout`
```

Locally, `jl4-cli ifema/close-out-netting.l4` works because the CLI searches the file's directory (`ifema/`) and finds `ifema-definitions.l4` there.

When deployed to jl4-service as a ZIP bundle, the same resolution should work — but in practice, the server's VFS and filesystem search paths diverge from the local CLI's behavior. The import resolution log shows the server checking `file://ifema/ifema-definitions.l4` in the VFS but missing, then checking `ifema/ifema-definitions.l4` on the filesystem but (sometimes) not finding it.

### Problem 2: Standard library availability

`close-out-netting.l4` wanted to use `nub` and `map` from `prelude.l4`. Locally this works because `prelude.l4` ships with jl4-cli at `~/.cabal/store/.../libraries/prelude.l4`. On the jl4-service NixOS deployment, the prelude is at a different Nix store path that the import resolver doesn't check. We had to inline the functions instead.

### Problem 3: No versioning

When a project imports `ifema-definitions`, there's no way to specify which version. If the IFEMA encoding evolves (adding fields, renaming types), downstream projects have no way to pin to a known-good version. Currently, the only "versioning" is to bundle a copy of the dependency in the deployment ZIP — which defeats the purpose of shared libraries.

## Current Resolution Algorithm

When `IMPORT \`module-name\``appears in a file at path`dir/file.l4`, jl4-cli searches:

1. VFS (Virtual File System, used by VS Code LSP and web IDE):
   - `project:/module-name.l4`
   - `file://dir/module-name.l4`
2. Filesystem:
   - `dir/module-name.l4` (same directory as importing file)
   - Global library paths:
     - `~/.local/share/jl4/libraries/module-name.l4`
     - `<cabal-store>/.../libraries/module-name.l4` (Cabal)
     - `<nix-store>/.../libraries/module-name.l4` (Nix)

This is a flat search with no support for:

- Relative paths (`../common/types.l4`)
- Package namespaces (`ifema/definitions` vs `actus/definitions`)
- Version constraints (`ifema-definitions@v2.1`)

## Proposed Design

### Phase 1: Path-aware imports

Allow relative and absolute paths in import names:

```l4
-- Current (still supported): bare module name, flat search
IMPORT `ifema-definitions`

-- New: relative path from importing file
IMPORT `../common/types`

-- New: package-qualified (resolves against a package registry or project config)
IMPORT `ifema/definitions`
```

Resolution order for `IMPORT \`ifema/definitions\``:

1. Check VFS (for IDE contexts)
2. Check relative to importing file: `dir/ifema/definitions.l4`
3. Check project root (if known): `<project>/ifema/definitions.l4`
4. Check global library paths
5. Check registered packages (see Phase 2)

### Phase 2: Version-aware imports

Introduce a project manifest file (e.g., `l4-project.toml` or `l4.lock`) that maps package names to versioned sources:

```toml
[dependencies]
ifema-definitions = { git = "github.com/ACTUS-FIBO/actus2026", tag = "v1.0", path = "ifema/ifema-definitions.l4" }
prelude = { builtin = true }  # ships with jl4-cli/jl4-service
```

Version identifiers could be:

- Git tags: `v1.0`, `v2.0-rc1`
- Git commits: `18ca361`
- Semantic versions resolved against a registry

### Phase 3: Package registry

A centralized or federated registry of L4 packages, analogous to npm/crates.io/Hackage. Projects publish versioned bundles; consumers declare dependencies in their manifest.

## Compatibility Requirements

The solution must work across all L4 execution contexts:

| Context                       | Path resolution   | VFS     | Notes                                 |
| ----------------------------- | ----------------- | ------- | ------------------------------------- |
| **jl4-cli** (local)           | Filesystem        | No      | Developer workflow, `jl4-cli file.l4` |
| **VS Code extension**         | Filesystem + VFS  | Yes     | LSP server, project workspace         |
| **Web IDE**                   | VFS only          | Yes     | No local filesystem access            |
| **jl4-service** (deployments) | Deployment bundle | Partial | ZIP uploaded via REST API             |

Key constraints:

- **VS Code**: The LSP must resolve imports for diagnostics and go-to-definition. The VFS must be kept in sync with the manifest.
- **Web IDE**: All files must be in the VFS. Dependencies either need to be pre-loaded or fetched on demand.
- **jl4-service**: Deployment bundles must be self-contained OR the service must have access to a package registry. Currently, bundles are self-contained ZIPs. The service should guarantee that standard libraries (prelude, etc.) are available without bundling.

## Open Questions

1. **Should the standard library (prelude, daydate, etc.) be guaranteed available in all contexts?** Currently it's available in jl4-cli but not reliably in jl4-service.

2. **How should jl4-service handle deployment bundles with subdirectories?** The current behavior is inconsistent — sometimes `ifema/ifema-definitions.l4` resolves from a ZIP with `ifema/` structure, sometimes it doesn't. The resolution should be deterministic.

3. **Should imports support glob patterns?** E.g., `IMPORT \`ifema/\*\``to import all modules in a directory (like`ifema-main.l4` does manually).

4. **How does versioning interact with the `@export` function schema?** If a type changes between versions, the auto-generated REST schema changes. Should the service enforce schema stability?

5. **Should there be a `l4 init` / `l4 add` CLI workflow** for managing project manifests, similar to `cargo init` / `cargo add`?

## References

- Haskell: Cabal package system, Hackage registry
- Rust: Cargo.toml, crates.io, path/git dependencies
- Node.js: package.json, npm registry, workspace protocol
- Python: pyproject.toml, pip, virtual environments
- ACTUS-FIBO actus2026 repo: the motivating use case
- jl4-service deployment API: `POST /service/deployments` with ZIP bundle
