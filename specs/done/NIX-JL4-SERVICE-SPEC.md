# Specification: NixOS Module for jl4-service + Thailand Cosmetics Deployment

**Status:** Implemented (pending deploy)
**Date:** 2026-03-13
**Branch:** `mengwong/nix-service`
**Target:** `dev.jl4.legalese.com`

## Summary

Bring up `jl4-service` (the multi-tenant successor to `jl4-decision-service`) as a NixOS-deployed service, pre-seeded with the Thailand cosmetics regulatory ruleset and the classic example functions.

## Background

- `jl4-decision-service` loads L4 files at startup via `--sourcePaths` and has some hardcoded examples baked into `Examples.hs`. It is deprecated.
- `jl4-service` is the replacement: multi-tenant, persistent (filesystem + CBOR cache), bundles uploaded via REST API or pre-seeded on disk. It had no Nix module.
- The Thailand cosmetics project (`legalese/thailand-cosmetics`) encodes Thailand's Cosmetics Act (BE 2558) and Manual on Cosmetic Advertising (BE 2567) in L4.

## What Was Built

### 1. NixOS Module for jl4-service

**Files:**

- `nix/jl4-service/package.nix` — Haskell build via `callCabal2nix`
- `nix/jl4-service/configuration.nix` — Systemd service, nginx proxy, pre-seed mechanism

**Key design decisions:**

- Runs on port 8003, proxied at `/service/` via nginx
- `DynamicUser = true` with `StateDirectory = "jl4-service"` for sandboxed persistence at `/var/lib/jl4-service/store`
- `ProtectSystem = "strict"` (not `"full"` like decision-service) because jl4-service needs a writable store
- **Pre-seed mechanism:** `ExecStartPre` script copies bundle directories from the Nix store into the persistent store on first boot. Creates a minimal `metadata.json` so jl4-service discovers and compiles them on startup. Subsequent restarts use CBOR cache.
- `bundles` option: `lib.types.attrsOf lib.types.path` — an attrset mapping deployment IDs to directories containing `.l4` files

### 2. Thailand Cosmetics Ruleset (`jl4/experiments/thailand-cosmetics/`)

**Files:**

- `article41-tribrid.l4` — Three-tier compliance evaluation (731 lines)
- `prelude.l4` — Symlink to `jl4-core/libraries/prelude.l4` for import resolution

**Exported functions:**

- `evaluate sub-claim` (default) — Evaluates a single advertising claim through all 3 tiers
- `evaluate full statement` — Evaluates a multi-claim statement with aggregated results

**Architecture:**

- **Tier 1:** Category-specific rules for 26 product categories (whitening, acne, sunscreen, etc.)
- **Tier 2:** Manual-specific cross-cutting requirements (Sections 4-9: substantiation, comparative advertising, awards, statistics, monarchy, giveaways)
- **Tier 3:** Core Article 41 prohibitions (7 types: false/exaggerated, misleading scientific, medicinal, sexual, immoral, divisive, ministerial) with puffery exemption

Input is a `Claim Assessment Input` record with ~70 `MAYBE BOOLEAN` predicates. Output includes per-tier violation details and an overall compliance verdict.

### 3. Classic Examples Bundle (`jl4/experiments/classic/`)

Extracted the hardcoded examples from `jl4-decision-service/src/Examples.hs` into proper `.l4` files with `@export` annotations:

- `compute_qualifies.l4` — walks AND (drinks OR eats)
- `vermin_and_rodent.l4` — Insurance exclusion logic (rodents/insects/vermin/birds)
- `the_answer.l4` — Constant function returning 42
- `britishcitizen5.l4` — Symlink to existing experiment (British citizenship)
- `parking.l4` — Symlink to existing experiment (parking rules)

### 4. Legacy Decision Service Updates

Also added `thailand-cosmetics` to `jl4-decision-service` sourcePaths across:

- `nix/jl4-decision-service/configuration.nix`
- `Dockerfile.jl4-decision-service`
- `dev-start.sh`
- `docker-compose.dev.yml`

## How Import Resolution Works

The decision service / jl4-service resolves `IMPORT prelude` by looking for `prelude.l4` in the same directory as the importing file. For Nix deployments, the directory (including dereferenced symlinks) is copied into the Nix store, so the symlink to prelude gets resolved into a real file copy. Validated with `jl4-cli` — typechecks clean.

## How @export Works

- `@export [default] <description>` annotation goes before `GIVEN` (or before `DECIDE` if no GIVEN)
- Both `DECIDE name ...` and `name params MEANS ...` forms produce the same `MkDecide` AST node, so `@export` works with either
- `default` marks the primary exported function
- The decision service discovers exports via `L4.Export.getExportedFunctions`

## Deployment Steps

```bash
cd ~/src/legalese/l4-ide/nix-service
nixos-rebuild switch --flake '.#jl4-dev' --target-host root@dev.jl4.legalese.com
```

## Verification

```bash
# Health check
curl https://dev.jl4.legalese.com/service/health

# List deployments (should show classic + thailand-cosmetics)
curl https://dev.jl4.legalese.com/service/deployments

# List functions in classic bundle
curl https://dev.jl4.legalese.com/service/deployments/classic/functions

# List functions in thailand-cosmetics bundle
curl https://dev.jl4.legalese.com/service/deployments/thailand-cosmetics/functions

# Evaluate compute_qualifies
curl -X POST https://dev.jl4.legalese.com/service/deployments/classic/functions/compute_qualifies/evaluation \
  -H "Content-Type: application/json" \
  -d '{"arguments":{"walks":true,"eats":true,"drinks":false}}'

# Evaluate thailand cosmetics sub-claim
curl -X POST 'https://dev.jl4.legalese.com/service/deployments/thailand-cosmetics/functions/evaluate%20sub-claim/evaluation' \
  -H "Content-Type: application/json" \
  -d '{"arguments":{"input":{...},"statement category":null}}'
```

## Next Steps

- [ ] Deploy to dev.jl4.legalese.com and verify all endpoints
- [ ] Test the query-plan endpoint for interactive elicitation of the thailand-cosmetics predicates
- [ ] Wire up the l4-wizard frontend to the jl4-service endpoints
- [ ] Consider deploying to prod (jl4.legalese.com) after dev validation
- [ ] Add more bundles (other pilot rulesets) as needed
- [ ] Create a PR to merge into main once validated

## Commits

1. `775b16a5` — Add jl4-service NixOS module and Thailand cosmetics ruleset
2. `2d6fddeb` — Add classic examples bundle (britishcitizen5, parking) to jl4-service
3. `124e4e65` — Extract hardcoded examples to classic bundle as .l4 files
