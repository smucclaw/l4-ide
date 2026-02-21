# jl4-service — Serialisation Upgrade (Option A → B)

**Status:** Proposal (implement after `JL4-SERVICE-SPEC.md` is shipped)
**Depends on:** `JL4-SERVICE-SPEC.md`

---

## Context

`jl4-service` (Plan 1) uses Option A: store source text in the bundle store, re-typecheck on
startup. This is safe and simple but pays parse+typecheck cost on every restart per deployment.

This plan upgrades the bundle store to cache the typechecked AST as CBOR (`codec-serialise`),
making restarts proportional to deserialization speed (milliseconds) instead of typecheck time.
It falls back to source recompilation if CBOR is absent (backward compatibility, first deploy).

The primary risk — `Serialise` instances for `Module Resolved` — is addressed by:

1. One orphan instance for `NormalizedUri` / `Uri` (the only external blocker)
2. An annotation-stripping `Serialise` instance for `Anno_` that discards all token/position data
   (not needed for evaluation; dramatically reduces bundle size)
3. `deriving anyclass (Serialise)` on all AST types (possible because all have `GHC.Generic`)
4. Post-deserialization supply reseeding (simple fold to find max `Unique` int)

---

## Step 1 — Add `codec-serialise` dependency

**`jl4-core/jl4-core.cabal`**: add behind a cabal flag so WASM builds aren't affected:

```cabal
flag serialise-support
  description: Enable Serialise instances for AST types (for jl4-service)
  default:     True
  manual:      True

if flag(serialise-support)
  build-depends: codec-serialise >= 0.2
  cpp-options:   -DSERIALISE_ENABLED
```

**`jl4-service/jl4-service.cabal`**: add `codec-serialise` directly (always enabled).

---

## Step 2 — Orphan instances for external types

**New file: `jl4-core/src/L4/Instances/Serialise.hs`**

```haskell
{-# OPTIONS_GHC -Wno-orphans #-}
module L4.Instances.Serialise where

import Codec.Serialise
import Language.LSP.Protocol.Types (NormalizedUri, Uri(..), fromNormalizedUri, toNormalizedUri)

instance Serialise Uri where
  encode (Uri t) = encode t
  decode         = Uri <$> decode

instance Serialise NormalizedUri where
  encode = encode . fromNormalizedUri
  decode = toNormalizedUri <$> decode
```

This is the only external type blocker. `Uri` is `newtype Uri = Uri { getUri :: Text }` and
`NormalizedUri` is `newtype NormalizedUri = NormalizedUri { fromNormalizedUri :: Uri }`.

---

## Step 3 — Annotation stripping via `Serialise Anno_`

**Add to `jl4-core/src/L4/Instances/Serialise.hs`**:

```haskell
import L4.Annotation (Anno_(..), emptyAnno)

-- Annotations carry PosTokens (source positions) needed for IDE features but
-- not for evaluation. Strip them during serialization to keep bundles small.
instance Serialise (Anno_ t e) where
  encode _ = encode ()
  decode   = pure emptyAnno
```

`emptyAnno` should be defined (or confirmed to exist) in `L4.Annotation` as an `Anno_` with
empty token lists and no range — used elsewhere for synthetic nodes. Confirm at implementation
time; add if absent.

---

## Step 4 — Derive `Serialise` for AST types

**Files to modify** (add `deriving anyclass (Serialise)` where `GHC.Generic` already exists):

| File                                 | Types                                                                                                                                                                                  |
| ------------------------------------ | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `jl4-core/src/L4/Syntax.hs`          | `Unique`, `Resolved`, `Name`, `RawName`, `Module`, `Decide`, `Expr`, `Type'`, `TypeSig`, `AppForm`, `Import`, `Entity`, `Section`, `GivenSig`, `GivethSig`, `Extension`, `Nlg`, `Desc` |
| `jl4-core/src/L4/TypeCheck/Types.hs` | `CheckEntity`, `Environment` (alias — no deriving needed, it's `Map`), `EntityInfo` (alias), `Substitution` (alias)                                                                    |
| `jl4-core/src/L4/Lexer.hs`           | `TokenType` and all token variants — only needed if not stripping Anno; with stripping these are NOT needed                                                                            |

With the annotation-stripping instance from Step 3, `PosToken`, `SrcRange`, `TokenType`, and
all lexer types are **not** required — they are discarded by the `Anno_` instance. This
significantly reduces the scope of Step 4.

Gate all `Serialise` derives behind `#if defined(SERIALISE_ENABLED)` CPP guards to keep WASM
and other builds clean.

---

## Step 5 — Supply reseeding after deserialization

**New function in `jl4-service/src/Compiler.hs`**:

```haskell
import Data.Foldable (toList)
-- Requires a fold/traversal over all Unique values in Module Resolved.
-- Use the existing optics/generic-traversal infrastructure.

reseedUniqueSupply :: IORef Int -> Module Resolved -> IO ()
reseedUniqueSupply supplyRef m = do
  let allUniques = [u.unique | u <- collectAllUniques m]
      maxU       = foldl' max 0 allUniques
  writeIORef supplyRef (maxU + 1)

-- collectAllUniques: fold over Module Resolved extracting every Unique.
-- Implement using Data.Generics (uniplate/syb) or manual recursion.
-- The evaluator's supply IORef is accessible via EvalState created in Backend.Jl4.
```

---

## Step 6 — Bundle store upgrade

**Extend `jl4-service/src/BundleStore.hs`**:

```haskell
-- New store layout (alongside existing sources/):
-- {store-path}/{deploymentId}/bundle.cbor   ← serialized compiled state

-- What is serialized:
data SerializedBundle = SerializedBundle
  { sbModule      :: Module Resolved
  , sbEnvironment :: Environment    -- Map RawName [Unique]
  , sbEntityInfo  :: EntityInfo     -- Map Unique (Name, CheckEntity)
  }
  deriving (Generic)
  deriving anyclass (Serialise)

saveBundleCbor
  :: BundleStore -> DeploymentId -> SerializedBundle -> IO ()

loadBundleCbor
  :: BundleStore -> DeploymentId -> IO (Maybe SerializedBundle)
  -- returns Nothing if bundle.cbor absent or fails to parse (safe fallback)
```

---

## Step 7 — Load path upgrade in `Compiler.hs`

```haskell
loadDeployment
  :: BundleStore -> DeploymentId
  -> IO (Either Text (Map Text ValidatedFunction, DeploymentMetadata))
loadDeployment store did = do
  mCbor <- loadBundleCbor store did
  case mCbor of
    Just sb -> do
      -- Fast path: deserialize + reseed supply
      fns <- buildFromResolvedModule sb.sbModule sb.sbEnvironment sb.sbEntityInfo
      meta <- loadMetadata store did
      pure (Right (fns, meta))
    Nothing -> do
      -- Slow path: recompile from source, then cache CBOR
      (sources, _, meta) <- loadBundle store did
      compileBundle sources >>= \case
        Right (fns, _) -> do
          let sb = buildSerializedBundle ...   -- extract from compile result
          saveBundleCbor store did sb          -- cache for next restart
          pure (Right (fns, meta))
        Left err -> pure (Left err)
```

---

## Verification

```bash
# Build with serialise flag
cabal build jl4-service jl4-core

# Deploy a bundle (compiles from source, saves bundle.cbor)
curl -X POST http://localhost:8080/deployments -F "sources=@bundle.zip" -F "id=test"

# Confirm bundle.cbor was written
ls /tmp/jl4-store/test/
# → sources/  metadata.json  bundle.cbor

# Restart — should load from CBOR (fast)
pkill jl4-service && cabal run jl4-service -- --store-path /tmp/jl4-store
# Time the startup: should be faster than first boot

# Verify evaluation still works
curl -X POST http://localhost:8080/deployments/test/functions/is-eligible/evaluation \
  -d '{"fnArguments": {"income": 60000}}'

# Delete CBOR to force source fallback
rm /tmp/jl4-store/test/bundle.cbor
pkill jl4-service && cabal run jl4-service -- --store-path /tmp/jl4-store
# Should recompile from source and regenerate bundle.cbor

# Run jl4-core tests to confirm no regressions from Generic/Serialise additions
cabal test jl4-core-test
cabal test jl4-decision-service-test
```

---

## Implementation order

1. Step 1 (cabal flag) → Step 2 (orphan instances) → Step 3 (Anno stripping)
   — can be done and tested independently of the rest
2. Step 4 (AST derives) — mechanical, do one file at a time, run `cabal build` after each
3. Step 5 (supply reseed) — after Step 4, write and test in isolation
4. Step 6 (store upgrade) — after Steps 3–5
5. Step 7 (load path) — final wiring, integration test

**Estimated scope**: Steps 1–3 are ~50 lines. Step 4 is mechanical but touches ~10 files.
Steps 5–7 are ~150 lines of new code. Total: a contained, incrementally testable body of work.
