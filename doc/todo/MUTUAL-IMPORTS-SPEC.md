# Mutual Imports Specification

**Status:** Draft  
**Author:** AI Assistant  
**Date:** 2025-01-09  
**Issue:** N/A (new feature)

## Problem Statement

Currently, L4 rejects circular imports with the error:

```
One of your dependencies depends on this module and forms a cycle:
```

Users want to organize code across multiple files where:

- `foo.l4` defines types and references functions from `bar.l4`
- `bar.l4` defines functions and references types from `foo.l4`

This is a common pattern in real-world legal specifications where:

- **Types** are defined centrally but reference predicates
- **Predicates** operate on types but are organized by domain

## Design Goals

1. **Support mutual recursion** across module boundaries
2. **Maintain incremental compilation** via Shake
3. **Preserve error locality** - errors reported in the file where they occur
4. **No new syntax required** - existing `IMPORT` works unchanged
5. **Backwards compatible** - existing non-cyclic code works identically

## Proposed Solution: Two-Phase SCC Resolution

### Overview

Instead of rejecting cycles, we:

1. Detect **Strongly Connected Components (SCCs)** in the import graph
2. For each SCC, run a **two-phase** type check:
   - **Phase 1:** Collect signatures from all modules in the SCC
   - **Phase 2:** Type-check bodies with all signatures in scope

This mirrors how L4 already handles mutual recursion _within_ a single file (see `withScanTypeAndSigEnvironment` in `TypeCheck.hs:1795-1807`).

### Phase Details

#### Phase 1: Signature Collection (`GetModuleSignatures`)

For each module, collect:

- **DECLARE types:** Type names, constructors, field types
- **ASSUME types:** Same as DECLARE
- **Function signatures:** From `DECIDE x IS A Type` or `ASSUME x IS A Type`

This uses the existing `scanTyDeclModule` and `scanFunSigModule` passes.

**Output:** `ModuleSignature` containing:

```haskell
data ModuleSignature = MkModuleSignature
  { declTypeSigs :: [DeclTypeSig]      -- DECLARE/ASSUME type signatures
  , funTypeSigs  :: [FunTypeSig]       -- Function signatures
  , moduleUri    :: NormalizedUri
  }
```

#### Phase 2: Body Type-Checking (`TypeCheckSCC`)

With all signatures from the SCC in scope, type-check each module's bodies:

- Function implementations (DECIDE bodies)
- Type synonym expansions
- Expression type inference

### Shake Rule Changes

#### Current Rules

```
GetLexTokens → GetParsedAst → GetImports → GetTypeCheckDependencies → TypeCheck
```

#### Proposed Rules

```
GetLexTokens → GetParsedAst → GetImports
                                   ↓
                           ComputeImportSCC  (new)
                                   ↓
                    ┌──────────────┼──────────────┐
                    ↓              ↓              ↓
            GetModuleSignatures (per module in SCC)  (new)
                    ↓              ↓              ↓
                    └──────────────┼──────────────┘
                                   ↓
                           TypeCheckSCC  (new)
                                   ↓
                              TypeCheck  (modified - delegates to TypeCheckSCC)
```

### New Shake Rules

#### 1. `ComputeImportSCC`

**Input:** `NormalizedUri`  
**Output:** `ImportSCC` - the SCC containing this module

```haskell
data ImportSCC = MkImportSCC
  { sccModules :: Set NormalizedUri  -- All modules in this SCC
  , sccId      :: Int                 -- Unique ID for caching
  }

data GetImportSCC = GetImportSCC
  deriving (Eq, Show, Generic, Hashable, NFData)

type instance RuleResult GetImportSCC = ImportSCC
```

**Implementation:**

1. Build import graph by traversing `GetImports` transitively
2. Run Tarjan's SCC algorithm
3. Return the SCC containing the requested module

**Caching:** SCCs are computed once and cached. When any module's imports change, invalidate affected SCCs.

#### 2. `GetModuleSignatures`

**Input:** `NormalizedUri`  
**Output:** `ModuleSignature`

```haskell
data GetModuleSignatures = GetModuleSignatures
  deriving (Eq, Show, Generic, Hashable, NFData)

type instance RuleResult GetModuleSignatures = ModuleSignature
```

**Implementation:**

1. Parse module (`GetParsedAst`)
2. Run signature collection passes:
   - `scanTyDeclModule` for DECLARE/ASSUME types
   - `scanFunSigModule` for function signatures
3. Return collected signatures

**Key property:** This rule does NOT depend on other modules' signatures. It only reads the current module's AST.

#### 3. `TypeCheckSCC`

**Input:** `Set NormalizedUri` (the SCC)  
**Output:** `Map NormalizedUri TypeCheckResult`

```haskell
data TypeCheckSCC = TypeCheckSCC (Set NormalizedUri)
  deriving (Eq, Show, Generic, Hashable, NFData)

type instance RuleResult TypeCheckSCC = Map NormalizedUri TypeCheckResult
```

**Implementation:**

1. Collect signatures from all modules: `uses GetModuleSignatures sccModules`
2. Merge into combined environment
3. For each module in SCC:
   - Type-check bodies with combined signatures in scope
   - Produce `TypeCheckResult`
4. Return map of results

### Modified Existing Rules

#### `TypeCheck` (modified)

```haskell
defineWithCallStack shakeRecorder $ \TypeCheckNoCallstack cs uri -> do
  scc <- use_ GetImportSCC uri

  if Set.size scc.sccModules == 1
    then do
      -- Non-cyclic case: use existing logic (fast path)
      existingTypeCheckLogic uri cs
    else do
      -- Cyclic case: delegate to SCC type-checking
      sccResults <- use_ (TypeCheckSCC scc.sccModules) uri
      pure $ Map.lookup uri sccResults
```

### Algorithm: SCC Computation

Using Tarjan's algorithm (O(V+E)):

```haskell
computeSCCs :: Map NormalizedUri [NormalizedUri] -> [Set NormalizedUri]
computeSCCs importGraph = tarjanSCC (Map.keys importGraph) adjacency
  where
    adjacency uri = Map.findWithDefault [] uri importGraph
```

The import graph is built by:

1. Starting from a root module
2. Traversing `GetImports` for each discovered module
3. Building adjacency map

### Error Handling

#### Errors in Signature Collection (Phase 1)

If a module has a syntax error or invalid type signature:

- Report error on that module
- Mark SCC as "partially valid"
- Other modules in SCC can still type-check (with missing imports)

#### Errors in Body Type-Checking (Phase 2)

Standard error reporting - errors attributed to the module where they occur.

#### Conflicting Definitions

If `foo.l4` and `bar.l4` both define `DECLARE Person`:

- Report "duplicate definition" error
- First definition wins (for continued checking)
- Error location: second definition site

### Implementation Plan

#### Phase 1: Foundation (Week 1-2)

1. **Add `ModuleSignature` type** to `L4/TypeCheck/Types.hs`
2. **Extract signature collection** from `doCheckProgramWithDependencies`:
   - Factor out `collectModuleSignatures :: Module Name -> Check ModuleSignature`
   - Reuse existing `scanTyDeclModule`, `scanFunSigModule`
3. **Add `GetModuleSignatures` rule** to `Rules.hs`

#### Phase 2: SCC Detection (Week 2-3)

4. **Implement Tarjan's SCC** algorithm (or use existing library)
5. **Add `ComputeImportSCC` rule**:
   - Build import graph
   - Compute SCCs
   - Cache results
6. **Add tests** for SCC detection

#### Phase 3: SCC Type-Checking (Week 3-4)

7. **Add `TypeCheckSCC` rule**:
   - Merge signatures from all modules
   - Type-check each module with combined env
8. **Modify `TypeCheck`** to delegate to `TypeCheckSCC` for cycles
9. **Handle error propagation** across SCC

#### Phase 4: Testing & Polish (Week 4-5)

10. **Add integration tests**:
    - Simple A↔B cycle
    - Three-way A→B→C→A cycle
    - Mixed cyclic/non-cyclic imports
    - Error cases
11. **Performance testing** with large SCCs
12. **Documentation** and examples

### Test Cases

#### Basic Mutual Recursion

**foo.l4:**

```l4
IMPORT bar

DECLARE Person HAS
    name IS A String
    friend IS A Person  -- forward reference within file works
    pet IS A Pet        -- cross-module reference

`has pet named` p n MEANS
    p's pet's name == n
```

**bar.l4:**

```l4
IMPORT foo

DECLARE Pet HAS
    name IS A String
    owner IS A Person   -- cross-module reference back to foo

`is owned by` pet person MEANS
    pet's owner == person
```

#### Three-Way Cycle

**a.l4:** `IMPORT b` → uses type from C  
**b.l4:** `IMPORT c` → uses type from A  
**c.l4:** `IMPORT a` → uses type from B

#### Error Localization

**foo.l4:**

```l4
IMPORT bar
DECIDE broken x IS A Number EQUALS undefined_from_bar x  -- ERROR here
```

**bar.l4:**

```l4
IMPORT foo
-- No definition of undefined_from_bar
```

Expected: Error reported in `foo.l4` at the call site.

### Performance Considerations

1. **SCC computation** is O(V+E) - negligible for typical project sizes
2. **Signature collection** is fast (no inference, just scanning)
3. **Body type-checking** parallelizable within SCC if no data dependencies
4. **Caching:** SCCs cached; invalidated only when imports change

### Alternatives Considered

#### 1. Forward Declarations (like Haskell `.hs-boot`)

**Pros:** Explicit, no magic  
**Cons:** Burden on users, extra files, L4 targets non-programmers

#### 2. Lazy/Demand-Driven Resolution

**Pros:** Simple to implement  
**Cons:** Hard to report good errors, non-deterministic behavior

#### 3. Merge Cyclic Modules Into One

**Pros:** Reuses existing single-module logic  
**Cons:** Loses file-level error isolation, confusing diagnostics

### Open Questions

1. **Qualified names:** How do cross-module references work with section-qualified names?
2. **Re-exports:** Should `foo.l4` re-export types from `bar.l4`?
3. **Selective imports:** Future `IMPORT bar (Pet)` syntax - how does it interact with cycles?

### References

- GHC's handling of `.hs-boot` files: [GHC Wiki](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/hs-boot-file-handling)
- OCaml's `module rec`: [OCaml Manual](https://v2.ocaml.org/manual/moduleexamples.html#s%3Afirst-class-modules)
- Tarjan's SCC algorithm: [Wikipedia](https://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm)
- Current L4 type-checker architecture: `jl4-core/src/L4/TypeCheck.hs`
- Current Shake rules: `jl4-lsp/src/LSP/L4/Rules.hs`
