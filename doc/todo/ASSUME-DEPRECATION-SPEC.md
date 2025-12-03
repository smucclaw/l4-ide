# Specification: ASSUME Keyword Deprecation Warning

**Status:** ✅ Implemented
**Related:** `RUNTIME-INPUT-STATE-SPEC.md` (rationale), `TYPICALLY-DEFAULTS-SPEC.md` (alternatives)

## Executive Summary

Add a deprecation warning to the L4 compiler and LSP for uses of the `ASSUME` keyword. The warning should:

1. Appear in LSP as a yellow/orange squiggle (warning severity)
2. Appear in CLI output as a warning (not error)
3. Not prevent compilation
4. Suggest alternatives (GIVEN, DECLARE)

## Motivation

### The Problem with ASSUME

The `ASSUME` keyword was originally introduced as a placeholder for explicit type declarations—a visible delta between type inference and type checking. When the type checker inferred types for otherwise undefined expressions, the LSP would offer an automatic `ASSUME` reification.

However, the keyword is semantically misleading:

```l4
ASSUME x IS A BOOLEAN
```

To a non-programmer (especially legal professionals), this reads as "assume x is true" rather than "x has type Boolean". The word "assume" in natural language implies a presumption about _value_, not just _type_.

This confusion is particularly problematic for L4's target audience of legal professionals who may not have programming experience.

### Long-term Direction

We intend to deprecate explicit use of `ASSUME` in well-styled L4 programs. Users should instead use:

- **GIVEN** for function parameters (already standard practice)
- **DECLARE** for module-level type declarations
- Implicit type inference without visible ASSUME artifacts

## Implementation

### Phase 1: Add Warning Type

**File:** `jl4-core/src/L4/TypeCheck/Types.hs`

Extend `CheckWarning` with a new constructor:

```haskell
data CheckWarning
  = PatternMatchRedundant [Branch Resolved]
  | PatternMatchesMissing [BranchLhs Resolved]
  | AssumeDeprecated Name                        -- NEW
  deriving stock (Eq, Generic, Show)
  deriving anyclass NFData
```

### Phase 2: Emit Warning During Type Checking

**File:** `jl4-core/src/L4/TypeCheck.hs`

In `inferAssume`, emit the warning after successful type checking:

```haskell
inferAssume :: Assume Name -> Check (Assume Resolved, [CheckInfo])
inferAssume (MkAssume ann _tysig appForm (Just (Type _tann)) _typically) = do
  errorContext (WhileCheckingAssume (getName appForm)) do
    d <- lookupAssumeCheckedByAnno ann
    -- Emit deprecation warning
    addWarning $ AssumeDeprecated (getName appForm)
    pure (d.payload, d.publicNames)
inferAssume (MkAssume ann _tysig appForm mt _typically) = do
  errorContext (WhileCheckingAssume (getName appForm)) do
    dHead <- lookupFunTypeSigByAnno ann
    extendKnownMany dHead.arguments do
      rmt <- case mt of
        Nothing -> pure Nothing
        Just t  -> do
          rt' <- inferType t
          expect (ExpectAssumeSignatureContext (rangeOf dHead.resultType)) dHead.resultType rt'
          pure (Just rt')
      assume <-
        MkAssume dHead.anno
          <$> traverse resolvedType dHead.rtysig
          <*> traverse resolvedType dHead.rappForm
          <*> pure rmt
          <*> pure Nothing
          >>= nlgAssume
      -- Emit deprecation warning
      addWarning $ AssumeDeprecated (getName appForm)
      pure (assume, [dHead.name])
```

### Phase 3: Pretty Print Warning

**File:** `jl4-core/src/L4/TypeCheck.hs`

Add case to `prettyCheckWarning`:

```haskell
prettyCheckWarning :: CheckWarning -> [Text]
prettyCheckWarning = \case
  PatternMatchRedundant b ->
    [ "The following CONSIDER branch is redundant: "
    , ""
    ] <>
    map (("  " <>) . prettyLayout) b
    <> [ "" ]
  PatternMatchesMissing b ->
    [ "The following branches still need to be considered:"
    , "" ]
    <>
    map (("  " <>) . prettyLayout) b
    <> [ "" ]
  AssumeDeprecated name ->                                    -- NEW
    [ "ASSUME is deprecated and will be removed in a future version."
    , ""
    , "The declaration of " <> quotedName name <> " uses ASSUME."
    , ""
    , "Consider using one of these alternatives:"
    , "  - GIVEN: for function parameters"
    , "  - DECLARE: for type declarations"
    , ""
    , "See documentation for migration guidance."
    ]
```

### Phase 4: LSP Diagnostic Display

The existing LSP infrastructure already handles this correctly:

**File:** `jl4-lsp/src/LSP/L4/Rules.hs`

```haskell
translateSeverity :: TypeCheck.Severity -> DiagnosticSeverity
translateSeverity TypeCheck.SInfo  = LSP.DiagnosticSeverity_Warning  -- renders as yellow
translateSeverity TypeCheck.SWarn  = LSP.DiagnosticSeverity_Warning  -- renders as yellow
translateSeverity TypeCheck.SError = LSP.DiagnosticSeverity_Error    -- renders as red
```

Since `CheckWarning` maps to `SWarn` via the `severity` function, the LSP will automatically:

- Show yellow/orange squiggly underline
- Display warning icon in the problems panel
- Not prevent other operations (unlike errors)

### Phase 5: CLI Output

The CLI already respects severity levels. Warnings are printed but don't cause non-zero exit codes.

## Visual Appearance

### In VS Code (LSP)

```
┌─────────────────────────────────────────────────────────────┐
│  1 │ ASSUME x IS A BOOLEAN                                  │
│    │ ~~~~~~                                                 │
│    │ ⚠️ ASSUME is deprecated...                             │
└─────────────────────────────────────────────────────────────┘
```

The squiggle should be yellow/orange (warning color), not red (error color).

### In CLI

```
$ jl4-cli check example.l4
example.l4:5:1: warning:
  ASSUME is deprecated and will be removed in a future version.

  The declaration of `x` uses ASSUME.

  Consider using one of these alternatives:
    - GIVEN: for function parameters
    - DECLARE: for type declarations

  See documentation for migration guidance.

Checking succeeded with 1 warning.
```

## Test Cases

### Unit Test: Warning Emitted

```haskell
describe "ASSUME deprecation" $ do
  it "emits warning for ASSUME declaration" $ do
    let result = typeCheck [l4|
      ASSUME x IS A BOOLEAN
      |]
    result `shouldHaveWarning` AssumeDeprecated "x"
    result `shouldSucceed`  -- Still compiles

  it "emits warning for ASSUME with type annotation" $ do
    let result = typeCheck [l4|
      ASSUME foo IS A NUMBER
      |]
    result `shouldHaveWarning` AssumeDeprecated "foo"

  it "does not emit warning for GIVEN" $ do
    let result = typeCheck [l4|
      GIVEN x IS A BOOLEAN
      GIVETH A BOOLEAN
      DECIDE f IF x
      |]
    result `shouldNotHaveWarning` (AssumeDeprecated _)

  it "does not emit warning for DECLARE" $ do
    let result = typeCheck [l4|
      DECLARE Person HAS
        name IS A STRING
      |]
    result `shouldNotHaveWarning` (AssumeDeprecated _)
```

### Golden Test: CLI Output

```haskell
describe "CLI output" $ do
  it "shows deprecation warning" $
    goldenCLI "assume_deprecation" "examples/assume-warning.l4"
```

With `examples/assume-warning.l4`:

```l4
ASSUME age IS A NUMBER
ASSUME married IS A BOOLEAN

GIVEN x IS A NUMBER
GIVETH A NUMBER
DECIDE double x IS x * 2
```

Expected output should show 2 warnings (for `age` and `married`) but succeed.

### Integration Test: LSP Diagnostics

```haskell
describe "LSP diagnostics" $ do
  it "reports ASSUME as warning severity" $ do
    diagnostics <- getLspDiagnostics "ASSUME x IS A BOOLEAN"
    length diagnostics `shouldBe` 1
    head diagnostics ^. severity `shouldBe` Just DiagnosticSeverity_Warning
    head diagnostics ^. message `shouldContain` "deprecated"
```

## Configuration (Future)

For teams with large existing codebases, we may want to add configuration:

```yaml
# .l4config.yaml (future)
warnings:
  assume-deprecated: error # Treat as error (strict mode)
  # assume-deprecated: warn   # Default behavior
  # assume-deprecated: ignore # Suppress warning
```

This is out of scope for the initial implementation.

## Migration Guide

When users see this warning, they should:

### For External Inputs (Most Common)

**Before:**

```l4
ASSUME age IS A NUMBER
ASSUME married IS A BOOLEAN

DECIDE canVote IF age >= 18
```

**After:**

```l4
GIVEN age IS A NUMBER
GIVEN married IS A BOOLEAN
GIVETH A BOOLEAN
DECIDE canVote IF age >= 18
```

### For Module-Level Constants

**Before:**

```l4
ASSUME PI IS A NUMBER
-- PI is never given a value, just typed
```

**After:**

```l4
DECLARE Constants HAS
  PI IS A NUMBER

-- Or define it:
DECIDE PI IS 3.14159
```

### For Type Declarations

**Before:**

```l4
ASSUME Person IS A TYPE
```

**After:**

```l4
DECLARE Person HAS
  name IS A STRING
  age IS A NUMBER
```

## Edge Cases

### 1. ASSUME in Imported Files

If an imported file uses ASSUME, the warning should appear for that file, not the importing file.

### 2. Multiple ASSUME Declarations

Each ASSUME should generate its own warning with the specific name.

### 3. LSP Auto-Fix (Future Enhancement)

In a future iteration, we could offer a quick-fix action:

- "Convert to GIVEN parameter"
- "Convert to DECLARE field"

This is out of scope for the initial implementation.

## Rollout Plan

1. **Phase 1:** Add warning (this spec) - gives users time to migrate
2. **Phase 2:** Add LSP quick-fix actions for easy migration
3. **Phase 3:** Change default severity to error (can be configured back to warning)
4. **Phase 4:** Remove ASSUME from the language (breaking change, major version)

Each phase should be separated by at least one release cycle to give users migration time.

## References

- `RUNTIME-INPUT-STATE-SPEC.md`: Original discussion of ASSUME semantics
- `TYPICALLY-DEFAULTS-SPEC.md`: Note on ASSUME + TYPICALLY being discouraged
- LSP Diagnostic Severity: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#diagnosticSeverity
