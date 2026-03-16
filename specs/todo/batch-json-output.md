# Specification: Clean JSON Output for jl4-cli Batch Mode

## Executive Summary

The `jl4-cli --batch` mode currently serializes evaluation results using Haskell's `Show` typeclass, producing strings like `MkEvalDirectiveResult { result = Reduction (Right (MkNF (ValString "hello"))) }` in the `"output"` field of the JSON response. This forces downstream consumers to regex-parse internal Haskell representations to extract actual values. This spec proposes adding proper `ToJSON` instances for L4 value types so that batch mode returns clean, idiomatic JSON.

## Current Implementation Status

**Related spec**: `BATCH-PROCESSING-SPEC.md` (batch input processing)
**Downstream consumer**: ACTUS-FIBO Python integration (`actus2026` repo)

### What Already Exists

| Component | Status | Notes |
|-----------|--------|-------|
| `jl4-cli --batch` mode | Exists | Accepts JSON input, evaluates `@export` functions |
| Batch JSON response envelope | Exists | `status`, `output`, `input`, `diagnostics` fields |
| `EvalDirectiveResult` type | Exists | Wraps evaluation results |
| `NF` / `Value` types | Exists | Internal value representations in the evaluator |
| `Show` instances for above | Exists | Used currently for serialization (the problem) |
| `ToJSON` instances for above | Missing | **This is what needs to be built** |

---

## Problem

### Current output format

When `jl4-cli --batch` evaluates an `@export` function, the response `"output"` field contains the Haskell `Show` representation of internal types:

```json
{
  "status": "success",
  "output": ["MkEvalDirectiveResult { result = Reduction (Right (MkNF (ValString \"hello\"))) }"],
  "input": { "name": "Alice" },
  "diagnostics": []
}
```

### Why this is a problem

1. **Fragile parsing**: Consumers must regex-parse Haskell constructors. The current Python workaround looks like:
   ```python
   matches = re.findall(r'ValString "([^"]+)"', output_str)
   ```
   This breaks for strings containing escaped quotes, nested structures, or any change to the internal type representation.

2. **Type information is lost**: There is no way to distinguish a number from a string without pattern-matching on `ValNumber` vs `ValString` textually.

3. **Nested structures are unparseable**: Records, lists, and `Maybe` values produce deeply nested `Show` output that is effectively impossible to parse correctly with regex.

4. **Breaks the contract**: The batch mode returns a JSON envelope, but the most important field (the actual result) is not valid JSON -- it is an opaque string containing Haskell syntax.

5. **Coupling to internals**: Any refactor of internal type names (e.g., renaming `MkNF` or `ValString`) silently breaks all downstream consumers.

---

## Proposed Solution

Add `ToJSON` instances (from the `aeson` library) for the L4 value types, and use these instances when serializing batch mode output instead of `Show`.

### Value to JSON mapping

| L4 Value | JSON Representation | Example |
|----------|---------------------|---------|
| `ValNumber (n % d)` | JSON number (`n/d` as `Double`) | `ValNumber (3 % 2)` -> `1.5` |
| `ValString s` | JSON string | `ValString "hello"` -> `"hello"` |
| `ValBool True` | JSON `true` | `ValBool True` -> `true` |
| `ValBool False` | JSON `false` | `ValBool False` -> `false` |
| `ValConstructor "JUST" [v]` | The JSON encoding of `v` (unwrap) | `JUST (ValNumber 42)` -> `42` |
| `ValConstructor "NOTHING" []` | JSON `null` | `NOTHING` -> `null` |
| `ValConstructor name []` (nullary/enum) | JSON string (the constructor name) | `APPROVED` -> `"APPROVED"` |
| `ValConstructor name fields` (record) | JSON object with field names as keys | See below |
| List (FOLLOWED BY / EMPTY chain) | JSON array | See below |
| `ValDate d` | JSON string (ISO 8601 format) | `ValDate 2026-03-15` -> `"2026-03-15"` |

### Record encoding

For a record constructor like:

```l4
DECLARE Person HAS
  name IS A String
  age IS A Number
```

The value `Person { name = "Alice", age = 30 }` should serialize as:

```json
{ "name": "Alice", "age": 30 }
```

Field names must be extracted from the type checker's resolved information for the constructor. If field names are unavailable (positional constructor), fall back to a JSON array of the field values.

### List encoding

L4 lists are represented internally as a chain of `FOLLOWED BY` (cons) and `EMPTY` (nil) constructors. These should be detected and flattened to a JSON array:

```
FOLLOWED BY "a" (FOLLOWED BY "b" (FOLLOWED BY "c" EMPTY))
```

becomes:

```json
["a", "b", "c"]
```

### Batch response format change

**Current:**

```json
{
  "status": "success",
  "output": ["MkEvalDirectiveResult { result = Reduction (Right (MkNF (ValString \"hello\"))) }"],
  "input": { "name": "Alice" },
  "diagnostics": []
}
```

**Proposed:**

```json
{
  "status": "success",
  "output": {
    "result": "hello",
    "trace": null
  },
  "input": { "name": "Alice" },
  "diagnostics": []
}
```

For functions returning multiple results (if `output` was previously a list):

```json
{
  "status": "success",
  "output": [
    { "result": "hello", "trace": null },
    { "result": 42, "trace": null }
  ],
  "input": { "name": "Alice" },
  "diagnostics": []
}
```

The `trace` field is reserved for future use (evaluation traces, rule citations). It should be `null` for now.

### Error cases

When evaluation fails, the output should include error information as structured JSON rather than a `Show`-formatted Haskell exception:

```json
{
  "status": "error",
  "output": null,
  "error": "Type mismatch: expected Number, got String for parameter 'age'",
  "input": { "name": "Alice", "age": "not a number" },
  "diagnostics": [...]
}
```

---

## Implementation Plan

### Step 1: Add `ToJSON` instances for value types

Add `ToJSON` instances for the core value/normal-form types. This is the main piece of work.

```haskell
instance ToJSON NF where
  toJSON (MkNF val) = toJSON val

instance ToJSON Value where
  toJSON (ValNumber r)  = toJSON (fromRational r :: Double)
  toJSON (ValString s)  = toJSON s
  toJSON (ValBool b)    = toJSON b
  toJSON (ValDate d)    = toJSON (show d)  -- ISO 8601
  toJSON (ValConstructor "NOTHING" []) = Null
  toJSON (ValConstructor "JUST" [v])   = toJSON v
  toJSON (ValConstructor "EMPTY" [])   = toJSON ([] :: [Value])
  toJSON (ValConstructor "FOLLOWED BY" [x, xs]) = case toJSON xs of
    Array arr -> toJSON (toJSON x : V.toList arr)
    _         -> toJSON [toJSON x, toJSON xs]  -- fallback
  toJSON (ValConstructor name [])      = toJSON name  -- enum
  toJSON (ValConstructor name fields)  = object (zipFieldNames name fields)
  -- ... handle remaining cases
```

The exact constructor names and type wrappers will need to be adjusted to match the actual codebase definitions. The above is illustrative.

### Step 2: Add `ToJSON` instance for `EvalDirectiveResult`

```haskell
instance ToJSON EvalDirectiveResult where
  toJSON (MkEvalDirectiveResult res) = object
    [ "result" .= resultToJSON res
    , "trace"  .= Null
    ]
```

### Step 3: Update batch response serialization

In the batch response handler (likely in `jl4/app/Main.hs` or a `Batch` module), replace `show` calls with `toJSON` calls when building the output field.

### Step 4: Handle field name resolution for records

Records require access to field names from the type checker. Options:

- **Preferred**: Carry field name metadata alongside the `Value` in a wrapper, populated during evaluation.
- **Alternative**: Pass the type environment to the `ToJSON` serialization layer.
- **Fallback**: Encode record fields as a JSON array (losing field names) and improve later.

### Estimated scope

- `ToJSON` instances for `Value`, `NF`, `EvalDirectiveResult`: ~50-80 lines
- List flattening logic: ~10 lines
- Record field name extraction: ~20-50 lines (depending on type info availability)
- Batch response handler update: ~10-20 lines
- **Total: ~90-160 lines of Haskell**

---

## Key Files (Likely Locations)

These paths are approximate and should be confirmed against the current codebase:

| File | Role |
|------|------|
| `jl4/app/Main.hs` | Batch mode entry point, response serialization |
| `jl4-core/src/L4/Evaluate*.hs` | `Value`, `NF` type definitions |
| `jl4-core/src/L4/Batch.hs` | Batch processing logic (if it exists as a separate module) |
| `jl4-core/src/L4/Export.hs` | `@export` function handling, `EvalDirectiveResult` |

---

## Testing

1. **Unit tests**: Add `ToJSON` round-trip tests for each value type (number, string, bool, date, Maybe, list, record, enum).

2. **Integration tests**: Run `jl4-cli --batch` on existing L4 examples and verify the output is valid JSON with correct values.

3. **Regression tests**: Ensure existing batch mode consumers (decision service) are updated or remain compatible.

4. **Edge cases to test**:
   - Numbers that are exact integers (should serialize as `42`, not `42.0` if possible)
   - Rational numbers with repeating decimals (e.g., `1/3`)
   - Empty lists
   - Nested records
   - `NOTHING` vs missing fields
   - Unicode strings
   - Dates in various formats

---

## Migration / Backwards Compatibility

This is a **breaking change** to the batch output format. Mitigation options:

1. **Recommended**: Ship the new format as the default. The old format was never a stable API (it was `Show` output). Notify known consumers.

2. **Alternative**: Add a `--output-format=json|show` flag, defaulting to `json`. Deprecate `show` after one release cycle.

Given that the primary known consumer (ACTUS-FIBO Python wrapper) is actively requesting this change, option 1 is preferred.

---

## Motivation

This change is needed for the **ACTUS-FIBO project**, where L4 cross-default detection logic is invoked from Python via `jl4-cli --batch`. The current Python integration uses fragile regex extraction:

```python
# Current: fragile regex parsing of Haskell Show output
output_str = result["output"][0]
matches = re.findall(r'ValString "([^"]+)"', output_str)
transaction_ids = matches
```

With this change, the integration becomes:

```python
# Proposed: clean JSON access
result = json.loads(stdout)
transaction_ids = result["output"]["result"]  # native JSON array
```

This is also a prerequisite for any future use of `jl4-cli --batch` as a service API (e.g., behind a REST endpoint or called from other languages).

---

## Priority

**High** -- this blocks clean integration with external systems calling L4 as a service.
