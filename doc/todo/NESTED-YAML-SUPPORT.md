# Nested YAML Support for Decision Service Issue #635

**Status**: ⚠️ In Progress - Data structure updated, tests created, further work needed

**Issue**: [#635, Item 3](https://github.com/smucclaw/l4-ide/issues/635)

## Problem

The decision service's YAML sidecar files could only represent flat parameter structures. For parameters with nested record types, the current `Parameter` data type had no way to represent the nested properties, limiting OpenAPI schema generation and making it impossible to document complex nested structures in YAML.

### Example of the Problem

For an L4 type like:
```l4
DECLARE Address HAS
  street IS A STRING
  city IS A STRING

DECLARE Person HAS
  name IS A STRING
  address IS AN Address
```

The old YAML structure could only represent:
```yaml
properties:
  person:
    type: "object"  # No way to specify nested fields!
    description: "A person with address"
```

## Solution Implemented

### 1. Extended `Parameter` Data Type

Added `parameterProperties` field to support recursive nesting:

```haskell
data Parameter = Parameter
  { parameterType :: !Text
  , parameterAlias :: !(Maybe Text)
  , parameterEnum :: ![Text]
  , parameterDescription :: !Text
  , parameterProperties :: !(Maybe (Map Text Parameter))  -- NEW: Nested properties
  }
```

### 2. Updated JSON Instances

**FromJSON**: Parses optional `properties` field from YAML
**ToJSON**: Includes `properties` field when present

### 3. Test Files Created

- **`doc/tutorial-code/nested-person.l4`**: L4 file with 3 levels of nesting (Person → Address, ContactInfo)
- **`doc/tutorial-code/nested-person.yaml`**: YAML demonstrating nested properties syntax

### 4. Updated Code

**Files Modified**:
- `jl4-decision-service/src/Server.hs`: 
  - Extended `Parameter` type
  - Updated `ToJSON`/`FromJSON` instances
  - Added `parameterProperties = Nothing` to `paramToParameter`
  
- `jl4-decision-service/src/Examples.hs`:
  - Added `parameterProperties = Nothing` to `paramToParameter`
  
- `jl4-decision-service/test/SchemaSpec.hs`:
  - Updated `Arbitrary` instance for `Parameter`

## What Works Now

✅ YAML files can include nested `properties` sections
✅ JSON serialization/deserialization handles nesting
✅ Backward compatible - existing flat YAML files still work

## What Still Needs Work

###  Priority 1: Extract Nested Properties from L4 Types

The `paramToParameter` functions currently set `parameterProperties = Nothing`. They need to:

1. Inspect the L4 type (e.g., `TyApp _ recordName []`)
2. Look up the record definition in the type environment
3. Recursively extract field names and types
4. Build the nested `Map Text Parameter` structure

**Affected files**: `Server.hs:600-608`, `Examples.hs:138-146`

### Priority 2: OpenAPI Schema Generation

Update `Schema.hs` to generate OpenAPI schemas with nested object properties when `parameterProperties` is present.

### Priority 3: Integration Tests

Add tests that:
- Load `.yaml` files with nested properties
- Verify OpenAPI schema includes nested structure
- Test evaluation with deeply nested JSON input

### Priority 4: Documentation

Update `jl4-decision-service/README.md` with examples of nested YAML syntax.

## Testing Plan

1. **Unit Tests**: YAML parsing with nested structures
2. **Integration Tests**: Load nested-person.yaml via `--sourcePaths`
3. **End-to-End Tests**: Evaluate functions with nested JSON input
4. **Regression Tests**: Ensure existing flat YAML files still work

## Example: Nested YAML Syntax

```yaml
properties:
  person:
    type: "object"
    description: "Person with nested address"
    properties:
      name:
        type: "string"
        description: "Full name"
      address:
        type: "object"
        description: "Residential address"
        properties:
          street:
            type: "string"
            description: "Street address"
          city:
            type: "string"
            description: "City name"
```

## Notes

- The recursive `Parameter` structure naturally matches OpenAPI's recursive schema model
- JSONDECODE already handles nested JSON correctly via bidirectional type checking
- This change only affects metadata/documentation layer, not runtime evaluation

## Related Work

- **Issue #635 Items 1, 2**: Already completed (conditional trace, IDE directive filtering)
- **JSONDECODE Spec**: Nested structures already work at runtime (doc/done/DECISION-SERVICE-JSONDECODE-SPEC.md)
