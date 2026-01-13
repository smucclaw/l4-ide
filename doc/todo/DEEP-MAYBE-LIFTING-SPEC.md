# Deep Maybe Lifting for Partial Evaluation

## Status: Proposed

## Problem

When evaluating functions with partial parameters (some provided, some missing), we need a principled way to represent "unknown" values. The current approach in `Backend/CodeGen.hs` is a pragmatic hack that only handles missing BOOLEAN parameters, generating different code depending on which parameters are present in each request.

This has limitations:
- Only works for BOOLEAN types (uses `fromMaybe FALSE`)
- Generates different wrapper code per invocation
- Doesn't handle partial data within complex types (records)

## Proposed Solution: Deep Maybe Lifting

Inspired by how JavaScript allows every type to be nullable, we can use L4's `MAYBE` type systematically to represent optional/unknown values at every level of the type structure.

### Core Idea

Transform input parameter types by "lifting" them to Maybe at every depth:

```
lift :: Type -> Type
lift BOOLEAN     = MAYBE BOOLEAN
lift NUMBER      = MAYBE NUMBER
lift STRING      = MAYBE STRING
lift DATE        = MAYBE DATE
lift (Record fields) = MAYBE (Record (map liftField fields))
lift (LIST OF a) = MAYBE (LIST OF (lift a))
```

### Example: vermin_and_rodent

Original type:
```l4
DECLARE Inputs HAS
    damage_caused_by_rodents IS A BOOLEAN
  , damage_caused_by_birds IS A BOOLEAN
  , damage_to_contents IS A BOOLEAN
```

Lifted type for partial evaluation:
```l4
DECLARE MaybeInputs HAS
    damage_caused_by_rodents IS A MAYBE BOOLEAN
  , damage_caused_by_birds IS A MAYBE BOOLEAN
  , damage_to_contents IS A MAYBE BOOLEAN
```

Now JSON `{"damage_caused_by_rodents": true}` decodes to:
```l4
MaybeInputs
  (JUST TRUE)   -- damage_caused_by_rodents
  NOTHING       -- damage_caused_by_birds (missing)
  NOTHING       -- damage_to_contents (missing)
```

### Wrapper Function Generation

For a function `f :: Inputs -> BOOLEAN`, generate:

```l4
-- Lifted wrapper that handles partial inputs
GIVEN maybeInputs IS A MaybeInputs
GIVETH A MAYBE BOOLEAN
f_partial maybeInputs MEANS
  CONSIDER (maybeInputs's damage_caused_by_rodents,
            maybeInputs's damage_caused_by_birds,
            maybeInputs's damage_to_contents)
    WHEN (JUST rodents, JUST birds, JUST contents) THEN
      JUST (f (Inputs rodents birds contents))
    OTHERWISE NOTHING  -- or propagate partial evaluation
```

### Alternative: Lazy Unwrapping with Short-Circuit

Instead of requiring all fields, use lazy evaluation:

```l4
f_partial maybeInputs MEANS
  f (Inputs
      (unwrap (maybeInputs's damage_caused_by_rodents))
      (unwrap (maybeInputs's damage_caused_by_birds))
      (unwrap (maybeInputs's damage_to_contents)))
```

Where `unwrap :: MAYBE a -> a` would:
- Return the value for `JUST x`
- Propagate as "omitted" for `NOTHING` (leveraging lazy evaluation)

This requires evaluator support for "omitted" values that short-circuit.

## Implementation Plan

### Phase 1: Type Transformation
1. Add `liftToMaybe :: Type' Resolved -> Type' Resolved` in a new module
2. Handle primitives (BOOLEAN, NUMBER, STRING, DATE)
3. Handle records (recursively lift all fields)
4. Handle lists (lift element type)

### Phase 2: Code Generation
1. Generate lifted `InputArgs` record type
2. Generate unwrapping code that handles NOTHING appropriately
3. Integrate with existing JSONDECODE flow

### Phase 3: Evaluator Integration
1. Ensure lazy evaluation properly short-circuits on NOTHING
2. Or implement explicit `unwrap` that propagates omitted values

## Benefits

1. **Uniform handling**: All types can be partial, not just booleans
2. **Type-safe**: L4's type system tracks what's optional
3. **Principled**: Follows established patterns (GADT-style lifting)
4. **Composable**: Works with nested structures at any depth

## Considerations

- **Performance**: Deep lifting creates more wrapper code
- **Complexity**: Evaluator may need changes for lazy NOTHING handling
- **Migration**: Existing code continues to work (this is opt-in for partial eval)

## Related

- Current implementation: `jl4-decision-service/src/Backend/CodeGen.hs`
- Query planning: Already uses partial evaluation concepts via BDD analysis
- Lazy evaluation: `jl4-core/src/L4/EvaluateLazy.hs` has `Omitted` for missing values
