# ASSUME in L4: A Conceptual Guide

## The Core Idea

**ASSUME declares external facts that must be provided at runtime.**

Think of L4 programs as having two kinds of inputs:

- **GIVEN**: Function parameters (local, per-call)
- **ASSUME**: Module parameters (global, environment-level)

```l4
ASSUME temperature IS A NUMBER      -- "I need this from somewhere"
ASSUME humidity IS A NUMBER

GIVEN threshold IS A NUMBER         -- "Pass this when you call me"
`exceeds threshold` threshold MEANS temperature > threshold
```

## Comparison to Other Languages

| Concept                     | L4 Equivalent | Similarity                                            |
| --------------------------- | ------------- | ----------------------------------------------------- |
| OCaml functor parameters    | ASSUME        | Module-level inputs that must be provided             |
| Haskell implicit parameters | ASSUME        | Values "floating" in scope without explicit threading |
| TLA+ ASSUME                 | ASSUME        | Axiomatic declarations for specification              |
| Dependency injection        | ASSUME        | External dependencies declared, provided later        |

**The closest analogy is OCaml functors**, but simpler:

- In OCaml: `module F (X : sig val temp : int end) = struct ... end`
- In L4: `ASSUME temperature IS A NUMBER` at module level

## Scoping Rules

### Within a Module

1. **Module-global**: ASSUMEs are visible throughout the entire module (declaration order doesn't matter)
2. **Section qualification**: Can be nested in sections for qualified names
3. **TDNR shadowing**: Multiple ASSUMEs with same name but different types are allowed (Type-Directed Name Resolution picks the right one)

```l4
§ Environment
ASSUME temperature IS A NUMBER    -- accessible as: temperature, Environment.temperature

§ Authorization
ASSUME `is authorized` IS A FUNCTION FROM Person TO BOOLEAN
```

**Note:** Unlike local bindings, top-level declarations (including ASSUME) do NOT require declaration-before-use. You can put all ASSUMEs at the bottom of the file if preferred.

### Across Imports

**ASSUMEs do NOT propagate through IMPORT.**

```l4
-- weather.l4
ASSUME temperature IS A NUMBER
DECIDE `is cold` MEANS temperature < 10

-- main.l4
IMPORT weather
-- `is cold` is available, but `temperature` is NOT
-- If main.l4 needs temperature, it must re-declare:
ASSUME temperature IS A NUMBER
```

**Rationale**: Each module should be explicit about its external dependencies. This prevents "spooky action at a distance" where importing a module suddenly requires providing values you didn't know about.

## Evaluation Semantics

When code references an ASSUME:

1. **If value provided** → Use it normally
2. **If value NOT provided** → Evaluation produces `ValAssumed` (stuck)

```
temperature > 25
    ↓
ValAssumed "temperature"  -- Stuck! Can't proceed.
```

This is intentional: ASSUMEs mark the boundary between what L4 can compute and what must come from the outside world.

## The Three Declaration Types

| Declaration | Purpose                 | Example                                 |
| ----------- | ----------------------- | --------------------------------------- |
| **DECLARE** | Define types            | `DECLARE Person HAS name IS A STRING`   |
| **ASSUME**  | Declare external values | `ASSUME temperature IS A NUMBER`        |
| **DECIDE**  | Define computations     | `DECIDE is_cold MEANS temperature < 10` |

Think of it as:

- **DECLARE** = "Here's what things look like" (structure)
- **ASSUME** = "Here's what I need from you" (inputs)
- **DECIDE** = "Here's what I compute" (logic)

## API Behavior (Decision Service)

When a function is `@export`ed, the system automatically:

1. Scans the function body for ASSUME references
2. Exposes those ASSUMEs as API input parameters
3. **Filters out function-typed ASSUMEs** (can't serialize functions in JSON)

```l4
ASSUME temperature IS A NUMBER
ASSUME `oracle` IS A FUNCTION FROM NUMBER TO BOOLEAN  -- Skipped!

@export
`is warm` MEANS temperature > 25
```

API schema exposes only `temperature`:

```json
{
  "parameters": {
    "temperature": { "type": "number" }
  }
}
```

### JSON Input Format

ASSUME-derived parameters appear identically to GIVEN parameters in API requests:

```json
{
  "temperature": 22,
  "humidity": 50
}
```

The decision service internally:

1. Decodes the JSON into a record with all parameters
2. Passes GIVEN params as positional function arguments
3. Injects ASSUME params as LET bindings that shadow global declarations

## Practical Mental Model

**ASSUMEs are like environment variables for legal/business logic:**

```l4
-- These are "configuration" - provided by the runtime environment
ASSUME jurisdiction IS A Jurisdiction
ASSUME current_date IS A DATE
ASSUME tax_rate IS A NUMBER

-- This is "computation" - uses the configuration
DECIDE `tax owed` amount MEANS amount * tax_rate
```

The caller provides the environment; the L4 code computes within that environment.

## Design Philosophy

1. **Separation of concerns**: Computable logic (DECIDE) vs. external facts (ASSUME)
2. **Explicit dependencies**: Each module declares what it needs
3. **API-first**: ASSUMEs automatically become API parameters
4. **No hidden requirements**: If a function needs something, it's visible in GIVEN or traced to ASSUME

## Implementation Notes

### How ASSUME Values Are Injected

The decision service generates wrapper code that shadows ASSUME declarations:

```l4
-- Original code
ASSUME temperature IS A NUMBER
`is warm` MEANS temperature > 25

-- Generated wrapper (simplified)
#EVAL
  CONSIDER decodeArgs inputJson
    WHEN RIGHT args THEN
      LET temperature = (args's temperature)  -- Shadows global ASSUME
      IN `is warm`
```

### Free Variable Detection

The system uses optics-based AST traversal (`cosmosOf gplate`) to find all variable references in a function body, then matches them against ASSUME declarations by their unique identifiers.

## Related Files

- `jl4-core/src/L4/Export.hs` - ASSUME extraction and export logic
- `jl4-decision-service/src/Backend/CodeGen.hs` - Wrapper generation with LET bindings
- `jl4-decision-service/src/Backend/FunctionSchema.hs` - API schema generation
- `jl4/examples/ok/assume-as-given.l4` - Test file demonstrating the feature
