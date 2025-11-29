# Issue #635 Planning Status

Planning session: 2025-11-28

## Completed Specifications

| Item | Description | Spec File |
|------|-------------|-----------|
| 1 | Conditional Decision Trace Returns | `CONDITIONAL-TRACE-SPEC.md` |
| 2 | IDE Directive Filtering | `DECISION-SERVICE-JSONDECODE-SPEC.md` |
| 3 | Enhanced YAML Support (nested objects) | `DECISION-SERVICE-JSONDECODE-SPEC.md` |
| 6 | EXPORT API Syntax | `EXPORT-SYNTAX-SPEC.md` |
| 8 | Performance Optimization | `PERFORMANCE-OPTIMIZATION-SPEC.md` |

## Remaining Items (To Be Planned)

| Item | Description | Notes |
|------|-------------|-------|
| 4 | Boolean Minimization | Larger feature - see Issue #638. Interactive partial evaluation, BDD-based "don't care" term elimination. |
| 5 | Dynamic File Management | Largely addressed by PR #649 (websession → decision service push). May need refinement for file watching. |
| 7 | Import/Export Coordination | Depends on Item 6 (EXPORT syntax). Scoping for exported functions from imported modules. |
| 9 | ASSUME Parameter Requirements | Require non-function ASSUMEd values to be passed in on evaluation. |

## Key Design Decisions

### JSONDECODE-Based Query Injection (Items 2, 3)

Instead of building Haskell AST nodes that get pretty-printed to L4, generate L4 wrapper code that uses `JSONDECODE` to deserialize input JSON directly. This:
- Strips all IDE directives from source
- Handles nested objects automatically via bidirectional type checking
- Eliminates code injection concerns

### EXPORT Syntax (Item 6)

Extend `@desc` annotation with convention-based keywords:
```l4
@desc default export This is the main function
GIVEN x IS A Number @desc The input value
GIVETH A Number
myFunction x MEANS ...
```

Key finding: `@desc` is currently parsed but **never attached** to AST nodes. Phase 1 must fix this attachment.

### Performance (Item 8)

Core insight: `execEvalExprInContextOfModule` already exists and evaluates against pre-compiled modules. The decision service just needs to:
1. Cache `TypeCheckResult` at load time
2. Build `Expr Resolved` directly (not via text concatenation)
3. Call the fast evaluation path

Expected: 10x improvement sequential, 100x for parallel batch.

### Conditional Trace (Item 1)

Add `X-L4-Trace: none | full` header. When `none`:
- Use `#EVAL` instead of `#EVALTRACE`
- Return empty reasoning tree

## Implementation Priority Suggestion

1. **Item 1 (Conditional Trace)** - Quick win, bounded scope
2. **Item 8 (Performance)** - High impact, infrastructure exists
3. **Item 6 (EXPORT syntax)** - Foundation for Item 7
4. **Items 2+3 (JSONDECODE)** - Clean solution, requires Item 6 first for full benefit
5. **Item 7 (Import/Export)** - After Item 6
6. **Item 9 (ASSUME)** - Can be done independently
7. **Item 4 (Boolean Minimization)** - Larger project, separate epic

## Recent Related PRs

- #650: SPLIT and CHARAT string primitives
- #649: Push saved programs from websessions to decision service
- #647: Mixfix and postfix notation support
- #644: JSONENCODE, JSONDECODE, FETCH, POST, ENV, CONCAT, AS STRING
