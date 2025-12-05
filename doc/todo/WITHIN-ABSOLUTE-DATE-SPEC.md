# WITHIN Absolute Date Support Specification

## Status

**Proposed** - Feature Request

## Overview

Extend the `WITHIN` clause in regulative rules to support absolute DATE expressions in addition to relative NUMBER (day count) expressions.

## Motivation

### Current Limitation

Currently, `WITHIN` clauses in L4 regulative rules only accept NUMBER types representing relative day counts from the present moment:

```l4
PARTY seller
MUST deliver_goods
WITHIN 30  -- 30 days from now
```

### Real-World Need

Many legal obligations have **absolute deadlines** specified as calendar dates, not relative day counts:

- "Payment must be made by December 31, 2025"
- "Notice must be delivered by the value date"
- "Representations must be true as of the effective date"

### Current Workaround Issues

The IFEMA formalization revealed that developers must either:

1. **Use arbitrary day counts** that don't reflect actual legal meaning:

   ```l4
   WITHIN 0  -- Really means "by the value date" but forced to use 0
   ```

2. **Lose temporal precision** when deadlines are event-driven dates:

   ```l4
   -- Want: WITHIN transaction's `value date`
   -- Must use: WITHIN 0  -- Imprecise approximation
   ```

3. **Cannot express date-relative deadlines** naturally:
   ```l4
   -- Want: WITHIN (`Payment Due Date` transaction) PLUS 2
   -- Currently: Type error (DATE where NUMBER expected)
   ```

## Proposed Solution

### Syntax Extension

Allow `WITHIN` clauses to accept DATE expressions:

```l4
WITHIN <date-expression>
```

Where `<date-expression>` can be:

- Date literals: `DATE OF 31, 12, 2025`
- Date fields: `transaction's `value date``
- Date computations: `(`effective date` PLUS 30)`
- Date functions: ``Settlement Date` `trade date` 2 `New York``

### Semantics

When `WITHIN` contains a DATE expression:

1. **Runtime evaluation**: The date expression is evaluated in the current context
2. **Deadline interpretation**: The obligation must be fulfilled by EOD (end of day) on that date
3. **Comparison**: At runtime, check if current timestamp ≤ deadline date
4. **Business day adjustment**: Optionally respect business day conventions (design decision needed)

### Examples

#### Absolute Date Deadline

```l4
PARTY borrower
MUST repay_loan
WITHIN DATE OF 31, 12, 2025
HENCE FULFILLED
LEST default_event
```

#### Field-Based Deadline

```l4
GIVEN transaction IS A Transaction
`Seller Delivery Obligation` transaction MEANS
  PARTY transaction's seller
  MUST  `Delivery Currency Action`
  WITHIN transaction's `value date`  -- Absolute date from transaction
  HENCE FULFILLED
  LEST  `Grace Period` transaction
```

#### Computed Deadline

```l4
GIVEN event IS AN `Event Of Default`
      deadline IS A DATE
`Cure Obligation` event deadline MEANS
  PARTY `defaulting party` event
  MUST  `Cure Default Action`
  WITHIN deadline PLUS 5  -- 5 days after the provided deadline
  HENCE FULFILLED
  LEST  `Termination Right` event
```

#### Mixed with PROVIDED (Business Day Check)

```l4
PARTY seller
MUST deliver_payment
PROVIDED `Is Business Day` (`value date` PLUS 2) `New York`
WITHIN `value date` PLUS 2  -- 2 business days after value date
HENCE FULFILLED
LEST grace_period
```

## Implementation Considerations

### Type System Changes

Currently:

```haskell
-- Simplified
type WithinClause = Number  -- Days from now
```

Proposed:

```haskell
data WithinClause
  = RelativeDays Number      -- Existing: 30 (days from now)
  | AbsoluteDate Date        -- New: DATE OF 31, 12, 2025
  | DateExpression DateExpr  -- New: computed date expressions
```

### Runtime Semantics

For relative day counts (existing behavior):

```
deadline_timestamp = current_time + (days * 24 * 3600)
```

For absolute dates (new behavior):

```
deadline_timestamp = end_of_day(evaluated_date)
```

### Design Questions

1. **Time zone handling**: What timezone should EOD be computed in?

   - Default to UTC?
   - Use jurisdiction-specific timezone from agreement metadata?
   - Allow explicit timezone in WITHIN clause?

2. **Business day adjustment**: Should WITHIN dates auto-adjust for business days?

   - Option A: Literal interpretation (weekend deadline fails on weekend)
   - Option B: Auto-adjust to following business day (requires center parameter)
   - Option C: Let PROVIDED handle business day checks separately (current pattern)

3. **Backward compatibility**: How to distinguish NUMBER vs DATE?

   - Type inference should handle this automatically
   - `WITHIN 30` → NUMBER → relative days
   - `WITHIN date_expr` → DATE → absolute deadline

4. **Interaction with simulation**: How should simulators handle absolute dates?
   - Need to track simulation "current date" not just step counter
   - Trace outputs should show "deadline: 2025-12-31" not "deadline: 30 days"

## Migration Path

### Phase 1: Type System Update

- Extend `WITHIN` to accept `DATE | NUMBER`
- Update type checker to infer which is being used
- Update runtime to handle both interpretations

### Phase 2: Update Existing Code

- Existing code using NUMBER continues to work (no breaking changes)
- New code can use DATE expressions where semantically appropriate

### Phase 3: Documentation

- Update language guide with absolute date examples
- Add best practices for choosing relative vs absolute
- Document timezone and business day considerations

## Related Work

### Comparison to Other Temporal Logic Systems

**CL (Symboleo)**: Uses explicit time constraints with calendar support:

```
ObligationState(fulfilled_before(2025-12-31))
```

**LegalRuleML**: Supports both relative and absolute temporal expressions:

```xml
<deadline type="absolute">2025-12-31</deadline>
<deadline type="relative" unit="days">30</deadline>
```

**Accord Project (Ergo)**: DateTime types with moment.js-style operations:

```
enforce before dateTime('2025-12-31T23:59:59Z')
```

## Open Questions

1. Should we support partial dates (e.g., "December 2025" without day)?
2. Do we need recurring deadlines (e.g., "every quarter end")?
3. Should WITHIN support time-of-day (not just date)?
4. How to handle deadlines that fall on non-business days?

## Success Criteria

This feature is successful when:

1. ✅ IFEMA formalization can express delivery obligations as `WITHIN transaction's value date`
2. ✅ Type checker accepts both `WITHIN 30` and `WITHIN DATE OF 31, 12, 2025`
3. ✅ Runtime correctly evaluates absolute date deadlines
4. ✅ Trace outputs show human-readable absolute dates
5. ✅ Documentation includes clear examples of when to use each form
6. ✅ No breaking changes to existing L4 code

## References

- Current IFEMA formalization: `~/src/legalese/actus2025/ifema.l4`
- Type checker: `~/src/smucclaw/l4-ide/jl4/src/L4/Syntax/Check.hs`
- Runtime interpreter: `~/src/smucclaw/l4-ide/jl4/src/L4/Syntax/Interpreter.hs`
- Related issue: WITHIN clauses in regulative rules (type error when using DATE)

## Author

Generated during IFEMA formalization work (December 2025)

## Next Steps

1. Discuss design decisions (timezone, business day handling) with language team
2. Prototype type system changes
3. Update parser and type checker
4. Implement runtime semantics
5. Update documentation and examples
6. Test with IFEMA and other real-world formalizations
