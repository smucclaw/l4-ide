# BECAUSE

Provides a reason or justification for a breach in regulative rules. Used with BREACH to document why a violation occurred.

## Syntax

```l4
BREACH BECAUSE reason
BREACH BY party BECAUSE reason
```

Where:
- `party` - The party responsible for the breach (optional)
- `reason` - A STRING explaining why the breach occurred

## Purpose

BECAUSE serves two purposes in L4:

1. **Documentation** - Records the legal reason for a breach, useful for generating explanations and audit trails
2. **Specificity** - Distinguishes between different types of breaches in complex contracts

## Examples

**Example file:** [because-example.l4](because-example.l4)

### Basic BECAUSE

```l4
-- Breach with reason only
PARTY Seller
MUST `deliver goods`
WITHIN 14
LEST BREACH BECAUSE "failed to deliver within deadline"
```

### Full Form: BY and BECAUSE

```l4
-- Breach with party and reason
PARTY Seller
MUST `deliver goods`
WITHIN 14
LEST BREACH BY Seller BECAUSE "failed to deliver within deadline"
```

### Multiple Breach Types

BECAUSE allows distinguishing between different breach scenarios:

```l4
-- Different breach reasons for different conditions
`delivery obligation` MEANS
  PARTY Seller
  MUST `deliver goods` qty PROVIDED qty AT LEAST 100
  WITHIN 14
  HENCE FULFILLED
  LEST BREACH BY Seller BECAUSE "insufficient quantity delivered"

`quality obligation` MEANS
  PARTY Seller
  MUST `pass inspection`
  WITHIN 7
  HENCE FULFILLED
  LEST BREACH BY Seller BECAUSE "goods failed quality inspection"
```

## Usage Patterns

### With LEST (Most Common)

BECAUSE typically appears in the LEST clause to explain what went wrong:

```l4
PARTY Employee
SHANT `disclose confidential information`
HENCE FULFILLED
LEST BREACH BY Employee BECAUSE "violated NDA section 3.2"
```

### With HENCE (Unusual but Valid)

In rare cases, BECAUSE can appear with HENCE when exercising a permission itself triggers a breach:

```l4
-- Smoking is permitted but still recorded as a policy violation
PARTY Employee
MAY smoke
WITHIN 30
HENCE BREACH BY Employee BECAUSE "smoking policy violation logged"
```

### In Chained Obligations

```l4
`payment chain` MEANS
  PARTY Buyer
  MUST `pay deposit` 1000
  WITHIN 7
  HENCE (
    PARTY Buyer
    MUST `pay balance` 9000
    WITHIN 30
    HENCE FULFILLED
    LEST BREACH BY Buyer BECAUSE "failed to pay balance"
  )
  LEST BREACH BY Buyer BECAUSE "failed to pay deposit"
```

## BREACH Forms

BREACH can be used in several forms:

| Form | Example | Description |
|------|---------|-------------|
| Simple | `BREACH` | Anonymous breach |
| With party | `BREACH BY Seller` | Identifies responsible party |
| With reason | `BREACH BECAUSE "reason"` | Documents why |
| Full form | `BREACH BY Seller BECAUSE "reason"` | Complete breach info |

## Best Practices

### 1. Be Specific

```l4
-- ✅ Good: Specific reason
LEST BREACH BY Seller BECAUSE "delivery exceeded 14-day SLA"

-- ❌ Vague: Doesn't help with analysis
LEST BREACH BECAUSE "failed"
```

### 2. Reference Legal Text

```l4
-- ✅ Good: References contract section
LEST BREACH BY Tenant BECAUSE "violated lease agreement section 8.3"

-- ✅ Also good: References regulation
LEST BREACH BY Employer BECAUSE "non-compliance with Employment Act s.42"
```

### 3. Use Meaningful Party Names

```l4
-- ✅ Good: Clear party identification
LEST BREACH BY `The Borrower` BECAUSE "exceeded debt covenant"

-- Less clear
LEST BREACH BY x BECAUSE "exceeded debt covenant"
```

### 4. Distinguish Breach Types

When a contract has multiple potential breaches, use BECAUSE to distinguish them:

```l4
-- Payment late vs. payment insufficient
LEST BREACH BY Buyer BECAUSE "payment received after deadline"

-- vs.

LEST BREACH BY Buyer BECAUSE "payment amount below minimum"
```

## Type Checking

The BECAUSE clause must be a STRING type:

```l4
-- ✅ Valid: String literal
LEST BREACH BECAUSE "reason text"

-- ✅ Valid: String expression
LEST BREACH BECAUSE (CONCAT "failed to pay ", TOSTRING amount)

-- ❌ Invalid: Not a string
-- LEST BREACH BECAUSE 42
```

## Evaluation and Tracing

When tracing regulative rules, BECAUSE reasons appear in the output:

```l4
#TRACE obligation AT 0 WITH
  -- No action taken, deadline passes
-- Output includes: BREACH BY Seller BECAUSE "failed to deliver"
```

This helps with:
- **Debugging** contracts during development
- **Explainability** when generating reports
- **Audit trails** for compliance

## Related Keywords

- **[BREACH](README.md#breach)** - The violation marker
- **[BY](README.md)** - Identifies the breaching party
- **[LEST](README.md#lest-breach-consequence)** - Breach consequence clause
- **[HENCE](README.md#hence-fulfillment-consequence)** - Fulfillment consequence

## See Also

- **[Regulative Rules Overview](README.md)** - Complete regulative syntax
- **[SHANT](SHANT.md)** - Prohibitions (often use BECAUSE)
- **[MUST](MUST.md)** - Obligations
- **[Regulative Rules Concept](../../concepts/legal-modeling/regulative-rules.md)** - Conceptual overview
