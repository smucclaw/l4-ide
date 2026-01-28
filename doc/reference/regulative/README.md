# Regulative Rule Keywords

Regulative keywords express legal obligations, permissions, prohibitions, and their consequences. They form the core of L4's contract and regulation modeling.

## Overview

### Deontic Modalities

| Keyword               | Meaning                   |
| --------------------- | ------------------------- |
| **[MUST](MUST.md)**   | Obligation (required)     |
| **[MAY](MAY.md)**     | Permission (allowed)      |
| **[SHANT](SHANT.md)** | Prohibition (forbidden)   |
| DO                    | Possibility (optionality) |

### Rule Structure

| Keyword               | Purpose                           |
| --------------------- | --------------------------------- |
| **[PARTY](PARTY.md)** | Who has the obligation/permission |
| **WITHIN**            | Temporal deadline                 |
| **HENCE**             | Consequence on fulfillment        |
| **LEST**              | Consequence on breach             |
| **PROVIDED**          | Guard condition                   |
| **BREACH**            | Explicit violation marker         |
| **[BECAUSE](BECAUSE.md)** | Reason for breach             |
| **FULFILLED**         | Successfully completed            |

## Basic Rule Structure

```l4
PARTY partyName
MUST/MAY/SHANT/DO action
WITHIN deadline
```

### Example

```l4
DECLARE Person IS ONE OF Alice, Bob
DECLARE Action IS ONE OF pay HAS amount IS A NUMBER

paymentObligation MEANS
  PARTY Alice
  MUST pay 100
  WITHIN 30
```

## WITHIN (Temporal Deadline)

Specifies when an action must/may be performed.

### Syntax

```l4
WITHIN timeUnits
```

### Examples

```l4
-- Simple deadline
PARTY Alice MUST pay 100 WITHIN 30
```

## HENCE (Fulfillment Consequence)

Specifies what happens when the obligation is fulfilled.

### Syntax

```l4
PARTY ...
MUST action
WITHIN deadline
HENCE consequentRule
```

### Examples

```l4
-- Chain of obligations
PARTY Alice
MUST pay 500
WITHIN 7
HENCE (
  PARTY Bob
  MUST deliver "goods"
  WITHIN 14
)
```

## LEST (Breach Consequence)

Specifies what happens when the obligation is breached.

### Syntax

```l4
PARTY ...
MUST action
WITHIN deadline
LEST breachConsequence
```

### Examples

```l4
-- Simple breach
PARTY Alice
MUST pay 100
WITHIN 30
LEST BREACH

-- Penalty clause
PARTY Alice
MUST pay 100
WITHIN 30
LEST (
  PARTY Alice
  MUST pay 150
  WITHIN 60
)
```

## PROVIDED (Guard Condition)

Adds a condition to an action.

### Syntax

```l4
MUST action parameter PROVIDED parameter-condition
```

### Examples

```l4
-- Conditional payment
PARTY Bob
MUST payment price PROVIDED price >= 20
WITHIN 3
```


## EXACTLY (Guard Condition)

Used in pattern matching for exact value matches, especially in regulative rules.
`price EXACTLY 20` is equivalent to `price PROVIDED price EQUALS 20`

### Syntax

```l4
MUST action parameter EXACTLY value
```

### Examples

```l4
-- In regulative rules
PARTY Alice
MUST pay price EXACTLY 100
WITHIN 30
```

## BREACH

Explicit marker that a rule violation has occurred.

### Syntax

```l4
LEST BREACH
LEST BREACH BY party
LEST BREACH BECAUSE reason
LEST BREACH BY party BECAUSE reason
```

### Examples

```l4
-- Simple breach
LEST BREACH

-- With responsible party
LEST BREACH BY Seller

-- With reason
LEST BREACH BECAUSE "delivery deadline exceeded"

-- Full form
LEST BREACH BY Seller BECAUSE "failed to deliver within 14 days"
```

See **[BECAUSE](BECAUSE.md)** for detailed documentation on breach reasons.

## FULFILLED

Marks successful completion of a contract.

```l4
HENCE FULFILLED
```

## Testing with #TRACE

Use `#TRACE` to simulate contract execution.

### Syntax

```l4
#TRACE contractName AT startTime WITH
  PARTY partyName DOES action AT eventTime
  ...
```

### Example

```l4
#TRACE paymentObligation AT 0 WITH
  PARTY Alice DOES pay 100 AT 15
```

## Complete Example

```l4
DECLARE Person IS ONE OF Seller, Buyer
DECLARE Action IS ONE OF
  delivery
  payment HAS amount IS A NUMBER

saleContract MEANS
  PARTY Seller
  MUST delivery
  WITHIN 3
  HENCE (
    PARTY Buyer
    MUST payment 100
    WITHIN 7
  )
  LEST BREACH

#TRACE saleContract AT 0 WITH
  PARTY Seller DOES delivery AT 2
  PARTY Buyer DOES payment 100 AT 5
```

## Related Pages

- **[PARTY](PARTY.md)** - Party declarations
- **[MUST](MUST.md)** - Obligations
- **[MAY](MAY.md)** - Permissions
- **[SHANT](SHANT.md)** - Prohibitions

## See Also

- **[Foundation Course: Regulative Rules](../../courses/foundation/module-5-regulative.md)** - Tutorial
- **[Regulative Rules Concept](../../concepts/legal-modeling/regulative-rules.md)** - Conceptual overview
