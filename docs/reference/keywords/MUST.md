# MUST

Creates an obligation for a party to perform an action. The party is required to do something.

## Syntax

```l4
PARTY partyName MUST DO action
PARTY partyName MUST DO action WITHIN deadline
PARTY partyName MUST NOT DO action
```

## Purpose

MUST expresses a legal obligation - something that a party is required to do. If not fulfilled within the deadline, consequences (LEST) may apply.

## Examples

**Example file:** [must-example.l4](must-example.l4)

## Obligation Fulfillment

- Obligation is **fulfilled** when the party performs the action before the deadline
- Obligation is **breached** when the deadline passes without the action
- Consequences in LEST clause activate on breach

## Related Keywords

- **[PARTY](PARTY.md)** - Identifies who has the obligation
- **[MAY](MAY.md)** - Permission (optional action)
- **[SHANT](SHANT.md)** - Prohibition
- **[DO](DO.md)** - Action verb
- **[WITHIN](WITHIN.md)** - Deadline
- **[HENCE](HENCE.md)** - Consequence on fulfillment
- **[LEST](LEST.md)** - Consequence on breach

## See Also

- **[PARTY examples](party-example.l4)** - Complete regulative rule examples
