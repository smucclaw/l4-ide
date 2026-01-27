# MAY

Creates a permission for a party to perform an action. The party is allowed but not required to act.

## Syntax

```l4
PARTY partyName MAY DO action
PARTY partyName MAY DO action WITHIN deadline
```

## Purpose

MAY expresses a legal permission - something a party is allowed to do. Unlike obligations (MUST), permissions are optional.

## Examples

**Example file:** [may-example.l4](may-example.l4)

## Permission Semantics

- Permission is **exercised** when the party performs the action
- Permission **expires** when the deadline passes without action
- Neither exercising nor not exercising a permission causes breach

## Related Keywords

- **[PARTY](PARTY.md)** - Identifies who has the permission
- **[MUST](MUST.md)** - Obligation (required action)
- **[SHANT](SHANT.md)** - Prohibition (forbidden action)
- **[REGULATIVE](REGULATIVE.md)** - Full regulative rule reference
