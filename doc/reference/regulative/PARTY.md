# PARTY

Identifies the legal party (person or entity) who has an obligation, permission, or prohibition in a regulative rule.

## Syntax

```l4
PARTY partyName
PARTY partyName MUST ...
PARTY partyName MAY ...
PARTY partyName SHANT ...
```

## Purpose

PARTY is the starting point for regulative rules, specifying WHO is subject to the rule. It introduces:

- Obligations (MUST)
- Permissions (MAY)
- Prohibitions (SHANT)

## Examples

**Example file:** [party-example.l4](party-example.l4)

### Basic Obligation

```l4
DECLARE Person IS ONE OF Alice, Bob

myRule MEANS
  PARTY Alice
  MUST DO sing
  WITHIN 30
```

### Permission

```l4
permissionRule MEANS
  PARTY Bob
  MAY DO dance
  WITHIN 60
```

### Prohibition

```l4
prohibitionRule MEANS
  PARTY Alice
  SHANT smoke
  WITHIN 30
```

### With Consequences

```l4
ruleWithConsequence MEANS
  PARTY Alice
  MUST DO pay
  WITHIN 30
  LEST PARTY Bob MAY DO sue WITHIN 60
```

## Regulative Rule Structure

A complete regulative rule typically includes:

```l4
ruleName MEANS
  PARTY who          -- The party subject to the rule
  MUST/MAY/SHANT     -- The deontic modality
  DO action          -- What action is regulated
  WITHIN deadline    -- Time constraint
  HENCE consequent   -- What follows if rule is followed
  LEST alternative   -- What follows if rule is violated
```

## Related Keywords

- **[MUST](MUST.md)** - Obligation
- **[MAY](MAY.md)** - Permission
- **[SHANT](SHANT.md)** - Prohibition
- **[REGULATIVE](README.md)** - Full regulative rule reference

## See Also

- **[Regulative Rules](../../concepts/legal-modeling/regulative-rules.md)** - Modeling obligations
