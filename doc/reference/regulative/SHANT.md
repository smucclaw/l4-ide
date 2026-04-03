# SHANT

Creates a prohibition preventing a party from performing an action. The party is forbidden from acting.

## Syntax

```l4
PARTY partyName SHANT action
PARTY partyName SHANT action WITHIN deadline
```

## Purpose

SHANT expresses a legal prohibition - something a party must NOT do. If the party performs the forbidden action, breach occurs.

## Examples

**Example file:** [shant-example.l4](shant-example.l4)

### Basic Prohibition

```l4
DECLARE Person IS ONE OF Alice, Bob
DECLARE Action IS ONE OF smoke, drink, gamble

noSmoking MEANS
  PARTY Alice
  SHANT smoke
  WITHIN 30
```

### Prohibition with Consequence

```l4
noDrinkingRule MEANS
  PARTY Bob
  SHANT drink
  WITHIN 30
  LEST BREACH BY Bob BECAUSE "violated policy"
```

### Conditional Prohibition

```l4
limitedGambling MEANS
  PARTY Alice
  SHANT gamble amt PROVIDED amt > 100
  WITHIN 30
```

## Prohibition Semantics

- Prohibition is **maintained** when the party does NOT perform the action
- Prohibition is **breached** when the party performs the forbidden action
- LEST consequences trigger on breach

## Related Keywords

- **[PARTY](PARTY.md)** - Identifies who has the prohibition
- **[MUST](MUST.md)** - Obligation (required action)
- **[MAY](MAY.md)** - Permission (optional action)
- **[REGULATIVE](README.md)** - Full regulative rule reference
