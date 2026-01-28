# DEONTIC

The type for regulative rules representing obligations, permissions, and prohibitions.

## Syntax

```l4
GIVETH A DEONTIC ActorType ActionType
ruleName MEANS
    PARTY actor
    MUST/MAY/SHANT/DO action
    WITHIN deadline
    HENCE consequence
    LEST breach
```

## Purpose

`DEONTIC` is L4's type for modeling legal obligations, permissions, and prohibitions. A DEONTIC value captures:

1. **Who** has the obligation/permission (`PARTY`)
2. **What** they must/may/must not do (`MUST`/`MAY`/`SHANT`/`DO`)
3. **When** it must occur (`WITHIN`)
4. **What happens** on compliance (`HENCE`)
5. **What happens** on breach (`LEST`)

The DEONTIC type is parameterized by two type arguments:

- **ActorType**: The type of parties who can act (e.g., `Buyer`, `Seller`)
- **ActionType**: The type of actions that can be performed (e.g., `deliver`, `pay`)

## Examples

**Example file:** [deontic-example.l4](deontic-example.l4)

### Basic Usage

```l4
DECLARE Actor IS ONE OF Buyer, Seller
DECLARE Action IS ONE OF `deliver goods`, `pay invoice` HAS amount IS A NUMBER

GIVETH A DEONTIC Actor Action
`delivery obligation` MEANS
    PARTY Seller
    MUST `deliver goods`
    WITHIN 14
    HENCE FULFILLED
    LEST BREACH
```

### Terminal Values

DEONTIC has two terminal values:

| Value       | Meaning                    |
| ----------- | -------------------------- |
| `FULFILLED` | Contract successfully done |
| `BREACH`    | Contract broken            |

### Breach with Blame

```l4
LEST BREACH BY Seller BECAUSE "failed to deliver on time"
```

## Type Signature

When a function returns a DEONTIC value, use:

```l4
GIVETH A DEONTIC ActorType ActionType
```

For example:

- `GIVETH A DEONTIC Person Action` — Obligations involving `Person` actors and `Action` actions
- `GIVETH A DEONTIC ContractParty ContractAction` — Custom actor and action types

## Composition

DEONTIC values can be composed in several ways:

### Chaining with HENCE

```l4
PARTY Seller MUST `deliver` WITHIN 14
HENCE
    PARTY Buyer MUST `pay` WITHIN 30
    HENCE FULFILLED
    LEST BREACH
LEST BREACH
```

### Parallel with RAND

All obligations must be fulfilled:

```l4
(PARTY Seller MUST `deliver` WITHIN 14 HENCE FULFILLED LEST BREACH)
RAND
(PARTY Buyer MUST `pay` WITHIN 30 HENCE FULFILLED LEST BREACH)
```

### Alternative with ROR

Either obligation fulfills the contract:

```l4
(PARTY Seller MUST `ship` WITHIN 14 HENCE FULFILLED LEST BREACH)
ROR
(PARTY Seller MUST `pickup` WITHIN 7 HENCE FULFILLED LEST BREACH)
```

## Evaluation

### #TRACE

Test DEONTIC values by simulating events:

```l4
#TRACE `delivery obligation` AT 0 WITH
    PARTY Seller DOES `deliver goods` AT 10
```

### EVALTRACE Function

The underlying evaluation function:

```l4
EVALTRACE : FORALL party action.
    DEONTIC party action ->    -- The contract
    NUMBER ->                  -- Start time
    LIST (EVENT party action) -> -- Events
    DEONTIC party action       -- Result
```

Use `EVENT` to construct events:

```l4
#EVAL EVALTRACE myContract 0 (LIST EVENT Seller `deliver` 5, EVENT Buyer `pay` 20)
```

## Deontic Modalities

| Modality  | Keyword | Meaning     | On Action      | On No Action   |
| --------- | ------- | ----------- | -------------- | -------------- |
| Must      | `MUST`  | Obligation  | HENCE (comply) | LEST (breach)  |
| May       | `MAY`   | Permission  | HENCE (comply) | Nothing        |
| Shall not | `SHANT` | Prohibition | LEST (breach)  | HENCE (comply) |

## Advanced Features

### PROVIDED Clause

Add conditions to action matching:

```l4
PARTY Buyer
MUST `pay invoice` amount PROVIDED amount >= 100
WITHIN 30
```

### EXACTLY Keyword

Require exact value matching:

```l4
PARTY Buyer
MUST EXACTLY `pay invoice` 500
WITHIN 30
```

### WAIT UNTIL

Advance time in traces without events:

```l4
#TRACE myContract AT 0 WITH
    (`WAIT UNTIL` 20)
```

## Related Keywords

- **[PARTY](PARTY.md)** — Specifies who has the obligation
- **[MUST](MUST.md)** — Creates an obligation
- **[MAY](MAY.md)** — Creates a permission
- **[SHANT](SHANT.md)** — Creates a prohibition
- **[BECAUSE](BECAUSE.md)** — Adds blame reason to breach
- **[Regulative Rules](README.md)** — Full regulative reference

## Further Reading

- [Regulative Rules Concept](../../concepts/legal-modeling/regulative-rules.md) — Deep dive into deontic logic
- [Foundation Module 5](../../courses/foundation/module-5-regulative.md) — Tutorial on regulative rules
- [Tom Hvitved's PhD Thesis](https://di.ku.dk/english/research/phd/phd-theses/2011/hvitved12phd.pdf) — Theoretical foundation
