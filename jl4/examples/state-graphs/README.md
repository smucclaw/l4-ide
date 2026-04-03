# State Transition Graph Examples

These files demonstrate state transition visualization for L4 regulative rules, inspired by Flood & Goodenough's "Contract as Automaton" model.

## Generated From

Source files with regulative rules (MUST/MAY/SHANT):

| Source File | Graphs Generated |
|-------------|------------------|
| `jl4/examples/ok/prohibition.l4` | noSmoking, noDrinking, mayDrink, mustDrink, limitedGambling, noSmokingWithConsequences, noSmokingExplicitBreach, noSmokingBreachByAlice, noSmokingBreachWithReason, noSmokingBreachReasonOnly, breachInHence |
| `jl4/examples/ok/contracts.l4` | aContract, x, y, quux, z, a, goesOn |
| `jl4/experiments/actors.l4` | SeriesAFinancing, SeriesAIssue |
| `jl4/experiments/looping-with-recursion.l4` | InitialSale |
| `jl4/experiments/patterns_and_idioms.l4` | simplePayment, serviceContract, paymentDue, loanAgreement |
| `jl4/experiments/safe-post.l4` | EquityFinancingConversion |
| `jl4/experiments/wedding.l4` | weddingceremony, Spouse1lovesSpouse2, Spouse2lovesSpouse1, Spouse1holdsSpouse2, Spouse2holdsSpouse1, supportinallcircumstances, careinsicknessandhealth, deathreleasesvows, noabandonment, fidelity, marriagecontract |

Command:
```bash
cabal run jl4-cli -- --state-graph <file.l4>
```

## File Formats

Each example is provided in three formats:
- `.dot` - GraphViz DOT source (text)
- `.svg` - Scalable Vector Graphics (for web/docs)
- `.png` - Portable Network Graphics (for embedding)

## Examples

### Simple Prohibition: `noSmoking`

A simple SHANT (MUST NOT) rule:
```
initial --[Alice SHANT smoke]--> Fulfilled
initial --[violation]--> Breach
```

### Chained Obligations: `noSmokingWithConsequences`

Shows a LEST clause creating an intermediate state with a reparation obligation:
```
initial --[Alice SHANT smoke]--> Fulfilled
initial --[timeout]--> "Alice must drink"
"Alice must drink" --[Alice MUST drink]--> Fulfilled
"Alice must drink" --[timeout]--> Breach
```

This demonstrates how regulative rules create state machines with:
- **Terminal states**: Fulfilled (green), Breach (red)
- **Intermediate states**: Reparation obligations
- **Transitions**: Actions with deontic modals and deadlines

### Permission: `mayDrink`

Shows a MAY rule (no failure path):
```
initial --[Bob MAY drink]--> Fulfilled
```

### Obligation: `mustDrink`

Shows a MUST rule with implicit breach on timeout:
```
initial --[Alice MUST drink]--> Fulfilled
initial --[timeout]--> Breach
```

### Conditional: `limitedGambling`

Shows a prohibition with a PROVIDED guard:
```
initial --[Alice SHANT gamble IF amt > 100]--> Fulfilled
initial --[violation]--> Breach
```

### Contract Examples (from `contracts.l4`)

- **`aContract`** - Simple obligation with deadline
- **`goesOn`** - Chain of obligations with HENCE/LEST paths

### Business Transactions (from `patterns_and_idioms.l4`)

- **`simplePayment`** - Basic payment obligation
- **`serviceContract`** - Service delivery with payment
- **`loanAgreement`** - Loan with repayment obligation

### Startup Financing (from `actors.l4`, `safe-post.l4`)

- **`SeriesAFinancing`** / **`SeriesAIssue`** - Investment term sheet obligations
- **`EquityFinancingConversion`** - SAFE conversion rules

### Wedding Vows (from `wedding.l4`)

Traditional wedding vows formalized as L4 regulative rules:

- **`weddingceremony`** - The exchange of vows (chained obligations):
  ```
  initial --[Spouse1 MUST exchange vows]--> "Spouse2 must exchange vows"
          --[Spouse2 MUST exchange vows]--> Fulfilled
  ```
- **`Spouse1lovesSpouse2`** / **`Spouse2lovesSpouse1`** - "To love and to cherish" (mutual)
- **`Spouse1holdsSpouse2`** / **`Spouse2holdsSpouse1`** - "To have and to hold" (mutual)
- **`supportinallcircumstances`** - "For richer or poorer"
- **`careinsicknessandhealth`** - "In sickness and in health"
- **`deathreleasesvows`** - "Till death do us part" (MAY permission)
- **`noabandonment`** / **`fidelity`** - Prohibitions (SHANT)
- **`marriagecontract`** - Combined parallel obligations using RAND

## Visual Conventions

- **Node shapes**:
  - Ellipse: Regular state
  - Double circle: Terminal state (Fulfilled or Breach)

- **Node colors**:
  - Light blue (#e8f4fd): Initial state
  - White (#ffffff): Intermediate state
  - Light green (#d4edda): Fulfilled (terminal success)
  - Light red (#f8d7da): Breach (terminal failure)

- **Edge styles**:
  - Solid green (#28a745): Success/HENCE path
  - Dashed red (#dc3545): Failure/LEST/timeout path

- **Edge labels**: `<party> <MODAL> <action> [<deadline>] [IF <guard>]`

## Rendering

To render DOT files manually:
```bash
# SVG (scalable, good for web)
dot -Tsvg noSmoking.dot -o noSmoking.svg

# PNG (raster, good for embedding)
dot -Tpng noSmoking.dot -o noSmoking.png

# PDF (good for printing)
dot -Tpdf noSmoking.dot -o noSmoking.pdf
```

## See Also

- [Contract as Automaton (Flood & Goodenough)](https://www.financialresearch.gov/working-papers/files/OFRwp-2015-04_Contract-as-Automaton-The-Computational-Representation-of-Financial-Agreements.pdf)
- `doc/regulative-spec.org` - L4 regulative rule specification
- `jl4-core/src/L4/StateGraph.hs` - Implementation
