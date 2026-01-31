# jl4-actus-analyzer

A static analyzer that classifies L4 contract encodings by ACTUS contract type and FIBO financial ontology class.

## Overview

This tool consumes FIBO/ACTUS RDF ontologies directly and uses pattern-based scoring to classify L4 contracts. It extracts semantic features from L4 AST (type declarations, deontic rules, state machines) and matches them against known financial contract patterns.

## Installation

```bash
# Build the analyzer
cabal build jl4-actus-analyzer

# Install the CLI tool
cabal install exe:jl4-actus --overwrite-policy=always
```

## Usage

### Command Line

```bash
# Analyze a single L4 file (default: Markdown output)
jl4-actus contract.l4

# Analyze multiple files together
jl4-actus file1.l4 file2.l4 file3.l4

# Output as JSON
jl4-actus --json contract.l4

# Output as RDF/Turtle
jl4-actus --rdf contract.l4

# Specify custom FIBO repository path
jl4-actus --fibo ~/src/edmcouncil/fibo contract.l4

# Disable ontology caching (slower but always fresh)
jl4-actus --no-cache contract.l4

# Rebuild the ontology cache
jl4-actus --rebuild-cache

# Set minimum confidence threshold (default: 0.3)
jl4-actus --min-confidence 0.5 contract.l4
```

### Programmatic API

```haskell
import L4.ACTUS.Analyzer

main :: IO ()
main = do
  result <- analyzeFile defaultConfig "contract.l4"
  case result of
    Left err -> print err
    Right classification -> do
      print classification.primaryActusType      -- e.g., Just "FXOUT"
      print classification.primaryConfidence     -- e.g., 0.87
      print classification.primaryFiboClass      -- FIBO URI
```

## Supported Contract Types

### ACTUS Types with Matching Rules

| ACTUS Code          | Description                  | Key Features Detected                                             |
| ------------------- | ---------------------------- | ----------------------------------------------------------------- |
| **FXOUT**           | Foreign Exchange Outright    | Currency types, two-currency exchange, value date, settlement     |
| **SWAPS**           | Plain Vanilla Swap           | Payment legs, notional amount, payment schedule                   |
| **OPTNS**           | Options                      | Strike price, expiration date, underlying asset, premium          |
| **PAM**             | Principal at Maturity        | Principal, interest rate, maturity date                           |
| **FUTUR**           | Futures                      | Contract size, settlement date, underlying, margin                |
| **MasterAgreement** | Master Agreement (container) | Schedule structure, multiple transaction types, close-out netting |

### ACTUS-to-FIBO Mappings

The analyzer maps ACTUS types to their corresponding FIBO ontology classes:

| ACTUS Code      | FIBO Class                                         |
| --------------- | -------------------------------------------------- |
| FXOUT           | `fibo-der-drc-cur:ForeignExchangeForwardAgreement` |
| SWAPS           | `fibo-der-drc-swp:Swap`                            |
| OPTNS           | `fibo-der-drc-opt:Option`                          |
| PAM             | `fibo-sec-dbt-di:DebtInstrument`                   |
| ANN             | `fibo-sec-dbt-di:DebtInstrument`                   |
| LAM             | `fibo-sec-dbt-di:DebtInstrument`                   |
| NAM             | `fibo-sec-dbt-di:DebtInstrument`                   |
| FUTUR           | `fibo-fbc-fi-fi:Future`                            |
| STK             | `fibo-sec-eq-ei:Share`                             |
| CDSWP           | `fibo-der-cr-cds:CreditDefaultSwap`                |
| TRSWP           | `fibo-der-drc-swp:TotalReturnSwap`                 |
| CEG             | `fibo-fbc-dae-dbt:CreditEnhancementAgreement`      |
| CEC             | `fibo-fbc-pas-caa:CollateralAccount`               |
| MasterAgreement | `fibo-fnd-agr-ctr:MasterAgreement`                 |

## Architecture

```
┌─────────────────────┐     ┌──────────────────────┐
│   L4 Source Files   │     │   FIBO/ACTUS RDF     │
│   (*.l4)            │     │   (ontology files)   │
└─────────┬───────────┘     └──────────┬───────────┘
          │                            │
          v                            v
┌─────────────────────┐     ┌──────────────────────┐
│   jl4-core Parser   │     │   RDF Loader (rdf4h) │
│   + TypeChecker     │     │   + Binary Cache     │
└─────────┬───────────┘     └──────────┬───────────┘
          │                            │
          v                            v
┌─────────────────────────────────────────────────┐
│              L4.ACTUS.Analyzer                  │
│  ┌─────────────────┐  ┌─────────────────────┐   │
│  │FeatureExtractor │  │  OntologyMatcher    │   │
│  │- Type patterns  │  │- Pattern rules      │   │
│  │- Deontic rules  │  │- Confidence scoring │   │
│  │- State machine  │  │- FIBO mapping       │   │
│  └────────┬────────┘  └──────────┬──────────┘   │
│           └──────────┬───────────┘              │
│                      v                          │
│           ┌──────────────────┐                  │
│           │ ClassificationResult               │
│           └──────────────────┘                  │
└─────────────────────┬───────────────────────────┘
                      v
        ┌──────────────────────────┐
        │  Output (JSON/RDF/MD)    │
        └──────────────────────────┘
```

## Feature Extraction

The analyzer extracts the following semantic features from L4 AST:

### Domain Indicators

| Indicator                  | What It Detects                                     |
| -------------------------- | --------------------------------------------------- |
| `CurrencyIndicator`        | Enum types with currency codes (USD, EUR, etc.)     |
| `FXTransactionIndicator`   | Records with two currency fields, value date        |
| `SwapIndicator`            | Leg structures, notional amounts, payment schedules |
| `OptionIndicator`          | Strike price, expiry date, underlying asset         |
| `LoanIndicator`            | Principal, interest rate, amortization schedule     |
| `SettlementIndicator`      | Netting provisions, delivery obligations            |
| `CreditSupportIndicator`   | Guaranty, collateral, margin requirements           |
| `MasterAgreementIndicator` | Schedule structure, close-out netting               |

### Deontic Patterns

Detects MUST/MAY/SHANT obligations from L4 regulative rules:

- Party roles (obligor, beneficiary)
- Action types (deliver, pay, exercise)
- Consequences (HENCE/LEST clauses)

### State Transitions

Extracts state machine patterns from HENCE/LEST clauses:

- Breach detection
- State progression
- Trigger conditions

### Type Patterns

Classifies L4 type declarations:

- `CurrencyEnumType` - Currency code enumerations
- `AmountRecordType` - Value + currency records
- `TransactionRecordType` - Transaction with parties, amounts, dates
- `PartyRecordType` - Party/counterparty definitions
- `ObligationRecordType` - Duty/obligation records
- `SettlementRecordType` - Settlement/netting records

## Confidence Scoring

Each rule has weighted matchers that contribute to a total confidence score:

```haskell
-- Example: FX Outright scoring
fxOutrightRule = MatchingRule
  { ruleActusType = "FXOUT"
  , ruleMatchers =
      [ FeatureMatcher "Currency types"           0.25 hasCurrencyTypes
      , FeatureMatcher "FX transaction type"      0.20 hasFXTransactionType
      , FeatureMatcher "Two currency exchange"    0.15 hasTwoCurrencyExchange
      , FeatureMatcher "Value date"               0.15 hasValueDateFeature
      , FeatureMatcher "Settlement provisions"    0.10 hasSettlementProvisions
      , FeatureMatcher "Bilateral parties"        0.10 hasBilateralParties
      , FeatureMatcher "Master agreement"         0.05 hasMasterAgreementStructure
      ]
  , ruleMinConfidence = 0.3
  }
```

- Scores are normalized to 0.0 - 1.0
- Matches below `minConfidence` threshold are not reported
- Primary classification is the highest-scoring match

## Container/Contained Classification

For master agreements that govern multiple transaction types, the analyzer reports:

1. **Container Type** - The master agreement itself (e.g., ISDA, IFEMA)
2. **Contained Types** - Transaction types governed by the agreement

Example output:

```json
{
  "container": {
    "type": "MasterAgreement",
    "fiboClass": "fibo-fnd-agr-ctr:MasterAgreement"
  },
  "containedTypes": [
    { "actusType": "FXOUT", "confidence": 0.87 },
    { "actusType": "SWAPS", "confidence": 0.42 }
  ]
}
```

## Extending the Analyzer

### Adding a New Contract Type

1. **Add domain indicator** (if new patterns needed) in `FeatureExtractor.hs`:

```haskell
data DomainIndicator
  = ...
  | NewContractInd NewContractIndicatorData

data NewContractIndicatorData = NewContractIndicatorData
  { ncHasFeatureX :: Bool
  , ncHasFeatureY :: Bool
  , ncLocation :: Maybe SourceLocation
  }
```

2. **Add detection logic** in `extractDomainIndicators`:

```haskell
detectRecordIndicator name fields
  | isNewContractName name =
      Just $ NewContractInd NewContractIndicatorData
        { ncHasFeatureX = hasFieldNamed ["featureX"] fields
        , ncHasFeatureY = hasFieldNamed ["featureY"] fields
        , ncLocation = Nothing
        }
```

3. **Add matching rule** in `Rules.hs`:

```haskell
newContractRule :: MatchingRule
newContractRule = MatchingRule
  { ruleActusType = "NEWCT"
  , ruleMatchers =
      [ FeatureMatcher "Feature X" 0.30 hasFeatureX
      , FeatureMatcher "Feature Y" 0.25 hasFeatureY
      -- ... more matchers
      ]
  , ruleMinConfidence = 0.3
  }

-- Add to defaultRules list
defaultRules = [..., newContractRule]
```

4. **Add FIBO mapping** in `ACTUS.hs`:

```haskell
actusToFIBOMapping = Map.fromList
  [ ...
  , ("NEWCT", mkURI "https://spec.edmcouncil.org/fibo/ontology/.../NewContract")
  ]
```

### Future: Ontology-Driven Rule Generation

The `buildRulesFromOntology` function is designed to eventually parse ACTUS coverage descriptions directly from RDF to generate matching rules automatically. Currently it returns `defaultRules`, but the infrastructure is in place for dynamic rule generation.

## Ontology Cache

The analyzer caches parsed ontology data for faster subsequent runs:

- **Cache location:** `.cache/jl4-actus/ontology.bin`
- **Cache invalidation:** Automatic when source RDF files change
- **Manual rebuild:** `jl4-actus --rebuild-cache`

## Dependencies

- **jl4-core** - L4 parser and type checker
- **rdf4h** - RDF parsing library with proper RDF/XML support
- **aeson** - JSON serialization
- **binary** - Binary cache serialization
- **optparse-applicative** - CLI argument parsing

## Testing

```bash
# Run all tests
cabal test jl4-actus-analyzer

# Tests include:
# - Feature extraction unit tests
# - Matching rule tests
# - FIBO mapping tests
# - Confidence scoring tests
# - Container/contained detection tests
```

## Example Output

### Markdown (default)

```markdown
# ACTUS Classification Report

## Primary Classification

| Property   | Value                                              |
| ---------- | -------------------------------------------------- |
| ACTUS Type | **FXOUT**                                          |
| Label      | foreign exchange outright                          |
| Confidence | 100.0%                                             |
| FIBO Class | `fibo-der-drc-cur:ForeignExchangeForwardAgreement` |

## Supporting Evidence

| Feature               | Weight |
| --------------------- | ------ |
| Currency types        | 25.0%  |
| FX transaction type   | 20.0%  |
| Two currency exchange | 15.0%  |

...
```

### JSON

```json
{
  "primaryClassification": {
    "actusType": "FXOUT",
    "actusLabel": "foreign exchange outright",
    "confidence": 1.0,
    "fiboClass": "https://spec.edmcouncil.org/fibo/..."
  },
  "containerType": "MasterAgreement",
  "containedTypes": [...],
  "evidence": [...]
}
```

## Related Resources

- [ACTUS Algorithmic Contract Types](https://www.actusfrf.org/)
- [FIBO Financial Industry Business Ontology](https://spec.edmcouncil.org/fibo/)
- [L4 Language Documentation](../doc/README.md)
