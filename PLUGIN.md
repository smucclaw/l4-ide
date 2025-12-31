# L4 Computational Law Plugin for Claude Code

A Claude Code plugin that provides comprehensive support for L4, the programming language for computational law. Write legal rules, contracts, and regulations as executable, type-checked, formally verifiable code.

## Features

- **L4 Language Skill**: Expert guidance for encoding legal logic in L4
- **LSP Integration**: Real-time code intelligence via cloud-based Language Server
  - Syntax highlighting and validation
  - Type checking and error diagnostics
  - Code completion and navigation
- **Cloud Validation**: Validate L4 code against the production LSP server at `wss://jl4.legalese.com/lsp`
- **Comprehensive Documentation**: Syntax references, workflow guides, and examples

## Installation

### Via Marketplace (Recommended)

1. Add the Legalese marketplace:

   ```
   /plugin marketplace add legalese/l4-ide
   ```

2. Install the L4 plugin:
   ```
   /plugin install l4-computational-law@legalese
   ```

### Manual Installation

1. Download the latest `l4.skill` from [GitHub Releases](https://github.com/legalese/l4-ide/releases)
2. Extract to your Claude Code skills directory:
   - Personal: `~/.claude/skills/l4/`
   - Project: `.claude/skills/l4/`

## Usage

Once installed, the L4 skill automatically activates when you:

- Mention L4, legal rules, contracts, or regulations
- Work with `.l4` or `.yaml` files containing L4 code
- Ask about computational law or formal legal specification

### Example Prompts

- "Help me encode this insurance policy in L4"
- "Write an L4 program for this contract clause"
- "Validate my L4 code for syntax errors"
- "Generate a web app from this L4 rule"

## Access Tiers

The L4 cloud services operate on a tiered access model:

### Free Tier

- Cloud validation via LSP WebSocket
- Basic type checking and syntax validation
- Community support via GitHub issues
- Fair use limits apply

### Premium Tier (Coming Soon)

- Higher API rate limits for intensive validation
- Priority support
- Advanced features:
  - Formal verification
  - Test case generation
  - Multi-file project analysis
- Commercial use license

Authentication and billing are handled transparently when connecting to cloud services. Contact hello@legalese.com for premium tier access.

## Documentation

- **Syntax Quick Reference**: `l4/references/syntax-quick-ref.md`
- **Workflow Guide**: `l4/references/workflow-guide.md`
- **GitHub Resources**: `l4/references/github-resources.md`
- **Complete Skill Guide**: `l4/SKILL.md`

## Cloud Validation

The plugin connects to the L4 Language Server at `wss://jl4.legalese.com/lsp` for:

- Real-time syntax validation
- Type checking
- Semantic analysis
- Code intelligence features

No local Haskell toolchain required! Validation runs entirely in the cloud.

### Validation Scripts

For standalone validation outside Claude Code:

```bash
# Validate an L4 file via WebSocket
cd l4/scripts
npm install
node validate-cloud.mjs ../path/to/your/file.l4
```

## What is L4?

L4 is a statically-typed functional programming language inspired by Haskell, designed specifically for encoding legal and regulatory logic. It enables:

- **Isomorphic formalization**: Map legal text structure directly to code
- **Type safety**: Catch logical errors at compile time
- **Formal verification**: Prove properties about legal rules
- **Executable specifications**: Generate web apps and decision services from legal code
- **Traceable reasoning**: Audit-grade execution traces with citations back to source rules

## Examples

### Simple Contract Rule

```l4
DECLARE PolicyStatus IS ONE OF
    Active
    Suspended
    Cancelled

DECIDE eligibleForClaim
    GIVEN policy HAS status IS A PolicyStatus
    IF policy's status IS Active
    THEN True
    ELSE False
```

### Insurance Premium Calculation

```l4
DEFINE calculatePremium
    GIVEN
        driver HAS
            age IS A NUMBER
            yearsLicensed IS A NUMBER
            accidentCount IS A NUMBER
    RETURN A NUMBER
    AS
        LET baseRate IS 1000
        LET ageFactor IS IF driver's age < 25 THEN 1.5 ELSE 1.0
        LET experienceFactor IS IF driver's yearsLicensed < 3 THEN 1.3 ELSE 1.0
        LET accidentPenalty IS driver's accidentCount * 200
        baseRate * ageFactor * experienceFactor + accidentPenalty
```

## Support

- **Documentation**: https://github.com/legalese/l4-ide
- **Issues**: https://github.com/legalese/l4-ide/issues
- **Email**: hello@legalese.com
- **Website**: https://legalese.com

## License

BSD-3-Clause

## Credits

Developed by Legalese
