# L4 Computational Law Plugin for Claude Code

A Claude Code plugin for writing **L4 rules-as-code** — a statically-typed, pure-functional programming language for computational law. Encode legislation, contracts, policies, and regulations as executable, type-checked rules, then deploy them as live REST APIs, MCP tools, and WebMCP agents via [Legalese Cloud](https://legalese.cloud).

## Features

- **L4 authoring skill** — expert guidance for formalising legal text as typed decision functions and regulative rules (`MUST` / `MAY` / `SHANT` / `DO`, with deadlines, `HENCE` / `LEST` consequences, and `BREACH` semantics).
- **Deploy to Legalese Cloud** — annotate functions with `@export` and `@desc`, deploy from the VS Code extension's Deploy tab, and your rules are instantly available as:
  - **REST** endpoints (`POST /deployments/{id}/functions/{fn}/evaluation`)
  - **MCP** JSON-RPC 2.0 tools for LLM agent tool-use (`POST /deployments/{id}/.mcp`)
  - **WebMCP** tools for browser AI agents (`<script src="/.webmcp/embed.js">`)
  - **OpenAPI 3.0** schemas (`GET /deployments/{id}/openapi.json`)
  - **Query planning** for interactive questionnaires that only ask inputs that still matter
  - **Audit-grade execution traces** (`?trace=full&graphviz=true` on any evaluation)
- **Reference documentation** — curated deep-dives on regulative rules, builtin libraries, and traps a general-purpose LLM won't get right, all discoverable from the skill.

## Installation

### Via Marketplace (recommended)

1. Add the Legalese marketplace:
   ```
   /plugin marketplace add legalese/l4-ide
   ```
2. Install the plugin:
   ```
   /plugin install l4-computational-law@legalese
   ```

Once installed, Claude Code auto-loads the `writing-l4-rules` skill whenever the plugin is active.

## Usage

The skill activates when you:

- Ask to formalise legal text, contracts, policies, or regulations
- Work with `.l4` files
- Ask about computational law, obligations/deadlines, or deploying L4 rules as APIs or agent tools

### Example prompts

- "Formalise this insurance policy clause in L4"
- "Draft an L4 rule for a 30-day payment obligation with breach reparation"
- "Model this statute as L4 with `#TRACE` test cases"
- "Export this decision function and deploy it to Legalese Cloud"
- "Turn this eligibility checklist into an MCP tool my agent can call"

## What is L4?

L4 is a statically-typed, pure-functional language designed for legal drafting:

- **Layout-sensitive** like Python
- **Haskell-style algebraic data types** for modelling legal ontologies
- **Backtick identifiers** that read like prose: `` `the applicant qualifies for benefits` ``
- **Regulative rules** with deadlines and reparations: `MUST … WITHIN 30 … HENCE … LEST BREACH`
- **`@export` / `@desc` annotations** that publish typed functions to REST, MCP, and OpenAPI with LLM-friendly parameter descriptions

Canonical documentation lives at <https://legalese.com/l4/>.

### Example: an exported decision function

```l4
@desc Given the day of the week, a flag about whether it's a public holiday, and prevailing weather conditions, calculate the cost of parking.
@export default Calculate the cost of parking
GIVEN
  day_of_week       IS A NUMBER  @desc the day of the week (1 = Monday, ..., 7 = Sunday)
  is_public_holiday IS A BOOLEAN @desc whether it is a public holiday
  current_weather   IS A STRING  @desc Current weather conditions. Possible values are: fair, rain, snow
GIVETH A NUMBER
DECIDE `Parking cost` IS ...
```

The `@desc` on each parameter flows into the generated OpenAPI schema and the MCP tool's `inputSchema`, so downstream LLMs know exactly how to construct a valid call. See the full working file at [.claude/skills/writing-l4-rules/assets/example-parking.l4](.claude/skills/writing-l4-rules/assets/example-parking.l4).

## Deployment with Legalese Cloud

[Legalese Cloud](https://legalese.cloud) is the managed hosting service for L4 rule bundles. One deployment gives you every interface at once — REST, MCP, WebMCP, OpenAPI, query planning, and traces — all backed by the same type-checked source.

Each organisation gets a subdomain (`https://{org-slug}.legalese.cloud`), OAuth-protected endpoints, handled compilation, and one-click deployment from the VS Code extension's Deploy tab. Discovery starts at:

```
https://{org-slug}.legalese.cloud/.well-known/oauth-protected-resource
```

You can also self-host with `jl4-service` — the API surface is identical, so client code is portable between self-hosted and managed deployments.

## Validation

Validate `.l4` files locally with the `l4` CLI:

```bash
# Fast typecheck (use for CI / pre-commit)
l4 check path/to/file.l4

# Full typecheck + evaluate #EVAL directives
l4 run path/to/file.l4

# From a Haskell checkout (no installed binary)
cabal run l4 -- run path/to/file.l4
```

A wrapper script ships inside the skill at [.claude/skills/writing-l4-rules/scripts/validate.sh](.claude/skills/writing-l4-rules/scripts/validate.sh).

## Documentation

Inside the plugin ([.claude/skills/writing-l4-rules/](.claude/skills/writing-l4-rules/)):

- [SKILL.md](.claude/skills/writing-l4-rules/SKILL.md) — operational guide for writing and deploying L4
- [references/regulative.md](.claude/skills/writing-l4-rules/references/regulative.md) — obligations, `#TRACE`, and the `MUST` / `HENCE` / `LEST` / `BREACH` machinery
- [references/builtins.md](.claude/skills/writing-l4-rules/references/builtins.md) — coercions, HTTP/JSON, temporal globals, and the library index
- [references/gotchas.md](.claude/skills/writing-l4-rules/references/gotchas.md) — ditto `^`, asyndetic `...` / `..`, `§` sections, computed fields, `IS` vs `MEANS` vs `IF`, mixfix

External:

- **Start here:** <https://legalese.com/l4/README.md>
- **Glossary:** <https://legalese.com/l4/reference/GLOSSARY.md>
- **Cheat sheet:** <https://legalese.com/l4/reference/cheat-sheet.md>
- **Regulative rules reference:** <https://legalese.com/l4/reference/regulative.md>
- **Libraries:** <https://legalese.com/l4/reference/libraries.md>
- **Legalese Cloud:** <https://legalese.cloud>

## Support

- **Documentation:** <https://legalese.com/l4/>
- **Issues:** <https://github.com/legalese/l4-ide/issues>
- **Email:** hello@legalese.com
- **Website:** <https://legalese.com>

## License

Apache 2.0

## Credits

Developed by Legalese Pte. Ltd. in Singapore
