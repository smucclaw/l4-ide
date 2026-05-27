# L4 — Rules as Code, in VS Code

**Write, test, and deploy contracts and legal rules as executable code.**

The L4 extension turns VS Code into a workbench for computational law. Encode contracts, regulations, and policy logic as typed, API calls, AI skills and MCPs — backed by a built-in Legalese AI coding assistant, live ladder-diagram visualisations of decision logic, and one-click deployment to Legalese Cloud.

[**Documentation →**](https://legalese.com/l4) &nbsp;·&nbsp; [Foundation Course](https://legalese.com/l4/courses/foundation) &nbsp;·&nbsp; [Legalese Cloud](https://legalese.cloud)

---

## Why L4?

Legal text is full of conditions, exceptions, and cross-references. L4 lets you encode that logic as code a computer — or a contract counterparty — can actually execute, with the rigour of a typed functional language and the readability of a structured legal clause.

This extension is the fastest way to write L4 rules, see what they decide, and ship them as APIs your team and your AI agents can call.

## What you get

### Legalese AI, in the sidebar

A built-in chat that knows L4 inside and out. Describe a clause, paste a statute, or ask for a refactor — Legalese AI drafts, validates, and deploys rules with you, driving the filesystem, language server, and MCP tools directly.

### See your decisions, not just your code

Click _Show decision graph_ above any rule and open an interactive ladder diagram that updates as you type. Spot logic errors, walk a stakeholder through an outcome, or just make sense of your own rules at a glance.

### Live result inspector

Annotate rules with `#EVAL`, `#EVALTRACE`, `#CHECK`, `#TRACE`, or `#ASSERT` and watch results update live as you edit. Click _Track result_ in the codelens to pin it to the Inspector tab — no re-running, no extra terminal.

### One-click cloud deploy

Mark a function with `@export`, then deploy from the sidebar. Every deployment becomes a REST API with OpenAPI spec, and an AI skill or MCP tool — automatically. Breaking-change detection warns you before you ship an incompatible update.

### Your rules, called by any AI agent

Deployed rules show up as MCP tools that Claude, Cursor, Copilot, and other agents can discover and call. Drop one `<script>` tag onto a webpage and browser-based AI agents can use them too. ([MCP & WebMCP tutorials →](https://legalese.com/l4/tutorials))

### Rich language support

Syntax highlighting, IntelliSense, error diagnostics, go-to-definition, symbol search, hover docs — everything you expect from a modern language extension, powered by the bundled `jl4-lsp` language server.

### Docs in the sidebar

Browse and search the L4 docs without leaving the editor. _Copy into new file_ drops example snippets straight into your project.

---

## Get started

1. **Install** the extension from the VS Code Marketplace.
2. **Open** a file with the `.l4` extension — the language server starts automatically on platform builds.
3. **Hit the L4 icon** in the activity bar and ask Legalese AI to draft your first rule.

New to L4? Start with the [Foundation Course](https://legalese.com/l4/courses/foundation) or the [Getting Started tutorial](https://legalese.com/l4/tutorials).

## A taste of L4

```l4
DECLARE Person
  HAS name    IS A STRING
      age     IS A NUMBER
      country IS A STRING

GIVEN p IS A Person
GIVETH A BOOLEAN
DECIDE `is adult` p IF p's age >= 18

GIVEN p IS A Person
GIVETH A BOOLEAN
DECIDE `can vote` p IF
  `is adult` p
  AND p's country = "UK"
```

The [Syntax reference](https://legalese.com/l4/reference/syntax) has the full language tour.

## Learn more

- [**L4 Documentation**](https://legalese.com/l4) — courses, tutorials, language reference
- [Foundation Course](https://legalese.com/l4/courses/foundation) and [Advanced Course](https://legalese.com/l4/courses/advanced)
- [Reference: Syntax](https://legalese.com/l4/reference/syntax), [Types](https://legalese.com/l4/reference/types), [Functions](https://legalese.com/l4/reference/functions), [Regulative rules](https://legalese.com/l4/reference/regulative), [Built-ins](https://legalese.com/l4/reference/builtins)
- [Legalese Cloud](https://legalese.cloud) — managed deployment for L4 rules

---

<details>
<summary><strong>Technical reference</strong> — sidebar tabs, MCP, WebMCP, CLI, settings</summary>

### Sidebar tabs

The L4 sidebar (L4 icon in the activity bar) has five tabs:

- **Legalese AI** — in-editor chat for composing and iterating on L4 rules. The assistant can drive the filesystem, LSP, and MCP tools directly.
- **Docs** — searchable L4 documentation. _Copy into new file_ buttons paste examples into new L4 files.
- **Inspector** — live evaluation results for `#EVAL`, `#EVALTRACE`, `#CHECK`, `#TRACE`, and `#ASSERT` directives. Click _Track result_ above a directive to add it.
- **Deploy** — exported functions from the active file with their parameter schemas; click _Deploy_ to ship.
- **Deployments** — active deployments with their function schemas ; undeploy with confirmation.

### Decision graph

Open an L4 file with boolean rules, look for the _Show decision graph_ codelens above each rule, and click it to open the live ladder-diagram visualisation.

### MCP — AI agent integration

The extension starts a local MCP proxy server automatically and registers it with VS Code's built-in MCP system (default port `19415`, configurable via `jl4.mcpPort`). Legalese AI and any external MCP-speaking agent (Cursor, GitHub Copilot, Claude Code, third-party clients) can discover and invoke your deployed L4 functions through it.

The _Add L4 Tools to Claude Code_ dropdown action in the sidebar wires the same MCP endpoint plus the `writing-l4-rules` skill into a local Claude Code installation on demand.

Deployed rules are also accessible via MCP endpoints on the service:

- `POST /.mcp` — all deployments (org-wide)
- `POST /{deployment-id}/.mcp` — scoped to one deployment
- `GET /.well-known/mcp` — MCP discovery metadata

Example MCP client config:

```json
{
  "mcpServers": {
    "l4-rules": {
      "url": "https://your-service/.mcp",
      "headers": {
        "Authorization": "Bearer sk_..."
      }
    }
  }
}
```

### WebMCP — browser AI integration

Add this script tag to your page:

```html
<script src="https://your-service/.webmcp/embed.js"></script>
```

The script registers your L4 functions with the browser's `navigator.modelContext` API so browser AI agents can call them as tools. Customise via data attributes:

```html
<script
  src="https://your-service/.webmcp/embed.js"
  data-scope="insurance-premium/*,safe-valuation/effective-sale-price"
  data-tools="auto"
  data-api-key="sk_..."
></script>
```

| Attribute      | Description                                                                                       |
| -------------- | ------------------------------------------------------------------------------------------------- |
| `data-scope`   | Filter by deployment/function (e.g. `deploy/*` for all functions, `deploy/fn` for a specific one) |
| `data-tools`   | Comma-separated tool categories: `auto` (default), `rules`, `rule-tools`, `file-tools`, or `all`  |
| `data-api-key` | API key for authenticated access                                                                  |

`data-tools` categories:

- `rules` — one tool per exported rule (direct evaluation)
- `rule-tools` — `search_rules`, `get_rule_schema`, `evaluate_rule` (discovery)
- `file-tools` — `list_files`, `read_file`, `search_identifier`, `search_text`
- `auto` (default) — `rules` if ≤10 functions, otherwise `rule-tools`
- `all` — `rules` + `rule-tools` + `file-tools`

### Cloud Deploy setup

**Legalese Cloud:** no configuration needed. Click _Sign in with Legalese Cloud_ in the sidebar footer to authenticate via browser. (Service provided by Legalese Pte. Ltd. — see [legalese.cloud](https://legalese.cloud).)

**Self-hosted jl4-service:** configure the service URL and API key in VS Code settings:

```json
{
  "jl4.serviceUrl": "http://localhost:8080",
  "jl4.serviceApiKey": "sk_..."
}
```

### L4 CLI

The extension ships with the `l4` command-line tool for running L4 files, checking rules, and other workflows outside the editor.

Run **L4: Install L4 CLI** from the command palette, or accept the prompt the first time you open an L4 project. This installs the bundled `l4` binary onto your PATH:

- **macOS / Linux**: symlinks the binary into `~/.local/bin/l4`. If `~/.local/bin` isn't on your PATH, the extension shows the `export PATH="$HOME/.local/bin:$PATH"` snippet to add to your `~/.zshrc` or `~/.bashrc`.
- **Windows**: copies the exe into `%LOCALAPPDATA%\Programs\l4\l4.exe` and appends that directory to your user PATH via `setx`. Restart open terminals for the change to take effect.

The installer is safe to re-run — each invocation refreshes the symlink or file, so upgrades propagate when you update the extension. The CLI is bundled only in platform-specific builds (macOS ARM64/x64, Linux x64, Windows x64). On the universal build, install manually with `cabal install exe:l4 --overwrite-policy=always`.

Run `l4 --help` for available subcommands.

### Requirements

- VS Code 1.94.0 or higher
- `jl4-lsp` language server (bundled in platform-specific builds)

### Configuration

| Setting                    | Purpose                                                                        |
| -------------------------- | ------------------------------------------------------------------------------ |
| `jl4.serverExecutablePath` | Path to the jl4-lsp executable                                                 |
| `jl4.trace.server`         | Enable server communication tracing (`off`/`messages`/`verbose`)               |
| `jl4.serviceUrl`           | URL of the jl4-service for deploying rules (leave empty for Legalese Cloud)    |
| `jl4.serviceApiKey`        | API key for authenticating with a self-hosted jl4-service                      |
| `jl4.mcpPort`              | Port for the local MCP proxy server (default: `19415`)                         |
| `legaleseAi.apiKey`        | API key for Legalese AI (used instead of your Legalese Cloud session when set) |

### Custom language server setup

Platform-specific extension builds (macOS ARM64, Windows x64, etc.) bundle `jl4-lsp` and need no setup. On the universal build, install the language server manually:

```bash
cabal install exe:jl4-lsp --overwrite-policy=always
```

Or specify the path manually:

```json
{
  "jl4.serverExecutablePath": "/path/to/jl4-lsp"
}
```

</details>

---

## License

Apache 2.0 — see license file for details.
