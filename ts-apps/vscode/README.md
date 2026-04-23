# L4 Rules-as-code Language Support and Tools

Language support for L4 rules-as-code with a built-in Legalese AI chat, MCP for AI agents like Claude Code and Copilot, WebMCP for browser AI, syntax highlighting, IntelliSense, CLI, decision graph visualizations, result inspection, and Legalese Cloud deployment.

Documentation: https://legalese.com/l4

## Features

- **Legalese AI**: Built-in chat tab for composing and iterating on L4 rules with an assistant that knows L4 and can drive the fs / LSP / MCP tool set directly.
- **MCP Server (Model Context Protocol)**: Expose deployed L4 rules as tools that any AI agent can discover and call
  - Built-in MCP proxy server registers automatically with VS Code
  - Legalese AI (in-editor) and any MCP-speaking agent (Cursor, Copilot, Claude Code, third parties) can discover and invoke your L4 functions as structured tools
  - Scoped access — expose all rules org-wide or restrict to specific deployments
  - Authenticated via Legalese Cloud or API key for self-hosted setups
- **WebMCP — Browser AI Integration**: Let browser-based AI agents call your L4 rules from any web page
  - Embed a single `<script>` tag to register L4 functions with `navigator.modelContext`
  - Browser AI agents discover and call your rules without a backend integration
  - Configurable scope, tool registration mode, and authentication via data attributes
- **Cloud Deploy**: Deploy L4 rules as REST APIs from the sidebar
  - Deploy Preview tab shows exported functions with parameter schemas
  - Breaking change detection when updating existing deployments
  - Deployments tab lists active deployments with full function metadata
  - Supports Legalese Cloud (browser login) and self-hosted jl4-service (API key)
- **Syntax Highlighting**: Full support for L4 syntax with proper color coding
- **Language Server Protocol (LSP)**: Advanced language features including:
  - IntelliSense and autocompletion
  - Error detection and diagnostics
  - Code navigation
  - Symbol search
- **Ladder Diagram Visualization**: Interactive decision graph of L4 rules as ladder diagrams
  - Real-time updates as you edit your code
  - Visual representation of logical relationships
  - Interactive exploration of rule structures
- **Result Inspector**: Live evaluation results for `#EVAL`, `#EVALTRACE`, `#CHECK`, and `#ASSERT` directives
  - Click "Track result" above a directive to add it to the inspector
  - Results update automatically as you edit your code
  - Collapsible sections grouped by file
  - Syntax-highlighted output

## What is L4?

L4 is a domain-specific language designed for expressing legal logic and rules in a formal, computable way. It allows legal professionals and developers to:

- Write legal rules in a structured, unambiguous format
- Visualize complex legal logic through ladder diagrams
- Validate legal reasoning through formal methods
- Deploy rules as REST APIs for integration with other systems
- Expose rules to AI agents via MCP and browser AI via WebMCP

## Getting Started

1. **Install the Extension**: Install this extension from the VS Code marketplace
2. **Open an L4 File**: Create or open a file with the `.l4` extension
3. **Configure the Language Server**: The extension requires the `jl4-lsp` language server

## Using the Sidebar

The L4 sidebar is accessible via the L4 icon in the activity bar. It has three tabs:

### Result Inspector

- Open an L4 file with `#EVAL`, `#CHECK`, or `#ASSERT` directives
- Click "Track result" in the CodeLens above a directive
- The sidebar switches to the Result Inspector tab and shows the evaluation result
- Results update live as you edit code

### Deploy Preview

- Shows exported functions from the active L4 file with their parameter schemas, types, and descriptions
- Updates automatically as you edit
- Click "Deploy" to start the deployment flow

### Deployments

- Lists active deployments with their functions (sourced from the service's OpenAPI spec)
- Expand a deployment to see its function schemas
- Undeploy with confirmation

## Using the L4 Decision Graph

1. Open an L4 file containing rules
2. Look for the "Show decision graph" codelens above L4 rules with boolean returns
3. Click on the codelens to open the ladder diagram visualization
4. The diagram will update automatically as you edit your code

## MCP — AI Agent Integration

Once you deploy L4 rules, they are automatically available as MCP tools that AI agents can discover and call.

### Local MCP Proxy (VS Code)

The extension starts a local MCP proxy server automatically. Legalese AI (built in), along with any external MCP-speaking agent (Cursor, GitHub Copilot, Claude Code, third-party clients), can connect to it to discover and invoke your deployed L4 functions.

The proxy registers with VS Code's built-in MCP system. The default port is `19415` (configurable via `jl4.mcpPort`). The "Add L4 Tools to Claude Code" dropdown action in the sidebar wires the same MCP endpoint plus the writing-l4-rules skill into a local Claude Code installation on demand.

### Remote MCP Endpoints

Deployed rules are also accessible via MCP endpoints on the service:

- `POST /.mcp` — all deployments (org-wide)
- `POST /{deployment-id}/.mcp` — scoped to one deployment
- `GET /.well-known/mcp` — MCP discovery metadata

Example MCP client config (e.g. for Claude Desktop):

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

## WebMCP — Browser AI Integration

WebMCP lets browser-based AI agents (like Claude on the web) discover and call your L4 rules directly from any web page.

Add this script tag to your page:

```html
<script src="https://your-service/.webmcp/embed.js"></script>
```

The script registers your L4 functions with the browser's `navigator.modelContext` API. Browser AI agents can then see and call them as tools.

### Configuration

Use data attributes to customize behavior:

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
| `data-tools`   | Registration mode: `auto` (default), `discovery`, `direct`, or `all`                              |
| `data-api-key` | API key for authenticated access                                                                  |

## Cloud Deploy Setup

### Legalese Cloud

No configuration needed. Click "Sign in with Legalese Cloud" in the sidebar footer to authenticate via browser.

Legalese Cloud is a service provided by Legalese Pte. Ltd.
More info at https://legalese.cloud

### Self-hosted jl4-service

Configure the service URL and API key in VS Code settings:

```json
{
  "jl4.serviceUrl": "http://localhost:8080",
  "jl4.serviceApiKey": "sk_..."
}
```

## L4 CLI

The extension ships with the `l4` command-line tool for running L4 files, checking rules, and other workflows outside the editor.

### Installing

Run **L4: Install L4 CLI** from the command palette, or accept the prompt the first time you open an L4 project. This installs the bundled `l4` binary onto your PATH:

- **macOS / Linux**: symlinks the binary into `~/.local/bin/l4`. If `~/.local/bin` isn't on your PATH, the extension shows the `export PATH="$HOME/.local/bin:$PATH"` snippet to add to your `~/.zshrc` or `~/.bashrc`.
- **Windows**: copies the exe into `%LOCALAPPDATA%\Programs\l4\l4.exe` and appends that directory to your user PATH via `setx`. Restart open terminals for the change to take effect.

The installer is safe to re-run — each invocation refreshes the symlink or file, so upgrades propagate when you update the extension.

### Requirements

The CLI is bundled only in platform-specific builds of the extension (macOS ARM64/x64, Linux x64, Windows x64). If you installed the universal build, install `l4` manually via `cabal install exe:l4 --overwrite-policy=always`.

### Usage

Once installed, run `l4 --help` in a new terminal to see available subcommands.

## Example L4 Code

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

## Requirements

- VS Code 1.94.0 or higher
- `jl4-lsp` language server (see setup instructions above)

## Configuration

- `jl4.serverExecutablePath`: Path to the jl4-lsp executable
- `jl4.trace.server`: Enable server communication tracing (off/messages/verbose)
- `jl4.serviceUrl`: URL of the jl4-service for deploying rules (leave empty for Legalese Cloud)
- `jl4.serviceApiKey`: API key for authenticating with a self-hosted jl4-service
- `jl4.mcpPort`: Port for the local MCP proxy server (default: 19415)

### Custom Language Server Setup

**Platform-specific versions (recommended):** If you installed a platform-specific version of the extension (e.g., for macOS ARM64, Windows x64, etc.), the `jl4-lsp` language server is bundled and ready to use—no additional setup required!

**Universal version:** If you installed the universal extension, or if the bundled binary is not available for your platform, install the language server:

```bash
cabal install exe:jl4-lsp --overwrite-policy=always
```

Alternatively, specify the path manually in VS Code settings:

```json
{
  "jl4.serverExecutablePath": "/path/to/jl4-lsp"
}
```

## License

Apache 2.0 - See license file for details.
