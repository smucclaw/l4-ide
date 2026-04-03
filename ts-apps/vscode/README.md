# L4 Rules-as-code Language Support and Tools

Language support for L4 rules-as-code with syntax highlighting, IntelliSense, ladder diagram visualization, result inspection, and cloud deployment.

Documentation: https://legalese.com/l4

## Features

- **Syntax Highlighting**: Full support for L4 syntax with proper color coding
- **Language Server Protocol (LSP)**: Advanced language features including:
  - IntelliSense and autocompletion
  - Error detection and diagnostics
  - Code navigation
  - Symbol search
- **Ladder Diagram Visualization**: Interactive visualization of L4 rules as ladder diagrams
  - Real-time updates as you edit your code
  - Visual representation of logical relationships
  - Interactive exploration of rule structures
- **Result Inspector**: Live evaluation results for `#EVAL`, `#EVALTRACE`, `#CHECK`, and `#ASSERT` directives
  - Click "Track result" above a directive to add it to the inspector
  - Results update automatically as you edit your code
  - Collapsible sections grouped by file
  - Syntax-highlighted output
- **Cloud Deploy**: Deploy L4 rules as REST APIs from the sidebar
  - Deploy Preview tab shows exported functions with parameter schemas
  - Breaking change detection when updating existing deployments
  - Deployments tab lists active deployments with full function metadata
  - Supports Legalese Cloud (browser login) and self-hosted jl4-service (API key)

## What is L4?

L4 is a domain-specific language designed for expressing legal logic and rules in a formal, computable way. It allows legal professionals and developers to:

- Write legal rules in a structured, unambiguous format
- Visualize complex legal logic through ladder diagrams
- Validate legal reasoning through formal methods
- Deploy rules as REST APIs for integration with other systems

## Getting Started

1. **Install the Extension**: Install this extension from the VS Code marketplace
2. **Open an L4 File**: Create or open a file with the `.l4` extension
3. **Configure the Language Server**: The extension requires the `jl4-lsp` language server

### Language Server Setup

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

## Using the Visualization

1. Open an L4 file containing rules
2. Look for "Visualize" or "Simplify and visualize" codelens above L4 expressions
3. Click on the codelens to open the ladder diagram visualization
4. The diagram will update automatically as you edit your code

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

## Contributing

This extension is part of the L4 IDE project. For issues, feature requests, or contributions, please visit the project repository.

## License

Apache 2.0 - See license file for details.

## Support

For support and documentation, please refer to the L4 project documentation and community resources.
