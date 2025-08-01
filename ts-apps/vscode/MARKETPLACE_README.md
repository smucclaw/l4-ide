# L4 Language Support

Language support for L4 rules-as-code functional programming language with syntax highlighting, IntelliSense, and ladder diagram visualization.

Documentation: https://l4.legalese.com

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

## What is L4?

L4 is a domain-specific language designed for expressing legal logic and rules in a formal, computable way. It allows legal professionals and developers to:

- Write legal rules in a structured, unambiguous format
- Visualize complex legal logic through ladder diagrams
- Validate legal reasoning through formal methods
- Generate executable code from legal specifications

## Getting Started

1. **Install the Extension**: Install this extension from the VS Code marketplace
2. **Open an L4 File**: Create or open a file with the `.l4` extension
3. **Configure the Language Server**: The extension requires the `jl4-lsp` language server

### Language Server Setup

The extension expects to find `jl4-lsp` on your system PATH. You can install it via:

```bash
cabal install exe:jl4-lsp --overwrite-policy=always
```

Alternatively, specify the path manually in VS Code settings:

```json
{
  "jl4.serverExecutablePath": "/path/to/jl4-lsp"
}
```

## Using the Visualization

1. Open an L4 file containing rules
2. Look for "Visualize" or "Simplify and visualize" codelens above L4 expressions
3. Click on the codelens to open the ladder diagram visualization
4. The diagram will update automatically as you edit your code

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

The extension provides several configuration options:

- `jl4.serverExecutablePath`: Path to the jl4-lsp executable
- `jl4.trace.server`: Enable server communication tracing (off/messages/verbose)

## Contributing

This extension is part of the L4 IDE project. For issues, feature requests, or contributions, please visit the project repository.

## License

Apache 2.0 - See license file for details.

## Support

For support and documentation, please refer to the L4 project documentation and community resources.
