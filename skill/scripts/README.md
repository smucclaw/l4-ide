# L4 Skill Scripts

This directory contains support scripts for the L4 skill.

## Cloud Validation (validate-cloud.mjs)

Validates L4 code using the cloud LSP service, without requiring local jl4-cli installation.

### Features

- Connects to `wss://jl4.legalese.com/lsp` (production L4 language server)
- Uses LSP (Language Server Protocol) over WebSocket
- Provides same validation quality as local jl4-cli
- Works from any environment with Node.js installed
- No Haskell toolchain required

### Installation

```bash
cd l4/scripts
npm install
```

### Usage

**Basic validation:**

```bash
node validate-cloud.mjs <file.l4>
```

**Custom LSP server:**

```bash
node validate-cloud.mjs --url wss://custom-host.com/lsp <file.l4>
```

**Debug mode:**

```bash
node validate-cloud.mjs --debug <file.l4>
```

### Output Format

The script reports diagnostics in a readable format:

**Success:**

```
✅ Checking succeeded.
No errors or warnings found.
```

**With errors:**

```
Found 2 diagnostic(s):
  Errors: 2
  Warnings: 0
  Info: 0

❌ Error @ file.l4:8:21
  The type of this definition must match its type signature...
```

Exit codes:

- `0`: Validation successful (no errors)
- `1`: Validation failed (errors found) or connection error

### Architecture

The script uses direct LSP-over-WebSocket communication:

1. Opens WebSocket connection to LSP server
2. Sends `initialize` request (JSON-RPC 2.0)
3. Sends `textDocument/didOpen` notification with file content
4. Receives `textDocument/publishDiagnostics` notifications
5. Formats and displays diagnostics

No MCP wrapper is needed - LSP is already a well-defined protocol suitable for direct use.

### Requirements

- Node.js (ESM support)
- `ws` package (WebSocket client)
- Network access to jl4.legalese.com

### Comparison to jl4-cli

| Feature                 | jl4-cli                    | validate-cloud.mjs |
| ----------------------- | -------------------------- | ------------------ |
| Installation            | Requires Haskell toolchain | Just npm install   |
| Speed                   | Faster (local)             | Network latency    |
| Offline                 | Works offline              | Requires internet  |
| Dependencies            | Large (GHC, cabal)         | Small (Node, ws)   |
| Validation              | ✅                         | ✅                 |
| Testing (#EVAL/#ASSERT) | ✅                         | ✅                 |
| Code generation         | ✅                         | ❌                 |

### Troubleshooting

**Connection timeout:**

- Check internet connection
- Verify jl4.legalese.com is accessible: `curl https://jl4.legalese.com/`
- Try with `--debug` flag to see detailed messages

**Parse errors:**

- Ensure file is valid UTF-8
- Check file has .l4 extension

**No diagnostics:**

- File might be valid! Use `#EVAL` statements to see execution results
- Try `--debug` to see what the server is sending
